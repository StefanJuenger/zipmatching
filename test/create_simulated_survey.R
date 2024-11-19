# Setup ----

# Load necessary libraries
library(dplyr)         # For data manipulation
library(sf)            # For handling spatial (geometric) data
library(tibble)        # For creating and managing tibbles (data frames)
library(tidyr)         # For data tidying and reshaping
library(haven)         # For reading data files (e.g., .dta)
library(corrr)         # For correlation analysis
library(faux)          # For generating random normal data


# Load data ----

## Load zip code data ----

zip_codes <-
  # Load spatial data from GeoPackage file
  sf::st_read("./data-raw/PLZ_Gebiete_7155512659375659703.gpkg") |>
  # Select only relevant columns: zip code and population
  dplyr::select(zip_code = plz, inhabitants_zip_code = einwohner, zip_area = qkm)

## Load municipality data ----

municipalities <-
  # Load spatial data from GeoPackage file
  sf::st_read("./data-raw/Gemeindegrenzen_2022__mit_Einwohnerzahl_4398740898366155627.gpkg")|>
  # Select AGS (municipality code) and population columns
  dplyr::select(ags = AGS, inhabitants_municipality = EWZ, mun_area = KFL)

## Load original survey data ----

survey <- haven::read_dta("J:/Work/GLES/05_Online-Tracking/05_Datenaufbereitung/ZA7714/Version_1-0-0/ZA7714_v1-0-0_RUF_2024-01-15.dta") |>
  mutate(afd_rating = ifelse(t14h < 0 , NA, t14h),
         zip_code =  ifelse(grepl("-93|-99|-97|-98", t71), NA, t71)) |>
  dplyr::select(afd_rating, zip_code)


# Run linking process with original data ----

# The goal of this step is to receive the correlation between the variable of interest
# (here: afd_rating) and the  explanatory variable received by the varying linking
# methods. We use the original correlations to simulate a new variable interest
# with the linking process based a random sample of zip codes.

## Prep data ----

# Create a reduced dataset based on the valid zip codes from the survey data
zip_codes_valid <-
  survey |>
  # Filter the survey data to include only rows with a valid zip code status
  dplyr::filter(zip_code %in% zip_codes$zip_code) |>
  # Perform a left join with the zip_codes dataset on the 'zip_code' column
  dplyr::left_join(zip_codes, ., by = "zip_code") |>
  # Convert the resulting data frame to a simple features (sf) 
  sf::st_as_sf()

## Centroid linking ----

centroid_matched <-
  zip_codes_valid |>
  # Calculate the centroid for each zip code area
  sf::st_point_on_surface() |>
  # Spatially join centroids with municipality data
  sf::st_join(municipalities) |>
  # Select relevant columns
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality, mun_area) |>
  # Remove any duplicate rows
  dplyr::distinct() |>
  # Arrange by zip code for easy viewing
  dplyr::arrange(zip_code)

## Areal linking ----

areal_matched <-
  zip_codes_valid |>
  # Spatial join using the largest overlap municipality for each zip code area
  sf::st_join(municipalities, left = TRUE, largest = TRUE) |>
  # Select relevant columns
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality, mun_area) |>
  # Remove duplicates to ensure each zip code matches one municipality
  dplyr::distinct() |>
  # Sort by zip code
  dplyr::arrange(zip_code)

## Areal interpolation linking ----

# Perform area-weighted interpolation for inhabitants
areal_interpolation_matched <- sf::st_interpolate_aw(
  # Use municipality inhabitants data for interpolation
  municipalities["inhabitants_municipality"],
  # Target zip code areas for the interpolation
  zip_codes_valid,
  # Set to FALSE as population data is not "extensive" (not purely additive)
  extensive = FALSE
) |>
  # Combine interpolated results with original zip code data
  dplyr::bind_cols(
    zip_codes_valid |>
      # Drop geometry to avoid duplication issues in final output
      sf::st_drop_geometry() |>
      # Select only zip code and its inhabitants count
      dplyr::select(zip_code, inhabitants_zip_code)
  ) |>
  # Choose relevant columns for output
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality) |>
  # Ensure unique rows
  dplyr::distinct() |>
  # Sort by zip code
  dplyr::arrange(zip_code)


# Perform area-weighted interpolation for the area
areal_interpolation_matched <- sf::st_interpolate_aw(
  # Use municipality inhabitants data for interpolation
  municipalities["mun_area"],
  # Target zip code areas for the interpolation
  zip_codes_valid,
  # Set to FALSE as population data is not "extensive" (not purely additive)
  extensive = FALSE) |>
  # Combine interpolated results with original zip code data
  dplyr::bind_cols(
    zip_codes_valid |>
      # Drop geometry to avoid duplication issues in final output
      sf::st_drop_geometry() |>
      # Select only zip code and its inhabitants count
      dplyr::select(zip_code, inhabitants_zip_code)
  ) |>
  # Choose relevant columns for output
  dplyr::select(zip_code, mun_area) |>
  # Ensure unique rows
  dplyr::distinct() |>
  # Drop geometry
  sf::st_drop_geometry() |>
  # Join with original
  dplyr::left_join(
    areal_interpolation_matched,
    .,
    by = "zip_code")

## Link data and save correlations and distribution of original data ----
# Create the linked_survey data frame by joining multiple data sources
linked_survey <-
  survey |> 
  # Left join with centroid_matched, dropping geometry information
  dplyr::left_join(centroid_matched |> 
                     sf::st_drop_geometry(), by = "zip_code") |>
  # Rename columns for clarity
  dplyr::rename(cent_inhabitants_mun = inhabitants_municipality,
                cent_area_mun = mun_area) |>
  # Left join with areal_matched, dropping specific columns and geometry
  dplyr::left_join(areal_matched |>
                     sf::st_drop_geometry() |>
                     dplyr::select(-inhabitants_zip_code), by = "zip_code") |>
  # Rename columns for clarity
  dplyr::rename(areal_inhabitants_mun = inhabitants_municipality,
                areal_area_mun = mun_area) |>
  # Left join with areal_interpolation_matched, dropping specific columns and geometry
  dplyr::left_join(areal_interpolation_matched |>
                     sf::st_drop_geometry() |>
                     dplyr::select(-inhabitants_zip_code), by = "zip_code") |>
  # Rename columns for clarity
  dplyr::rename(interpolation_inhabitants_mun = inhabitants_municipality,
                interpolation_area_mun = mun_area)  |>
  # Calculate population densities based on the joined data
  dplyr::mutate(
    cent_pop_dens = cent_inhabitants_mun / cent_area_mun,
    areal_pop_dens = areal_inhabitants_mun / areal_area_mun,
    interpolation_pop_dens = interpolation_inhabitants_mun / interpolation_area_mun
  )

# Calculate correlations between afd_rating and population density variables
correlations <-
  linked_survey |> 
  # Select relevant columns: afd_rating and those containing "pop_dens"
  dplyr::select(afd_rating, contains("pop_dens")) |>
  # Compute pairwise correlations while handling missing values
  corrr::correlate(use = "pairwise.complete.obs")  |>  
  # Select only the terms and their correlation with afd_rating
  dplyr::select(term, afd_rating) |> 
  # Filter out the correlation of afd_rating with itself
  dplyr::filter(term != "afd_rating")

# Loop through each correlation to create dynamic variable names
for (i in 1:nrow(correlations)) {
  term_name <- correlations$term[i]  # Get the name of the term
  value <- correlations$afd_rating[i] # Get the corresponding correlation value
  # Assign the value to a new variable with a modified name
  assign(paste0("cor_", gsub(" ", "_", term_name)), value)
}

# Calculate the mean of afd_rating, ignoring NA values
mean_afd_rating <- mean(linked_survey$afd_rating, na.rm = TRUE)
# Calculate the standard deviation of afd_rating, ignoring NA values
sd_afd_rating <- sd(linked_survey$afd_rating, na.rm = TRUE)

# Create simulated data ----

# Set a seed for random number generation to ensure reproducibility
set.seed(201024)
    
# Set number of cases
num_cases <- 1000

# Create tibble
sim_data <- tibble::tibble(id = seq(1,1000))
    
## Generate random zip codes  ----
    
# Sample zip codes for analysis
zip_codes_sample <-
  zip_codes |>
  # Remove geometry information from the spatial data frame
  sf::st_drop_geometry() |>
  # Select only the 'zip_code' column
  dplyr::select(zip_code) |>
  # Randomly sample 950 unique zip codes
  dplyr::slice_sample(n = 950) |>
  # Join the sampled zip codes back to the original zip_codes data
  # (to ensure validity)
  dplyr::inner_join(zip_codes |>
                      sf::st_drop_geometry() |>
                      dplyr::select(zip_code),
                    by = "zip_code") |>
  # Sample 1118 observations from the joined dataset with replacement
  dplyr::sample_n(num_cases, replace = TRUE) 
    
## Set zip codes to missing and invalid to mirror original data ----

# Calculate the number of zip codes to modify: 1.7% of the total rows
# in zip_codes_sample
n_to_modify <- floor(0.017 * nrow(zip_codes_sample))  

# Randomly select which rows will have their zip codes modified
rows_to_modify <- sample(1:nrow(zip_codes_sample), n_to_modify)

## Update the simulated data data with sampled zip codes and set missing/invalid ----

sim_data <-
  sim_data |>
  # Bind the sampled zip codes to the sim_data data (adds zip_code column)
  dplyr::bind_cols(zip_codes_sample) |>
  # Modify the zip_code column
  dplyr::mutate(
    # set rows invalid by appending a random digit to the existing zip code
    zip_code = ifelse(row_number() %in% rows_to_modify,
                      paste0(zip_code, sample(0:9, n_to_modify, replace = TRUE)),
                      zip_code),
    # Randomly set 7% of the zip_code values to NA
    zip_code = ifelse(runif(n()) < 0.07, NA, zip_code)
  ) 

## Prep data ----
# Repeat Steps from before

# Create a reduced dataset based on the valid zip codes from the simulated
# survey data
zip_codes_valid <-
sim_data |>
# Filter the imulated survey data to include only rows with a valid zip 
# code status
dplyr::filter(zip_code %in% zip_codes$zip_code) |>
# Perform a left join with the zip_codes dataset on the 'zip_code' column
dplyr::left_join(zip_codes, ., by = "zip_code") |>
# Convert the resulting data frame to a simple features (sf) 
sf::st_as_sf()

## Centroid linking ----

centroid_matched <-
  zip_codes_valid |>
  # Calculate the centroid for each zip code area
  sf::st_point_on_surface() |>
  # Spatially join centroids with municipality data
  sf::st_join(municipalities) |>
  # Select relevant columns
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality, mun_area) |>
  # Remove any duplicate rows
  dplyr::distinct() |>
  # Arrange by zip code for easy viewing
  dplyr::arrange(zip_code)

## Areal linking ----

areal_matched <-
  zip_codes_valid |>
  # Spatial join using the largest overlap municipality for each zip code area
  sf::st_join(municipalities, left = TRUE, largest = TRUE) |>
  # Select relevant columns
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality, mun_area) |>
  # Remove duplicates to ensure each zip code matches one municipality
  dplyr::distinct() |>
  # Sort by zip code
  dplyr::arrange(zip_code)

## Areal interpolation linking ----

# Perform area-weighted interpolation for inhabitants
areal_interpolation_matched <- 
  sf::st_interpolate_aw(
  # Use municipality inhabitants data for interpolation
  municipalities["inhabitants_municipality"],
  # Target zip code areas for the interpolation
  zip_codes_valid,
  # Set to FALSE as population data is not "extensive" (not purely additive)
  extensive = FALSE
  ) |>
  # Combine interpolated results with original zip code data
  dplyr::bind_cols(
    zip_codes_valid |>
      # Drop geometry to avoid duplication issues in final output
      sf::st_drop_geometry() |>
      # Select only zip code and its inhabitants count
      dplyr::select(zip_code, inhabitants_zip_code)
  ) |>
  # Choose relevant columns for output
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality) |>
  # Ensure unique rows
  dplyr::distinct() |>
  # Sort by zip code
  dplyr::arrange(zip_code)
    
    
# Perform area-weighted interpolation for area
areal_interpolation_matched <- 
  sf::st_interpolate_aw(
  # Use municipality inhabitants data for interpolation
  municipalities["mun_area"],
  # Target zip code areas for the interpolation
  zip_codes_valid,
  # Set to FALSE as population data is not "extensive" (not purely additive)
  extensive = FALSE
) |>
  # Combine interpolated results with original zip code data
  dplyr::bind_cols(
    zip_codes_valid |>
      # Drop geometry to avoid duplication issues in final output
      sf::st_drop_geometry() |>
      # Select only zip code and its inhabitants count
      dplyr::select(zip_code, inhabitants_zip_code)
  ) |>
  # Choose relevant columns for output
  dplyr::select(zip_code, mun_area) |>
  # Ensure unique rows
  dplyr::distinct() |>
  # Drop geometry
  sf::st_drop_geometry() |>
  # Join with original
  dplyr::left_join(
    areal_interpolation_matched,
    .,
    by = "zip_code")
    
## Link data ----

linked_sim_survey <-
  sim_data %>% 
  # Left join with centroid_matched, dropping geometry information
  dplyr::left_join(centroid_matched |>
                     sf::st_drop_geometry(), by = "zip_code") |>
  # Rename columns for clarity
  dplyr::rename(cent_inhabitants_mun = inhabitants_municipality,
                cent_area_mun = mun_area) |>
  # Left join with areal_matched, dropping specific columns and geometry
  dplyr::left_join(areal_matched |>
                     sf::st_drop_geometry() |>
                     dplyr::select(-inhabitants_zip_code), by = "zip_code") |>
  # Rename columns for clarity
  dplyr::rename(areal_inhabitants_mun = inhabitants_municipality,
                areal_area_mun = mun_area) |>
  # Left join with areal_interpolation_matched, dropping specific columns and geometry
  dplyr::left_join(areal_interpolation_matched |>
                     sf::st_drop_geometry() |>
                     dplyr::select(-inhabitants_zip_code), by = "zip_code") |>
  # Rename columns for clarity
  dplyr::rename(interpolation_inhabitants_mun = inhabitants_municipality,
                interpolation_area_mun = mun_area) |>
  # Calculate population densities based on the joined data
  dplyr::mutate(
    cent_pop_dens = cent_inhabitants_mun / cent_area_mun,
    areal_pop_dens = areal_inhabitants_mun / areal_area_mun,
    interpolation_pop_dens = interpolation_inhabitants_mun / interpolation_area_mun
  )

## Simulate variable of interest ----

# Create a new data frame, densities, with selected density variables, removing NA values
densities <-  
  linked_sim_survey |> 
  dplyr::select(cent_pop_dens, areal_pop_dens, interpolation_pop_dens) |> 
  tidyr::drop_na()

# Simulate dependent variable using the densities and correlation parameters from original survey data

sim_dv <- 
  faux::rnorm_pre(densities, 
                  mu = mean_afd_rating, 
                  sd = sd_afd_rating, 
                  r = c(cor_cent_pop_dens, cor_areal_pop_dens, cor_interpolation_pop_dens),
                  empirical = TRUE) |>
  # Convert simulated data to a tibble
  tibble::as_tibble() |>  
  # Rescale simulated values to a range of [1, 11]
  dplyr::mutate(rescaled_value = 1 + (value - min(value)) * (11 - 1) / (max(value) - min(value)))

# Prepare the final simulated data by combining original sim_data with simulated values
sim_data <- 
  sim_data |> 
  # Filter sim_data to include only relevant zip codes
  dplyr::filter(zip_code %in% zip_codes$zip_code) |> 
  # Bind simulated values as a new column, afd_rating
  dplyr::bind_cols(
    afd_rating = sim_dv$rescaled_value
  ) |> 
  # Append rows from sim_data that do not match the zip codes
  dplyr::bind_rows(
    sim_data |> filter(!zip_code %in% zip_codes$zip_code)
  ) |> 
  # Replace NA afd_rating values with random samples from 1 to 11
  dplyr::mutate(
    afd_rating = ifelse(is.na(afd_rating), 
                        sample(1:11, sum(is.na(afd_rating)), replace = TRUE), 
                        afd_rating)
  )

## Save data ----

# Save the final simulated data frame to an RDS file
saveRDS(sim_data, "./data-raw/simulated_survey_data.rds")
