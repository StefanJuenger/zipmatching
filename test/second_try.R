# Load required libraries for data manipulation, spatial data handling, and
# visualization
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(sf)          # For handling spatial (geometric) data
library(tibble)      # For creating and managing tibbles (data frames)

# Load zip code data ----
# Read the zip code areas from a GeoPackage file
# Source: https://hub.arcgis.com/maps/esri-de-content::postleitzahlengebiete-in-deutschland
zip_codes <-
  # Load spatial data from GeoPackage
  sf::st_read("./data-raw/PLZ_Gebiete_7155512659375659703.gpkg") |>
  # Select only relevant columns: zip code and population
  dplyr::select(zip_code = plz, inhabitants_zip_code = einwohner)

# Sample points within zip code areas ----
# Randomly sample points within each zip code area. Note: This can take a long
# time due to the number of points sampled.
points_in_zip_codes <-
  zip_codes |>
  # Sample 1000 points per area
  sf::st_sample(size = c(1000, 1000), progress = TRUE, exact = FALSE) |>
  # Convert sampled points to an sf object
  sf::st_as_sf() |>
  # Spatially join sampled points with zip code data
  sf::st_join(zip_codes) |>
  # Arrange points by zip code for easier viewing
  dplyr::arrange(zip_code) |>
  # Assign unique IDs to each sampled point
  dplyr::mutate(id = 1:dplyr::n()) |>
  # Select only the ID and zip code columns
  dplyr::select(id, zip_code)

# Load municipality data ----
# Read the municipality boundaries and population from a GeoPackage file
# Source: https://hub.arcgis.com/maps/60eb682c95f44ba7b10fee66d871859d
municipalities <-
  # Load spatial data from GeoPackage
  sf::st_read("./data-raw/Gemeindegrenzen_2022__mit_Einwohnerzahl_4398740898366155627.gpkg") |>
  # Select AGS (municipality code) and population columns
  dplyr::select(ags = AGS, inhabitants_municipality = EWZ)

# Join sampled points with municipality inhabitants data ----
# Spatially join the sampled points data with municipality data to match each
# point with actual inhabitants data from municipalities
points_with_real_inhabitants_municipality <-
  points_in_zip_codes |>
  # Join points with municipality data
  sf::st_join(municipalities) |>
  # Rename population column for clarity
  dplyr::rename(real_inhabitants_municipality = inhabitants_municipality)


# Linking methods ----
# Different methods to link or match data between zip code areas and
# municipalities

# 1. Centroid matched method ----
# This method uses the centroid (center point) of each zip code area to find the
# corresponding municipality.
# It assumes that the center point accurately represents the zip code's location
# in terms of municipality boundaries.
centroid_matched <-
  zip_codes |>
  # Calculate the centroid for each zip code area
  sf::st_point_on_surface() |>
  # Spatially join centroids with municipality data
  sf::st_join(municipalities) |>
  # Select relevant columns
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality) |>
  # Remove any duplicate rows
  dplyr::distinct() |>
  # Arrange by zip code for easy viewing
  dplyr::arrange(zip_code)

# 2. Areal matching method ----
# This method assigns each zip code area to the municipality in which the
# majority of its area lies.
# Useful when a zip code area overlaps multiple municipalities.
areal_matched <-
  zip_codes |>
  # Spatial join using the largest overlap municipality for each zip code area
  sf::st_join(municipalities, left = TRUE, largest = TRUE) |>
  # Select relevant columns
  dplyr::select(zip_code, inhabitants_zip_code, inhabitants_municipality) |>
  # Remove duplicates to ensure each zip code matches one municipality
  dplyr::distinct() |>
  # Sort by zip code
  dplyr::arrange(zip_code)

# 3. Areal interpolation matching method ----
# This method uses areal interpolation to distribute municipality inhabitants
# data proportionally across overlapping zip code areas.
# It estimates the inhabitants for each zip code based on the proportion of its
# area that overlaps with each municipality.
areal_interpolation_matched <-
  sf::st_interpolate_aw(
    # Use municipality inhabitants data for interpolation
    municipalities["inhabitants_municipality"],
    # Target zip code areas for the interpolation
    zip_codes,
    # Set to FALSE as population data is not "extensive" (not purely additive)
    extensive = FALSE
  ) |>
  # Combine interpolated results with original zip code data
  dplyr::bind_cols(
    zip_codes |>
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

# Calculate the differences between real and estimated inhabitants ----
# This section calculates how much the estimated number of inhabitants differs
# from the actual number of inhabitants
# for each method: Centroid Matching, Areal Matching, and Areal Interpolation
# Matching.

# 1. Difference with Centroid Matching ----
diff_real_centroid <-
  # Perform a left join between the real data and centroid-matched data
  dplyr::left_join(
    # The real inhabitants data joined to points
    points_with_real_inhabitants_municipality,
    # Centroid matched data (drop the geometry for non-spatial comparison)
    centroid_matched |>
      # Drop geometry column, keeping only tabular data
      sf::st_drop_geometry()
  ) |>
  # Create new columns for the differences
  dplyr::mutate(
    # Add a column indicating the type of comparison
    `Type` = "Difference with Centroid Matching",
    # Calculate the difference in inhabitants
    Difference = real_inhabitants_municipality - inhabitants_municipality
  )

# 2. Difference with Areal Matching ----
diff_real_areal <-
  # Left join between real inhabitants data and areal-matched data
  dplyr::left_join(
    # The real inhabitants data
    points_with_real_inhabitants_municipality,
    # Areal matched data (drop geometry for comparison)
    areal_matched |>
      sf::st_drop_geometry()
  ) |>
  # Create new columns
  dplyr::mutate(
    # Indicate the type of comparison
    `Type` = "Difference with Areal Matching",
    # Calculate difference between real and estimated inhabitants
    Difference = real_inhabitants_municipality - inhabitants_municipality
  )

# 3. Difference with Areal Interpolation Matching ----
diff_real_areal_interpolation <-
  # Join real inhabitants data with areal interpolation-matched data
  dplyr::left_join(
    # The real inhabitants data
    points_with_real_inhabitants_municipality,
    # Areal interpolation matched data (without geometry)
    areal_interpolation_matched |>
      sf::st_drop_geometry()
  ) |>
  # Create new columns
  dplyr::mutate(
    # Indicate the comparison type
    `Type` = "Difference with Areal Interpolation Matching",
    # Calculate the difference between real and interpolated inhabitants
    Difference = real_inhabitants_municipality - inhabitants_municipality
  ) |>
  # Ensure unique rows to avoid duplication
  dplyr::distinct()

# Combine all differences into one data frame for analysis ----
# This section combines the differences calculated for each method (Centroid,
# Areal, Areal Interpolation) and performs summary statistics to evaluate
# accuracy and consistency of each method.
differences <-
  # Combine rows from all difference data frames into one
  dplyr::bind_rows(
    # Difference data from Centroid Matching
    diff_real_centroid,
    # Difference data from Areal Matching
    diff_real_areal,
    # Difference data from Areal Interpolation Matching
    diff_real_areal_interpolation
  ) |>
  # Drop spatial geometry for non-spatial analysis
  sf::st_drop_geometry() |>
  # Group by matching method type (Centroid, Areal, etc.)
  dplyr::group_by(Type) |>
  # Add indicators for accuracy evaluation
  dplyr::mutate(
    # Binary indicator if Difference is exactly 0 (perfect match)
    correct = ifelse(Difference == 0, 1, 0),
    # Indicator if Difference within ±500 inhabitants
    more_or_less_correct = ifelse(Difference > -500 & Difference < 500, 1, 0)
  ) |>
  # Summarize the data for each matching method
  dplyr::summarize(
    # Median of the Difference column
    median = median(Difference, na.rm = TRUE),
    # Mean of the Difference column
    mean = mean(Difference, na.rm = TRUE),
    # Minimum difference
    min = min(Difference, na.rm = TRUE),
    # Maximum difference
    max = max(Difference, na.rm = TRUE),
    # Standard deviation of the differences
    sd = sd(Difference, na.rm = TRUE),
    # Variance of differences divided by 1000 for scaling
    var1000 = var(Difference, na.rm = TRUE) / 1000,
    # Interquartile range (IQR) of differences
    iqr = IQR(Difference, na.rm = TRUE),
    # Proportion of exact matches (where Difference = 0)
    prop_correct = mean(correct, na.rm = TRUE),
    # Proportion of matches within ±500 inhabitants
    prop_more_or_less_correct = mean(more_or_less_correct, na.rm = TRUE)
  )

# Calculate the differences between inhabitants estimated by Centroid Matching and Areal Matching ----
diff_centroid_areal <-
  # Create a new tibble (data frame) to store the differences
  tibble::tibble(
    # Assign a label for the type of difference calculated
    `Type` = "Difference Centroid and Areal Matching",
    # Calculate the difference in inhabitants between the two matching methods
    Difference = centroid_matched$inhabitants_municipality -
      # Subtract Areal Matching inhabitants from Centroid Matching
      areal_matched$inhabitants_municipality
  )

# Calculate the differences between inhabitants estimated by Centroid Matching and Areal Interpolation Matching ----
diff_centroid_interpolated <-
  # Create a new tibble to store the differences
  tibble::tibble(
    # Label for the difference type
    `Type` = "Difference Centroid and Areal Interpolation Matching",
    # Calculate the difference for this matching comparison
    Difference = centroid_matched$inhabitants_municipality -
      # Subtract Areal Interpolation inhabitants from Centroid Matching
      areal_interpolation_matched$inhabitants_municipality
  )

# Calculate the differences between inhabitants estimated by Areal Matching and Areal Interpolation Matching ----
diff_areal_interpolated <-
  # Create a new tibble to store the differences
  tibble::tibble(
    # Label for the difference type
    `Type` = "Difference Areal and Areal Interpolation Matching",
    # Calculate the difference in inhabitants between the two methods
    Difference = areal_matched$inhabitants_municipality -
      # Subtract Areal Interpolation inhabitants from Areal Matching
      areal_interpolation_matched$inhabitants_municipality
  )



# Aggregate differences at the zip code level ----
# This process calculates the mean difference for each matching method
# (Centroid, Areal, Areal Interpolation) by zip code, and then determines the
# method with the smallest absolute mean difference per zip code.
diff_real_zip_code_aggregated <-
  diff_real_centroid |>
  # Group data by zip code
  dplyr::group_by(zip_code) |>
  # Drop geometry for non-spatial operations
  sf::st_drop_geometry() |>
  # Summarize by calculating mean difference
  dplyr::summarize(
    # Mean difference for centroid matching method
    mean_difference_centroid = mean(Difference, na.rm = TRUE)
  ) |>
  # Join with the areal matching differences
  dplyr::left_join(
    diff_real_areal |>
      dplyr::group_by(zip_code) |>
      sf::st_drop_geometry() |>
      dplyr::summarize(
        # Mean difference for areal matching method
        mean_difference_areal = mean(Difference, na.rm = TRUE)
      )
  ) |>
  # Join with the areal interpolation matching differences
  dplyr::left_join(
    diff_real_areal_interpolation |>
      dplyr::group_by(zip_code) |>
      sf::st_drop_geometry() |>
      dplyr::summarize(
        # Mean difference for areal interpolation
        mean_difference_areal_interpolation = mean(Difference, na.rm = TRUE)
      )
  )

# Calculate absolute mean differences ----
diff_real_zip_code_aggregated <-
  diff_real_zip_code_aggregated |>
  # Create columns for absolute mean differences
  dplyr::mutate(
    # Absolute value of mean differences (centroid method)
    mean_difference_centroid_abs = abs(mean_difference_centroid),
    # Absolute mean difference for areal method
    mean_difference_areal_abs = abs(mean_difference_areal),
    # Absolute mean difference for areal interpolation
    mean_difference_areal_interpolation_abs =
      abs(mean_difference_areal_interpolation)
  )

# Identify the method with the smallest absolute difference per zip code ----
diff_real_zip_code_aggregated <-
  diff_real_zip_code_aggregated |>
  # Use bind_cols to add the method with minimum difference
  dplyr::bind_cols(
    # Find column name with minimum absolute difference
    min_method = names(diff_real_zip_code_aggregated[-c(1:4)])[
      # Inverse to get min absolute values, excluding grouping cols
      max.col(-diff_real_zip_code_aggregated[-c(1:4)])
    ]
  ) |>
  # Adjust cases with ties for consistency
  dplyr::mutate(
    min_method = ifelse(
      # Tie condition check
      mean_difference_centroid_abs == mean_difference_areal_interpolation_abs &
        mean_difference_centroid_abs == mean_difference_areal_abs,
      # Set to "centroid" if all methods tie
      "mean_difference_centroid_abs",
      min_method
    )
  ) |>
  # Remove intermediate absolute difference columns
  dplyr::select(-contains("abs"))

# Join with zip code geometry and assign codes for each method ----
diff_real_zip_code_aggregated <-
  diff_real_zip_code_aggregated |>
  # Join back with zip code spatial data
  dplyr::left_join(zip_codes) |>
  # Convert back to an sf object for spatial analysis
  sf::st_as_sf() |>
  # Create a code for each method for easier visualization
  dplyr::mutate(
    min_method_code = dplyr::case_when(
      # Assign code 1 for centroid method
      min_method == "mean_difference_centroid_abs" ~ 1,
      # Code 2 for areal method
      min_method == "mean_difference_areal_abs" ~ 2,
      # Code 3 for areal interpolation method
      min_method == "mean_difference_areal_interpolation_abs" ~ 3
    )
  ) |>
  dplyr::mutate(
    difference = dplyr::case_when(
      min_method == "mean_difference_centroid_abs" ~ mean_difference_centroid,
      min_method == "mean_difference_areal_abs" ~ mean_difference_areal,
      min_method == "mean_difference_areal_interpolation_abs" ~
        mean_difference_areal_interpolation,
      TRUE ~ NA
    )
  )

# Create a histogram to visualize the distribution of differences for each matching method ----
difference_histogram <-
  # Combine differences from all matching methods into a single data frame
  dplyr::bind_rows(
    # Centroid method differences
    diff_real_centroid,
    # Areal method differences
    diff_real_areal,
    # Areal interpolation method differences
    diff_real_areal_interpolation
  ) |>
  # Remove spatial geometry for visualization purposes
  sf::st_drop_geometry() |>
  # Initialize ggplot with Difference as x-axis variable
  ggplot(aes(x = Difference)) +
  # Add histogram with 10 bins
  geom_histogram(bins = 10) +
  # Limit x-axis for a clear view of the distribution
  xlim(-2500000, 2500000) +
  # Create a separate histogram for each matching method type
  facet_wrap(~`Type`)

# Save the histogram plot to a file ----
ggplot2::ggsave(
  # Specify the output file path and name
  "./test/difference_histogram.png",
  # Reference the plot created above
  difference_histogram,
  # Set high resolution for clarity in saved image
  dpi = 600
)

# Create density plots to compare scaled differences across matching methods ----
difference_densities <-
  # Combine pairwise differences for each matching method
  dplyr::bind_rows(
    # Centroid and Areal method differences
    diff_centroid_areal,
    # Centroid and Interpolated method differences
    diff_centroid_interpolated,
    # Areal and Interpolated method differences
    diff_areal_interpolated
  ) |>
  # Group data by matching method type
  dplyr::group_by(`Type`) |>
  # Scale differences for comparability across methods
  dplyr::mutate(Difference = scale(Difference)) |>
  # Initialize ggplot with Difference as x-axis variable
  ggplot(aes(x = Difference)) +
  # Create density plot for distribution visualization
  geom_density() +
  # Set x-axis limits to focus on main data range
  xlim(-10, 10) +
  # Separate density plots by type of matching method
  facet_wrap(~`Type`)

# Save the density plot to a file ----
ggplot2::ggsave(
  # Specify output file path and name
  "./test/difference_densities.png",
  # Reference the plot created above
  difference_densities,
  # Set high resolution for clear image output
  dpi = 600
)

# Create a map to visualize which method minimizes the difference for each zip code ----
diff_real_zip_code_aggregated_map <-
  # Initialize ggplot for map
  ggplot() +
  # Add map layers with fill representing the best matching method
  geom_sf(
    data = diff_real_zip_code_aggregated, aes(fill = min_method), lwd = 0
  ) +
  # Use viridis color scale to differentiate methods
  scale_fill_viridis_d()

# Save the map plot to a file ----
ggplot2::ggsave(
  # Specify file name and output path
  "./test/diff_real_zip_code_aggregated_map.png",
  # Reference the map plot
  diff_real_zip_code_aggregated_map,
  # High resolution for clear saved image
  dpi = 600
)

diff_real_zip_code_aggregated_map_differences <-
  # Initialize ggplot for map
  ggplot() +
  # Add map layers with fill representing the difference
  geom_sf(
    data = diff_real_zip_code_aggregated, aes(fill = difference), lwd = 0
  ) +
  # Use viridis color scale to differentiate differences
  scale_fill_viridis_c()

# Save the map plot to a file ----
ggplot2::ggsave(
  # Specify file name and output path
  "./test/diff_real_zip_code_aggregated_map_differences.png",
  # Reference the map plot
  diff_real_zip_code_aggregated_map_differences,
  # High resolution for clear saved image
  dpi = 600
)
