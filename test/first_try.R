library(dplyr)
library(ggplot2)
library(sf)
library(tibble)

# load data ----
# zip codes
# https://hub.arcgis.com/maps/esri-de-content::postleitzahlengebiete-in-deutschland
zip_codes <-
  sf::st_read("./data-raw/PLZ_Gebiete_-7531060512283041383.geojson") |>
  sf::st_make_valid()

zip_codes_sample <-
  zip_codes |>
  dplyr::slice_sample(n = 100) |>
  sf::st_sample(1000) |>
  sf::st_as_sf() |>
  sf::st_join(zip_codes) |>
  dplyr::arrange(plz) |>
  dplyr::mutate(id = 1:dplyr::n()) |>
  dplyr::select(id, plz)

# takes quite long...
zip_codes_reduced <-
  zip_codes_sample |>
  sf::st_drop_geometry() |>
  dplyr::left_join(zip_codes, by = "plz") |>
  dplyr::distinct(plz, note, einwohner, geometry) |>
  sf::st_as_sf()

# municipalities
# https://hub.arcgis.com/maps/60eb682c95f44ba7b10fee66d871859d
municipalities <-
  sf::st_read("./data-raw/Gemeindegrenzen_2022__mit_Einwohnerzahl_-5006860441633717519.geojson") |>
  sf::st_make_valid()

# linking methods ----

# 1. centroid matched
centroid_matched <-
  zip_codes_reduced |>
  sf::st_point_on_surface() |>
  sf::st_join(municipalities) |>
  dplyr::select(plz, einwohner, AGS) |>
  dplyr::distinct() |>
  sf::st_drop_geometry() |>
  dplyr::left_join(municipalities, by = "AGS") |>
  sf::st_as_sf() |>
  dplyr::select(plz, AGS, einwohner, EWZ)

# 2. areal matching
areal_matched <-
  zip_codes_reduced |>
  sf::st_join(municipalities, left = TRUE, largest = TRUE) |>
  dplyr::select(plz, einwohner, AGS) |>
  dplyr::distinct() |>
  sf::st_drop_geometry() |>
  dplyr::left_join(municipalities, by = "AGS") |>
  sf::st_as_sf() |>
  dplyr::select(plz, AGS, einwohner, EWZ)

# centroid_matched$geometry == areal_matched$geometry

# 3. areal interpolation matching
areal_interpolation_matched <-
  sf::st_interpolate_aw(
    municipalities["EWZ"],
    zip_codes_reduced,
    extensive = FALSE
  ) |>
  dplyr::bind_cols(
    zip_codes_reduced |>
      sf::st_drop_geometry() |>
      dplyr::select(plz, einwohner)
  ) |>
  dplyr::select(plz, einwohner, EWZ)

diff_centroid_areal <-
  tibble::tibble(
    `Difference Type` = "Difference Centroid and Areal Matching",
    Difference = centroid_matched$EWZ - areal_matched$EWZ
  )

diff_centroid_interpolated <-
  tibble::tibble(
    `Difference Type` = "Difference Centroid and Areal Interpolation Matching",
    Difference = centroid_matched$EWZ - areal_interpolation_matched$EWZ
  )

diff_areal_interpolated <-
  tibble::tibble(
    `Difference Type` = "Difference Areal and Areal Interpolation Matching",
    Difference = areal_matched$EWZ - areal_interpolation_matched$EWZ
  )

dplyr::bind_rows(
  diff_centroid_areal,
  diff_centroid_interpolated,
  diff_areal_interpolated
) |>
  ggplot(aes(x = Difference)) +
  geom_density() +
  facet_wrap(~`Difference Type`)








