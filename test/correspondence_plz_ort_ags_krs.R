library(dplyr)
library(readr)
library(stringr)


correspondence_plz_ort_ags_krs <-
  readr::read_delim(
    "./data-raw/300003780_5500.plz/plz/de/plz-ags.txt",
    delim = ";",
    locale = locale('de', encoding = 'ISO8859-1')
  ) |>
  dplyr::transmute(
    PLZ = PLZ,
    ORT = ONAME,
    AGS = PLZ_KGS,
    KRS = stringr::str_sub(AGS, 1, 5),
    SOURCE = "© Deutsche Post Direkt GmbH & © BKG (2024) dl-de/by-2-0 (Daten verändert)"
  )

readr::write_csv(
  correspondence_plz_ort_ags_krs,
  "./data-raw/correspondence_plz_ort_ags_krs.csv"
  )
