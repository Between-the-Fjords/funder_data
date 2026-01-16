# Clean point measurements of soil moisture

clean_sm <- function(sm_raw) {
  ### Restructure data ####
  # Rename columns
  # Select and change order of columns
  # Recode siteID:s

  sm_raw |>
    # rename(date = Date, siteID = site, "1" = moisture1, "2" = moisture2, "3" = moisture3, "4" = moisture4) |>
    select(date = Date, time, siteID = site, blockID, plotID, treatment, moisture1:moisture4, weather, comment) |>
    pivot_longer(!c(date, time, siteID, blockID, plotID, treatment, weather, comment), names_to = "measurement", values_to = "value") |>
    mutate(siteID = recode(siteID,
      # old name (replace) = valid name (do not change)
      "Gud" = "Gudmedalen",
      "Lav" = "Lavisdalen",
      "Ram" = "Rambera",
      "Ulv" = "Ulvehaugen",
      "Skj" = "Skjelingahaugen",
      "Alr" = "Alrust",
      "Arh" = "Arhelleren",
      "Fau" = "Fauske",
      "Hog" = "Hogsete",
      "Ovs" = "Ovstedalen",
      "Vik" = "Vikesland",
      "Ves" = "Veskre"
    )) |>
    mutate(
      value = as.numeric(value),
      measurement = as.numeric(str_remove(measurement, "moisture")),
      weather = str_replace(weather, "/|; ", "_")
    ) |>
    relocate(c(measurement, value), .before = weather) |>
    # remove 165 rows where value is NA, because too dry/wet (below detection limit or above water table)
    # Non-existing plots: Fau2GB, Ulv1B
    # No measurements made in site Skjelingahaugen due to bad weather conditions
    filter(!is.na(value)) %>%
    funcabization(dat = ., convert_to = "FunCaB")

  ### Check for duplicate rows
  #   distinct(plotID, measurement, .keep_all = TRUE) # no duplicates
}
