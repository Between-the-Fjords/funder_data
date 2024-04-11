# Clean slope and aspect data

clean_slope <- function(slope_raw){

  # Rename columns
  # Recode siteID:s

  slope_raw |>
    select(siteID = site, blockID, plotID, treatment, slope, aspect, comment) |>
    mutate(siteID = recode(siteID,
                           # old name (replace) = valid name (do not change)
                           'Gud' = "Gudmedalen",
                           'Lav' = "Lavisdalen",
                           'Ram' = "Rambera",
                           'Ulv' = "Ulvehaugen",
                           'Skj' = "Skjelingahaugen",
                           'Alr' = "Alrust",
                           'Arh' = "Arhelleren",
                           'Fau' = "Fauske",
                           'Hog' = "Hogsete",
                           'Ovs' = "Ovstedalen",
                           'Vik' = "Vikesland",
                           'Ves' = "Veskre")) |>
    # Remove 3 rows with na in data (Fau2GB, Skj4C and Ulv1B)
    filter(slope != "na") |>
    pivot_longer(cols = c(slope, aspect), names_to = "variable", values_to = "value") |>
    relocate(comment, .after = value) %>%
    funcabization(dat = ., convert_to = "FunCaB")

}
