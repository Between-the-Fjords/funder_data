# clean microarthropods data

clean_microarthropods <- function(microart_raw, cnp_depht_raw){

  # get dates from soil coring
  dates <- cnp_depht_raw |>
    filter(block == 2) |>
    distinct(siteID, date)

  microart_raw |>
    left_join(dates, by = "siteID") |>
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
                         'Ves' = "Veskre"),
         blockID = paste0(substr(siteID,1,3),blockID),
         plotID = paste0(blockID,treatment),
         year = 2022) |>
    rename(Mite_unknownjuvenile = Unknown_mite_juvenile) |>
    pivot_longer(cols = c(Mite_fungivorous:Mite_unknownjuvenile, Collembola_fungivorous, Collembola_predaceous), names_to = "name", values_to = "abundance") |>
    separate(col = name, into = c("microarthropods", "functional_group"), sep = "_") |>
    mutate(microarthropods = tolower(microarthropods)) |>
    select(year, sampling_date = date, siteID, blockID, treatment, plotID, extraction_height, extraction_round, microarthropods, functional_group, abundance, observer, comments = Comments)

}



