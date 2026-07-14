# clean microarthropods data

clean_microarthropods <- function(microart_raw) {

  microart_raw |>
    mutate(
      # add sampling dates:
      sampling_date = siteID,
      sampling_date = recode_values(
        sampling_date,
        "Alr" ~ "2022-08-24",
        "Vik" ~ "2022-08-22",
        "Hog" ~ "2022-08-23",
        "Fau" ~ "2022-08-25",
        "Ovs" ~ "2022-08-29",
        "Ves" ~ "2022-08-30",
        "Arh" ~ "2022-08-31",
        "Ram" ~ "2022-09-01",
        "Ulv" ~ "2022-09-07",
        "Skj" ~ "2022-09-08",
        "Gud" ~ "2022-09-06",
        "Lav" ~ "2022-09-05"
      ),
      siteID = recode(siteID,
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
      ),
      blockID = paste0(substr(siteID, 1, 3), blockID),
      plotID = paste0(blockID, treatment),
      year = 2022
    ) |>
    rename(Mite_unknownjuvenile = Unknown_mite_juvenile) |>
    pivot_longer(cols = c(Mite_fungivorous:Mite_unknownjuvenile,
                          Collembola_fungivorous,
                          Collembola_predaceous),
                 names_to = "name", values_to = "abundance") |>
    separate(col = name,
             into = c("microarthropods", "feeding_group"),
             sep = "_") |>
    mutate(microarthropods = tolower(microarthropods),
           # simplify feeding group names and harmonise with nematodes:
           feeding_group = recode_values(
             feeding_group,
             "fungivorous" ~ "fungivore",
             "predaceous" ~ "predator",
             "nematophagous" ~ "nematophage",
             "unknownjuveile" ~ "unknown_juvenile")
           ) %>%
    funcabization(dat = ., convert_to = "FunCaB") |>
    select(year, sampling_date, siteID, blockID, treatment, plotID,
           extraction_height, extraction_round, microarthropods, feeding_group,
           abundance, observer, comments = Comments)
}
