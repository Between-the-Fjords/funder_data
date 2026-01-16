# clean nematode data

clean_nematode <- function(nematode_raw, nema_weight_raw, feeder_raw, cnp_depht_raw) {
       # get dates from soil coring
       dates <- cnp_depht_raw |>
              filter(block == 2) |>
              distinct(siteID, date) |>
              bind_rows(tibble(
                     siteID = "Ram",
                     date = "2022-09-01"
              ))

       # File containing families group by feeding categories
       feeders <- feeder_raw |>
              clean_names() |>
              mutate(
                     family = tolower(family),
                     functional_group = tolower(functional_group)
              ) |>
              # add missing
              bind_rows(
                     tibble(
                            family = c("unknown_preditor", "unknown_bacterial_feeder", "unknown_plant_feeder"),
                            functional_group = c("predator", "bacteria", "plant")
                     )
              ) |>
              # rename to match microarthropods data
              mutate(functional_group = case_match(
                     functional_group,
                     "plant" ~ "phytophagous",
                     "bacteria" ~ "bacteriophagous",
                     "fungi" ~ "fungivorous",
                     "omnivor" ~ "omnivorous",
                     "predator" ~ "predaceous"
              ))

       # File containing information about the amount of soil used per sample
       # part of the soil is used to calculate soil moisture and convert fresh soil to dry soil (dry/fresh soil ratio)
       # from this fresh soil for extraction can be converted to dry soil from extraction by multiplying by dry wet ratio
       weight <- nema_weight_raw |>
              clean_names() |>
              mutate(
                     siteID = substr(plot_id, 1, 3),
                     blockID = substr(plot_id, 4, 4),
                     treatment = substr(plot_id, 5, 7)
              ) |>
              mutate(treatment = recode(treatment,
                     "BF" = "FB",
                     "BG" = "GB",
                     "FG" = "GF",
                     "BFG" = "FGB"
              )) |>
              mutate(
                     plotID = paste0(siteID, blockID, treatment),
                     blockID = paste0(siteID, blockID)
              ) |>
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
              select(date, siteID, blockID, plotID, treatment, fresh_soil_sample_weight_for_extraction_g:comments) |>
              # UNCLEAR WHAT DIFFERENT WEIGHTS ARE !!!
              rename(
                     fresh_soil = fresh_soil_sample_weight_for_extraction_g,
                     dry_soil = dry_soil_sample_weight_for_extraction_g
              ) |>
              select(siteID:treatment, fresh_soil, dry_soil)

       # File with nematode counts
       nematodes <- nematode_raw |>
              clean_names() |>
              # plot_id has sometimes 1 and 2 (ARH2FGB(2?))
              mutate(
                     comment = if_else(plot_id == "ARH2FGB(2?)", "uncertain if batch 2", comment),
                     plot_id = if_else(plot_id == "ARH2FGB(2?)", "ARH2FGB_2", plot_id),
                     plot_id = str_replace_all(plot_id, "-", "_")
              ) |>
              separate(plot_id, sep = "_", c("plotID", "sample")) |> # warning message, because lot's of NAs for sample
              mutate(sample = replace_na(sample, "1")) |>
              mutate(
                     siteID = str_to_title(substr(plotID, 1, 3)),
                     blockID = substr(plotID, 4, 4),
                     blockID = paste0(siteID, blockID),
                     treatment = substr(plotID, 5, 7),
                     plotID = paste0(blockID, treatment)
              ) |>
              # remove date, because it is date of lab processing
              select(-date) |>
              # add date for field sampling
              left_join(dates, by = "siteID") |>
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
              select(sampling_date = date, siteID, blockID, plotID, treatment, sample, abundance, nematode, zoom:comment) |>
              # make cols numeric
              mutate(
                     unknown = as.numeric(unknown),
                     unknown_predator = as.numeric(unknown_predator),
                     unknown_bacterial_feeder = as.numeric(unknown_bacterial_feeder),
                     psilenchidae = as.numeric(psilenchidae),
                     criconematidae = as.numeric(criconematidae),
                     alaimidae = as.numeric(alaimidae),
                     bastianiidae = as.numeric(bastianiidae),
                     diphterophoridae = as.numeric(diphterophoridae),
                     tylencholaimidae = as.numeric(tylencholaimidae),
                     anguinidae = as.numeric(anguinidae),
                     tripylidae = as.numeric(tripylidae)
              ) |>
              pivot_longer(cols = c("unknown":"tripylidae"), names_to = "family", values_to = "count", values_drop_na = TRUE) |>
              filter(!count == 0) |> # remove 0 that were in the raw data
              # sum per plot including mergin for multiple sampling rounds
              # loosing following cols:
              # nematode (= count from 1-150), zoom (10, 20 or NA),
              # video (numeric value, probably only relevant for machine learning videos),
              # num_merge, and total (empty)
              group_by(sampling_date, siteID, blockID, plotID, treatment, abundance, family, comment) |>
              summarise(count = sum(count)) |>
              # join soil data
              left_join(weight, by = c("siteID", "blockID", "plotID", "treatment")) |>
              # calculate abundance per g soil = count / dry soil weight
              mutate(abundance_per_g = count / dry_soil) |>
              left_join(feeders, by = "family") |>
              ungroup() %>%
              funcabization(dat = ., convert_to = "FunCaB") |>
              select(sampling_date:treatment, functional_group, family, abundance_per_g, count, dry_soil, abundance, comment)
}

# # is this needed???
# nematodes |>
#   ungroup() |>
#   group_by(sampling_date, siteID, blockID, plotID, treatment, abundance, comment) |>
#   mutate(total_abundance = sum(abundance_per_g)) |>
#   ungroup() |>
#   mutate(proportion_per_100g = abundance_per_g / total_abundance * 100)
