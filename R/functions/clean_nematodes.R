# clean nematode data

clean_nematode <- function(nematode_raw, nema_weight_raw, feeder_raw, cnp_depht_raw){

  # get dates from soil coring
  dates <- cnp_depht_raw |>
    filter(block == 2) |>
    distinct(siteID, date) |>
    bind_rows(tibble(siteID = "Ram",
                     date = "2022-09-01"))

  # File containing families group by feeding categories
  feeders <- feeder_raw |>
    clean_names() |>
    mutate(family = tolower(family),
           functional_group = tolower(functional_group)) |>
    # add missing
    bind_rows(
      tibble(family = c("unknown_preditor",
                        "unknown_bacterial_feeder",
                        "unknown_plant_feeder"),
             functional_group = c("predator",
                                  "bacteria",
                                  "plant"))
    ) |>
    # rename to match microarthropods data
    mutate(functional_group = case_match(functional_group,
                                         "plant" ~ "phytophagous",
                                         "bacteria" ~ "bacteriophagous",
                                         "fungi" ~ "fungivorous",
                                         "omnivor" ~ "omnivorous",
                                         "predator" ~ "predaceous"))

  # File containing information about the amount of soil used per sample
  # part of the soil is used to calculate soil moisture and convert fresh soil to dry soil (dry/fresh soil ratio)
  # from this fresh soil for extraction can be converted to dry soil from extraction by multiplying by dry wet ratio
  weight <- nema_weight_raw |>
    clean_names() |>
    mutate(siteID = substr(plot_id, 1, 3),
           blockID = substr(plot_id, 4, 4),
           treatment = substr(plot_id, 5, 7)) |>
    mutate(treatment = recode(treatment,
                              'BF' = 'FB',
                              'BG' = 'GB',
                              'FG' = 'GF',
                              'BFG' = 'FGB')) |>
    mutate(plotID = paste0(siteID, blockID, treatment),
           blockID = paste0(siteID, blockID)) |>
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
    select(date, siteID, blockID, plotID, treatment, fresh_soil_sample_weight_for_extraction_g:comments) |>
    # UNCLEAR WHAT DIFFERENT WEIGHTS ARE !!!
    rename(fresh_soil = fresh_soil_sample_weight_for_extraction_g,
           dry_soil = dry_soil_sample_weight_for_extraction_g) |>
    select(siteID:treatment,fresh_soil,dry_soil)

  # File with nematode counts
  nematodes <- nematode_raw |>
    clean_names() |>
    # plot_id has sometimes 1 and 2 (ARH2FGB(2?))
    mutate(comment = if_else(plot_id == "ARH2FGB(2?)", "uncertain if batch 2", comment),
           plot_id = if_else(plot_id == "ARH2FGB(2?)", "ARH2FGB_2", plot_id),
           plot_id = str_replace_all(plot_id, "-", "_")) |>
    separate(plot_id, sep = "_", c("plotID","sample")) |> # warning message, because lot's of NAs for sample
    mutate(sample = replace_na(sample, "1")) |>
    mutate (siteID = str_to_title(substr(plotID, 1, 3)),
            blockID = substr(plotID, 4, 4),
            blockID = paste0(siteID, blockID),
            treatment = substr(plotID, 5, 7),
            plotID = paste0(blockID, treatment)) |>
    # remove date, because it is date of lab processing
    select(-date) |>
    # add date for field sampling
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
                           'Ves' = "Veskre")) |>
    select(sampling_date = date, siteID, blockID, plotID, treatment, sample, abundance, nematode, zoom:comment) |>
    # make cols numeric
    mutate(unknown = as.numeric(unknown),
           unknown_predator = as.numeric(unknown_predator),
           unknown_bacterial_feeder = as.numeric(unknown_bacterial_feeder),
           psilenchidae = as.numeric(psilenchidae),
           criconematidae = as.numeric(criconematidae),
           alaimidae = as.numeric(alaimidae),
           bastianiidae = as.numeric(bastianiidae),
           diphterophoridae = as.numeric(diphterophoridae),
           tylencholaimidae = as.numeric(tylencholaimidae),
           anguinidae = as.numeric(anguinidae),
           tripylidae = as.numeric(tripylidae)) |>
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
    ungroup() |>
    select(sampling_date:treatment, functional_group, family, abundance_per_g, count, dry_soil, abundance, comment)

}

# # is this needed???
# nematodes |>
#   ungroup() |>
#   group_by(sampling_date, siteID, blockID, plotID, treatment, abundance, comment) |>
#   mutate(total_abundance = sum(abundance_per_g)) |>
#   ungroup() |>
#   mutate(proportion_per_100g = abundance_per_g / total_abundance * 100)


cleaning_nematodes <- function(families, sample_weights) {

  # a few samples had a lot of nematodes extrcated, and up to 3 Falcon tubes
  # had to be used to store them all (Arh1FB: 2 tubes. Arh1GB: 2 tubes.
  # Arh2FGB: 2 tubes. Ves1FB: 2 tubes. Ves1GB: 2 tubes. Ves1GF: 2 tubes.
  # Ves2FB: 2 tubes. Ves2FGB: 2 tubes. Ves2GB: 3 tubes. Ves2GF: 2 tubes.
  # this section takes care of summing total
  # extracted nematodes by sample
  total_nematodes_extracted_summed_by_plotID <- families |>
    rename(plotID = plot_id,
           total_extracted_nematodes = abundance) |>
    # fix sample names to merge tubes from the same samples
    mutate(
      plotID = substr(plotID, 1, 7),
      plotID = sub(x = plotID, "_", ""),
      siteID = str_to_title(substr(plotID, 1, 3)),
      blockID = str_to_title(substr(plotID, 1, 4)),
      treatment = substr(plotID, 5, 7),
      plotID = paste0(blockID, treatment)
    ) |>
    select(plotID, total_extracted_nematodes) |>
    distinct() |>
    mutate(total_extracted_nematodes = sum(total_extracted_nematodes), .by = plotID) |>
    distinct()

  # this datasheet contains nematode sample weights
  soil_weights <- sample_weights |>
    clean_names() |>
    rename(plotID = plot_id) |>
    mutate(
      blockID = substr(plotID, 1, 4),
      treatment = substr(plotID, 5, 7),
      # correct wrongly named PFG removal treatments
      treatment = recode_values(
        treatment,
        "C" ~ "C",
        "BF" ~ "FB",
        "BG" ~ "GB",
        "FG" ~ "GF",
        "FGB" ~ "FGB",
        default = treatment
      ),
      plotID = paste0(blockID, treatment)
    ) |>
    select(plotID, dry_soil_sample_weight_for_extraction_g)

  # this dataframe contains nematode family feeding groups after
  # Yeates et al. 1993 (PMID: 19279775)
  feeding_groups <- data.frame(
    family = c(
      "dolichodoridae",
      "paratylenchidae",
      "pratylenchidae",
      "tylenchidae",
      "mononchidae",
      "hoplolaimidae",
      "dorylaimoidea",
      "aphelenchoididae",
      "prismatolaimidae",
      "cephalobidae",
      "criconematidae",
      "hemicycliophoridae",
      "psilenchidae",
      "ecphyadophoridae",
      "plectidae",
      "rhabditidae",
      "achromadoridae",
      "alaimidae",
      "bastianiidae",
      "bunonematidae",
      "desmodoridae",
      "diplogasteridae",
      "monhysteridae",
      "neodiplogasteridae",
      "odontolaimidae",
      "panagrolaimidae",
      "teratocephalidae",
      "metateratocephalidae",
      "aphelenchidae",
      "diphtherophoridae",
      "tylencholaimidae",
      "anguinidae",
      "tripylidae",
      "unknown_bacterial_feeder",
      "unknown_plant_feeder",
      "unknown_predator"
    ),
    feeding_group = c(
      "plant_feeder",
      "plant_feeder",
      "plant_feeder",
      "plant_feeder",
      "predator",
      "plant_feeder",
      "omnivore",
      "fungivore",
      "bacterivore",
      "bacterivore",
      "plant_feeder",
      "plant_feeder",
      "plant_feeder",
      "plant_feeder",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "omnivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "bacterivore",
      "fungivore",
      "fungivore",
      "fungivore",
      "fungivore",
      "predator",
      "bacterivore",
      "plant_feeder",
      "predator"
    ),
    stringsAsFactors = FALSE
  )

  # this dataset will contain the total number of extracted nematodes per sample
  # and the abundance of each identified family per sample
  families |>
    rename(
      plotID = plot_id,
      total_extracted_nematodes = abundance
    ) |>
    # fix sample names in order to merge samples that "needed two tubes"
    mutate(
      plotID = substr(plotID, 1, 7),
      plotID = sub(x = plotID, "_", "")
    ) |>
    # add basic FUNDER data columns
    mutate(
      year = "2022",
      siteID = str_to_title(substr(plotID, 1, 3)),
      blockID = str_to_title(substr(plotID, 1, 4)),
      treatment = substr(plotID, 5, 7),
      plotID = paste0(blockID, treatment)
    ) |>
    select(-c(nematode, date, zoom, video, num_merge, comment, total, total_extracted_nematodes)) |>
    # pivot to long format
    pivot_longer(cols = unknown:tripylidae, names_to = "family", values_to = "abundance") |>
    # convert abundance data to numeric
    mutate(
      abundance = recode_values(
        abundance,
        NA ~ 0,
        0 ~ 0,
        1 ~ 1,
        2 ~ 2,
        3 ~ 3
      ),
      abundance = as.numeric(abundance)
    ) |>
    # sum abundance per family by sample
    mutate(abundance = sum(abundance), .by = c(plotID, family)) |>
    # get rid of redundant rows
    distinct() |>
    # add the total number of extracted nematodes summed by plotID
    left_join(total_nematodes_extracted_summed_by_plotID, by = "plotID") |>
    # add soil sample weight to the nematode data
    left_join(soil_weights, by = "plotID") |>
    # estimate the total number of nematodes in 100 g dry soil
    mutate(
      total_nematode_abundance_per_g_dry_soil = round((total_extracted_nematodes / dry_soil_sample_weight_for_extraction_g), digits = 1),
      .by = plotID
    ) |>
    # estimate the number of individuals per family in 100 g dry soil by finding the relative abundance of each family of the 150 identified nematodes per plotID, and multiplying that relative abundance with the total nematodes per 100 grams of dry soil
    mutate(
      per_family_abundance_per_g_dry_soil = round((abundance / sum(abundance)) * total_nematode_abundance_per_g_dry_soil, digits = 1), .by = c(plotID)
    ) |>
    # remove raw family abundance
    select(-abundance, -total_extracted_nematodes, -dry_soil_sample_weight_for_extraction_g) |>
    # add feeding groups
    left_join(feeding_groups, by = "family") |>
    mutate(
      # retrieval date:
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
      siteID = recode_values(
        siteID,
        "Gud" ~ "Gudmedalen",
        "Lav" ~ "Lavisdalen",
        "Ram" ~ "Rambera",
        "Ulv" ~ "Ulvehaugen",
        "Skj" ~ "Skjelingahaugen",
        "Alr" ~ "Alrust",
        "Arh" ~ "Arhelleren",
        "Fau" ~ "Fauske",
        "Hog" ~ "Hogsete",
        "Ovs" ~ "Ovstedalen",
        "Vik" ~ "Vikesland",
        "Ves" ~ "Veskre"
      )
    ) |>
    # relocate columns
    relocate(
      year, sampling_date, siteID, blockID, plotID, treatment,
      total_nematode_abundance_per_g_dry_soil, per_family_abundance_per_g_dry_soil,
      family, feeding_group
    ) |>
    funcabization(convert_to = "FunCaB")

}
