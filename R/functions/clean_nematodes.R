# clean nematode data

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
  # Yeates et al. 1993 (PMID: 19279775) (with noted exceptions)
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
      "metateratocephalidae", # not in Yeates et al. 1993
      "aphelenchidae",
      "diphtherophoridae",
      "tylencholaimidae",
      "anguinidae",
      "tripylidae",
      "unknown_bacterial_feeder", # not in Yeates et al. 1993
      "unknown_plant_feeder", # not in Yeates et al. 1993
      "unknown_predator" # not in Yeates et al. 1993
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
      total_nematode_abundance_per_g = (total_extracted_nematodes / dry_soil_sample_weight_for_extraction_g),
      .by = plotID
    ) |>
    # estimate the number of individuals per family in 100 g dry soil by finding
    # the relative abundance of each family of the 150 identified nematodes per
    # plotID, and multiplying that relative abundance with the total nematodes
    # per 100 grams of dry soil
    mutate(
      family_abundance_per_g = (abundance / sum(abundance) * total_nematode_abundance_per_g),
      .by = c(plotID)
    ) |>
    # remove raw family abundance
    select(-abundance, -total_extracted_nematodes) |>
    # add feeding groups
    left_join(feeding_groups, by = "family") |>
    mutate(
      # sampling date:
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
      family, feeding_group, total_nematode_abundance_per_g,
      family_abundance_per_g, sample_weight_g = dry_soil_sample_weight_for_extraction_g
    ) |>
    funcabization(convert_to = "FunCaB")

}
