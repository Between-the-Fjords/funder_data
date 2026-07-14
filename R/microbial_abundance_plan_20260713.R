# microbial abundance plan

microbial_abundance_plan <- list(

  # 1) get plotIDs from OSF
  tar_target(
    name = plotIDs_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_plotIDs.xlsx",
      path = here::here("raw_data/microbial_abundance"),
      remote_path = "xii-xvi_soil_microbes_fungi/"),
    format = "file"
  ),

  # 2) get plotIDs into R
  tar_target(
    name = plotIDs,
    command = read_xlsx(plotIDs_download)
  ),

  # 3) generate sample distribution
  tar_target(
    name = funder_samples,
    command = funder_samples_in_extraction_batches(file = plotIDs, df = TRUE)
  ),

  # 4) get qPCR-to-sample map
  tar_target(
    name = qpcr_to_sample_map,
    command = qpcr_map_maker(data = funder_samples)
  ),

  # 5) get bacterial abunadcne data from OSF
  tar_target(
    name = get_bac_ab,
    command = get_microbial_abundance_from_osf(
      remote_path = "xii-xvi_soil_microbes_fungi/FUNDER_raw_microbial_abundance/bacteria",
      path = here::here("raw_data/microbial_abundance/bacteria/")
    )
  ),

  # 6) map bacterial qPCR data to samples
  tar_target(
    name = bacterial_abundance,
    command = assemble_and_clean_bacteria(map = qpcr_to_sample_map)
  ),

  # 7) correct DNA extraction batch effects in bacteria
  tar_target(
    name = corrected_bacteria,
    command = correct_bacteria_batch_effect(data = bacterial_abundance)
  ),

  # 8) get fungal abunadcne data from OSF
  tar_target(
    name = get_fun_ab,
    command = get_microbial_abundance_from_osf(
      remote_path = "xii-xvi_soil_microbes_fungi/FUNDER_raw_microbial_abundance/fungi",
      path =  here::here("raw_data/microbial_abundance/fungi/")
    )
  ),

  # 9) map fungal qPCR data to samples
  tar_target(
    name = fungal_abundance,
    command = assemble_and_clean_fungi(map = qpcr_to_sample_map)
  ),

  # 10) correct DNA extraction batch effects in fungi
  tar_target(
    name = corrected_fungi,
    command = correct_fungi_batch_effect(data = fungal_abundance)
  ),

  # 11) combine bacteria and fungi
  tar_target(
    name = microbial_abundance,
    command = combine_bacteria_and_fungi(bac = corrected_bacteria,
                                         fun = corrected_fungi)
  ),

  # 12) subset soil samples, remove uninformative columns and sort remaining
  tar_target(
    name = soil_microbial_abundance,
    command = subset_qpcr_samples(
      data = microbial_abundance,
      sample_material = "soil") |>
      select(-c(material, corrected, uncorrected)) |>
      relocate(year, siteID, blockID, plotID,
               treatment, sub, group,
               uncorrected_abundance_per_g, corrected_abundance_per_g,
               DNA_extraction_batch, run,
               well_position)
  ),

  # 13) subset litter samples, remove uninformative columns and sort remaining
  tar_target(
    name = litter_microbial_abundance,
    command = subset_qpcr_samples(
      data = microbial_abundance,

      sample_material = "litter") |>
      select(-c(material, corrected, uncorrected)) |>
      rename(litter_type = sub) |>
      relocate(year, siteID, blockID, plotID,
               treatment, litter_type, group,
               uncorrected_abundance_per_g, corrected_abundance_per_g,
               DNA_extraction_batch, run,
               well_position)
  ),

  # 14) save microbial abundance outputs output
  # soil
  tar_target(
    name = clean_soil_microbial_abundance,
    command = save_csv(
      file = soil_microbial_abundance,
      name = "FUNDER_clean_soil_microbial_abundance_2022.csv"),
    format = "file"),

  # litter
  tar_target(
    name = clean_litter_microbial_abundance,
    command = save_csv(
      file = litter_microbial_abundance,
      name = "FUNDER_clean_litter_microbial_abundance_2022.csv"),
    format = "file")

)
