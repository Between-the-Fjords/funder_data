# clean environmental data

nutrient_cycling_plan <- list(

  # funder meta data
  tar_target(
    name = funder_meta,
    command = create_funder_meta_data()
  ),

  # reflectance
  tar_target(
    name = prs_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_PRS_2021.xlsx",
                       path = "raw_data",
                       remote_path = "5_Carbon_and_nutrient_cycle/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = prs_raw,
    command = read_excel(prs_download, skip = 4) |>
      filter(`WAL #` != "Method Detection Limits (mdl):") |>
      rename(ID = `Sample ID`)
  ),

  # detection limits for the elements
  tar_target(
    name = prs_detection_limit,
    command = read_excel(prs_download, skip = 4) |>
      slice(1) |>
      select(`NO3-N`:Cd)|>
      pivot_longer(cols = everything(), names_to = "elements", values_to = "detection_limit")
  ),

  tar_target(
    name = prs_clean,
    command = clean_prs(prs_raw, prs_detection_limit, funder_meta)
  ),

  tar_target(
    name = prs_output,
    command = save_csv(file = prs_clean, name = "FUNDER_clean_available_nutrients_2021.csv"),
    format = "file"
  ),

  # LOI
  tar_target(
    name = loi_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_LOI_2022.csv",
                       path = "raw_data",
                       remote_path = "5_Carbon_and_nutrient_cycle/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = loi_raw,
    command = read_csv2(loi_download)
  ),

  tar_target(
    name = loi_clean,
    command = clean_loi(loi_raw)
  ),

  tar_target(
    name = loi_output,
    command = save_csv(file = loi_clean, name = "FUNDER_clean_LOI_2022.csv"),
    format = "file"
  ),

  # CNP
  tar_target(
    name = cnp_depth_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_CNP_core_depths_2022.csv",
                       path = "raw_data",
                       remote_path = "5_Carbon_and_nutrient_cycle/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = cnp_ram_depth_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_Rambera_CNP_core_depths_2022.csv",
                       path = "raw_data",
                       remote_path = "5_Carbon_and_nutrient_cycle/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = cnp_depht_raw,
    command = read_csv2(cnp_depth_download)
  ),

  tar_target(
    name = cnp_ram_depht_raw,
    command = read_csv2(cnp_ram_depth_download)
  ),

  # NOT FINISHED YET, NEEDS ALSO CNP DATA!!!
  tar_target(
    name = cnp_clean,
    command = clean_cnp(cnp_depht_raw, cnp_ram_depht_raw, funder_meta)
  )


)

