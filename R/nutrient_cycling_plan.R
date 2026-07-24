# clean environmental data

nutrient_cycling_plan <- list(
  # funder meta data
  tar_target(
    name = funder_meta,
    command = create_funder_meta_data()
  ),

  # prs
  tar_target(
    name = prs_download,
    command = get_file(
      node = "tx9r2",
      file = "xviii_FUNDER_raw_PRS_2021.xlsx",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xviii_available_nutrients"
    ),
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
      select(`NO3-N`:Cd) |>
      pivot_longer(cols = everything(), names_to = "elements", values_to = "detection_limit")
  ),
  tar_target(
    name = prs_clean,
    command = clean_prs(prs_raw, prs_detection_limit, funder_meta)
  ),
  tar_target(
    name = prs_output,
    command = save_csv(file = prs_clean, name = "xviii_FUNDER_clean_available_nutrients_2021.csv"),
    format = "file"
  ),

  # LOI
  tar_target(
    name = loi_download,
    command = get_file(
      node = "tx9r2",
      file = "xvii_FUNDER_raw_LOI_2022.csv",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xvii_soil_organic_matter"
    ),
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
    command = save_csv(file = loi_clean, name = "xvii_FUNDER_clean_LOI_2022.csv"),
    format = "file"
  ),

  # # CNP
  # tar_target(
  #   name = cnp_depth_download,
  #   command = get_file(
  #     node = "tx9r2",
  #     file = "FUNDER_raw_CNP_core_depths_2022.csv",
  #     path = here::here("raw_data"),
  #     remote_path = "xvi-xxii_carbon_and_nutrient_cycling/Raw_data"
  #   ),
  #   format = "file"
  # ),
  # tar_target(
  #   name = cnp_ram_depth_download,
  #   command = get_file(
  #     node = "tx9r2",
  #     file = "FUNDER_raw_Rambera_CNP_core_depths_2022.csv",
  #     path = here::here("raw_data"),
  #     remote_path = "xvi-xxii_carbon_and_nutrient_cycling/Raw_data"
  #   ),
  #   format = "file"
  # ),
  #
  # # sample fresh weight
  # tar_target(
  #   name = cnp_weight_download,
  #   command = get_file(
  #     node = "tx9r2",
  #     file = "FUNDER_raw_CNP_fresh_sample_weights_2022.xlsx",
  #     path = here::here("raw_data"),
  #     remote_path = "xvi-xxii_carbon_and_nutrient_cycling/Raw_data"
  #   ),
  #   format = "file"
  # ),
  # tar_target(
  #   name = cnp_depht_raw,
  #   command = read_csv2(cnp_depth_download)
  # ),
  # tar_target(
  #   name = cnp_ram_depht_raw,
  #   command = read_csv2(cnp_ram_depth_download)
  # ),
  #
  # # !!!
  # # not sure if needed. Fresh weight of all soil samples, separate sheet for block 1-3 and 4
  # # tar_target(
  # #   name = cnp_weight_raw,
  # #   command = read_excel(cnp_weight_download)
  # # ),
  #
  # # NOT FINISHED YET, NEEDS ALSO CNP DATA!!!
  # # NEED TO DECIDE HOW TO DEAL WITH RAM, WHERE SAMPLES ARE TAKEN DIFFERENTLY
  # tar_target(
  #   name = cnp_clean,
  #   command = clean_cnp(cnp_depht_raw, cnp_ram_depht_raw, funder_meta)
  # ),

  # Soil carbon and nitrogen
  tar_target(
    name = get_soil_cn_sheet_1,
    command = get_file(
      node = "tx9r2",
      file = "xvi_24256_CN_EA_PeterF_B1_241106_report.xlsx",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xvi_soil_carbon_and_nitrogen"
    ),
    format = "file"
  ),
  tar_target(
    name = cn1,
    command = readxl::read_xlsx(get_soil_cn_sheet_1,
                                sheet = 2) |>
      row_to_names(29) |>
      clean_names()
  ),

  tar_target(
    name = get_soil_cn_sheet_2,
    command = get_file(
      node = "tx9r2",
      file = "xvi_24256_CN_EA_PeterF_B2_241107_report.xlsx",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xvi_soil_carbon_and_nitrogen"
    ),
    format = "file"
  ),
  tar_target(
    name = cn2,
    command = readxl::read_xlsx(get_soil_cn_sheet_2,
                                sheet = 2) |>
      row_to_names(29) |>
      clean_names()
  ),

  tar_target(
    name = get_soil_cn_sheet_3,
    command = get_file(
      node = "tx9r2",
      file = "xvi_24256_CN_EA_PeterF_B3_241108_report.xlsx",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xvi_soil_carbon_and_nitrogen"
    ),
    format = "file"
  ),
  tar_target(
    name = cn3,
    command = readxl::read_xlsx(get_soil_cn_sheet_3,
                                sheet = 2) |>
      row_to_names(29) |>
      clean_names()
  ),

  tar_target(
    name = get_soil_cn_sheet_4,
    command = get_file(
      node = "tx9r2",
      file = "xvi_24256_CN_EA_PeterF_B4_241112_report.xlsx",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xvi_soil_carbon_and_nitrogen"
    ),
    format = "file"
  ),
  tar_target(
    name = cn4,
    command = readxl::read_xlsx(get_soil_cn_sheet_4,
                                sheet = 2) |>
      row_to_names(29) |>
      clean_names()
  ),

  # clean and assemble soil CN data
  tar_target(
    name = clean_soil_cn_data,
    command = make_cn(cn1, cn2, cn3, cn4)
  ),

  # write soil CN output
  tar_target(
    name = write_soil_cn_output,
    command = save_csv(file = clean_soil_cn_data,
                       name = "xvi_FUNDER_clean_soil_CN_2022.csv"),
    format = "file"
  ),

  # Soil pH and macronutrients
  tar_target(
    name = get_bioner_results,
    command =  get_file(
      node = "tx9r2",
      file = "xix_Peter_UiB_FUNDER_prøve_ID.xlsx",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xix_soil_ph_and_macronutrients"
    ),
    format = "file"
  ),

  tar_target(
    name = bioner_results,
    command = readxl::read_xlsx(here::here(get_bioner_results))
  ),

  tar_target(
    name = get_uib_ph,
    command =  get_file(
      node = "tx9r2",
      file = "xix_FUNDER_raw_ph_samples.xlsx",
      path = here::here("raw_data"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xix_soil_ph_and_macronutrients"
    ),
    format = "file"
  ),

  tar_target(
    name = uib_ph_results,
    command = readxl::read_xlsx(here::here(get_uib_ph))
  ),

  tar_target(
    name = ph_and_macronutrients,
    command = clean_ph_and_macronutrients(bioner = bioner_results,
                                          uib = uib_ph_results)
  ),

  # save output
  tar_target(
    name = save_ph_and_macronutrients,
    command = save_csv(file = ph_and_macronutrients,
                       name = "xix_FUNDER_clean_soil_ph_macronutrients_2022.csv")
  )

)


