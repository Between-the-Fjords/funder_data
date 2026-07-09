### Clean root traits and biomass

root_plan <- list(

  # RIC lab data: C and FGB samples
  tar_target(
    name = root_lab_c_fgb_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_Root_Weight_2023_C_FGB.csv",
      path = here::here("raw_data/roots"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # RIC lab data: remaining treatments
  tar_target(
    name = root_lab_rest_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_root_weight_rest_2022.csv",
      path = here::here("raw_data/roots"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  tar_target(
    name = root_lab_data,
    command = clean_root_lab_data(
      c_fgb = root_lab_c_fgb_download,
      rest = root_lab_rest_download
    )
  ),

  # Root scan data: C and FGB
  tar_target(
    name = scan_c_fgb_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_Roots_Traits_2023_C_FGB.csv",
      path = here::here("raw_data/roots"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # Root scan data: FB, GB and GF
  tar_target(
    name = scan_fb_gb_gf_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_ric_double-removal_root_traits_2024.txt",
      path = here::here("raw_data/roots"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # Root scan data: F, G, B (blocks 1-3) and all block 4
  tar_target(
    name = scan_f_g_b_block4_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_leo_and_miska_scan_results_all.txt",
      path = here::here("raw_data/roots"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  tar_target(
    name = root_scan_data,
    command = clean_root_scan_data(
      c_fgb = scan_c_fgb_download,
      fb_gb_gf = scan_fb_gb_gf_download,
      f_g_b_block4 = scan_f_g_b_block4_download
    )
  ),

  tar_target(
    name = root_traits,
    command = clean_root_traits(
      lab = root_lab_data,
      scan = root_scan_data
    )
  ),

  # Standing root biomass from 2021 (separate dataset; joined in finish_roots)
  tar_target(
    name = root_biomass_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_root_biomass_2021.xlsx",
      path = here::here("raw_data/roots"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # Combined root traits, productivity and standing biomass
  tar_target(
    name = root_traits_clean,
    command = finish_roots(
      biomass = root_biomass_download,
      traits = root_traits
    )
  ),

  # Operator-bias corrected RIC traits (interaction model; reference operator in pipeline).
  # See FUNDER-RIC_root_traits_operator_bias.qmd and docs/operator_bias.html for method.
  # root_biomass (2021) is not corrected — it has no operator in the source data.
  tar_target(
    name = root_traits_corrected,
    command = apply_operator_correction(
      root_traits_clean,
      trait_names = RIC_TRAIT_COLS,
      reference_operator = "Michaela",
      operator_model = "interaction"
    )
  ),

  tar_target(
    name = root_traits_long,
    command = make_root_traits_long(root_traits_corrected) |>
      dataDocumentation::funcabization(convert_to = "FunCaB")
  ),

  tar_target(
    name = root_traits_output,
    command = save_csv(
      file = root_traits_long,
      name = "FUNDER_clean_root_biomass_productivity_traits_2021-2022.csv"
    ),
    format = "file"
  )

)
