### Clean root data

root_plan <- list(

  # ROOT TRAITS
  # root biomass data
  # all sites, block 1-4 for single and double treatments, block 4 for C and FGB
  tar_target(
    name = root_ric_biomass_raw,
    command = read_csv(here::here("raw_data/root_traits/BIOMASS_DATASHEET_ALL.csv"))
  ),

  tar_target(
    name = root_ric_biomass_clean,
    command = clean_ric_root_biomass(root_ric_biomass_raw)
  ),
 
  # Root traits from block 1-3 for single and double treatments
  # Root traits scan results (RIC 2025, removal treatments)
  tar_target(
    name = root_traits_F_G_B_raw,
    command = read_delim(
      file = here::here("raw_data/root_traits/F_G_B_removal_treatments-SCAN_RESULTS_2025.txt"),
      col_types = cols(
        `Length(cm)` = col_double(),
        `AvgDiam(mm)` = col_double()
      )
    ) |>
      slice(-1:-4)  # remove header lines
  ),

  # Root traits scan results (FAU4FB, Peter)
  tar_target(
    name = root_traits_ric_FAU4FB_raw,
    command = read_delim(
      file = here::here("raw_data/root_traits/Fau4FB-SCAN_RESULTS.txt"),
      col_types = cols(
        `Length(cm)` = col_double(),
        `AvgDiam(mm)` = col_double()
      )
    ) |>
      slice(-1:-4)  # remove header lines
  ),

  # Root traits scan results (GF/GB/FB double-removal treatments, 2024)
  tar_target(
    name = root_traits_ric_GF_GB_FB_raw,
    command = read_delim(
      file = here::here("raw_data/root_traits/double-removal_treatments-SCAN_RESULTS_2024.txt"),
      col_types = cols(
        `Length(cm)` = col_double(),
        `AvgDiam(mm)` = col_double()
      )
    ) |>
      slice(-1:-4) |>
      # Harmonize column name and format to match other tables
      rename(`RHIZO 2022a` = `RHIZO 2022b`) |>
      mutate(`RHIZO 2022a` = toupper(`RHIZO 2022a`))
  ),

  # Block 4 root traits (scan_results_all.txt)
  tar_target(
    name = root_traits_block_4_raw,
    command = read_delim(here::here("raw_data/root_traits/scan_results_all.txt"),
    col_types = cols(
        `Length(cm)` = col_double(),
        `AvgDiam(mm)` = col_double()
      )
    ) |>
      slice(-c(1:4))  # remove header lines)
  ),

  tar_target(
    name = root_traits_ric_joined,
    command = clean_ric_root_traits(root_traits_F_G_B_raw, root_traits_ric_FAU4FB_raw, root_traits_ric_GF_GB_FB_raw, root_traits_block_4_raw)
  ),

  # Cleaned roots for C/FGB 2023 (from FUNDER-RIC_clean_data.Rmd)
  # 72 observations: 2 sites, block 1-3, treatment C and FGB
  tar_target(
    name = root_traits_C_FGB_2023_raw,
    command = read_csv(here::here("raw_data/root_traits/FUNDER_clean_Roots_C_FGB_2023.csv"))
  ),

  tar_target(
    name = root_traits_C_FGB_clean,
    command = clean_C_FGB_traits(root_traits_C_FGB_2023_raw)
  ),

  tar_target(
    name = ric_depth_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_root_ingrowth_core_depths_2022.csv",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = ric_depth_raw,
    command = read_csv2(ric_depth_download)
  ),

  tar_target(
    name = ric_depth_clean,
    command = clean_ric_depth(ric_depth_raw)
  ),

    tar_target(
    name = root_traits_clean,
    command = join_root_traits(root_ric_biomass_clean, root_traits_ric_joined, root_traits_C_FGB_clean, ric_depth_clean)
  ),

  # Operator-bias corrected root traits (use *_corrected columns for analyses)
  tar_target(
    name = root_traits_clean_corrected,
    command = apply_operator_correction(root_traits_clean)
  ),

  # tar_target(
  #   name = ric_output,
  #   command = save_csv(file = ric_depth_clean, name = "FUNDER_clean_root_ingrowth_core_depths_2022.csv"),
  #   format = "file"
  # )

  # ROOT BIOMASS
  tar_target(
    name = root_biomass_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_root_biomass_2021.xlsx",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = root_biomass_raw,
    command = read_excel(root_biomass_download)
  ),

  # clean root biomass data
  tar_target(
    name = root_biomass_clean,
    command = clean_root_biomass(root_biomass_raw, funder_meta)
  )

)

