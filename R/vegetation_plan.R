### Clean vegetation data

vegetation_plan <- list(
  # REFLECTANCE
  tar_target(
    name = ndvi_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_greenseeker_2022.csv",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = ndvi_raw,
    command = read_csv2(ndvi_download)
  ),
  tar_target(
    name = ndvi_clean,
    command = clean_greenseeker(ndvi_raw)
  ),
  tar_target(
    name = ndvi_output,
    command = save_csv(file = ndvi_clean, name = "FUNDER_clean_reflectance_greenseeker_2022.csv"),
    format = "file"
  ),

  # BIOMASS
  tar_target(
    name = biomass_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER Forbs_Graminoids_Bryophytes.xlsx",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = biomass_raw,
    command = read_excel(biomass_download, sheet = "Dried weight")
  ),
  tar_target(
    name = biomass_clean,
    command = clean_biomass(biomass_raw)
  ),
  tar_target(
    name = biomass_output,
    command = save_csv(file = biomass_clean, name = "FUNDER_clean_biomass_2022.csv"),
    format = "file"
  ),

  # COMMUNITY
  # 2022 community data
  tar_target(
    name = community_2022_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_vascular_community_2022.csv",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # import 2022community data
  tar_target(
    name = community_2022_raw,
    command = read_csv(community_2022_download)
  ),

  # 2015-2021 community data
  tar_target(
    name = community_2015_2021_download,
    command = get_file(
      node = "4c5v2",
      file = "FunCaB_clean_composition_2015-2019.csv",
      path = here::here("raw_data"),
      remote_path = "3_Plant_composition"
    ),
    format = "file"
  ),

  # import 2015-2021 community data
  tar_target(
    name = community_2015_2021_raw,
    command = read_csv(community_2015_2021_download)
  ),

  # clean 2015-2021 community data
  tar_target(
    name = community_2015_2021_clean,
    command = clean_community_2015_2021(community_2015_2021_raw)
  ),

  # functional group 2015-2021
  # Add species new in 2022 that are missing functional groups
  tar_target(
    name = fun_gr,
    command = community_2015_2021_raw |>
      distinct(species, functional_group) |>
      bind_rows(
        tibble(
          species = c("NID.sp", "Ped.bor", "Phe.con", "Rub.cha", "Val.sam"),
          functional_group = "forb"
        )
      )
  ),

    # clean 2022 community data
  tar_target(
    name = community_2022_clean,
    command = clean_community_2022(community_2022_raw, fun_gr)
  ),
  
  # join 2015-2021 and 2022 community data
  tar_target(
    name = community_clean,
    command = join_community(community_2015_2021_clean, community_2022_clean)
  ),

  # turf maps corrections
  tar_target(
    name = turf_map_corrections_download,
    command = get_file(
      node = "tx9r2",
      file = "Turf mat corrections Funder.xlsx",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  tar_target(
    name = turf_map_corrections,
    command = read_excel(turf_map_corrections_download)
  ),

  tar_target(
    name = turf_map_corrections_fixed,
    command = fix_turf_map_corrections(turf_map_corrections, funder_meta)
  ),

  # apply turf map corrections to community data
  tar_target(
    name = community_clean_fixed,
    command = apply_turf_map_corrections(community_clean, turf_map_corrections_fixed, fun_gr)
  ),

  # community out
  tar_target(
    name = community_output,
    command = save_csv(file = community_clean_fixed, name = "FUNDER_clean_community_2015-2022.csv"),
    format = "file"
  ),

  # BRYOPHYTE
  # community data
  tar_target(
    name = bryophyte_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_bryophyte_community_2022.csv",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # import bryophyte data
  tar_target(
    name = bryophyte_raw,
    command = read_csv(bryophyte_download)
  ),

  # bryophyte dictionary data
  tar_target(
    name = bryophyte_dic_download,
    command = get_file(
      node = "tx9r2",
      file = "Bryophyte community voucher overview with protocol - FUNDER 2022.xlsx",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # import bryophyte dictionary
  tar_target(
    name = bryophyte_dictionary,
    command = read_excel(bryophyte_dic_download)
  ),

  # clean bryophyte structure
  tar_target(
    name = bryophyte_structure,
    command = clean_bryophyte_structure(bryophyte_raw, funder_meta)
  ),
  tar_target(
    name = bryophyte_structure_output,
    command = save_csv(file = bryophyte_structure, name = "FUNDER_clean_bryophyte_structure_2022.csv"),
    format = "file"
  ),

  # clean bryophyte cover and presence data
  # These targets require API key and model to be set in the environment
  # # Call OpenAI API to parse species info from unique texts
  # tar_target(
  #   name = bryophyte_llm_results,
  #   command = {
  #     unique_texts <- bryophyte_dictionary$species_correction_Kristian_Hassel |> unique()
  #     bryophyte_llm_results <- get_species_from_llm(unique_texts)
  #   }
  # ),

  # tar_target(
  #   name = llm_results_out,
  #   command = write_excel_csv(bryophyte_llm_results, "raw_data/FUNDER_llm_results2.csv", na = ""),
  #   format = "file"
  # ),

  # download LLM results
  tar_target(
    name = bryophyte_llm_results_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_llm_results.csv",
      path = here::here("raw_data"),
      remote_path = "1_Vegetation/Raw_data"
    ),
    format = "file"
  ),

  # import LLM results
  tar_target(
    name = bryophyte_llm_results2,
    command = read_csv(bryophyte_llm_results_download)
  ),
  tar_target(
    name = joined_bryophyte,
    command = join_bryophyte_with_llm(bryophyte_dictionary, bryophyte_llm_results2)
  ),
  tar_target(
    name = bryophyte_clean,
    command = clean_bryophyte(bryophyte_raw, joined_bryophyte, funder_meta)
  ),

  # cover data
  tar_target(
    name = bryophyte_cover,
    command = bryophyte_clean |>
      # remove rows where cover_percent is NA
      filter(!is.na(cover_percent)) |>
      select(date:treatment, voucherID,
        species = scientific_final, vernacular_species = vernacular_final,
        cover_percent, observer, weather, comments
      )
  ),
  tar_target(
    name = bryophyte_cover_output,
    command = save_csv(file = bryophyte_cover, name = "FUNDER_clean_bryophyte_cover_2022.csv"),
    format = "file"
  ),

  # presence data
  tar_target(
    name = bryophyte_presence,
    command = bryophyte_clean |>
      select(date:treatment, voucherID,
        species = scientific_final, vernacular_species = vernacular_final,
        `1`, `2`, `3`, `4`, `5`, observer, weather, comments
      ) |>
      pivot_longer(
        cols = c(`1`, `2`, `3`, `4`, `5`),
        names_to = "subplot", values_to = "presence"
      ) |>
      filter(!is.na(presence)) |>
      select(date:vernacular_species, subplot, presence, observer, weather, comments)
  ),
  tar_target(
    name = bryophyte_presence_output,
    command = save_csv(file = bryophyte_presence, name = "FUNDER_clean_bryophyte_presence_2022.csv"),
    format = "file"
  )
)
