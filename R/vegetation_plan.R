### Clean vegetation data

vegetation_plan <- list(

  # reflectance
  tar_target(
    name = ndvi_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_greenseeker_2022.csv",
                       path = "raw_data",
                       remote_path = "1_Vegetation/Raw_data"),
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

  # Root ingrowth cores
  tar_target(
    name = ric_depth_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_root_ingrowth_core_depths_2022.csv",
                       path = "raw_data",
                       remote_path = "1_Vegetation/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = ric_depth_raw,
    command = read_csv2(ric_depth_download)
  ),

  # NOT FINISHED YET, ALSO NEED RIC DATA !!!!
  tar_target(
    name = ric_depth_clean,
    command = clean_ric_depth(ric_depth_raw)
  ),

  # tar_target(
  #   name = ric_output,
  #   command = save_csv(file = ric_depth_clean, name = "FUNDER_clean_root_ingrowth_core_depths_2022.csv"),
  #   format = "file"
  # )

  # biomass
  tar_target(
    name = biomass_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER Forbs_Graminoids_Bryophytes.xlsx",
                       path = "raw_data",
                       remote_path = "1_Vegetation/Raw_data"),
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

  # community
  tar_target(
    name = community_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_vascular_community_2022.csv",
                       path = "raw_data",
                       remote_path = "1_Vegetation/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = community_raw,
    command = read_csv(community_download)
  ),

  # bryophyte
  # tar_target(
  #   name = bryophyte_download,
  #   command = get_file(node = "tx9r2",
  #                      file = "FUNDER_bryophyte_community_2022.csv",
  #                      path = "raw_data",
  #                      remote_path = "1_Vegetation/Raw_data"),
  #   format = "file"
  # ),

    tar_target(
    name = bryophyte_raw,
    command = read_csv("raw_data/FUNDER_raw_bryophyte_community_2022.csv")
  ),

      tar_target(
    name = bryophyte_dictionary,
    command = read_excel("raw_data/Bryophyte community voucher overview with protocol - FUNDER 2022.xlsx")
  ),

  tar_target(
    name = bryophyte_structure,
    command = clean_bryophyte_structure(bryophyte_raw, funder_meta)
  ),

  tar_target(
    name = bryophyte_structure_output,
    command = save_csv(file = bryophyte_structure, name = "FUNDER_clean_bryophyte_structure_2022.csv"),
    format = "file"
  )


)
