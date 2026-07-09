# root traits and biomass plan

library(targets)
library(tarchetypes)

source("R/functions/clean_root_traits_and_biomass_peter.R")

tar_option_set(packages = c("dataDownloader", "dataDocumentation",
                            "tidyverse", "janitor", "readxl"))

list(

  # get root lab data from processing RICs retrieved in 2022
  # C and FGB samples
  tar_target(
    name = root_lab_c_fgb,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_Root_Weight_2023_C_FGB.csv",
      path = "raw_data/roots",
      remote_path = "1_Vegetation/Raw_data/")
  ),

  # the rest of the samples
  tar_target(
    name = root_lab_rest,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_root_weight_rest_2022.csv",
      path = "raw_data/roots",
      remote_path = "1_Vegetation/Raw_data/")
  ),

  # clean and combine the root lab data
  tar_target(
    name = root_lab_data,
    command = clean_root_lab_data(
      c_fgb = root_lab_c_fgb,
      rest = root_lab_rest)
  ),

  # get root scan data for C and FGB treatments
  tar_target(
    name = scan_c_fgb,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_Roots_Traits_2023_C_FGB.csv",
      path = "raw_data/roots",
      remote_path = "1_Vegetation/Raw_data/")
  ),

  # get root scan data for FB, GB and GF treatments
  tar_target(
    name = scan_fb_gb_gf,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_ric_double-removal_root_traits_2024.txt",
      path = "raw_data/roots",
      remote_path = "1_Vegetation/Raw_data/")
  ),

  # get root scan data for C and FGB treatments
  tar_target(
    name = scan_f_g_b_block4,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_leo_and_miska_scan_results_all.txt",
      path = "raw_data/roots",
      remote_path = "1_Vegetation/Raw_data/")
  ),

  # clean and combine root scan data
  tar_target(
    name = root_scan_data,
    command = clean_root_scan_data(
      c_fgb = scan_c_fgb, # "raw_data/roots/FUNDER_raw_Roots_Traits_2023_C_FGB.csv",
      fb_gb_gf = scan_fb_gb_gf, # "raw_data/roots/FUNDER_raw_ric_double-removal_root_traits_2024.txt",
      f_g_b_block4 = scan_f_g_b_block4) # "raw_data/roots/FUNDER_leo_and_miska_scan_results_all.txt")
  ),

  # combine root_lab_data and root_scan_data
  tar_target(
    name = root_traits,
    command = clean_root_traits(lab = root_lab_data,
                                scan = root_scan_data)
  ),

  # get biomass data from 2021
  tar_target(
    name = root_biomass_2021,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_root_biomass_2021.xlsx",
      path = "raw_data/roots",
      remote_path = "1_Vegetation/Raw_data/")
  ),

  # clean biomass from 2021, combine with root traits and return cleaned
  tar_target(
    name = root_traits_and_biomass,
    command = finish_roots(biomass = root_biomass_2021,
                           traits = root_traits)
  )

)

