# mesofauna plan

mesofauna_plan <- list(

  # microarthropodes
  tar_target(
    name = microart_download,
    command = get_file(
      node = "tx9r2",
      file = "ix_FUNDER_raw_microarthropod_composition_2023.csv",
      path = here::here("raw_data"),
      remote_path = "ix-xv_soil_biota/ix_microarthropod_community"
    ),
    format = "file"
  ),
  tar_target(
    name = microart_raw,
    command = read_csv2(microart_download)
  ),

  # get microarthropod sample dimensions
  tar_target(
    name = soil_core_dim_download,
    command = get_file(
      node = "tx9r2",
      file = "ix_FUNDER_raw_microarthropod_core_depths_2022.csv",
      path = here::here("raw_data"),
      remote_path = "ix-xv_soil_biota/ix_microarthropod_community"
    ),
    format = "file"
  ),
  tar_target(
    name = soil_core_dim,
    command = read_csv2(soil_core_dim_download) |>
    mutate(core_depth = sub("na", NA, core_depth),
           core_depth = as.numeric(sub(",", ".", core_depth)),
           plotID = paste0(str_to_title(Site), Block, treatment),
           blockID = substr(plotID, 1, 4)) |>
      select(plotID, blockID, treatment, core_depth) |>
      funcabization(convert_to = "FunCaB")
  ),

  # get soil bulk density for each site from VCG OSF repo
  tar_target(
    name = bulk_density_download,
    command = get_file(
      node = "npfa9",
      file = "VCG_clean_soil_structure_2013_2014_2018.csv",
      path = here::here("raw_data"),
      remote_path = "8_Environmental_data"
    ),
    format = "file"
  ),
  tar_target(
    name = bulk_density,
    command = read.csv(bulk_density_download) |>
      filter(variable == "bulk_density") |>
      mutate(mean_bulk_density = mean(value), .by = siteID) |>
      select(siteID, mean_bulk_density) |>
      distinct()
  ),

  # clean microarthropods
  tar_target(
    name = microart_clean,
    command = clean_microarthropods(microart_raw, soil_core_dim, bulk_density)
  ),


  tar_target(
    name = microart_output,
    command = save_csv(file = microart_clean,
                       name = "ix_FUNDER_clean_microarthropod_composition_2022.csv"),
    format = "file"
  ),

  # get raw nematode data
  tar_target(
    name = get_raw_nematode_families,
    command = get_file(
      node = "tx9r2",
      file = "x_FUNDER_raw_Nematodes_families_2023.xlsx",
      path = here::here("raw_data"),
      remote_path = "ix-xv_soil_biota/x_nematode_community"
    ),
    format = "file"
  ),

  tar_target(
    name = raw_nematodes_families,
    command = readxl::read_xlsx(
      get_raw_nematode_families,
      col_types = c("text", "numeric", "numeric", "date", "text", "text",
                    rep("numeric", times = 38), "text")) |>
      clean_names() |>
      # correct misspelling:
      rename(diphtherophoridae = diphterophoridae)
  ),

  # get nematode sample weights
  tar_target(
    name = get_raw_nematode_sample_weights,
    command = get_file(
      node = "tx9r2",
      file = "x_FUNDER_raw_Nematode_sample_weight_2023.csv",
      path = here::here("raw_data"),
      remote_path = "ix-xv_soil_biota/x_nematode_community"
    ),
    format = "file"
  ),

  tar_target(
    name = raw_nematode_sample_weights,
    command = read_csv2(get_raw_nematode_sample_weights)
  ),


  # clean the nematode dataset
  tar_target(
    name = get_clean_nematodes,
    command = cleaning_nematodes(families = raw_nematodes_families,
                                 sample_weights = raw_nematode_sample_weights)
  ),

  # save nematode output
  tar_target(
    name = save_nematode_output,
    command = save_csv(file = get_clean_nematodes,
                       name = "x_FUNDER_clean_nematode_community_2022.csv"),
    format = "file"
  )

)
