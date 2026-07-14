# mesofauna plan

mesofauna_plan <- list(

  # microarthropodes
  tar_target(
    name = microart_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_microarthropod_composition_2023.csv",
      path = here::here("raw_data"),
      remote_path = "x-xi_microarthropods_nematodes/x_microarthropods"
    ),
    format = "file"
  ),
  tar_target(
    name = microart_raw,
    command = read_csv2(microart_download)
  ),
  tar_target(
    name = microart_clean,
    command = clean_microarthropods(microart_raw)
  ),
  tar_target(
    name = microart_output,
    command = save_csv(file = microart_clean,
                       name = "x_FUNDER_clean_microarthropod_composition_2022.csv"),
    format = "file"
  ),

  # get raw nematode data
  tar_target(
    name = get_raw_nematode_families,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_Nematodes_families_2023.xlsx",
      path = here::here("raw_data"),
      remote_path = "x-xi_microarthropods_nematodes/xi_nematodes"
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
      file = "FUNDER_raw_Nematode_sample_weight_2023.csv",
      path = here::here("raw_data"),
      remote_path = "x-xi_microarthropods_nematodes/xi_nematodes"
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
                       name = "xi_FUNDER_clean_nematode_community_2022.csv"),
    format = "file"
  )

)
