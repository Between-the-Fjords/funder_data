# mesofauna plan

mesofauna_plan <- list(

  # microarthropodes
  tar_target(
    name = microart_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_microarthropod_composition_2023.csv",
                       path = "raw_data",
                       remote_path = "2_Mesofauna/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = microart_raw,
    command = read_csv2(microart_download)
  ),

  tar_target(
    name = microart_clean,
    command = clean_microarthropods(microart_raw, cnp_depht_raw)
  ),

  tar_target(
    name = microart_output,
    command = save_csv(file = microart_clean, name = "FUNDER_clean_microarthropod_composition_2022.csv"),
    format = "file"
  ),

  # nematodes
  tar_target(
    name = feeder_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_Nematode_feeding_group_2023.csv",
                       path = "raw_data",
                       remote_path = "2_Mesofauna/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = nema_weigth_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_Nematode_sample_weight_2023.csv",
                       path = "raw_data",
                       remote_path = "2_Mesofauna/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = nematode_download,
    command = get_file(node = "tx9r2",
                       file = "FUNDER_raw_Nematodes_families_2023.xlsx",
                       path = "raw_data",
                       remote_path = "2_Mesofauna/Raw_data"),
    format = "file"
  ),

  tar_target(
    name = feeder_raw,
    command = read_csv2(feeder_download)
  ),

  tar_target(
    name = nema_weight_raw,
    command = read_csv2(nema_weigth_download)
  ),

  tar_target(
    name = nematode_raw,
    command = read_excel(nematode_download)
  ),

  tar_target(
    name = nematode_clean,
    command = clean_nematode(nematode_raw, nema_weight_raw, feeder_raw, cnp_depht_raw)
  ),

  tar_target(
    name = nematode_output,
    command = save_csv(file = nematode_clean, name = "FUNDER_clean_nematode_composition_2022.csv"),
    format = "file"
  )

)
