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
  )
)
