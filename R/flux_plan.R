### Clean CO2 flux data

flux_plan <- list(
  # Download and unzip site metadata
  tar_target(
    name = cflux_metadata_download,
    command = {
      path <- get_file(
        node = "tx9r2",
        file = "C-flux site metadata.zip",
        path = here::here("raw_data"),
        remote_path = "5_Carbon_and_nutrient_cycle/Raw_data/FUNDER_raw_cflux"
      )
      if (!dir.exists(here::here("raw_data", "Cflux_site_metadata"))) {
        unzip(zipfile = path, exdir = here::here("raw_data"))
      }
      path
    },
    format = "file"
  ),

  # Download and unzip CO2/H2O/PAR data
  tar_target(
    name = cflux_conc_download,
    command = {
      path <- get_file(
        node = "tx9r2",
        file = "CO2_H20_PAR_Squirrel.zip",
        path = here::here("raw_data"),
        remote_path = "5_Carbon_and_nutrient_cycle/Raw_data/FUNDER_raw_cflux"
      )
      if (!dir.exists(here::here("raw_data", "CO2_H20_PAR_Squirrel"))) {
        unzip(zipfile = path, exdir = here::here("raw_data"))
      }
      path
    },
    format = "file"
  ),

  # Download and unzip temperature data
  tar_target(
    name = cflux_temp_download,
    command = {
      path <- get_file(
        node = "tx9r2",
        file = "Temperature_iButton.zip",
        path = here::here("raw_data"),
        remote_path = "5_Carbon_and_nutrient_cycle/Raw_data/FUNDER_raw_cflux"
      )
      if (!dir.exists(here::here("raw_data", "Temperature_iButton"))) {
        unzip(zipfile = path, exdir = here::here("raw_data"))
      }
      path
    },
    format = "file"
  ),

  # Process raw files into cleaned flux data with GPP
  # make_plots = TRUE saves one PDF page per flux measurement
  tar_target(
    name = cflux_clean,
    command = {
      cflux_metadata_download
      cflux_conc_download
      cflux_temp_download
      clean_cflux(output_dir = here::here("raw_data"), make_plots = FALSE)
    }
  ),

  # Export cleaned dataset
  tar_target(
    name = cflux_output,
    command = save_csv(
      file = cflux_clean,
      name = "FUNDER_clean_cfluxes_2022.csv"
    ),
    format = "file"
  )
)
