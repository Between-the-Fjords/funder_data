# clean environmental data

environmenet_plan <- list(
  # slope and aspect
  tar_target(
    name = slope_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_slope_and_aspect_2022.csv",
      path = "raw_data",
      remote_path = "6_Environment/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = slope_raw,
    command = read_csv2(slope_download)
  ),
  tar_target(
    name = slope_clean,
    command = clean_slope(slope_raw)
  ),

  # Export cleaned dataset
  tar_target(
    name = slope_output,
    command = save_csv(file = slope_clean, name = "FUNDER_clean_slope_and_aspect_2022.csv"),
    format = "file"
  ),

  # soil depth
  tar_target(
    name = depth_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_soil_depth_measurements_2022.csv",
      path = "raw_data",
      remote_path = "6_Environment/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = depth_raw,
    command = read_csv2(depth_download)
  ),
  tar_target(
    name = depth_clean,
    command = clean_depth(depth_raw)
  ),

  # Export cleaned dataset
  tar_target(
    name = depth_output,
    command = save_csv(file = depth_clean, name = "FUNDER_clean_soil_depth_measurements_2022.csv"),
    format = "file"
  ),


  # soil moisture point measurements
  tar_target(
    name = sm_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_soil_moisture_point_measurements_2022.csv",
      path = "raw_data",
      remote_path = "6_Environment/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = sm_raw,
    command = read_csv2(sm_download)
  ),
  tar_target(
    name = sm_clean,
    command = clean_sm(sm_raw)
  ),

  # Export cleaned dataset
  tar_target(
    name = sm_output,
    command = save_csv(file = sm_clean, name = "FUNDER_clean_soil_moisture_point_measurements_2022.csv"),
    format = "file"
  ),

  # microclimate data
  tar_target(
    name = climate_download,
    command = {
      get_file(
        node = "tx9r2",
        file = "FUNDER_raw_climate_TOMST.zip",
        path = "raw_data",
        remote_path = "6_Environment/Raw_data"
      )

      if (!file.exists("raw_data/FUNDER_raw_climate_TOMST")) {
        unzip(zipfile = "raw_data/FUNDER_raw_climate_TOMST.zip", exdir = "raw_data")
      }
    },
    format = "file"
  ),
  tar_target(
    name = climate_ID_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_raw_TOMST_ID.csv",
      path = "raw_data",
      remote_path = "6_Environment/Raw_data"
    ),
    format = "file"
  ),
  tar_target(
    name = climate_ID_raw,
    command = read_csv2(climate_ID_download)
  ),
  tar_target(
    name = climate_clean,
    command = clean_climate(climate_ID_raw)
  ),
  tar_target(
    name = climate_output,
    command = save_csv(file = climate_clean, name = "FUNDER_clean_microclimate_2022.csv"),
    format = "file"
  )

  # NGCD gridded climate data (1km resolution) – disabled by default (slow).
  # To enable: uncomment the block below and run tar_make().
  # Or process manually: source("R/functions/process_downloaded_ngcd.R")
  #
  # tar_target(
  #   name = ngcd_coordinates,
  #   command = read_csv("raw_data/coordinates.csv")
  # ),
  # tar_target(
  #   name = ngcd_download,
  #   command = {
  #     ngcd_path <- "raw_data/NGCD"
  #     if (!dir.exists(ngcd_path)) {
  #       stop("NGCD data not found. Create 'raw_data/NGCD' and add zip or .nc files.")
  #     }
  #     zips <- list.files(ngcd_path, pattern = "\\.zip$", full.names = TRUE)
  #     if (length(zips) > 0) return(zips)
  #     ncs <- list.files(ngcd_path, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
  #     if (length(ncs) == 0) stop("No .zip or .nc files found in 'raw_data/NGCD'.")
  #     ncs
  #   },
  #   format = "file"
  # ),
  # tar_target(
  #   name = ngcd_extracted,
  #   command = extract_ngcd_for_sites(nc_files = ngcd_download, coordinates = ngcd_coordinates)
  # ),
  # tar_target(
  #   name = ngcd_clean,
  #   command = clean_ngcd(ngcd_extracted)
  # ),
  # tar_target(
  #   name = ngcd_output,
  #   command = save_csv(file = ngcd_clean, name = "FUNDER_clean_NGCD_climate_2008_2025.csv"),
  #   format = "file"
  # )
)
