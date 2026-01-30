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
)
