# data dictionary plan

data_dic_plan <- list(

  # add command to download data from OSF!!!

  # attribute table
  tar_target(
    name = attribute_file,
    command = "clean_data/data_description.xlsx",
    format = "file"
  ),

  tar_target(
    name = attribute_table,
    command = read_excel(attribute_file) |>
      mutate(TableID = as.character(TableID))

  ),

  # reflectance
  tar_target(
    name = ndvi_dic,
    command = make_data_dictionary(data = ndvi_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),


  # biomass
  tar_target(
    name = biomass_dic,
    command = make_data_dictionary(data = biomass_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),

  # microarthropods
  tar_target(
    name = microart_dic,
    command = make_data_dictionary(data = microart_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),

  # PRS
  tar_target(
    name = prs_dic,
    command = make_data_dictionary(data = prs_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),

  # LOI
  tar_target(
    name = loi_dic,
    command = make_data_dictionary(data = loi_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),

  # slope and aspect
  tar_target(
    name = slope_dic,
    command = make_data_dictionary(data = slope_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),

  # soil depth
  tar_target(
    name = depth_dic,
    command = make_data_dictionary(data = depth_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),

  # soil moisture
  tar_target(
    name = sm_dic,
    command = make_data_dictionary(data = sm_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),

  # climate
  tar_target(
    name = climate_dic,
    command = make_data_dictionary(data = climate_clean,
                                   description_table = attribute_table,
                                   table_ID = NA_character_)
  ),



  # merge data dictionaries
  tar_target(
    name = data_dic,
    command = write_xlsx(list(biomass = biomass_dic,
                              ndvi = ndvi_dic,
                              microarthropods = microart_dic,
                              prs = prs_dic,
                              loi = loi_dic,
                              slope = slope_dic,
                              depth = depth_dic,
                              sm = sm_dic),
    path = "clean_data/FUNDER_data_dictionary.xlsx"),
    format = "file"
  )
)
