# plant litter decomposition plan

plant_litter_decomposition_plan <- list(

  # get litter bags weights pre burial
  tar_target(
    name = get_litter_pre_burial_weights,
    command = get_file(
      node = "tx9r2",
      file = "xx_FUNDER_raw_beforeburrying_litter_biomass_2021.xlsx",
      path = here::here("raw_data/soil_carbon_and_nitrogen"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xx_plant_litter_decomposition/"),
    format = "file"
  ),

  # clean pre burial weights
  tar_target(
    name = litter_pre_burial_weights,
    command = clean_pre(data = get_litter_pre_burial_weights)
  ),

  # get litter bags weights post burial
  tar_target(
    name = get_litter_post_burial_weights,
    command = get_file(
      node = "tx9r2",
      file = "xx_FUNDER_raw_afterburrying_litter_biomass_2022.xlsx",
      path = here::here("raw_data/soil_carbon_and_nitrogen"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xx_plant_litter_decomposition/"),
    format = "file"
  ),

  # clean post burial weights
  tar_target(
    name = litter_post_burial_weights,
    command = clean_post(data = get_litter_post_burial_weights)
  ),

  # get comments for the litter bags
  tar_target(
    name = litter_comments,
    command = get_com(pre = litter_pre_burial_weights,
                      post = litter_post_burial_weights)
  ),

  # combine pre weights, post weights, and comments
  tar_target(
    name = clean_plant_litter_decomposition,
    command = finish(pre = litter_pre_burial_weights,
                     post = litter_post_burial_weights,
                     comment = litter_comments)
  ),

  # save output
  tar_target(
    name = plant_litter_decomposition_output,
    command = save_csv(
      file = clean_plant_litter_decomposition,
      name = "xx_FUNDER_clean_plant_litter_decomposition_2022.csv"
    )
  )
)
