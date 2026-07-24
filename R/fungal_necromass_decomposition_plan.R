# fungal necromass decomposition plan

fungal_necromass_decomposition_plan <- list(

  # get necromass bags weights pre burial
  tar_target(
    name = get_necromass_pre_burial_weights,
    command = get_file(
      node = "tx9r2",
      file = "xxi_FUNDER_raw_fungal_necromass_start_weights.xlsx",
      path = here::here("raw_data/"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xxi_fungal_necromass_decomposition/"),
    format = "file"
  ),

  # clean weights pre burial and make tag corrections
  tar_target(
    name = necromass_pre_burial_weights,
    command = clean_necro_pre(data = get_necromass_pre_burial_weights)
  ),

  # get necromass bag weights post burial
  tar_target(
    name = get_necromass_post_burial_weights,
    command = get_file(
      node = "tx9r2",
      file = "xxi_FUNDER_mass_loss_2022 - fungal_necromass.csv",
      path = here::here("raw_data/"),
      remote_path = "xvi-xxii_carbon_and_nutrient_cycling/xxi_fungal_necromass_decomposition/"),
    format = "file"
  ),

  # clean weights post burial and make tag corrections
  tar_target(
    name = necromass_post_burial_weights,
    command = clean_necro_post(data = get_necromass_post_burial_weights)
  ),

  # combine pre and post weights and clean
  tar_target(
    name = necromass_post_comments,
    command = get_necromass_comments(post = necromass_post_burial_weights)
  ),

  # combine pre weights, post weights, and comments
  tar_target(
    name = clean_fungal_necromass_decomposition,
    command = finish_necromass(
      pre = necromass_pre_burial_weights,
      post = necromass_post_burial_weights,
      comment = necromass_post_comments)
  ),

  # save output
  tar_target(
    name = fungal_necromass_decomposition_output,
    command = save_csv(
      file = clean_fungal_necromass_decomposition,
      name = "xxi_FUNDER_clean_fungal_necromass_decomposition_2022.csv"
    )
  )
)
