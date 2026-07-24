# fungal community plan

fungal_community_plan <- list(

  # 1) get soil and litter fungal OTUs and taxonomy
  tar_target(
    name = fungi_raw_soil_litter,
    command = get_file(
      node = "tx9r2",
      file = "xiii-xiv_FUNDER_soil_and_litter_OTU_with_taxonomy_ITS2.txt",
      path = here::here("raw_data/fungal_community"),
      remote_path = "ix-xv_soil_biota/xiii-xv_fungal_communities/"
    ),
    format = "file",
  ),

  # 2) format fungal OTU table
  tar_target(
    name = fungi_otu,
    command = get_fungi_otu_table(file = fungi_raw_soil_litter)
  ),

  # 3) format fungal taxonomy table
  tar_target(
    name = fungi_tax,
    command = get_fungi_tax_table(file = fungi_raw_soil_litter)
  ),

  # 4) get sample data
  tar_target(
    name = fungi_plotIDs,
    command = get_file(
      node = "tx9r2",
      file = "xiii-xiv_FUNDER_plotIDs.xlsx",
      path = here::here("raw_data/fungal_community"),
      remote_path = "ix-xv_soil_biota/xiii-xv_fungal_communities/"
    ),
    format = "file",
  ),

  # 4) format sample data
  tar_target(
    name = fungi_sam,
    command = get_soil_litter_fungi_sample_data(file = fungi_plotIDs,
                                                df = FALSE)
  ),

  # 5) assemble phyloseq object
  tar_target(
    name = fungi_phyloseq,
    command = assemble_fungi_phyloseq(otu_tab = fungi_otu,
                                      tax_tab = fungi_tax,
                                      sam_tab = fungi_sam)
  ),

  # 6) funcabise and subset soil samples
  tar_target(
    name = soil_fungi_phyloseq,
    command = subset_phyloseq(ps = fungi_phyloseq, to_keep = "soil")
  ),

  # 7) funcabise and subset plant litter samples
  tar_target(
    name = litter_fungi_phyloseq,
    command = subset_phyloseq(ps = fungi_phyloseq, to_keep = "litter")
  ),

  ##### Targets 16 - 21 take care of assembling and cleaning the ######
  ##### fungal necromass fungal community datasets: ###################

  # 8) get necromass fungal OTUs and taxonomy
  tar_target(
    name = fungi_raw_necromass,
    command = get_file(
      node = "tx9r2",
      file = "xv_FUNDER_fungal_necromass_OTU_with_taxonomy_ITS2.txt",
      path = here::here("raw_data/fungal_community"),
      remote_path = "ix-xv_soil_biota/xiii-xv_fungal_communities/"
    ),
    format = "file",
  ),

  # 9) get fungal OTU table
  tar_target(
    name = fungi_otu_necromass,
    command = get_fungi_otu_table(file = fungi_raw_necromass)
  ),

  # 10) get fungal taxonomy table
  tar_target(
    name = fungi_tax_necromass,
    command = get_fungi_tax_table(file = fungi_raw_necromass)
  ),

  # 11) get sample data
  tar_target(
    name = necromass_sample_data,
    command = get_file(
      node = "tx9r2",
      file = "xv_necromass_sample_data.xlsx",
      path = here::here("raw_data/fungal_community"),
      remote_path = "ix-xv_soil_biota/xiii-xv_fungal_communities/"
    ),
    format = "file",
  ),

  # 12) format necromass sample data
  tar_target(
    name = fungi_sam_necromass,
    command = get_necromass_fungi_sample_data(file = necromass_sample_data)
  ),

  # 12) combine to phyloseq object
  tar_target(
    name = necro_phy,
    command = assemble_fungi_phyloseq(otu_tab = fungi_otu_necromass,
                                      tax_tab = fungi_tax_necromass,
                                      sam_tab = fungi_sam_necromass)
  ),

  # 13) funcabise necromass
  tar_target(
    name = necromass_fungi_phyloseq,
    command = subset_phyloseq(ps = necro_phy, to_keep = "necromass")
  ),

  # 14) save the outputs
  # soil
  tar_target(
    name = soil_fungi_output,
    command = saveRDS(
      object = soil_fungi_phyloseq,
      file = "clean_data/xiii_FUNDER_clean_soil_fungal_community_2022.RDS"),
    format = "file"
  ),

  #litter
  tar_target(
    name = litter_fungi_output,
    command = saveRDS(
      object = litter_fungi_phyloseq,
      file = "clean_data/xiv_FUNDER_clean_litter_fungal_community_2022.RDS"),
    format = "file"
  ),

  # necromass
  tar_target(
    name = necromass_fungi_output,
    command = saveRDS(
      object = necromass_fungi_phyloseq,
      file = "clean_data/xv_FUNDER_clean_necromass_fungal_community_2022.RDS"),
    format = "file"
  )

)
