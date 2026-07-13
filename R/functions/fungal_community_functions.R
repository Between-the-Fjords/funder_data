#############################################################
################ fungal community functions #################
#############################################################

# this gets OTU table
get_fungi_otu_table <- function(file) {

  # get the results from bioinformatic filtering
  raw_fungi <- read.delim(file)

  # get OTUs
  otu <- raw_fungi |>
    filter(Kingdom == "Fungi",
           Tax_above_threshold != "",
           Tax_above_threshold != "NA") |>
    select(-c(Kingdom, Phylum, Class, Order, Family, Genus, Species,
              Kingdom_support, Phylum_support, Class_support, Order_support,
              Family_support, Genus_support, Species_support,
              Strand, Tax_above_threshold)) |>
    column_to_rownames("OTUId") %>%
    mutate("OTU" = rownames(.), .before = 1) |>
    t() |>
    data.frame() %>%
    mutate(sample_number = rownames(.), .before = 1,
           sample_number = sub("S", "", sample_number),
           sample_number = as.numeric(sample_number),
           sample_number = paste0("sample_", sample_number)) |>
    row_to_names(row_number = 1)

  rownames(otu) <- otu$sample_NA

  otu <- otu |>
    select(-sample_NA) |>
    mutate_if(is.character, as.numeric)

  fungi_otu <- phyloseq::otu_table(as.matrix(otu), taxa_are_rows = FALSE)

  return(fungi_otu)
}

# this gets taxonomy
get_fungi_tax_table <- function(file) {

  # get the results from bioinformatic filtering
  raw_fungi <- read.delim(file)

  # get OTUs
  tax <- raw_fungi |>
    # select OTU-ID's and strings with taxonomy above the quality threshold set
    # during taxonomic annotation
    select(
      OTUId, Tax_above_threshold
    ) |>
    # separate taxonomic string to dfferent levels
    separate(Tax_above_threshold,
             c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
             sep = ",") |>
    # 'clean' to lower initial letter
    clean_names() |>
    # clean taxonomic level names
    mutate(kingdom = sub("k:", "", kingdom),
           phylum = sub("p:", "", phylum),
           class = sub("c:", "", class),
           order = sub("o:", "", order),
           family = sub("f:", "", family),
           genus = sub("g:", "", genus),
           species = sub("s:", "", species)) |>
    # filter out anything not fungi
    filter(kingdom == "Fungi") |>
    column_to_rownames("otu_id")

  # convert to phyloseq format
  fungi_tax <- phyloseq::tax_table(tax)

  # fix column names
  colnames(fungi_tax) <- colnames(tax)

  # fix taxa names
  phyloseq::taxa_names(fungi_tax) <- rownames(tax)

  return(fungi_tax)

}

# this function distributes all samples across the DNA extraction batches.
# the
funder_samples_in_extraction_batches <- function(file, df) {

  set.seed(123)
  funder <- readxl::read_xlsx(path = file) # Get FUNDER plot IDs

  # create a vector for the soil samples:
  soil_samples <- unique(funder$plotID)
  soil_samples <- sort(paste0(soil_samples, "_", "soil", "_", "sample"))

  # create vectors for the litter bag samples:
  litter_samples <- funder |>
    # subset the four sites included in the litter assay
    filter(site == "Ovs" | site == "Skj" | site == "Fau" | site == "Ulv")

  # make vector for forb litter
  forbs <- paste0(litter_samples$plotID, "_", "litter", "_", "forbs")
  # for graminoid litter
  graminoids <- paste0(litter_samples$plotID, "_", "litter", "_", "graminoids")

  # common vector with forbs and graminoids
  litter_samples <- sort(c(forbs, graminoids))
  # remove superfluous vectors
  rm(forbs, graminoids)

  # create vector for the block 4 control samples:
  block4 <- funder |>
    filter(treatment == "FB" | treatment == "GB" | treatment == "GF" | treatment == "FGB" | treatment == "C") |>
    mutate(plotID = gsub("1", "4", plotID),
           plotID = gsub("2", "4", plotID),
           plotID = gsub("3", "4", plotID))

  block4_samples <- paste0(unique(block4$plotID), "_", "soil", "_", "control")

  # combine soil samples, litter bag samples, and block 4 samples to one vector:
  samples <- sample(c(soil_samples, litter_samples, block4_samples)); samples

  #####

  ### Pick 90 random samples for plate 1-6:

  # plate 1
  # subset 90 random samples
  plate1 <- sample(samples, size = 90, replace = FALSE)

  # remove from vector with all samples
  samples <- setdiff(samples, plate1)

  # add mock and blanks
  plate1 <- c(plate1, "mock_community", "extraction_blank", "pcr_blank", "sequencing_blank")

  # use sample() to shuffle the samples to random plate positions
  plate1 <- sample(plate1)

  # add the two technical replicates to the very end of the plate
  plate1 <- c(plate1, "tech_replicate1", "tech_replicate2")

  # assign sample to use as technical replicate nr 1
  plate1[95] <- "Arh2G_soil_sample_replicate"

  # assign sample to use as technical replicate nr 2
  plate1[96] <- "Vik4GB_soil_control_replicate"

  # Ulv3C_litter_forbs was missing, replace with extra technical replicate
  plate1[3] <- "Skj3GB_litter_forbs_replicate"

  # Ulv1B_litter_forbs was missing, replace with extra technical replicate
  plate1[12] <- "Ram1GF_soil_sample_replicate"

  # convert vector to data frame via matrix
  plate1df <- matrix(plate1, ncol = 12, nrow = 8)
  plate1df <- as.data.frame(plate1df)

  # fix rownames...
  rownames(plate1df) <- LETTERS[1:8]

  # and column names
  colnames(plate1df) <- 1:12

  plate2 <- sample(samples, size = 90, replace = FALSE) # subset 90 random samples
  samples <- setdiff(samples, plate2) # remove from vector with all samples
  plate2 <- c(plate2, "mock_community", "extraction_blank", "pcr_blank", "sequencing_blank") # add mock and blanks
  plate2 <- sample(plate2) # use sample() to shuffle the samples to random plate positions
  plate2 <- c(plate2, "tech_replicate1", "tech_replicate2") # add the two technical replicates to the very end of the plate
  plate2[95] <- "Lav2GF_soil_sample_replicate" # assign sample to use as technical replicate nr 1
  plate2[96] <- "Ves1G_soil_sample_replicate" # assign sample to use as technical replicate nr 2
  plate2[55] <- "Lav3GB_soil_sample_replicate" # Ulv1G_litter_forbs was missing, replace with extra technical replicate
  plate2[92] <- "Vik3GF_soil_sample_replicate" # Fau3G_litter_graminoids was missing, replace with extra technical replicate
  plate2df <- matrix(plate2, ncol = 12, nrow = 8) # convert vector to matrix
  plate2df <- as.data.frame(plate2df)
  rownames(plate2df) <- LETTERS[1:8] # fix rownames...
  colnames(plate2df) <- 1:12 # and column names

  plate3 <- sample(samples, size = 90, replace = FALSE) # subset 90 random samples
  samples <- setdiff(samples, plate3) # remove from vector with all samples
  plate3 <- c(plate3, "mock_community", "extraction_blank", "pcr_blank", "sequencing_blank") # add mock and blanks
  plate3 <- sample(plate3) # use sample() to shuffle the samples to random plate positions
  plate3 <- c(plate3, "tech_replicate1", "tech_replicate2") # add the two technical replicates to the very end of the plate
  plate3[95] <- "Ram1GB_soil_sample_replicate" # assign sample to use as technical replicate nr 1
  plate3[96] <- "Lav1F_soil_sample_replicate" # assign sample to use as technical replicate nr 2
  plate3[52] <- "Ulv3C_litter_graminoids_replicate" # Ulv1B_soil was missing, replace with extra technical replicate
  plate3df <- matrix(plate3, ncol = 12, nrow = 8) # convert vector to matrix
  plate3df <- as.data.frame(plate3df)
  rownames(plate3df) <- LETTERS[1:8] # fix rownames...
  colnames(plate3df) <- 1:12 # and column names

  plate4 <- sample(samples, size = 90, replace = FALSE) # subset 90 random samples
  samples <- setdiff(samples, plate4) # remove from vector with all samples
  plate4 <- c(plate4, "mock_community", "extraction_blank", "pcr_blank", "sequencing_blank") # add mock and blanks
  plate4 <- sample(plate4) # use sample() to shuffle the samples to random plate positions
  plate4 <- c(plate4, "tech_replicate1", "tech_replicate2") # add the two technical replicates to the very end of the plate
  plate4[95] <- "Ovs2GF_litter_graminoids_replicate" # assign sample to use as technical replicate nr 1
  plate4[96] <- "Fau2FB_soil_sample_replicate" # assign sample to use as technical replicate nr 2
  plate4[34] <- "Ves1GF_soil_sample_replicate" # Fau2GB_litter_forbs was missing, replace with extra technical replicate
  plate4[59] <- "Ovs1GB_soil_sample_replicate" # Fau2GB_soil was missing, replace with extra technical replicate
  plate4[84] <- "Fau4FGB_soil_control_replicate" # Fau2GB_litter_graminoids was missing, replace with extra technical replicate
  plate4[86] <- "Ram4GF_soil_control_replicate" # Fau3G_litter_graminoids was missing, replace with extra technical replicate
  plate4df <- matrix(plate4, ncol = 12, nrow = 8) # convert vector to matrix
  plate4df <- as.data.frame(plate4df)
  rownames(plate4df) <- LETTERS[1:8] # fix rownames...
  colnames(plate4df) <- 1:12 # and column names

  plate5 <- sample(samples, size = 90, replace = FALSE) # subset 90 random samples
  samples <- setdiff(samples, plate5) # remove from vector with all samples
  plate5 <- c(plate5, "mock_community", "extraction_blank", "pcr_blank", "sequencing_blank") # add mock and blanks
  plate5 <- sample(plate5) # use sample() to shuffle the samples to random plate positions
  plate5 <- c(plate5, "tech_replicate1", "tech_replicate2") # add the two technical replicates to the very end of the plate
  plate5[95] <- "Ulv1GB_litter_forbs_replicate" # assign sample to use as technical replicate nr 1
  plate5[96] <- "Ram2GF_soil_sample_replicate" # assign sample to use as technical replicate nr 2
  plate5[7] <- "Ulv1C_soil_sample_replicate" # Skj4C_soil_control was missing, replace with extra technical replicate
  plate5[27] <- "Gud1GF_soil_sample_replicate" # Ulv1B_litter_graminoids was missing, replace with extra technical replicate
  plate5df <- matrix(plate5, ncol = 12, nrow = 8) # convert vector to matrix
  plate5df <- as.data.frame(plate5df)
  rownames(plate5df) <- LETTERS[1:8] # fix rownames...
  colnames(plate5df) <- 1:12 # and column names


  plate6 <- sample(samples, size = 90, replace = FALSE) # subset 90 random samples
  samples <- setdiff(samples, plate6) # remove from vector with all samples
  plate6 <- c(plate6, "mock_community", "extraction_blank", "pcr_blank", "sequencing_blank") # add mock and blanks
  plate6 <- sample(plate6) # use sample() to shuffle the samples to random plate positions
  plate6 <- c(plate6, "tech_replicate1", "tech_replicate2") # add the two technical replicates to the very end of the plate
  plate6[95] <- "Skj3FGB_litter_graminoids_replicate" # assign sample to use as technical replicate nr 1
  plate6[96] <- "Hog2B_soil_sample_replicate" # assign sample to use as technical replicate nr 2
  plate6df <- matrix(plate6, ncol = 12, nrow = 8) # convert vector to matrix
  plate6df <- as.data.frame(plate6df)
  rownames(plate6df) <- LETTERS[1:8] # fix rownames...
  colnames(plate6df) <- 1:12 # and column names

  funder_samples_df <- list(plate1df, plate2df, plate3df, plate4df, plate5df, plate6df)
  funder_samples <- list(plate1, plate2, plate3, plate4, plate5, plate6)

  # set TRUE for microbial abundance dataset
  if(df == TRUE) {
    return(funder_samples_df)
  }

  # set FALSE for fungal community dataset
  if(df == FALSE) {
    return(funder_samples)
  }


}

# this gets sample data for soil and litter samples
get_soil_litter_fungi_sample_data <- function(file, df) {

  # call the plotID's file to get vectors of all FUNDER sample ID's
  funder_samples <- funder_samples_in_extraction_batches(
    file = "raw_data/fungal_community/FUNDER_plotIDs.xlsx",
    df = TRUE)

  # relate sample number to IDs and DNA extraction batches ("plate")
  sam <- data.frame(
    sample_number = paste0("sample_", 1:576),
    sample_id = unlist(funder_samples),
    plate = rep(1:6, each = 96)) |>

    # separate sample ID's to several columns and tidy up a bit
    separate(sample_id, c("plotID", "sample_material",
                          "sample_category", "replicate_category"),
             sep = "_") |>
    replace_na(list(replicate_category = "not_replicate")) |>
    column_to_rownames("sample_number") |>
    mutate(
      sample_category = ifelse(
        test = is.na(sample_category),
        yes = paste0(plotID, "_", sample_material),
        no = sample_category),

      sample_material = ifelse(
        test = sample_material == "blank",
        yes = "technical_validation",
        no = sample_material),

      sample_material = ifelse(
        test = sample_material == "community",
        yes = "technical_validation",
        no = sample_material),

      plotID = ifelse(
        test = sample_material == "technical_validation",
        yes = NA,
        no = plotID))

  fungi_sam <- phyloseq::sample_data(sam)

  return(fungi_sam)

}

# this gets sample data for necromass samples
get_necromass_fungi_sample_data <- function(file) {

  sam <- readxl::read_xlsx(file) |>
    # mark technical replicates and fix sample names
    mutate(
      replicate_category = ifelse(
        test = grepl("tech_replicate", sampleID),
        yes = "replicate",
        no = "not_replicate"),
      sampleID = recode_values(
        sample_no,
        "S062" ~ "Ovs1F_nonmel",
        "S085" ~ "Skj1G_mel",
        "S095" ~ "Fau3C_nonmel",
        "S096" ~ "Skj3GF_mel",
        "S118" ~ "Skj1FGB_mel",
        "S157" ~ "Fau2FB_nonmel",
        "S191" ~ "Skj2G_mel",
        "S192" ~ "Skj2B_nonmel",
        default = sampleID
      )) |>
    separate(sampleID, sep = "_", into = c("plotID", "content")) |>
    mutate(sampleID = paste0(plotID, "_", content),
           siteID = substr(sampleID, 1, 3),
           siteID = recode_values(
             siteID,
             "Gud" ~ "Gudmedalen",
             "Lav" ~ "Lavisdalen",
             "Ram" ~ "Rambera",
             "Ulv" ~ "Ulvehaugen",
             "Skj" ~ "Skjelingahaugen",
             "Alr" ~ "Alrust",
             "Arh" ~ "Arhelleren",
             "Fau" ~ "Fauske",
             "Hog" ~ "Hogsete",
             "Ovs" ~ "Ovstedalen",
             "Vik" ~ "Vikesland",
             "Ves" ~ "Veskre"
           ),
           blockID = substr(sampleID, 1, 4),
           treatment = substr(plotID, 5, 7),
           year = 2022) |>
    select(sample_no, year, siteID, blockID, plotID, treatment, content, replicate_category) %>%
    mutate(sample_number = rownames(.), .before = 1,
           sample_number = sub("S", "", sample_number),
           sample_number = as.numeric(sample_number),
           sample_number = paste0("sample_", sample_number),
           content = ifelse(test = is.na(siteID),
                            yes = paste0(plotID, "_", content),
                            no = content),
           blockID = ifelse(test = is.na(siteID),
                            yes = NA,
                            no = blockID),
           plotID = ifelse(test = is.na(siteID),
                           yes = NA,
                           no = plotID),
           treatment = ifelse(test = is.na(siteID),
                              yes = NA,
                              no = treatment)) |>
    select(-sample_no)


  sam <- phyloseq::sample_data(sam) # format to phyloseq compatability:
  #sample_names(sam) <- sam$sample_number # fix sample names

  return(sam)
}

# this assembles phyloseq object with all samples, including blanks,
# mock communities, and replicates
assemble_fungi_phyloseq <- function(otu_tab, tax_tab, sam_tab) {

  phyloseq::phyloseq(otu_tab = otu_tab,
                     tax_tab = tax_tab,
                     sam_tab = sam_tab)

}

# this subsets the phyloseq objects based on sample material, then removes
# OTUs with 0 abundance and funcabises after the subsetting
subset_phyloseq <- function(ps, to_keep) {

  # funcabise sample data
  ps_sam_names <- sample_names(ps)
  df <- data.frame(ps@sam_data) |>
    mutate(year = 2022, ,
           siteID = substr(plotID, 1, 3),
           siteID = recode_values(
             siteID,
             "Gud" ~ "Gudmedalen",
             "Lav" ~ "Lavisdalen",
             "Ram" ~ "Rambera",
             "Ulv" ~ "Ulvehaugen",
             "Skj" ~ "Skjelingahaugen",
             "Alr" ~ "Alrust",
             "Arh" ~ "Arhelleren",
             "Fau" ~ "Fauske",
             "Hog" ~ "Hogsete",
             "Ovs" ~ "Ovstedalen",
             "Vik" ~ "Vikesland",
             "Ves" ~ "Veskre"
           ),
           blockID = substr(plotID, 1, 4),
           treatment = substr(plotID, 5, 7)) |>
    dataDocumentation::funcabization(convert_to = "FunCaB") |>
    left_join(
      data.frame(
        dataDocumentation::create_funder_meta_data()
      )
    ) |>
    relocate(year, siteID, blockID, plotID, treatment,
             any_of(c("sample_category")))

  df <- sample_data(df)
  sample_names(df) <- ps_sam_names
  ps@sam_data <- df

  # for subsetting soil samples
  if(to_keep == "soil") {
    soil_fungi_phyloseq <- phyloseq::subset_samples(physeq = ps,
                                                    sample_material != "litter")

    soil_fungi_phyloseq <- prune_taxa(taxa_sums(soil_fungi_phyloseq) > 0,
                                      soil_fungi_phyloseq)

    soil_fungi_phyloseq <- speedyseq::select_sample_data(soil_fungi_phyloseq,
                                                         -c(sample_material,
                                                            plate))

    soil_fungi_phyloseq <- speedyseq::rename_sample_data(soil_fungi_phyloseq,
                                                         content = sample_category)

    soil_fungi_phyloseq <- speedyseq::mutate_sample_data(soil_fungi_phyloseq,
                                                         plotID = gsub("NANA", NA, plotID))

    soil_fungi_phyloseq <- prune_taxa(
      names(sort(taxa_sums(soil_fungi_phyloseq), decreasing = TRUE)),
      soil_fungi_phyloseq)

    taxa_names(soil_fungi_phyloseq) <- paste0("OTU_", 1:ntaxa(soil_fungi_phyloseq))

    return(soil_fungi_phyloseq)

    # for subsetting plant litter samples
  }
  else if(to_keep == "litter") {
    litter_fungi_phyloseq <- phyloseq::subset_samples(physeq = ps,
                                                      sample_material != "soil")
    litter_fungi_phyloseq <- prune_taxa(taxa_sums(litter_fungi_phyloseq) > 0,
                                        litter_fungi_phyloseq)

    litter_fungi_phyloseq <- speedyseq::select_sample_data(litter_fungi_phyloseq,
                                                           -c(sample_material,
                                                              plate))

    litter_fungi_phyloseq <- speedyseq::rename_sample_data(litter_fungi_phyloseq,
                                                           content = sample_category)

    litter_fungi_phyloseq <- speedyseq::mutate_sample_data(litter_fungi_phyloseq,
                                                           plotID = gsub("NANA", NA, plotID))

    litter_fungi_phyloseq <- prune_taxa(
      names(sort(taxa_sums(litter_fungi_phyloseq), decreasing = TRUE)),
      litter_fungi_phyloseq)

    taxa_names(litter_fungi_phyloseq) <- paste0("OTU_", 1:ntaxa(litter_fungi_phyloseq))

    return(litter_fungi_phyloseq)

    # for necromass samples
  }
  else if(to_keep == "necromass") {

    necromass_fungi_phyloseq <- prune_taxa(taxa_sums(ps) > 0, ps)

    necromass_fungi_phyloseq <- speedyseq::mutate_sample_data(necromass_fungi_phyloseq,
                                                              plotID = gsub("NANA", NA, plotID),
                                                              content = replace_values(
                                                                content,
                                                                "mel" ~ "melanised",
                                                                "nonmel" ~ "nonmelanised"
                                                              ))

    necromass_fungi_phyloseq <- prune_taxa(
      names(sort(taxa_sums(necromass_fungi_phyloseq), decreasing = TRUE)),
      necromass_fungi_phyloseq)

    taxa_names(necromass_fungi_phyloseq) <- paste0("OTU_", 1:ntaxa(necromass_fungi_phyloseq))

    return(necromass_fungi_phyloseq)

  }

}
