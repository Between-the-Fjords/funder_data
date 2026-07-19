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

# this gets sample data for soil and litter samples
get_soil_litter_fungi_sample_data <- function(file, df) {

  funder_samples <- funder_samples_in_extraction_batches(file = file, df = df)

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
    select(sample_no, year, siteID, blockID, plotID, treatment, content, replicate_category) |>
    mutate(
      content = ifelse(
        is.na(siteID),
        paste0(plotID, "_", content),
        content
      ),
      blockID = ifelse(is.na(siteID), NA, blockID),
      plotID = ifelse(is.na(siteID), NA, plotID),
      treatment = ifelse(is.na(siteID), NA, treatment),
      sample_number = sub("^S0*", "", sample_no),
      sample_number = as.numeric(sample_number),
      sample_number = paste0("sample_", sample_number)
    ) |>
    select(-sample_no) |>
    column_to_rownames("sample_number")

  sam <- phyloseq::sample_data(sam)

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
