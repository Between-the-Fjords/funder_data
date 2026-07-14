#############################################################
############### microbial abundance functions ###############
#############################################################

### DNA was extraacted in six batches of 90 (+ replicates) by plate extraction,
### and stored on PCR plates. this code distributes samples to their
### DNA extraction batch positions:
funder_samples_in_extraction_batches <- function(file, df) {

  set.seed(123)
  funder <- if (is.data.frame(file)) {
    file
  } else {
    readxl::read_xlsx(path = file)
  }

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

### this distributes samples to the qPCR plate positions:
qpcr_run_maker <- function(DNA_extraction_batch, run, df){

  standard_curve = c("Neg", "S1", "S2", "S3", "S4", "S5", "S6", "S7")
  DNA_extraction_batch = DNA_extraction_batch
  if (run == 1) {
    return(data.frame("01" = standard_curve,
                      "02" = standard_curve,
                      "03" = standard_curve,
                      "04" = df[,1],
                      "05" = df[,1],
                      "06" = df[,1],
                      "07" = df[,2],
                      "08" = df[,2],
                      "09" = df[,2],
                      "10" = df[,3],
                      "11" = df[,3],
                      "12" = df[,3],
                      row.names = c(LETTERS[1:8])) |>
             tidyr::pivot_longer(
               cols = everything(), values_to = "content") |>
             dplyr::mutate(
               DNA_extraction_batch = as.character(DNA_extraction_batch),
               run = paste0(DNA_extraction_batch, "_", run),
               row = rep(LETTERS[1:8], each = 12),
               name = sub("X", "", name),
               well_position = paste0(row, name)) |>
             dplyr::select(DNA_extraction_batch, run, well_position, content))
  }
  if (run == 2) {
    return(data.frame("01" = standard_curve,
                      "02" = standard_curve,
                      "03" = standard_curve,
                      "04" = df[,4],
                      "05" = df[,4],
                      "06" = df[,4],
                      "07" = df[,5],
                      "08" = df[,5],
                      "09" = df[,5],
                      "10" = df[,6],
                      "11" = df[,6],
                      "12" = df[,6],
                      row.names = c(LETTERS[1:8])) |>
             tidyr::pivot_longer(
               cols = everything(), values_to = "content") |>
             dplyr::mutate(
               DNA_extraction_batch = as.character(DNA_extraction_batch),
               run = paste0(DNA_extraction_batch, "_", run),
               row = rep(LETTERS[1:8], each = 12),
               name = sub("X", "", name),
               well_position = paste0(row, name)) |>
             dplyr::select(DNA_extraction_batch, run, well_position, content))
  }
  if (run == 3) {
    return(data.frame("01" = standard_curve,
                      "02" = standard_curve,
                      "03" = standard_curve,
                      "04" = df[,7],
                      "05" = df[,7],
                      "06" = df[,7],
                      "07" = df[,8],
                      "08" = df[,8],
                      "09" = df[,8],
                      "10" = df[,9],
                      "11" = df[,9],
                      "12" = df[,9],
                      row.names = c(LETTERS[1:8])) |>
             tidyr::pivot_longer(
               cols = everything(), values_to = "content") |>
             dplyr::mutate(
               DNA_extraction_batch = as.character(DNA_extraction_batch),
               run = paste0(DNA_extraction_batch, "_", run),
               row = rep(LETTERS[1:8], each = 12),
               name = sub("X", "", name),
               well_position = paste0(row, name)) |>
             dplyr::select(DNA_extraction_batch, run, well_position, content))
  }
  if (run == 4) {
    return(data.frame("01" = standard_curve,
                      "02" = standard_curve,
                      "03" = standard_curve,
                      "04" = df[,10],
                      "05" = df[,10],
                      "06" = df[,10],
                      "07" = df[,11],
                      "08" = df[,11],
                      "09" = df[,11],
                      "10" = df[,12],
                      "11" = df[,12],
                      "12" = df[,12],
                      row.names = c(LETTERS[1:8])) |>
             tidyr::pivot_longer(
               cols = everything(), values_to = "content") |>
             dplyr::mutate(
               DNA_extraction_batch = as.character(DNA_extraction_batch),
               run = paste0(DNA_extraction_batch, "_", run),
               row = rep(LETTERS[1:8], each = 12),
               name = sub("X", "", name),
               well_position = paste0(row, name)) |>
             dplyr::select(DNA_extraction_batch, run, well_position, content))
  }

}

### create qPCR-to-sample map
qpcr_map_maker <- function(data) {

  map <- bind_rows(

    # Extraction batch 1
    qpcr_run_maker(DNA_extraction_batch = 1, run = 1, df = data.frame(data[1])),
    qpcr_run_maker(DNA_extraction_batch = 1, run = 2, df = data.frame(data[1])),
    qpcr_run_maker(DNA_extraction_batch = 1, run = 3, df = data.frame(data[1])),
    qpcr_run_maker(DNA_extraction_batch = 1, run = 4, df = data.frame(data[1])),
    # Extraction batch 2
    qpcr_run_maker(DNA_extraction_batch = 2, run = 1, df = data.frame(data[2])),
    qpcr_run_maker(DNA_extraction_batch = 2, run = 2, df = data.frame(data[2])),
    qpcr_run_maker(DNA_extraction_batch = 2, run = 3, df = data.frame(data[2])),
    qpcr_run_maker(DNA_extraction_batch = 2, run = 4, df = data.frame(data[2])),
    # Extraction batch 3
    qpcr_run_maker(DNA_extraction_batch = 3, run = 1, df = data.frame(data[3])),
    qpcr_run_maker(DNA_extraction_batch = 3, run = 2, df = data.frame(data[3])),
    qpcr_run_maker(DNA_extraction_batch = 3, run = 3, df = data.frame(data[3])),
    qpcr_run_maker(DNA_extraction_batch = 3, run = 4, df = data.frame(data[3])),
    # Extraction batch 4
    qpcr_run_maker(DNA_extraction_batch = 4, run = 1, df = data.frame(data[4])),
    qpcr_run_maker(DNA_extraction_batch = 4, run = 2, df = data.frame(data[4])),
    qpcr_run_maker(DNA_extraction_batch = 4, run = 3, df = data.frame(data[4])),
    qpcr_run_maker(DNA_extraction_batch = 4, run = 4, df = data.frame(data[4])),
    # Extraction batch 5
    qpcr_run_maker(DNA_extraction_batch = 5, run = 1, df = data.frame(data[5])),
    qpcr_run_maker(DNA_extraction_batch = 5, run = 2, df = data.frame(data[5])),
    qpcr_run_maker(DNA_extraction_batch = 5, run = 3, df = data.frame(data[5])),
    qpcr_run_maker(DNA_extraction_batch = 5, run = 4, df = data.frame(data[5])),
    # Extraction batch 6
    qpcr_run_maker(DNA_extraction_batch = 6, run = 1, df = data.frame(data[6])),
    qpcr_run_maker(DNA_extraction_batch = 6, run = 2, df = data.frame(data[6])),
    qpcr_run_maker(DNA_extraction_batch = 6, run = 3, df = data.frame(data[6])),
    qpcr_run_maker(DNA_extraction_batch = 6, run = 4, df = data.frame(data[6])),
  )

  return(map)

}

### download microbial abundance data from OSF
get_microbial_abundance_from_osf <- function(path, remote_path) {

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # get FUNDER node
  node <- osf_retrieve_node("https://osf.io/tx9r2/")

  files <- osf_ls_files(node, n_max = Inf, path = remote_path)

  # download files from directory of interest to a local directory given in path
  osf_download(
    files,
    path = path,
    recurse = TRUE,
    conflicts = "overwrite"
  )
}

### this makes import of bacteria cleaner
import_bacteria <- function(path, dna_extraction_batch, run){
  read.csv(path, sep = ";") |>
    janitor::row_to_names(19) |>
    janitor::clean_names() |>
    mutate(cq = sub(",", ".", cq),
           cq = as.numeric(cq),
           starting_quantity_sq = sub(",", ".", starting_quantity_sq),
           starting_quantity_sq = as.numeric(starting_quantity_sq),
           DNA_extraction_batch = dna_extraction_batch,
           run = run) |>
    select(DNA_extraction_batch, run, well, content, starting_quantity_sq, cq)
}

### assemble and clean bacterial abundance dataset
assemble_and_clean_bacteria <- function(map) {

  # DNA extraction batch 1
  q1_1 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_1_col_1-3_2026-02-12.csv"),
                          dna_extraction_batch = "1",
                          run = "1_1")
  q1_2 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_1_col_4-6_2026-02-12.csv"),
                          dna_extraction_batch = "1",
                          run = "1_2")
  q1_3 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_1_col_7-9_2026-02-12.csv"),
                          dna_extraction_batch = "1",
                          run = "1_3")
  q1_4 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_1_col_10-12_2026-02-12.csv"),
                          dna_extraction_batch = "1",
                          run = "1_4")

  # DNA extraction batch 1
  q2_1 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_2_col_1-3_2026-02-11.csv"),
                          dna_extraction_batch = "2",
                          run = "2_1")
  q2_2 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_2_col_4-6_2026-02-11.csv"),
                          dna_extraction_batch = "2",
                          run = "2_2")
  q2_3 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_2_col_7-9_2026-02-11.csv"),
                          dna_extraction_batch = "2",
                          run = "2_3")
  q2_4 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_2_col_10-12_2026-02-12.csv"),
                          dna_extraction_batch = "2",
                          run = "2_4")

  # DNA extraction batch 3
  q3_1 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_3_col_1-3_2026-02-07.csv"),
                          dna_extraction_batch = "3",
                          run = "3_1")
  q3_2 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_3_col_4-6_2026-02-07.csv"),
                          dna_extraction_batch = "3",
                          run = "3_2")
  q3_3 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_3_col_7-9_2026-02-08.csv"),
                          dna_extraction_batch = "3",
                          run = "3_3")
  q3_4 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_3_col_10-12_2026-02-09.csv"),
                          dna_extraction_batch = "3",
                          run = "3_4")

  # DNA extraction batch 4
  q4_1 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_4_col_1-3_2026-02-09.csv"),
                          dna_extraction_batch = "4",
                          run = "4_1")
  q4_2 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_4_col_4-6_2026-02-09.csv"),
                          dna_extraction_batch = "4",
                          run = "4_2")
  q4_3 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_4_col_7-9_2026-02-09.csv"),
                          dna_extraction_batch = "4",
                          run = "4_3")
  q4_4 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_4_col_10-12_2026-02-09.csv"),
                          dna_extraction_batch = "4",
                          run = "4_4")

  # DNA extraction batch 5
  q5_1 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_5_col_1-3_2026-02-09.csv"),
                          dna_extraction_batch = "5",
                          run = "5_1")
  q5_2 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_5_col_4-6_2026-02-09.csv"),
                          dna_extraction_batch = "5",
                          run = "5_2")
  q5_3 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_5_col_7-9_2026-02-10.csv"),
                          dna_extraction_batch = "5",
                          run = "5_3")
  q5_4 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_5_col_10-12_2026-02-10.csv"),
                          dna_extraction_batch = "5",
                          run = "5_4")

  # DNA extraction batch 6
  q6_1 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_6_col_1-3_2026-02-10.csv"),
                          dna_extraction_batch = "6",
                          run = "6_1")
  q6_2 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_6_col_4-6_2026-02-10.csv"),
                          dna_extraction_batch = "6",
                          run = "6_2")
  q6_3 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_6_col_7-9_2026-02-10.csv"),
                          dna_extraction_batch = "6",
                          run = "6_3")
  q6_4 <- import_bacteria(path = here::here("raw_data", "microbial_abundance", "bacteria", "plate_6_col_10-12_2026-02-11.csv"),
                          dna_extraction_batch = "6",
                          run = "6_4")

  bacterial_abundance <- rbind(q1_1, q1_2, q1_3, q1_4,
                               q2_1, q2_2, q2_3, q2_4,
                               q3_1, q3_2, q3_3, q3_4,
                               q4_1, q4_2, q4_3, q4_4,
                               q5_1, q5_2, q5_3, q5_4,
                               q6_1, q6_2, q6_3, q6_4) |>
    rename(well_position = well) |>
    select(-content) |>
    left_join(map, by = c("DNA_extraction_batch", "run", "well_position")) |>
    separate(col = content, into = c("plotID", "material", "sub", "replicate")) |>
    left_join(dataDocumentation::create_funder_meta_data()) |>
    dataDocumentation::funcabization(convert_to = "FunCaB") |>
    select(siteID, blockID, plotID, treatment, material, sub, replicate,
           DNA_extraction_batch, run, well_position, starting_quantity_sq#, cq
    ) |>
    rename(bacterial_abundance = starting_quantity_sq) |>
    mutate(bacterial_abundance = replace_na(
      bacterial_abundance, 0
    ))

  rm(q1_1, q1_2, q1_3, q1_4,
     q2_1, q2_2, q2_3, q2_4,
     q3_1, q3_2, q3_3, q3_4,
     q4_1, q4_2, q4_3, q4_4,
     q5_1, q5_2, q5_3, q5_4,
     q6_1, q6_2, q6_3, q6_4)

  return(bacterial_abundance)

}

### correct DNA extrcation batch effect for bacteria
# The abundance data for both bacteria and fungi had a visible batch effect 
# resulting from the six DNA extraction batches (see DNA extraction above), 
# and samples in batches 1 and 2 had on average noticeably lower estimated 
# starting quantities than samples in the four other batches. Because we 
# believe this to have been caused by lab procedures, we correct the batch 
# effects here and provide both corrected and uncorrected data, and leave it
# to the user to decide which to use. 
correct_bacteria_batch_effect <- function(data) {

  ##### soil samples #####
  soil <- data |>
    filter(!is.na(siteID),
           material == "soil")

  # one-row matrix to correct values
  correct_me <- soil |>
    # log-transform abundance, +1 to each sample to avoid -Inf on 0-abundance samples
    mutate(log_ab = log(bacterial_abundance + 1),
           # get unique identifier per sample replicate
           unq = paste0(plotID, "_", sub, "_", well_position)) |>
    # format to one-row matrix
    column_to_rownames("unq") |>
    select(log_ab) |>
    t()

  correct_me <- as.matrix(correct_me)

  # get metadata file containing variables to retain variation in the data from
  metadata <- soil |>
    # get climate variables into the data
    left_join(data.frame(
      siteID = c("Alrust", "Arhelleren", "Fauske",
                 "Gudmedalen", "Hogsete", "Lavisdalen",
                 "Ovstedalen", "Rambera", "Skjelingahaugen",
                 "Ulvehaugen", "Veskre", "Vikesland"),
      mean_temp = factor(c(8.5, 10.5, 10.5,
                           6.5, 8.5, 6.5,
                           10.5, 8.5, 6.5,
                           6.5, 8.5, 10.5)),
      mean_precip = factor(c(700, 2100, 700,
                             2100, 1400, 1400,
                             2800, 2100, 2800,
                             700, 2800, 1400))
    ), by = "siteID") |>
    # get unique identifier per sample replicates
    mutate(unq = paste0(plotID, "_", sub, "_", well_position)) |>
    column_to_rownames("unq") |>
    # select columns for model matrix
    select(siteID, treatment, mean_temp, mean_precip, sub, DNA_extraction_batch)

  # get model matrix for correction
  mod_mat <- model.matrix(~ treatment + mean_temp + mean_precip + siteID + sub, metadata)

  # remove batch effect with limma
  corrected <- limma::removeBatchEffect(
    x = correct_me,
    batch = metadata$DNA_extraction_batch,
    design = mod_mat
  )

  # get the corrected data into a data frame
  corrected_soil_abundance <- corrected |>
    t() |>
    data.frame() |>
    rownames_to_column("unq") |>
    separate(unq, into = c("plotID", "sub", "well_position"), sep = "_") |>
    left_join(data) |>
    # back-transform abundance
    mutate(corrected_bacterial_abundance = exp(log_ab) - 1,
           corrected_bacterial_abundance = pmax(corrected_bacterial_abundance, 0))

  ##### litter samples #####
  litter <- data |>
    filter(!is.na(siteID),
           material == "litter")

  # one-row matrix to correct values
  correct_me <- litter |>
    # log-transform abundance, +1 to each sample to avoid -Inf on 0-abundance samples
    mutate(log_ab = log(bacterial_abundance + 1),
           # get unique identifier per sample replicate
           unq = paste0(plotID, "_", sub, "_", well_position)) |>
    # format to one-row matrix
    column_to_rownames("unq") |>
    select(log_ab) |>
    t()

  correct_me <- as.matrix(correct_me)

  # get metadata file containing variables to retain variation in the data from
  metadata <- litter |>
    # get climate variables into the data
    left_join(data.frame(
      siteID = c("Alrust", "Arhelleren", "Fauske",
                 "Gudmedalen", "Hogsete", "Lavisdalen",
                 "Ovstedalen", "Rambera", "Skjelingahaugen",
                 "Ulvehaugen", "Veskre", "Vikesland"),
      mean_temp = factor(c(8.5, 10.5, 10.5,
                           6.5, 8.5, 6.5,
                           10.5, 8.5, 6.5,
                           6.5, 8.5, 10.5)),
      mean_precip = factor(c(700, 2100, 700,
                             2100, 1400, 1400,
                             2800, 2100, 2800,
                             700, 2800, 1400))
    ), by = "siteID") |>
    # get unique identifier per sample replicates
    mutate(unq = paste0(plotID, "_", sub, "_", well_position)) |>
    column_to_rownames("unq") |>
    # select columns for model matrix
    select(siteID, treatment, mean_temp, mean_precip, sub, DNA_extraction_batch)

  # get model matrix for correction
  mod_mat <- model.matrix(~ treatment + mean_temp + mean_precip + siteID + sub, metadata)

  # remove batch effect with limma
  corrected <- limma::removeBatchEffect(
    x = correct_me,
    batch = metadata$DNA_extraction_batch,
    design = mod_mat
  )

  # get the corrected data into a data frame
  corrected_litter_abundance <- corrected |>
    t() |>
    data.frame() |>
    rownames_to_column("unq") |>
    separate(unq, into = c("plotID", "sub", "well_position"), sep = "_") |>
    left_join(data) |>
    # back-transform abundance
    mutate(corrected_bacterial_abundance = exp(log_ab) - 1,
           corrected_bacterial_abundance = pmax(corrected_bacterial_abundance, 0))

  corrected_abundance <- rbind(
    corrected_soil_abundance,
    corrected_litter_abundance
  ) |>
    select(-log_ab)

  return(corrected_abundance)
}

### assemble and clean fungal abundance dataset
assemble_and_clean_fungi <- function(map) {

  # plate 1
  q1_1 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_1_col_1-3_2025-09-30 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "1_1", .before = 1)

  q1_2 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_1_col_4-6_2025-09-30 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "1_2", .before = 1)

  q1_3 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_1_col_7-9_2025-10-03 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "1_3", .before = 1)

  q1_4 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_1_col_10-12_2025-10-04 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "1_4", .before = 1)

  # plate 2
  q2_1 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_2_col_1-3_2025-10-14 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "2_1", .before = 1)

  q2_2 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_2_col_4-6_2025-10-14 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "2_2", .before = 1)

  q2_3 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_2_col_7-9_2025-10-14 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "2_3", .before = 1)

  q2_4 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_2_col_10-12_2025-10-14 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "2_4", .before = 1)

  # plate 3
  q3_1 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_3_col_1-3_2025-10-15 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "3_1", .before = 1)

  q3_2 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_3_col_4-6_2025-10-15 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "3_2", .before = 1)

  q3_3 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_3_col_7-9_2025-10-15 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "3_3", .before = 1)

  q3_4 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_3_col_10-12_2025-10-16 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "3_4", .before = 1)

  # plate 4
  q4_1 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_4_col_1-3_2025-10-16 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "4_1", .before = 1)

  q4_2 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_4_col_4-6_2025-10-16 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "4_2", .before = 1)

  q4_3 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_4_col_7-9_2025-10-16 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "4_3", .before = 1)

  q4_4 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_4_col_10-12_2025-10-16 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "4_4", .before = 1)

  # plate 5
  q5_1 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_5_col_1-3_2025-10-17 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "5_1", .before = 1)

  q5_2 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_5_col_4-6_2025-10-17 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "5_2", .before = 1)

  q5_3 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_5_col_7-9_2025-10-17 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "5_3", .before = 1)

  q5_4 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_5_col_10-12_2025-10-17 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "5_4", .before = 1)

  # plate 6
  q6_1 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_6_col_1-3_2025-09-18 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "6_1", .before = 1)

  q6_2 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_6_col_4-6_2025-09-22 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "6_2", .before = 1)

  q6_3 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_6_col_7-9_2025-09-22 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "6_3", .before = 1)

  q6_4 <- read.csv(here::here("raw_data", "microbial_abundance", "fungi", "plate_6_col_10-12_2025-09-23 -  Quantification Cq Results.csv"),
                   sep = ";", dec = ",") |>
    select(2, 8, 11) |>
    rename("well_position" = 1,
           "starting_quantity" = 3) |>
    mutate(starting_quantity = sub("NaN", 0, starting_quantity),
           starting_quantity = sub(",", ".", starting_quantity),
           starting_quantity = as.numeric(starting_quantity)) |>
    mutate(run = "6_4", .before = 1)

  # Combine:
  fungi_raw <- rbind(q1_1, q1_2, q1_3, q1_4,
                     q2_1, q2_2, q2_3, q2_4,
                     q3_1, q3_2, q3_3, q3_4,
                     q4_1, q4_2, q4_3, q4_4,
                     q5_1, q5_2, q5_3, q5_4,
                     q6_1, q6_2, q6_3, q6_4) |>
    janitor::clean_names() |>
    mutate(DNA_extraction_batch = substr(run, 1, 1)) |>
    left_join(map) |>
    mutate(comment = "")

  rm(q1_1, q1_2, q1_3, q1_4,
     q2_1, q2_2, q2_3, q2_4,
     q3_1, q3_2, q3_3, q3_4,
     q4_1, q4_2, q4_3, q4_4,
     q5_1, q5_2, q5_3, q5_4,
     q6_1, q6_2, q6_3, q6_4)

  fungi_raw <- fungi_raw |>
    separate(col = content, into = c("plotID", "material", "sub", "replicate")) |>
    # filter(material == "soil",
    #        sub == "sample",
    #        is.na(replicate)) |>
    # select(-replicate) |>
    left_join(dataDocumentation::create_funder_meta_data()) |>
    dataDocumentation::funcabization(convert_to = "FunCaB") |>
    rename(starting_quantity_sq = starting_quantity) |>
    select(siteID, blockID, plotID, treatment, material, sub, replicate,
           DNA_extraction_batch, run, well_position, starting_quantity_sq#, cq
    ) |>
    rename(fungal_abundance = starting_quantity_sq)

}

### correct DNA extrcation batch effect for fungi
correct_fungi_batch_effect <- function(data) {

  ##### soil samples #####
  soil <- data |>
    filter(!is.na(siteID),
           material == "soil")

  # one-row matrix to correct values
  correct_me <- soil |>
    # log-transform abundance, +1 to each sample to avoid -Inf on 0-abundance samples
    mutate(log_ab = log(fungal_abundance + 1),
           # get unique identifier per sample replicate
           unq = paste0(plotID, "_", sub, "_", well_position)) |>
    # format to one-row matrix
    column_to_rownames("unq") |>
    select(log_ab) |>
    t()

  correct_me <- as.matrix(correct_me)

  # get metadata file containing variables to retain variation in the data from
  metadata <- soil |>
    # get climate variables into the data
    left_join(data.frame(
      siteID = c("Alrust", "Arhelleren", "Fauske",
                 "Gudmedalen", "Hogsete", "Lavisdalen",
                 "Ovstedalen", "Rambera", "Skjelingahaugen",
                 "Ulvehaugen", "Veskre", "Vikesland"),
      mean_temp = factor(c(8.5, 10.5, 10.5,
                           6.5, 8.5, 6.5,
                           10.5, 8.5, 6.5,
                           6.5, 8.5, 10.5)),
      mean_precip = factor(c(700, 2100, 700,
                             2100, 1400, 1400,
                             2800, 2100, 2800,
                             700, 2800, 1400))
    ), by = "siteID") |>
    # get unique identifier per sample replicates
    mutate(unq = paste0(plotID, "_", sub, "_", well_position)) |>
    column_to_rownames("unq") |>
    # select columns for model matrix
    select(siteID, treatment, mean_temp, mean_precip, sub, DNA_extraction_batch)

  # get model matrix for correction
  mod_mat <- model.matrix(~ treatment + mean_temp + mean_precip + siteID + sub, metadata)

  # remove batch effect with limma
  corrected <- limma::removeBatchEffect(
    x = correct_me,
    batch = metadata$DNA_extraction_batch,
    design = mod_mat
  )

  # get the corrected data into a data frame
  corrected_soil_abundance <- corrected |>
    t() |>
    data.frame() |>
    rownames_to_column("unq") |>
    separate(unq, into = c("plotID", "sub", "well_position"), sep = "_") |>
    left_join(data) |>
    # back-transform abundance
    mutate(corrected_fungal_abundance = exp(log_ab) - 1,
           corrected_fungal_abundance = pmax(corrected_fungal_abundance, 0))






  ##### litter samples #####
  litter <- data |>
    filter(!is.na(siteID),
           material == "litter")

  # one-row matrix to correct values
  correct_me <- litter |>
    # log-transform abundance, +1 to each sample to avoid -Inf on 0-abundance samples
    mutate(log_ab = log(fungal_abundance + 1),
           # get unique identifier per sample replicate
           unq = paste0(plotID, "_", sub, "_", well_position)) |>
    # format to one-row matrix
    column_to_rownames("unq") |>
    select(log_ab) |>
    t()

  correct_me <- as.matrix(correct_me)

  # get metadata file containing variables to retain variation in the data from
  metadata <- litter |>
    # get climate variables into the data
    left_join(data.frame(
      siteID = c("Alrust", "Arhelleren", "Fauske",
                 "Gudmedalen", "Hogsete", "Lavisdalen",
                 "Ovstedalen", "Rambera", "Skjelingahaugen",
                 "Ulvehaugen", "Veskre", "Vikesland"),
      mean_temp = factor(c(8.5, 10.5, 10.5,
                           6.5, 8.5, 6.5,
                           10.5, 8.5, 6.5,
                           6.5, 8.5, 10.5)),
      mean_precip = factor(c(700, 2100, 700,
                             2100, 1400, 1400,
                             2800, 2100, 2800,
                             700, 2800, 1400))
    ), by = "siteID") |>
    # get unique identifier per sample replicates
    mutate(unq = paste0(plotID, "_", sub, "_", well_position)) |>
    column_to_rownames("unq") |>
    # select columns for model matrix
    select(siteID, treatment, mean_temp, mean_precip, sub, DNA_extraction_batch)

  # get model matrix for correction
  mod_mat <- model.matrix(~ treatment + mean_temp + mean_precip + siteID + sub, metadata)

  # remove batch effect with limma
  corrected <- limma::removeBatchEffect(
    x = correct_me,
    batch = metadata$DNA_extraction_batch,
    design = mod_mat
  )

  # get the corrected data into a data frame
  corrected_litter_abundance <- corrected |>
    t() |>
    data.frame() |>
    rownames_to_column("unq") |>
    separate(unq, into = c("plotID", "sub", "well_position"), sep = "_") |>
    left_join(data) |>
    # back-transform abundance
    mutate(corrected_fungal_abundance = exp(log_ab) - 1,
           corrected_fungal_abundance = pmax(corrected_fungal_abundance, 0))

  corrected_abundance <- rbind(
    corrected_soil_abundance,
    corrected_litter_abundance
  ) |>
    select(-log_ab)

  return(corrected_abundance)
}

### combine bacteria and fungi into microbial abundance
combine_bacteria_and_fungi <- function(bac, fun) {

  # join the two dataframes
  full_join(bac, fun) |>

    # pivot to long format
    rename(uncorrected_bacteria = bacterial_abundance,
           uncorrected_fungi = fungal_abundance,
           corrected_bacteria = corrected_bacterial_abundance,
           corrected_fungi = corrected_fungal_abundance) |>
    pivot_longer(cols = c(uncorrected_bacteria,
                          uncorrected_fungi,
                          corrected_bacteria,
                          corrected_fungi),
                 names_to = c(".value", "group"),
                 names_pattern = "(uncorrected|corrected)_(bacteria|fungi)") |>
    mutate(
      # estimate abundance (copy numbers) per gram soil:
      # starting_quantity = per uL.
      # Multiply by 10 because DNA extracts were diluted 1:10 before amplification,
      # multiply by 100 because 100 uL was eluted from each sample,
      # and multiply by 4 because DNA was isolated from 0.25 grams sample material:
      uncorrected_abundance_per_g = round(uncorrected * 10 * 100 * 4, digits = 2),
      corrected_abundance_per_g = round(corrected * 10 * 100 * 4, digits = 2)) |>
    mutate(year = 2022, .before = 1) |>
    arrange(blockID)

}

### subset based on sample material
subset_qpcr_samples <- function(data, sample_material) {

  data |>
    filter(material == sample_material)

}
