#################################################
################ root functions #################
#################################################

# claen and combine ric lab data
clean_root_lab_data <- function(c_fgb, rest) {

  c <- read.csv(c_fgb, sep = ";", dec = ",") |>

    # dplyr::rename columns to match the next data sheet
    dplyr::rename(wet_root_biomass_g = mass_wet,
                  #retrieval_date = date,
                  tube_mass_g = tube_mass,
                  tube_and_root_mass_g = mass_dry_and_tube) |>

    # do calculations to match the next data sheet
    dplyr::mutate(ric_length_m = length / 100,
                  ric_volume_m3 = ric_length_m * pi * (0.03^2),
                  dry_root_biomass_g = tube_and_root_mass_g - tube_mass_g,
                  date_in_freeze_dryer = "not_recorded",
                  date_out_freeze_dryer = "not_recorded",
                  comments = "",
                  operator = "Lucas") |>

    # keep useful columns
    dplyr::select(operator, plotID, treatment, ric_length_m,
                  ric_volume_m3, wet_root_biomass_g,
                  tube_mass_g, tube_and_root_mass_g, dry_root_biomass_g,
                  date_in_freeze_dryer, date_out_freeze_dryer, comments)

  # get results from the rest of the treatments
  rest <- read.csv(rest, sep = ",") |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(ric_length_m)) |>
    dplyr::rename(plotID = plot_id) |>

    # many samples have plotID in all capital letters, we need to fix that:
    dplyr::mutate(# fix a typo in treatment for plot Gud2GB:
      treatment = ifelse(test = plotID == "Gud2GB",  yes = "GB", no = treatment),
      # many samples have plotID in all capital letters, we need to fix that:
      site = stringr::str_to_title(substr(plotID, 1, 3)),
      block_id = substr(plotID, 4, 4),
      plotID = stringr::str_to_title(site),
      plotID = paste0(site, block_id, treatment),

      # add operator column:
      operator = NA,
      operator = ifelse(test = treatment %in% c("F", "G", "B"),
                        yes = "Leo",
                        no = "Lou"),
      operator = ifelse(test = block_id == 4,
                        yes ="Michaela", no = operator),
      # fix special cases:
      operator = ifelse(plotID == "Arh2G",
                        yes = "Lou",
                        no = operator),
      operator = ifelse(plotID == "Lav4GF",
                        yes = "Lou",
                        no = operator),
      operator = ifelse(plotID == "Skj1GB",
                        yes = "Leo",
                        no = operator)) |>
    # remove sample without data:
    dplyr::select(operator, plotID, treatment, ric_length_m,
                  ric_volume_m3, wet_root_biomass_g,
                  tube_mass_g, tube_and_root_mass_g, dry_root_biomass_g,
                  date_in_freeze_dryer, date_out_freeze_dryer, comments)

  root_lab_data <- rbind(c, rest) |>
    dplyr::mutate(treatment = factor(treatment, levels = c("C", "F", "G", "B",
                                                           "FB", "GB", "GF", "FGB")))

  return(root_lab_data)

}

# get and clean root scan data
clean_root_scan_data <- function(c_fgb, fb_gb_gf, f_g_b_block4) {

  # this contains samples from C and FGB in blocks 1-3
  c_fgb_data <- read.csv(c_fgb, sep = ";") |>
    clean_names() |>
    slice(-1:-4) |> # removing useless lines
    mutate(length_cm = as.numeric(length_cm),
           avg_diam_mm = as.numeric(avg_diam_mm)) |>
    rename(plotID = rhizo_2022a) |>
    mutate(root_length_m = length_cm / 100,
           avg_root_diameter_m = avg_diam_mm / 1000) |>
    select(plotID, operator, root_length_m, avg_root_diameter_m)

  # this contains samples from FB, GF and GB treatments in block 1-3
  fb_gf_gb_data <- read_delim(fb_gb_gf,
                              col_types = cols(
                                `Length(cm)` = col_double(),
                                `AvgDiam(mm)` = col_double())
  ) |> # changing useful column type to double
    clean_names() |>
    slice(-1:-4) |> # removing useless lines
    mutate(operator = "Lou") |>
    rename(plotID = rhizo_2022b) |>
    mutate(plotID = sub("Ves3BG", "Ves3GB", plotID),
           plotID = sub("Ves1FG", "Ves1GF", plotID)) |>
    mutate(root_length_m = length_cm / 100,
           avg_root_diameter_m = avg_diam_mm / 1000) |>
    select(plotID, operator, root_length_m, avg_root_diameter_m)

  # this contains samples from F, G, and B treatment in blocks 1-3, as well as
  # all treatemnts from block 4
  f_g_b_block4_data <- read_delim(f_g_b_block4,
                                  col_types = cols(
                                    `Length(cm)` = col_double(),
                                    `AvgDiam(mm)` = col_double())
  ) |> # changing useful column type to double
    clean_names() |>
    rename(plotID = rhizo_2022a) |>
    slice(-1:-4) |> # removing useless lines
    mutate(operator = ifelse(test = substr(plotID, 4, 4) == 4,
                             yes = "Michaela", no = "Leo"),
           plotID = sub("LAU4F", "FAU4F", plotID),
           treatment = substr(plotID, 5, 7),
           plotID = paste0(substr(stringr::str_to_title((tolower(plotID))), 1, 4), treatment),
           root_length_m = length_cm / 100,
           avg_root_diameter_m = avg_diam_mm / 1000) |>
    select(plotID, operator, root_length_m, avg_root_diameter_m)

  # join datasets into one
  traits <- rbind(c_fgb_data, fb_gf_gb_data, f_g_b_block4_data) |>
    mutate(treatment = substr(plotID, 5, 7))

  return(traits)

}

# combine root_lab_data and root_scan_data and calculate root traits
clean_root_traits <- function(lab, scan) {

  root_traits <- dplyr::left_join(lab, scan) |>
    dplyr::mutate(

      # specific root length
      specific_root_length_m_per_g = root_length_m / dry_root_biomass_g,

      # root_tissue_density_g_per_m3
      root_tissue_density_g_per_m3 = dry_root_biomass_g/((avg_root_diameter_m/2)^2*pi*root_length_m),

      # root_dry_matter_content
      root_dry_matter_content = dry_root_biomass_g / wet_root_biomass_g,

      # root_productivity_g_per_m3_per_year
      root_productivity_g_per_m3_per_year = dry_root_biomass_g / ric_volume_m3) |>

    dplyr::select(
      plotID,
      treatment,
      operator,
      # keep these traits:
      avg_root_diameter_m,
      dry_root_biomass_g,
      root_productivity_g_per_m3_per_year,
      specific_root_length_m_per_g,
      root_length_m) |>

    # rename 'biomass' traits to 'turnover'

    rename(
      dry_root_turnover_g = dry_root_biomass_g
    )

}

# clean biomass from 2021, combine with root traits and return cleaned
finish_roots <- function(biomass, traits) {

  biomass <- readxl::read_xlsx(biomass) |>

    # fix typos in treatment and plotID:
    mutate(treatment = replace_values(
      treatment,
      "FG" ~ "GF",
      "BG" ~ "GB"
    ),
    plotID = paste0(blockID, treatment)) |>
    select(plotID, root_biomass)

  roots <- right_join(biomass, traits) |>
    dplyr::mutate(
      year = 2022,
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
    relocate(year, siteID, blockID, plotID, treatment, operator,
             root_biomass, avg_root_diameter_m, dry_root_turnover_g,
             root_productivity_g_per_m3_per_year, root_length_m,
             specific_root_length_m_per_g)

  return(roots)

}
