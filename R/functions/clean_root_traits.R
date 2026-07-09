# clean root traits and biomass

# clean and combine ric lab data
clean_root_lab_data <- function(c_fgb, rest) {

  c <- read.csv(c_fgb, sep = ";", dec = ",") |>
    rename(
      wet_root_biomass_g = mass_wet,
      tube_mass_g = tube_mass,
      tube_and_root_mass_g = mass_dry_and_tube
    ) |>
    mutate(
      ric_length_m = length / 100,
      ric_volume_m3 = ric_length_m * pi * (0.03^2),
      dry_root_biomass_g = tube_and_root_mass_g - tube_mass_g,
      date_in_freeze_dryer = "not_recorded",
      date_out_freeze_dryer = "not_recorded",
      comments = "",
      operator = "Lucas"
    ) |>
    select(
      operator, plotID, treatment, ric_length_m, ric_volume_m3, wet_root_biomass_g,
      tube_mass_g, tube_and_root_mass_g, dry_root_biomass_g,
      date_in_freeze_dryer, date_out_freeze_dryer, comments
    )

  rest <- read.csv(rest, sep = ",") |>
    clean_names() |>
    filter(!is.na(ric_length_m)) |>
    rename(plotID = plot_id) |>
    mutate(
      treatment = if_else(plotID == "Gud2GB", "GB", treatment),
      site = str_to_title(substr(plotID, 1, 3)),
      block_id = substr(plotID, 4, 4),
      plotID = paste0(site, block_id, treatment),
      operator = if_else(treatment %in% c("F", "G", "B"), "Leo", "Lou"),
      operator = if_else(block_id == 4, "Michaela", operator),
      operator = if_else(plotID == "Arh2G", "Lou", operator),
      operator = if_else(plotID == "Lav4GF", "Lou", operator),
      operator = if_else(plotID == "Skj1GB", "Leo", operator)
    ) |>
    select(
      operator, plotID, treatment, ric_length_m, ric_volume_m3, wet_root_biomass_g,
      tube_mass_g, tube_and_root_mass_g, dry_root_biomass_g,
      date_in_freeze_dryer, date_out_freeze_dryer, comments
    )

  bind_rows(c, rest) |>
    mutate(treatment = factor(treatment, levels = c("C", "F", "G", "B", "FB", "GB", "GF", "FGB")))
}

# get and clean root scan data
clean_root_scan_data <- function(c_fgb, fb_gb_gf, f_g_b_block4) {

  c_fgb_data <- read.csv(c_fgb, sep = ";") |>
    clean_names() |>
    slice(-1:-4) |>
    mutate(
      length_cm = as.numeric(length_cm),
      avg_diam_mm = as.numeric(avg_diam_mm)
    ) |>
    rename(plotID = rhizo_2022a) |>
    mutate(
      root_length_m = length_cm / 100,
      avg_root_diameter_m = avg_diam_mm / 1000
    ) |>
    select(plotID, root_length_m, avg_root_diameter_m)

  fb_gf_gb_data <- read_delim(
    fb_gb_gf,
    col_types = cols(
      `Length(cm)` = col_double(),
      `AvgDiam(mm)` = col_double()
    )
  ) |>
    clean_names() |>
    slice(-1:-4) |>
    rename(plotID = rhizo_2022b) |>
    mutate(
      plotID = sub("Ves3BG", "Ves3GB", plotID),
      plotID = sub("Ves1FG", "Ves1GF", plotID),
      root_length_m = length_cm / 100,
      avg_root_diameter_m = avg_diam_mm / 1000
    ) |>
    select(plotID, root_length_m, avg_root_diameter_m)

  f_g_b_block4_data <- read_delim(
    f_g_b_block4,
    col_types = cols(
      `Length(cm)` = col_double(),
      `AvgDiam(mm)` = col_double()
    )
  ) |>
    clean_names() |>
    rename(plotID = rhizo_2022a) |>
    slice(-1:-4) |>
    mutate(
      plotID = sub("LAU4F", "FAU4F", plotID),
      treatment = substr(plotID, 5, 7),
      plotID = paste0(substr(str_to_title(tolower(plotID)), 1, 4), treatment),
      root_length_m = length_cm / 100,
      avg_root_diameter_m = avg_diam_mm / 1000
    ) |>
    select(plotID, root_length_m, avg_root_diameter_m)

  bind_rows(c_fgb_data, fb_gf_gb_data, f_g_b_block4_data) |>
    distinct(plotID, .keep_all = TRUE) |>
    mutate(
      # Diameter 0 is not biologically possible; treat as failed measurement
      avg_root_diameter_m = na_if(avg_root_diameter_m, 0)
    )
}

# combine root_lab_data and root_scan_data and calculate root traits
clean_root_traits <- function(lab, scan) {

  lab |>
    left_join(scan, by = "plotID") |>
    mutate(
      specific_root_length_m_per_g = root_length_m / dry_root_biomass_g,
      root_tissue_density_g_per_m3 = if_else(
        is.na(avg_root_diameter_m) | is.na(root_length_m) | root_length_m == 0,
        NA_real_,
        dry_root_biomass_g / ((avg_root_diameter_m / 2)^2 * pi * root_length_m)
      ),
      root_dry_matter_content = dry_root_biomass_g / wet_root_biomass_g,
      dry_root_turnover_g = dry_root_biomass_g
    ) |>
    select(
      plotID,
      treatment,
      operator,
      ric_length_m,
      ric_volume_m3,
      wet_root_biomass_g,
      avg_root_diameter_m,
      dry_root_turnover_g,
      root_tissue_density_g_per_m3,
      root_dry_matter_content,
      specific_root_length_m_per_g,
      root_length_m
    )
}

add_ric_burial_dates <- function(data) {
  data |>
    mutate(
      retrieval_date = lubridate::dmy(case_when(
        siteID %in% c("Alrust") ~ "24/08/2022",
        siteID %in% c("Arhelleren") ~ "31/08/2022",
        siteID %in% c("Fauske") ~ "25/08/2022",
        siteID %in% c("Gudmedalen") ~ "06/09/2022",
        siteID %in% c("Hogsete") ~ "23/08/2022",
        siteID %in% c("Lavisdalen") ~ "05/09/2022",
        siteID %in% c("Ovstedalen") ~ "29/08/2022",
        siteID %in% c("Rambera") ~ "01/09/2022",
        siteID %in% c("Skjelingahaugen") ~ "08/09/2022",
        siteID %in% c("Ulvehaugen") ~ "07/09/2022",
        siteID %in% c("Veskre") ~ "30/08/2022",
        siteID %in% c("Vikesland") ~ "22/08/2022",
        TRUE ~ NA_character_
      )),
      burial_date = lubridate::dmy(case_when(
        siteID %in% c("Alrust") ~ "02/08/2021",
        siteID %in% c("Arhelleren") ~ "16/08/2021",
        siteID %in% c("Fauske") ~ "02/08/2021",
        siteID %in% c("Gudmedalen") ~ "09/08/2021",
        siteID %in% c("Hogsete") ~ "09/08/2021",
        siteID %in% c("Lavisdalen") ~ "09/08/2021",
        siteID %in% c("Ovstedalen") ~ "16/08/2021",
        siteID %in% c("Rambera") ~ "16/08/2021",
        siteID %in% c("Skjelingahaugen") ~ "16/08/2021",
        siteID %in% c("Ulvehaugen") ~ "02/08/2021",
        siteID %in% c("Veskre") ~ "16/08/2021",
        siteID %in% c("Vikesland") ~ "09/08/2021",
        TRUE ~ NA_character_
      )),
      duration = as.numeric(retrieval_date - burial_date)
    )
}

# clean biomass from 2021, combine with root traits and return cleaned
finish_roots <- function(biomass, traits) {

  biomass_clean <- readxl::read_xlsx(biomass) |>
    mutate(
      treatment = replace_values(
        treatment,
        "FG" ~ "GF",
        "BG" ~ "GB"
      ),
      plotID = paste0(blockID, treatment)
    ) |>
    select(plotID, root_biomass)

  # Keep all plots with RIC traits, 2021 biomass, or both
  traits |>
    full_join(biomass_clean, by = "plotID") |>
    mutate(
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
      treatment = substr(plotID, 5, 7)
    ) |>
    add_ric_burial_dates() |>
    mutate(
      duration_years = duration / 365.25,
      root_productivity_g_per_m3_per_year = if_else(
        !is.na(dry_root_turnover_g) & !is.na(ric_volume_m3) & ric_volume_m3 > 0 &
          !is.na(duration_years) & duration_years > 0,
        dry_root_turnover_g / ric_volume_m3 / duration_years,
        NA_real_
      )
    ) |>
    select(-duration_years) |>
    dataDocumentation::funcabization(convert_to = "FunCaB") |>
    relocate(
      siteID, blockID, plotID, treatment, operator,
      burial_date, retrieval_date, duration,
      root_biomass, ric_length_m, ric_volume_m3, wet_root_biomass_g,
      avg_root_diameter_m, dry_root_turnover_g, root_tissue_density_g_per_m3,
      root_dry_matter_content, root_productivity_g_per_m3_per_year,
      root_length_m, specific_root_length_m_per_g
    )
}
