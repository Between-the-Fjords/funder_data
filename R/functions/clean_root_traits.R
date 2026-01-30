# clean root trait data

# clean ric root biomass
clean_ric_root_biomass <- function(root_ric_biomass_raw) {

  root_ric_biomass_raw |>
    select(site, blockID, plotID, treatment, `ric_length (m)`, `ric_volume (m3)`, `wet_root_biomass (g)`, `dry_root_biomass (g)`) |>
    janitor::clean_names() |>
    rename(plotID = plot_id, blockID = block_id, siteID = site) |>
    # Standardize plotID: first 3 letters as Site (e.g. Gud), then block number, then uppercase treatment
    mutate(
      plotID_raw = plotID,
      plot_lower = str_to_lower(plotID_raw),
      siteID = str_to_title(str_sub(plot_lower, 1, 3)),
      block_num = str_extract(plot_lower, "(?<=^[a-z]{3})[0-9]+"),
      treatment = str_extract(plot_lower, "[a-zA-Z]+$"),
      treatment = str_to_upper(treatment),
      treatment = case_when(
        treatment == "FG" ~ "GF",
        treatment == "BG" ~ "GB",
        TRUE ~ treatment
      ),
      plotID = if_else(
        !is.na(block_num) & !is.na(treatment),
        paste0(siteID, block_num, treatment),
        plotID_raw
      ),
      blockID = if_else(!is.na(block_num), paste0(siteID, block_num), NA_character_)
    ) |>
    select(siteID, blockID, plotID, treatment, ric_length_m, ric_volume_m3, wet_root_biomass_g, dry_root_biomass_g) |>
    tidylog::filter(!is.na(ric_length_m))
}

# join and clean all winRhIZO files
clean_ric_root_traits <- function(root_traits_F_G_B_raw, root_traits_ric_FAU4FB_raw, root_traits_ric_GF_GB_FB_raw, root_traits_block_4_raw) {

joined_root_traits_raw <- bind_rows(
  root_traits_F_G_B_raw,
  root_traits_ric_FAU4FB_raw,
  root_traits_ric_GF_GB_FB_raw |> 
  mutate(Operator = "Lou"),
  # block 4
  root_traits_block_4_raw |> 
   tidylog:: anti_join(root_traits_F_G_B_raw, by = "RHIZO 2022a")
)

joined_root_traits_raw |>
  rename(
    plotID = `RHIZO 2022a`,
    root_length_cm = `Length(cm)`,
    average_root_diameter_mm = `AvgDiam(mm)`
  ) |> # renaming relevant columns
  # Standardize plotID: first 3 letters as Site (e.g. Gud), then block number, then uppercase treatment
  mutate(
    plotID_raw = plotID,
    plot_lower = str_to_lower(plotID_raw),
    siteID = str_to_title(str_sub(plot_lower, 1, 3)),
    block_num = str_extract(plot_lower, "(?<=^[a-z]{3})[0-9]+"),
    treatment = str_extract(plot_lower, "[a-zA-Z]+$"),
    treatment = str_to_upper(treatment),
    # Fix treatment typos (does not affect siteID or blockID)
    treatment = case_when(
      treatment == "FG" ~ "GF",
      treatment == "BG" ~ "GB",
      TRUE ~ treatment
    ),
    plotID = if_else(
      !is.na(block_num) & !is.na(treatment),
      paste0(siteID, block_num, treatment),
      plotID_raw
    ),
    blockID = if_else(!is.na(block_num), paste0(siteID, block_num), NA_character_)
  ) |>
  mutate(
    root_length_m = root_length_cm / 100,
    average_root_diameter_m = average_root_diameter_mm / 1000
  ) |> # Changing units of columns
  select(
    siteID,
    blockID,
    plotID,
    treatment,
    root_length_m,
    average_root_diameter_m,
    Operator
  ) # keep ID columns and key traits

}

# clean C and FGB traits
clean_C_FGB_traits <- function(root_traits_C_FGB_2023_raw) {

root_traits_C_FGB_2023_raw |>
  rename('retrieval_date' = 'Retrieval_date',
        'ric_length_m' = 'length',
        'wet_root_biomass_g' = 'mass_wet',
        'dry_root_biomass_g' = 'mass_dry',
        'average_root_diameter_m' = 'AvgDiam',
        'root_length_m' = 'Length') |> # Harmonizing the names of the relevant columns
    #keeping only the relevants column
    select(siteID, plotID,treatment,ric_length_m,wet_root_biomass_g,dry_root_biomass_g,average_root_diameter_m,root_length_m) |>
   # fix units
   mutate(ric_length_m=ric_length_m/100,root_length_m=root_length_m/100,average_root_diameter_m=average_root_diameter_m/1000,
   #calculating ric_volume in m^3
   ric_volume_m3=ric_length_m*pi*(0.03^2),
   blockID = str_sub(plotID,1,4),
   Operator = "Lucas") |>
   select(siteID, blockID, plotID, treatment, ric_length_m, ric_volume_m3, wet_root_biomass_g, dry_root_biomass_g, average_root_diameter_m, root_length_m, Operator)
    
}


# join all root traits data
#
# Overview of the three input datasets (before joining).
#
# 1. root_ric_biomass_clean (from BIOMASS_DATASHEET_ALL.csv)
#    Biomass
#    all sites, block 1-4 for single and double treatments, block 4 for C and FGB
#
# 2. root_traits_ric_joined (from 3 winRhIZO scan files, cleaned by clean_ric_root_traits)
#    only traits
#    Root traits from block 1-3 for single and double treatments
#
# 3. root_traits_C_FGB_clean (from FUNDER_clean_Roots_C_FGB_2023.csv)
#    Biomass and traits
#    72 observations: 2 sites, block 1-3, treatment C and FGB
#
join_root_traits <- function(root_ric_biomass_clean, root_traits_ric_joined, root_traits_C_FGB_clean, ric_depth_clean) {

  # Merging of the roots biomass and the roots traits in one table;
  # use a left join (via tidylog) to keep all biomass plots and see which
  # ones do not have matching traits in the join message,
  # then add the C and FGB rows at the end of the table
  root_ric_biomass_clean |> 
    # biomass has block 4 as well, that is why some are only in biomass data
    tidylog::left_join(root_traits_ric_joined, by = c("plotID", "siteID", "blockID", "treatment")) |> 
    bind_rows(root_traits_C_FGB_clean) |>

  # Changing siteID from 3-letter type (ex:"Alr") to spelt out type (ex:"Alrust") for all site names
  mutate( siteID = recode(siteID, 
                       'Gud' = "Gudmedalen",
                       'Lav' = "Lavisdalen",
                       'Ram' = "Rambera",
                       'Ulv' = "Ulvehaugen",
                       'Skj' = "Skjelingahaugen",
                       'Alr' = "Alrust",
                       'Arh' = "Arhelleren",
                       'Fau' = "Fauske",
                       'Hog' = "Hogsete",
                       'Ovs' = "Ovstedalen",
                       'Vik' = "Vikesland",
                       'Ves' = "Veskre")) |> 
  # Adding climatic conditions : precipitation_level represents the annual average precipitation in the site temperature_level represents the average temperature during the 4 warmest months in the site
  mutate(temperature_level=case_when(
  siteID%in%c("Fauske","Vikesland","Arhelleren","Ovstedalen")~10.5,
  siteID%in%c("Alrust","Hogsete","Rambera","Veskre")~8.5,
  TRUE~6.5),
  precipitation_level=case_when(
  siteID%in%c("Fauske","Alrust","Ulvehaugen")~700,
  siteID%in%c("Vikesland","Hogsete","Lavisdalen")~1400,
  siteID%in%c("Gudmedalen","Rambera","Arhelleren")~2100,
  TRUE~2800)) |>  
  # Adding the RIC burial and retrieval dates (as Date); format day/month/year
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
    ))
  ) |>
  # 2 missing plots in ric_depth_clean: Ram2GB and Ovs3F
  tidylog::left_join(ric_depth_clean, by = c("plotID", "siteID", "blockID", "treatment")) |>

  # calculate root traits
  mutate(specific_root_length_m_per_g = root_length_m/dry_root_biomass_g,root_tissue_density_g_per_m3 = dry_root_biomass_g/((average_root_diameter_m/2)^2*pi*root_length_m),
  root_dry_matter_content = dry_root_biomass_g/wet_root_biomass_g,root_productivity_g_per_m3_per_year = dry_root_biomass_g/ric_volume_m3,
  duration = as.numeric(retrieval_date - burial_date)) |>

    tidylog::select(siteID:treatment, 
    burial_date, retrieval_date, duration,
    dry_root_biomass_g, specific_root_length_m_per_g, root_tissue_density_g_per_m3, 
    root_dry_matter_content, root_productivity_g_per_m3_per_year, 
    temperature_level, precipitation_level,
    wet_root_biomass_g, average_root_diameter_m, root_length_m, ric_depth_m,
    ric_length_m, ric_volume_m3, operator = Operator)

}
