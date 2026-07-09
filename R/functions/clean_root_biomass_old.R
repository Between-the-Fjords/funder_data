# clean root biomass data (legacy workflow)

clean_root_biomass_old <- function(root_biomass_raw, funder_meta) {
  root_biomass_clean <- root_biomass_raw |>
    # Fix siteID to full names
    mutate(
      siteID = str_to_lower(siteID),
      siteID = recode(
        siteID,
        "gud" = "Gudmedalen",
        "lav" = "Lavisdalen",
        "ram" = "Rambera",
        "ulv" = "Ulvehaugen",
        "skj" = "Skjelingahaugen",
        "alr" = "Alrust",
        "arh" = "Arhelleren",
        "fau" = "Fauske",
        "hog" = "Hogsete",
        "ovs" = "Ovstedalen",
        "vik" = "Vikesland",
        "ves" = "Veskre"
      )
    ) |>
    # Fix treatment typos: BG -> GB, FG -> GF
    mutate(
      treatment = recode(
        treatment,
        "BG" = "GB",
        "FG" = "GF"
      )
    ) |>
    # Create plotID from blockID + treatment
    # blockID already exists and will be validated against funder_meta
    mutate(
      plotID = paste0(blockID, treatment)
    ) |>
    # Lowercase comments
    rename(comments = Comments) %>%
    # convert to FunCaB format
    dataDocumentation::funcabization(dat = ., convert_to = "FunCaB")

  ### SKJ4C and Ulv4FGB  are missing in the data
  # Validate against funder_meta
  root_biomass_clean <- root_biomass_clean |>
    tidylog::left_join(funder_meta, by = c("siteID", "blockID", "plotID", "treatment"))

  # Check for typos and outliers in root biomass
  root_biomass_clean <- root_biomass_clean |>
    mutate(
      root_biomass_outlier = case_when(
        TRUE ~ FALSE
      )
    )

  return(root_biomass_clean)
}
