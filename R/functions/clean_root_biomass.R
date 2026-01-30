# clean root biomass data

clean_root_biomass <- function(root_biomass_raw, funder_meta) {
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
  # Assuming root biomass column is named something like "root_biomass", "biomass", "root_mass", etc.
  # Adjust column name as needed
  root_biomass_clean <- root_biomass_clean |>
    mutate(
      # Identify potential outliers (values > 3 SD from mean, or negative values)
      # This is a placeholder - adjust based on actual column names and expected ranges
      root_biomass_outlier = case_when(
        # Add specific outlier checks here once we know the column name
        # For example: root_biomass > 1000 ~ TRUE,  # if values should be < 1000
        # root_biomass < 0 ~ TRUE,  # negative values are errors
        TRUE ~ FALSE
      )
    )
  
  return(root_biomass_clean)
}
