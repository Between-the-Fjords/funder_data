# Script to clean LOI data

# Soils were dried at 55C for 48 h before being sub-sampled and dried at 105C for 24h.
# Samples were burned at 550C for 5h and weighed (mass loss is organic matter)
# Samples were burned again at 900C for 5h and weighed (mass loss is inorganic C)

clean_loi <- function(loi_raw){

  # Fix column names and variables in accordance with FUNDER naming convention
  loi_raw |>
    rename(siteID = site) |>
    mutate(siteID = recode(siteID,
                           # old name (replace) = valid name (do not change)
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

  # Calculate soil weights without crucible included
    mutate(dw_g = dry_weight_g - crucible_weight_g) |>
    mutate(LOI_550_g = after_550_g - crucible_weight_g) |>
    mutate(LOI_950_g = after_950_g - crucible_weight_g) |>

  # Calculate loss of organic matter and inorganic C and express it as a proportion of the dry weight of soil.
    mutate(OM_g = dw_g - LOI_550_g) |>
    mutate(inorgC_g = LOI_550_g - LOI_950_g) |>
    mutate(OM_proportion = OM_g/dw_g) |>
    mutate(inorgC_proportion = inorgC_g/dw_g) |>
  # Pick out relevant columns and remove rows with missing values (plots that don't exist)
    select(siteID, blockID, plotID, treatment, organic_matter = OM_proportion, incorganic_carbon = inorgC_proportion) |>
    pivot_longer(cols = c(organic_matter, incorganic_carbon), names_to = "variable", values_to = "value") |>
    mutate(unit = "proportion") |>
    # remove 4 rows that are NA for organic and inorganic C
    drop_na(value)

}

