# Script to clean LOI data

# Soils were dried at 55C for 48 h before being sub-sampled and dried at 105C for 24h.
# Samples were burned at 550C for 5h and weighed (mass loss is organic matter)
# Samples were burned again at 900C for 5h and weighed (mass loss is inorganic C)

library(tidyverse)

# Load data
LOI <- read_csv2("Data/FUNDER_raw_LOI_2022.csv")

# Fix column names and variables in accordance with FUNDER naming convention
LOI <- LOI |>
  rename(siteID = site) |>
  relocate(treatment, .after = plotID) |>
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
                         'Ves' = "Veskre"))

# Calculate soil weights without crucible included

LOI <- LOI |>
  mutate(dw_g = dry_weight_g - crucible_weight_g) |>
  mutate(LOI_550_g = after_550_g - crucible_weight_g) |>
  mutate(LOI_950_g = after_950_g - crucible_weight_g)

# Calculate loss of organic matter and inorganic C and express it as a proportion of the dry weight of soil.

LOI <- LOI |>
  mutate(OM_g = dw_g - LOI_550_g) |>
  mutate(inorgC_g = LOI_550_g - LOI_950_g) |>
  mutate(OM_proportion = OM_g/dw_g) |>
  mutate(inorgC_proportion = inorgC_g/dw_g)

# Pick out relevant columns and remove rows with missing values (plots that don't exist)
LOI_clean <- LOI |>
  select(siteID, blockID, plotID, treatment, OM_proportion, inorgC_proportion) |>
  drop_na(OM_proportion)

# Export cleaned dataset
write_csv2(LOI_clean, file = "Clean_data/FUNDER_clean_LOI_2022.csv")

