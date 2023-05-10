# Clean point measurements of soil moisture

library(tidyverse)

# Read raw data ####
moisture_raw <- read_csv2("Data/FUNDER_raw_soil_moisture_point_measurements_2022.csv")

### Restructure data ####
# Rename columns
# Select and change order of columns
# Recode siteID:s

moisture <- moisture_raw |>
  rename(date = Date, siteID = site, "1" = moisture1, "2" = moisture2, "3" = moisture3, "4" = moisture4) |>
  select(date, time, siteID, blockID, plotID, treatment, "1", "2", "3", "4", weather, comment) |>
  pivot_longer(!c(date, time, siteID, blockID, plotID, treatment, weather, comment), names_to = "measurement", values_to = "moisture") |>
  relocate(c(weather, comment), .after = moisture) |>
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

### Remove missing data ####
# Non-existing plots: Fau2GB, Ulv1B
# No measurements made in site Skjelingahaugen due to bad weather conditions

moisture <- moisture |>
  filter(siteID != "Skjelingahaugen") |>
  filter(plotID != "Fau2GB") |>
  filter(plotID != "Ulv1B")

### Fix datatypes ####
# Force moisture to be read as numeric variable
moisture$moisture <- as.numeric(moisture$moisture)

### Check for duplicate rows
# moisture |>
#   distinct(plotID, measurement, .keep_all = TRUE) # no duplicates

# Export cleaned data

write_csv2(moisture, file = "Clean_data/FUNDER_clean_soil_moisture_point_measurements_2022.csv")
