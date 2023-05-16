# Clean soil depth measurements

library(tidyverse)
library(stringr)

soil_depth_raw <- read_csv2("Data/FUNDER_raw_soil_depth_measurements_2022.csv")

# Reshape data into long format
soil_depth <- soil_depth_raw |>
  select(-block) |>
  rename("1" = depth1, "2" = depth2, "3" = depth3) |>
  relocate(treatment, .after = plotID) |>
  pivot_longer(!c(siteID, treatment, plotID), names_to = "measurement", values_to = "depth_cm")


# Change siteID and plotID to fit FUNDER naming convention
# First: split old plotID up and clean df to only contain relevant columns
soil_depth <- soil_depth |>
  separate(plotID, c("site", "blockID", "treat")) |>
  select(-c(site, treat))

# Edit siteID to correct naming convention and create new plotID
soil_depth$siteID <- str_to_title(soil_depth$siteID)
soil_depth$plotID <- paste(soil_depth$siteID, soil_depth$blockID, soil_depth$treatment, sep = "")

# Edit blockID based on FUNDER naming convention
soil_depth$blockID <- paste(soil_depth$siteID, soil_depth$blockID, sep = "")

# Recode siteID to full site name
soil_depth <- soil_depth |>
  relocate(plotID, .after = blockID) |>
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

# Replace "," with "." as decimal deliminator
soil_depth$depth_cm <- gsub(",", ".", soil_depth$depth_cm)

# Force soil depth to be read as a numeric variable
soil_depth$depth_cm <- as.numeric(soil_depth$depth_cm)

# ***Remove plotIDs without measurements:
soil_depth |>
  subset(is.na(depth_cm))
# NAplots: Fau2GB, Ulv1B, Skj4C (these plots have been terminated)

soil_depth_clean <- soil_depth |>
  filter(plotID != "Fau2GB") |>
  filter(plotID != "Ulv1B") |>
  filter(plotID != "Skj4C")

# Export cleaned dataset
write_csv2(soil_depth_clean, file = "Clean_data/FUNDER_clean_soil_depth_measurements_2022.csv")
