# Clean CNP core depth data

library(tidyverse)

# Read data
CNP <- read_csv2("Data/FUNDER_raw_CNP_core_depths_2022.csv")

# Fix column names and variables in accordance with FUNDER naming convention
CNP <- CNP |>
  mutate(blockID = paste(siteID, block, sep = "")) |>
  relocate(blockID, plotID, .after = siteID) |>
  relocate(date, .before = siteID) |>
  select(!block)

# Replace "," with "." as decimal deliminator
CNP$depth_cm <- gsub(",", ".", CNP$depth_cm,)

# Force soil depth to be read as a numeric variable
CNP$depth_cm <- as.numeric(CNP$depth_cm)

# Sample Fau2GF has one recorded depth of 66 cm clearly a typo from data entry
# Checked field noter and value should be 6 cm
# Manually fix typo:

CNP[CNP$plotID == "Fau2GF" & CNP$core_no == "1", "depth_cm"] <- 6

# Remove rows where depth is NA
CNP_clean <- CNP |>
  drop_na(depth_cm)

# Export cleaned dataset
write_csv2(CNP_clean, file = "Clean_data/FUNDER_clean_CNP_core_depths_2022.csv")


