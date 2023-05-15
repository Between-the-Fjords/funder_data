# Clean greenseeker measurements

library(tidyverse)

# Load dataset
greenseeker <- read_csv2("Data/FUNDER_raw_greenseeker_measurements_2022.csv")

# Comments column describes weather conditions on the day measurements were done. In raw data-file it's entered on the two first rows of each new site (will be interpreted as belonging to a single plot). This information will be broken out to a separate dataset that only displays the weather conditions.

# Clean up names according to FUNDER naming convention
greenseeker <- greenseeker |>
  select(-comment, -block) |>
  separate(ID, c("site", "blockID", "treat")) |>
  select(-site, -treat)

greenseeker$Site <- str_to_title(greenseeker$Site)

# Fix plotIDs
greenseeker <- greenseeker |>
  mutate(blockID = paste(Site, blockID, sep = "")) |>
  mutate(plotID = paste(blockID, treatment, sep = ""))

# Fix siteIDs
greenseeker <- greenseeker |>
  rename("siteID" = "Site") |>
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

# Change order of columns
greenseeker <- greenseeker |>
  select(date, siteID, blockID, plotID, treatment, time_1, time_2, "1_before", "2_before", "1_after", "2_after")

# Measurements were done twice during the same day, once before noon ("first") and once in the afternoon ("second")
# Dataset is split to clean each measurement event separately before being merged together again

# First measurement (before cutting)
green_before <- greenseeker |>
  select(!c(time_2, "1_after", "2_after")) |>
  pivot_longer(!c(date, siteID, blockID, plotID, treatment, time_1), names_to = "rep", values_to = "record") |>
  mutate(rep = recode(rep,
                      "1_before" = "1",
                      "2_before" = "2")) |>
  mutate(measurement = "before") |>
  rename("time" = "time_1")

# Second measurement (after cutting)
green_after <- greenseeker |>
  select(!c(time_1, "1_before", "2_before")) |>
  pivot_longer(!c(date, siteID, blockID, plotID, treatment, time_2), names_to = "rep", values_to = "record") |>
  mutate(rep = recode(rep,
                      "1_after" = "1",
                      "2_after" = "2")) |>
  mutate(measurement = "after")|>
  rename("time" = "time_2")

# Bind df:s together
greenseeker_long <- rbind(green_before, green_after)

# Change order of columns
greenseeker_long <- greenseeker_long |>
  relocate(time, .after = date) |>
  relocate(measurement, .after = treatment)

# Change decimal deliminator to "."
greenseeker_long$record <- gsub(",", ".", greenseeker_long$record)
greenseeker_long$record <- as.numeric(greenseeker_long$record)

# Some values stand out: need to check and correct:
# Hog3B second 1: very high value
# Hog1GF second 2: high value
# Many NAs - especially C-plots Alr

greenseeker_long[greenseeker_long$plotID == "Hog3B" &
                   greenseeker_long$measurement == "second" &
                   greenseeker_long$rep == "1", "record"] <- 0.69 #replace
greenseeker_long[greenseeker_long$plotID == "Hog1GF" &
                   greenseeker_long$measurement == "second" &
                   greenseeker_long$rep == "2", "record"] <- 0.48 #replace

# Export cleaned dataset
write_csv2(greenseeker_long, file = "Clean_data/FUNDER_clean_greenseeker_measurements_2022.csv")

##################################################################

weather <- greenseeker |>
  select(Site, comment) |>
  drop_na() |>
  separate(comment, c("measurement_time", "weather"), sep = ":") |>
  separate(measurement_time, c("text", "time"), sep = " ") |>
  select(-text) |>
  pivot_wider(names_from = time , values_from = weather) |>
  rename("weather_before_measurement" = "before",
         "weather_after_measurement" = "after",
         "siteID" = "Site") |>
  mutate(siteID = recode(siteID,
                         # old name (replace) = valid name (do not change)
                         'GUD' = "Gudmedalen",
                         'LAV' = "Lavisdalen",
                         'RAM' = "Rambera",
                         'ULV' = "Ulvehaugen",
                         'SKJ' = "Skjelingahaugen",
                         'ALR' = "Alrust",
                         'ARH' = "Arhelleren",
                         'FAU' = "Fauske",
                         'HOG' = "Hogsete",
                         'OVS' = "Ovstedalen",
                         'VIK' = "Vikesland",
                         'VES' = "Veskre"))

# Export weather conditions during measurements
write_csv2(weather, file = "Clean_data/FUNDER_clean_greenseeker_measurements_weather_conditions_2022.csv")

