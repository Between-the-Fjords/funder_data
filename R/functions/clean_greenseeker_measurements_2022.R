# Clean greenseeker measurements

# Comments column describes weather conditions on the day measurements were done. In raw data-file it's entered on the two first rows of each new site (will be interpreted as belonging to a single plot). This information will be broken out to a separate dataset that only displays the weather conditions.

# Clean up names according to FUNDER naming convention
clean_greenseeker <- function(ndvi_raw){

  ndvi_clean <- ndvi_raw |>
    rename(siteID = Site, blockID = block) |>
    mutate(siteID = str_to_title(siteID)) |>
    # Fix plotIDs
    mutate(blockID = paste(siteID, blockID, sep = ""),
           plotID = paste(blockID, treatment, sep = "")) |>
    # Fix siteIDs
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
  # Change order of columns
    select(date, siteID, blockID, plotID, treatment, time_1 = time...8, time_2 = time...11, "1_before", "2_before", "1_after", "2_after", comment) |>

    # make long table
    rename("time_before" = time_1, "time_after" = time_2) |>
    pivot_longer(!c(date, siteID, blockID, plotID, treatment, time_before, time_after, comment),
                 names_to = c("replicate", "measurement"),
                 names_sep = "_",
                 values_to = "value") |>
    mutate(time = if_else(measurement == "before", time_before, time_after)) |>
    select(date:treatment, time, measurement, replicate, value, comment) |>

    # Change decimal deliminator to "."
    mutate(value = as.numeric(str_replace(value, ",", "."))) |>
    # introduced NAs are ok, because they were written "na" -> remove
    # Many NAs - especially C-plots Alr
    filter(!is.na(value)) |>

    # clean data: some values stand out and need to check and correct.
    # Hog3B and Hog1GF shave too high values, replace them
    mutate(value = case_when(plotID == "Hog1GF" & measurement == "after" & replicate == "2" ~ 0.48,
                             plotID == "Hog3B" & measurement == "after" & replicate == "1" ~ 0.69,
                             TRUE ~ value))

  # dd |>
  #   ggplot(aes(x = treatment, y = value)) +
  #   geom_point()

  # Deal with comments
  # Alrust and Arhelleren are missing comments
  weather_comments <- ndvi_clean |>
    #filter(!is.na(comment)) |>
    #distinct(date, siteID, measurement, comment) |>
    mutate(comment = case_when(measurement == "before" & grepl("before", comment) ~ comment,
                               measurement == "after" & grepl("after", comment) ~ comment,
                               TRUE ~ NA_character_)) |>
    filter(!is.na(comment)) |>
    distinct(siteID, measurement, comment) |>
    mutate(comment = str_remove(comment, "Weather before: |Weather after: "))

  ndvi_clean |>
    select(-comment) |>
    left_join(weather_comments, by = c("siteID", "measurement")) |>
    rename(weather = comment)

}

