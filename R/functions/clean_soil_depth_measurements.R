# Clean soil depth measurements

clean_depth <- function(depth_raw){

  # Reshape data into long format
  depth_raw |>
    pivot_longer(!c(siteID, block, treatment, plotID), names_to = "measurement", values_to = "depth") |>
    # fix funder nomenclature
    mutate(siteID = str_to_title(siteID),
           plotID = paste0(siteID, block, treatment),
           blockID = paste0(siteID, block)) |>
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
    # Replace "," with "." as decimal deliminator
    # 9 new NAs that were na before: non existing plots (Fau2GB, Ulv1B, Skj4C)
    mutate(depth = as.numeric(gsub(",", ".", depth)),
           unit = "cm",
           measurement = as.numeric(str_remove(measurement, "depth"))) |>
    filter(!is.na(depth)) |>
    select(siteID, blockID, plotID, treatment, measurement, depth, unit)

}

