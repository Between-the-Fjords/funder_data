# Cleaning RIC depths


clean_ric_depth <- function(ric_depth_raw){

  ric_depth_raw |>
    mutate(site = str_to_title(tolower(site)),
           blockID = paste0(site, block),
           plotID = paste0(blockID, treatment)) |>
    mutate(site = recode(site,
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
    # makes some NAs with were na before
    mutate(depth = as.numeric(gsub(",", ".", RIC_cm)),
           unit = "cm") |>
    # removes 5 rows
    filter(!is.na(depth)) |>
    select(siteID = site, blockID, plotID, treatment, depth, unit, comment)

}
