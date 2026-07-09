# Cleaning RIC depths (legacy workflow)

clean_ric_depth_old <- function(ric_depth_raw) {
  ric_depth_raw |>
    mutate(
      site = str_to_title(tolower(site)),
      blockID = paste0(site, block),
      plotID = paste0(blockID, treatment)
    ) |>
    mutate(site = recode(site,
      "Gud" = "Gudmedalen",
      "Lav" = "Lavisdalen",
      "Ram" = "Rambera",
      "Ulv" = "Ulvehaugen",
      "Skj" = "Skjelingahaugen",
      "Alr" = "Alrust",
      "Arh" = "Arhelleren",
      "Fau" = "Fauske",
      "Hog" = "Hogsete",
      "Ovs" = "Ovstedalen",
      "Vik" = "Vikesland",
      "Ves" = "Veskre"
    )) |>
    mutate(
      depth = as.numeric(gsub(",", ".", RIC_cm)),
      ric_depth_m = depth / 100
    ) |>
    filter(!is.na(depth)) |>
    select(siteID = site, blockID, plotID, treatment, ric_depth_m, comment)
}
