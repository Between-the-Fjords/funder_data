# bryophyte water holding capacity

clean_whc_func <- function(whc) {

  whc |>
    clean_names() |>
    separate(site, into = c("siteID", "blockID", "treatment"), sep = "-") |>
    mutate(
      year = "2022",
      siteID = str_to_title(siteID),
      blockID = paste0(siteID, blockID),
      siteID = case_match(
        siteID,
        "Gud" ~ "Gudmedalen",
        "Lav" ~ "Lavisdalen",
        "Ram" ~ "Rambera",
        "Ulv" ~ "Ulvehaugen",
        "Skj" ~ "Skjelingahaugen",
        "Alr" ~ "Alrust",
        "Arh" ~ "Arhelleren",
        "Fau" ~ "Fauske",
        "Hog" ~ "Hogsete",
        "Ovs" ~ "Ovstedalen",
        "Vik" ~ "Vikesland",
        "Ves" ~ "Veskre"
      ),
      plotID = paste0(blockID, treatment),
      saturated_weight = as.numeric(sub(",", ".", saturated_weight)),
      dry_weight = as.numeric(sub(",", ".", dry_weight)),
      whc = round(c((saturated_weight - dry_weight)) / dry_weight, digits = 2)) |>
    rename(comments = comment) |>
    select(year, siteID, blockID, plotID, treatment, whc, comments) |>
    funcabization(convert_to = "FunCaB")

}
