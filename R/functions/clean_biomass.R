# clean biomass

clean_biomass <- function(biomass_raw) {
  biomass_raw |>
    janitor::clean_names() |>
    filter(!is.na(site)) |>
    mutate(site = recode(site,
      "GUD" = "Gudmedalen",
      "LAV" = "Lavisdalen",
      "RAM" = "Rambera",
      "ULV" = "Ulvehaugen",
      "SKJ" = "Skjelingahaugen",
      "ALR" = "Alrust",
      "ARH" = "Arhelleren",
      "FAU" = "Fauske",
      "HOG" = "Hogsete",
      "OVS" = "Ovstedalen",
      "VIK" = "Vikesland",
      "VES" = "Veskre"
    )) |>
    pivot_longer(cols = c(bryophytes_g, forbs_g, graminoids_g, lichen_g), names_to = "removed_fg", values_to = "biomass") |>
    # remove entries without biomass
    filter(biomass > 0) |>
    mutate(removed_fg = case_match(
      removed_fg,
      "bryophytes_g" ~ "B",
      "forbs_g" ~ "F",
      "graminoids_g" ~ "G",
      "lichen_g" ~ "L"
    )) |>
    # indicate fg not in treatment (= leftover)
    mutate(
      no_treatment = case_when(
        removed_fg == "L" ~ "leftover",
        treatment == "FGB" & removed_fg %in% c("F", "G", "B") ~ "leftover",
        treatment == "FB" & removed_fg %in% c("F", "B") ~ "leftover",
        treatment == "GF" & removed_fg %in% c("F", "G") ~ "leftover",
        treatment == "GB" & removed_fg %in% c("G", "B") ~ "leftover",
        treatment == "G" & removed_fg == "G" ~ "leftover",
        treatment == "B" & removed_fg == "B" ~ "leftover",
        treatment == "F" & removed_fg == "F" ~ "leftover",
        TRUE ~ NA_character_
      ),
      year = 2022,
      blockID = paste0(substr(site, 1, 3), block),
      plotID = paste0(blockID, treatment)
    ) %>% 
    funcabization(dat = ., convert_to = "FunCaB") |>
    select(year, siteID = site, blockID, plotID, treatment, removed_fg, biomass, no_treatment, comments)
}


# biomass_clean |>
#   mutate(treatment = factor(treatment, levels = c("C", "B", "F", "G", "FB", "GB", "GF", "FGB")),
#          siteID = factor(siteID, levels = c("Skjelingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen",
#                                             "Veskre", "Rambera", "Hogsete", "Alrust",
#                                             "Ovstedalen", "Arhelleren", "Vikesland", "Fauske"))) %>%
#   filter(!no_treatment %in% c("leftover"),
#          treatment != "C") |>
#   ggplot(aes(x = treatment, y = biomass, fill = removed_fg)) +
#   geom_col() +
#   scale_fill_manual(values = c("darkgreen", "violet", "lightblue3")) +
#   facet_wrap(~ siteID) +
#   theme_bw()


# clean biomass removal 2022

clean_biomass_22 <- function(biomass_22_raw) {

  biomass_22_raw |>
    clean_names() |>
    mutate(
      site = recode(
        site,
        "GUD" = "Gudmedalen",
        "LAV" = "Lavisdalen",
        "RAM" = "Rambera",
        "ULV" = "Ulvehaugen",
        "SKJ" = "Skjelingahaugen",
        "ALR" = "Alrust",
        "ARH" = "Arhelleren",
        "FAU" = "Fauske",
        "HOG" = "Hogsete",
        "OVS" = "Ovstedalen",
        "VIK" = "Vikesland",
        "VES" = "Veskre"
      )
    ) |>
    mutate(
      blockID = paste0(substr(site, 1, 3), block),
      plotID = paste0(blockID, treatment),
      date = as.Date(date),
      temperature_level = dplyr::case_match(
        site,
        "Alrust"          ~ "sub-alpine",
        "Arhelleren"      ~ "boreal",
        "Fauske"          ~ "boreal",
        "Gudmedalen"      ~ "alpine",
        "Hogsete"         ~ "sub-alpine",
        "Lavisdalen"      ~ "alpine",
        "Ovstedalen"      ~ "boreal",
        "Rambera"         ~ "sub-alpine",
        "Skjelingahaugen" ~ "alpine",
        "Ulvehaugen"      ~ "alpine",
        "Veskre"          ~ "sub-alpine",
        "Vikesland"       ~ "boreal",
        .default = NA_character_
      ),
      precipitation_level = dplyr::case_match(
        site,
        "Alrust"          ~ 1,
        "Arhelleren"      ~ 3,
        "Fauske"          ~ 1,
        "Gudmedalen"      ~ 3,
        "Hogsete"         ~ 2,
        "Lavisdalen"      ~ 2,
        "Ovstedalen"      ~ 4,
        "Rambera"         ~ 3,
        "Skjelingahaugen" ~ 4,
        "Ulvehaugen"      ~ 1,
        "Veskre"          ~ 4,
        "Vikesland"       ~ 2,
        .default = NA_real_
      )
    ) %>%
    funcabization(dat = ., convert_to = "FunCaB") |>
    select(
      year,
      date,
      siteID = site,
      blockID,
      plotID,
      treatment,
      removed_fg = removed_functional_group,
      round,
      biomass,
      temperature_level,
      precipitation_level,
      name,
      remark
    )


}