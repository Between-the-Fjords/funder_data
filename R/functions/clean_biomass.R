# clean biomass

clean_biomass <- function(biomass_raw){

  biomass_raw |>
    clean_names() |>
    filter(!is.na(site)) |>
    mutate(site = recode(site,
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
                         'VES' = "Veskre")) |>
    pivot_longer(cols = c(bryophytes_g, forbs_g, graminoids_g, lichen_g), names_to = "removed_fg", values_to = "biomass") |>
    # remove entries without biomass
    filter(biomass > 0) |>
    mutate(removed_fg = case_match(removed_fg,
                                   "bryophytes_g" ~ "B",
                                   "forbs_g" ~ "F",
                                   "graminoids_g" ~ "G",
                                   "lichen_g" ~ "L")) |>
    # indicate fg not in treatment (= leftover)
    mutate(no_treatment = case_when(removed_fg == "L" ~ "leftover",
                                    treatment == "FB" & removed_fg == "G" ~ "leftover",
                                    treatment == "GF" & removed_fg == "B" ~ "leftover",
                                    treatment == "GB" & removed_fg == "F" ~ "leftover",
                                    treatment == "G" & removed_fg %in% c("F", "B") ~ "leftover",
                                    treatment == "B" & removed_fg %in% c("F", "G") ~ "leftover",
                                    treatment == "F" & removed_fg %in% c("G", "B") ~ "leftover",
                                    TRUE ~ NA_character_)) |>
    select(siteID = site, blockID = block, treatment, removed_fg, biomass, no_treatment, comments)
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
