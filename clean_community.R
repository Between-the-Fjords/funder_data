# clean community 2022

comm15 <- read_csv("clean_data/FunCaB_clean_composition_2015-2019.csv") |>
  mutate(blockID = if_else(blockID == "Ves5", "Ves4", blockID)) |>
  # 286 rows with species is NA because they have veg height etc.
  filter(!is.na(species))

fun_gr <- comm15 |>
    distinct(species, functional_group)

# cover
cover <- community_raw |>
  clean_names() |>
  filter(measure == "Cover") |>
  select(-x1, -measure) |>
  mutate(across(ach_mil:unknown_leaf, ~as.numeric(.)),
         year = year(date),
         veg_height = as.numeric(str_replace(veg_height, ",", "."))) |>
  pivot_longer(cols = "ach_mil":"unknown_leaf", names_to = "species", values_to = "cover") |>
  filter(!is.na(cover)) |>
  mutate(species = str_replace(species, "_", "."),
         species = str_to_title(species),
         turfID = if_else(treatment %in% c("C", "F", "B", "G", "FB", "GF", "GB", "FGB"), NA_character_, treatment),
         treatment = if_else(!is.na(turfID), "C", treatment),
         turfID = case_when(turfID == "28 TT1 288" ~ "287 TT1 288"),
         block_id = paste0(substr(site_id, 1, 3), block_id),
         plot_id = paste0(block_id, treatment)) |>
  select(year, date, siteID = site_id, blockID = block_id, plotID = plot_id, treatment, species, cover,
         vegetation_height = veg_height, litter, total_graminoids = graminoid, total_forbs = forb, total_lichen = lichen, total_rock = rock, total_soil = bare_soil,
         turfID, weather, recorder, comments, comments_data_entering)


# Convert to funcab names
cover <- funcabization(cover, convert_to = "FunCaB") |>
  mutate(species = case_when(species == "Unknown.seedling" ~ "NID.seedling",
                             species == "Unknown.leaf" ~ "NID.sp",
                             species == "Fes.ovi_viv" ~ "Fes.ovi",
                             species == "Ant.alp" ~ "Ant.dio",
                             species == "Sag.pro" ~ "Sag.sp",
                             species == "Luz.syl" & plotID %in% c("Arh1F", "Arh3G") ~ "Luz.pil",
                             species == "Luz.syl" & plotID %in% c("Arh1B", "Arh1FB", "Arh1G", "Arh1GB", "Vik2GB") ~ "Luz.mul",
                             TRUE ~ species)) |>
  # fix plotID
  mutate(plotID = if_else(comments_data_entering %in% c("NB!: There are two plots called Alr2FGB in the physical data sheets - Vigdis and I (Susanne) think this one is actually Alr2GF (which there was no data for on the sheets, since there were two FGBs), so I changed the name to Alr2GF in this data document."), "Alr2GF", plotID)) |>
  mutate(treatment = if_else(comments_data_entering %in% c("NB!: There are two plots called Alr2FGB in the physical data sheets - Vigdis and I (Susanne) think this one is actually Alr2GF (which there was no data for on the sheets, since there were two FGBs), so I changed the name to Alr2GF in this data document."), "GF", treatment)) |>
  left_join(fun_gr)

title <- community |>
  distinct(plotID) |>
  tidylog::left_join(cover |>
              distinct(plotID, title = paste0(plotID, "-", recorder)), by = "plotID") |>
  mutate(title = coalesce(title, plotID))

cover <- cover |>
  # remove functional groups that should not be there
  # fix forbs
  mutate(total_forbs = if_else(treatment %in% c("FGB", "FB", "GF", "F"), NA_real_, total_forbs)) |>
  filter(!(treatment %in% c("FGB", "FB", "GF", "F") & functional_group == "forb")) |>
  # fix graminoids
  mutate(total_graminoids = if_else(treatment %in% c("FGB", "GB", "GF", "G"), NA_real_, total_graminoids)) |>
  filter(!(treatment %in% c("FGB", "GB", "GF", "G") & functional_group == "graminoid"))
  # fix bryophytes (no such column! Needs fixing in bryophyte data)
  # mutate(total_bryophytes = if_else(treatment %in% c("FGB", "FB", "GB", "B"), NA_real_, total_bryophytes))


# for turf maps
community <- bind_rows(comm15, cover) |>
  filter(treatment != "XC")

community <- community |>
  left_join(title, by = "plotID")

#write_csv(community, "community_2015-2022.csv")


# checks
# comm15 |>
#   filter(treatment != "XC") |>
#   distinct(plotID) |>
#   anti_join(cover |>
#               distinct(plotID))

# missing plot IDs
# Fau2GB # -> has not been treated since 2017, lost
# Skj4C # probably forgotten in 2022
# Ulv1B # missed plot, cannot be found anymore



# species not in 2015-2019 data
# cover |>
#   distinct(species) |>
#   anti_join(comm15 |> distinct(species))

#Ram8C: Phe.con ???
#Lav1&2 Ped.bor (Pedicularis?)
#Fau1F: Vis.vul (Viscaria vulgaris?)




# subplots
subplot <- community_raw |>
  clean_names() |>
  filter(measure != "Cover") |>
  select(-measure, -c(graminoid:veg_height)) |>
  mutate(across(ach_mil:unknown_leaf, ~as.character(.)),
         year = year(date),
         block_id = paste0(substr(site_id, 1, 3), block_id)) |>
  pivot_longer(cols = "ach_mil":"unknown_leaf", names_to = "species", values_to = "cover") |>
  filter(!is.na(cover)) |>
  mutate(cover = if_else(cover == "TRUE", "1", cover),
         species = str_replace(species, "_", "."),
         species = str_to_title(species),
         turfID = if_else(treatment %in% c("C", "F", "B", "G", "FB", "GF", "GB", "FGB"), NA_character_, treatment),
         treatment = if_else(!is.na(turfID), "C", treatment),
         plot_id = paste0(block_id, treatment)) |>
  # fix cover
  mutate(cover = case_when(cover == "?" ~ "1",
                           cover == "*" ~ "1*",
                           cover == "1 J" ~ "J",
                           cover == "1 S" ~ "S",
                           cover == "2 J" ~ "J",
                           TRUE ~ cover),
         presence = 1,
         fertile = if_else(str_detect(cover, "\\*"), 1, NA_real_),
         juvenile = if_else(str_detect(cover, "J"), 1, NA_real_),
         seedling = case_when(str_detect(cover, "S") ~ 1,
                              str_detect(cover, "2") ~ 2,
                              str_detect(cover, "3") ~ 3,
                              str_detect(cover, "4") ~ 4,
                              TRUE ~ NA_real_)) |>
  select(year, date, siteID = site_id, blockID = block_id, plotID = turf_id, subplotID = x1, treatment, species, presence, fertile, juvenile, seedling,
         turfID, weather, recorder, comments, comments_data_entering)
subplot <- funcabization(subplot, convert_to = "FunCaB")


### FunCaBization package and tests
funder <- tibble(
  blockID = c("Gud1", "Gud2", "Arh1", "Arh5"),
  var = c("bla", "bla", "ding", "dong"),
  treatment = c("C", "F", "FB", "FBG")
)

funcab <- tibble(
  blockID = c("Gud5", "Gud12"),
  var = c("bla", "bla"),
  treatment = c("FB", "FBG")
)
funcabization(dat = funder, convert_to = "FunCaB")
funcabization(dat = funcab, convert_to = "Funder")

funcabization <- function(dat, convert_to = "FunCaB"){

  dic <- read_csv("raw_data/FunCab_Funder_blockID_dictionary.csv")

  # convert to FunCaB
  if(convert_to == "FunCaB"){
    dat |>
      # join with dicionary by funder blockID
      left_join(dic, by = c("blockID" = "funder_blockID")) |>
      mutate(blockID = coalesce(funcab_blockID, blockID),
             plotID = paste0(blockID, treatment)) |>
      select(-funcab_blockID)
    # convert to Funder
  } else if(convert_to == "Funder") {
    dat |>
      # join with dictionary by funcab blockID
      left_join(dic, c("blockID" = "funcab_blockID")) |>
      mutate(blockID = coalesce(funder_blockID, blockID),
             plotID = paste0(blockID, treatment)) |>
      select(-funder_blockID)
  }

}
