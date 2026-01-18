# clean 2015-2021 and 2022 community data

# clean 2015-2021 community data
clean_community_2015_2021 <- function(community_2015_2021_raw) {
  comm15 <- community_2015_2021_raw |>
    mutate(blockID = if_else(blockID == "Ves5", "Ves4", blockID)) |>
    # 286 rows with species is NA because they have veg height etc.
    filter(!is.na(species))
}


# clean 2022 community data
clean_community_2022 <- function(community_2022_raw, fun_gr) {
  # cover
  cover <- community_2022_raw |>
    clean_names() |>
    # filter for cover (remove presence data)
    filter(measure == "Cover") |>
    select(-x1, -measure) |>
    mutate(across(ach_mil:unknown_leaf, ~ as.numeric(.)),
      year = year(date),
      veg_height = as.numeric(str_replace(veg_height, ",", "."))
    ) |>
    # pivot to long format
    pivot_longer(cols = "ach_mil":"unknown_leaf", names_to = "species", values_to = "cover") |>
    filter(!is.na(cover)) |>
    mutate(
      species = str_replace(species, "_", "."),
      species = str_to_title(species),
      turfID = if_else(treatment %in% c("C", "F", "B", "G", "FB", "GF", "GB", "FGB"), NA_character_, treatment),
      treatment = if_else(!is.na(turfID), "C", treatment),
      turfID = case_when(turfID == "28 TT1 288" ~ "287 TT1 288"),
      block_id = paste0(substr(site_id, 1, 3), block_id),
      plot_id = paste0(block_id, treatment)
    ) |>
    select(year, date,
      siteID = site_id, blockID = block_id, plotID = plot_id, treatment, species, cover,
      vegetation_height = veg_height, litter, total_graminoids = graminoid, total_forbs = forb, total_lichen = lichen, total_rock = rock, total_soil = bare_soil,
      turfID, weather, recorder, comments, comments_data_entering
    )

  # Convert to funcab names
  cover <- dataDocumentation::funcabization(cover, convert_to = "FunCaB") |>
    mutate(species = case_when(
      species == "Unknown.seedling" ~ "NID.seedling",
      species == "Unknown.leaf" ~ "NID.sp",
      species == "Fes.ovi_viv" ~ "Fes.ovi",
      species == "Ant.alp" ~ "Ant.dio",
      species == "Sag.pro" ~ "Sag.sp",
      species == "Luz.syl" & plotID %in% c("Arh1F", "Arh3G") ~ "Luz.pil",
      species == "Luz.syl" & plotID %in% c("Arh1B", "Arh1FB", "Arh1G", "Arh1GB", "Vik2GB") ~ "Luz.mul",
      TRUE ~ species
    )) |>
    # fix plotID
    mutate(plotID = if_else(comments_data_entering %in% c("NB!: There are two plots called Alr2FGB in the physical data sheets - Vigdis and I (Susanne) think this one is actually Alr2GF (which there was no data for on the sheets, since there were two FGBs), so I changed the name to Alr2GF in this data document."), "Alr2GF", plotID)) |>
    mutate(treatment = if_else(comments_data_entering %in% c("NB!: There are two plots called Alr2FGB in the physical data sheets - Vigdis and I (Susanne) think this one is actually Alr2GF (which there was no data for on the sheets, since there were two FGBs), so I changed the name to Alr2GF in this data document."), "GF", treatment)) |>
    # add functional group
    left_join(fun_gr)

  cover <- cover |>
    # remove functional groups that should not be there
    # fix forbs
    mutate(total_forbs = if_else(treatment %in% c("FGB", "FB", "GF", "F"), NA_real_, total_forbs)) |>
    filter(!(treatment %in% c("FGB", "FB", "GF", "F") & functional_group == "forb")) |>
    # fix graminoids
    mutate(total_graminoids = if_else(treatment %in% c("FGB", "GB", "GF", "G"), NA_real_, total_graminoids)) |>
    filter(!(treatment %in% c("FGB", "GB", "GF", "G") & functional_group == "graminoid"))

}

  # join 2015-2021 and 2022 community data
  join_community <- function(community_2015_2021_clean, community_2022_clean) {
    community <- bind_rows(community_2015_2021_clean, community_2022_clean) |>
    filter(treatment != "XC")
}

# apply turf map corrections
apply_turf_map_corrections <- function(community_clean, turf_map_corrections_fixed) {
  
  # Start with the community data
  community_corrected <- community_clean
  
  # Rule 4: Handle deletions first (if "delete" in comment)
  # Note: year_expanded contains individual years (one per row after unnest in fix_turf_map_corrections)
  # So each year gets its own row, and the join by year applies corrections to all years in year_expanded
  # Use formatted species names (from_species, to_species) to match community_clean format
  # If to_species is NA because "delete" was removed, use from_species
  deletions <- turf_map_corrections_fixed |>
    filter(str_detect(comment, regex("delete", ignore_case = TRUE))) |>
    filter(!is.na(from_species) | !is.na(to_species)) |>
    select(siteID, blockID, plotID, treatment, year_expanded, from_species, to_species) |>
    # Create a species column (use to_species if from_species is NA, otherwise from_species)
    # This handles cases where to_species became NA when "delete" was moved to comment
    mutate(species_to_delete = coalesce(from_species, to_species)) |>
    # Rename year_expanded to year for joining with community_clean
    select(siteID, blockID, plotID, treatment, year = year_expanded, species = species_to_delete) |>
    filter(!is.na(species))  # Remove any rows where species is still NA
  
  # Remove deleted species
  if (nrow(deletions) > 0) {
    community_corrected <- community_corrected |>
      tidylog::anti_join(deletions, by = c("siteID", "blockID", "plotID", "treatment", "year", "species"))
  }
  
  # Rule 1: Change species name (both from_species and to_species exist)
  # year_expanded: Each year from the original year range gets its own row, so corrections apply to all years
  species_changes <- turf_map_corrections_fixed |>
    filter(!is.na(from_species) & !is.na(to_species)) |>
    filter(!str_detect(comment, regex("delete", ignore_case = TRUE))) |>  # Exclude deletions
    select(siteID, blockID, plotID, treatment, year = year_expanded, from_species, to_species) |>
    distinct()
  
  # Apply species name changes
  if (nrow(species_changes) > 0) {
    community_corrected <- community_corrected |>
      left_join(species_changes, by = c("siteID", "blockID", "plotID", "treatment", "year", "species" = "from_species")) |>
      mutate(species = coalesce(to_species, species)) |>
      select(-to_species)
  }
  
  # Rule 2: Adjust cover for to_species (increase or decrease)
  # year_expanded: Each year from the original year range gets its own row, so corrections apply to all years
  cover_adjustments_to <- turf_map_corrections_fixed |>
    filter(!is.na(to_species) & is.na(from_species)) |>
    filter(!str_detect(comment, regex("delete", ignore_case = TRUE))) |>  # Exclude deletions
    filter(!(is.na(increase_to_cover) & is.na(decrease_to_cover))) |>  # Both NA = skip
    select(siteID, blockID, plotID, treatment, year = year_expanded, to_species, increase_to_cover, decrease_to_cover) |>
    mutate(
      cover_adjustment = if_else(!is.na(increase_to_cover), increase_to_cover,
                                 if_else(!is.na(decrease_to_cover), -decrease_to_cover, 0))
    ) |>
    select(siteID, blockID, plotID, treatment, year, species = to_species, cover_adjustment) |>
    distinct()
  
  # Track cases where cover would go below 0
  negative_cover_cases_to <- NULL
  
  # Apply cover adjustments for to_species
  if (nrow(cover_adjustments_to) > 0) {
    community_corrected <- community_corrected |>
      left_join(cover_adjustments_to, by = c("siteID", "blockID", "plotID", "treatment", "year", "species")) |>
      mutate(
        cover_original = cover,
        cover = if_else(!is.na(cover_adjustment), cover + cover_adjustment, cover)
      )
    
    # Track cases where cover goes below 0
    negative_cover_cases_to <- community_corrected |>
      filter(!is.na(cover_adjustment) & cover < 0) |>
      select(siteID, blockID, plotID, treatment, year, species, cover_original, cover_adjustment, cover_new = cover) |>
      mutate(rule = "Rule 2 (to_species cover adjustment)")
    
    # Set cover to 0 if it goes below 0 (temporary - user will review)
    community_corrected <- community_corrected |>
      mutate(cover = if_else(cover < 0, 0, cover)) |>
      select(-cover_original, -cover_adjustment)
  }
  
  # Rule 3: Decrease cover from from_species (only if decrease_from_cover is not NA)
  # year_expanded: Each year from the original year range gets its own row, so corrections apply to all years
  cover_adjustments_from <- turf_map_corrections_fixed |>
    filter(!is.na(from_species) & is.na(to_species)) |>
    filter(!is.na(decrease_from_cover)) |>  # Only if decrease_from_cover has a value
    filter(!str_detect(comment, regex("delete", ignore_case = TRUE))) |>  # Exclude deletions
    select(siteID, blockID, plotID, treatment, year = year_expanded, from_species, decrease_from_cover) |>
    mutate(cover_adjustment = -decrease_from_cover) |>
    select(siteID, blockID, plotID, treatment, year, species = from_species, cover_adjustment) |>
    distinct()
  
  # Track cases where cover would go below 0
  negative_cover_cases_from <- NULL
  
  # Apply cover adjustments for from_species
  if (nrow(cover_adjustments_from) > 0) {
    community_corrected <- community_corrected |>
      left_join(cover_adjustments_from, by = c("siteID", "blockID", "plotID", "treatment", "year", "species")) |>
      mutate(
        cover_original = cover,
        cover = if_else(!is.na(cover_adjustment), cover + cover_adjustment, cover)
      )
    
    # Track cases where cover goes below 0
    negative_cover_cases_from <- community_corrected |>
      filter(!is.na(cover_adjustment) & cover < 0) |>
      select(siteID, blockID, plotID, treatment, year, species, cover_original, cover_adjustment, cover_new = cover) |>
      mutate(rule = "Rule 3 (from_species cover decrease)")
    
    # Set cover to 0 if it goes below 0 (temporary - user will review)
    community_corrected <- community_corrected |>
      mutate(cover = if_else(cover < 0, 0, cover)) |>
      select(-cover_original, -cover_adjustment)
  }
  
  # Combine and report negative cover cases
  negative_cover_cases <- bind_rows(
    negative_cover_cases_to,
    negative_cover_cases_from
  )
  
  if (!is.null(negative_cover_cases) && nrow(negative_cover_cases) > 0) {
    warning(
      "Found ", nrow(negative_cover_cases), 
      " cases where cover would go below 0 after adjustments. ",
      "These have been set to 0. Use attr(result, 'negative_cover_cases') to review."
    )
    attr(community_corrected, "negative_cover_cases") <- negative_cover_cases
  }
  
  return(community_corrected)
}


# makeing turf maps
# title <- community |>
  #   distinct(plotID) |>
  #   tidylog::left_join(cover |>
  #     distinct(plotID, title = paste0(plotID, "-", recorder)), by = "plotID") |>
  #   mutate(title = coalesce(title, plotID))

  # community <- community |>
  #   left_join(title, by = "plotID")

  # write_csv(community, "community_2015-2022.csv")

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

# Ram8C: Phe.con ???
# Lav1&2 Ped.bor (Pedicularis?)
# Fau1F: Vis.vul (Viscaria vulgaris?)


# subplots
clean_subplot <- function(community_raw) {
  subplot <- community_raw |>
    clean_names() |>
    filter(measure != "Cover") |>
    select(-measure, -c(graminoid:veg_height)) |>
    mutate(across(ach_mil:unknown_leaf, ~ as.character(.)),
      year = year(date),
      block_id = paste0(substr(site_id, 1, 3), block_id)
    ) |>
    pivot_longer(cols = "ach_mil":"unknown_leaf", names_to = "species", values_to = "cover") |>
    filter(!is.na(cover)) |>
    mutate(
      cover = if_else(cover == "TRUE", "1", cover),
      species = str_replace(species, "_", "."),
      species = str_to_title(species),
      turfID = if_else(treatment %in% c("C", "F", "B", "G", "FB", "GF", "GB", "FGB"), NA_character_, treatment),
      treatment = if_else(!is.na(turfID), "C", treatment),
      plot_id = paste0(block_id, treatment)
    ) |>
    # fix cover
    mutate(
      cover = case_when(
        cover == "?" ~ "1",
        cover == "*" ~ "1*",
        cover == "1 J" ~ "J",
        cover == "1 S" ~ "S",
        cover == "2 J" ~ "J",
        TRUE ~ cover
      ),
      presence = 1,
      fertile = if_else(str_detect(cover, "\\*"), 1, NA_real_),
      juvenile = if_else(str_detect(cover, "J"), 1, NA_real_),
      seedling = case_when(
        str_detect(cover, "S") ~ 1,
        str_detect(cover, "2") ~ 2,
        str_detect(cover, "3") ~ 3,
        str_detect(cover, "4") ~ 4,
        TRUE ~ NA_real_
      )
    ) |>
    select(year, date,
      siteID = site_id, blockID = block_id, plotID = turf_id, subplotID = x1, treatment, species, presence, fertile, juvenile, seedling,
      turfID, weather, recorder, comments, comments_data_entering
    )
  subplot <- funcabization(subplot, convert_to = "FunCaB")
}


### FunCaBization package and tests
# funder <- tibble(
#   blockID = c("Gud1", "Gud2", "Arh1", "Arh5"),
#   var = c("bla", "bla", "ding", "dong"),
#   treatment = c("C", "F", "FB", "FBG")
# )

# funcab <- tibble(
#   blockID = c("Gud5", "Gud12"),
#   var = c("bla", "bla"),
#   treatment = c("FB", "FBG")
# )
# funcabization(dat = funder, convert_to = "FunCaB")
# funcabization(dat = funcab, convert_to = "Funder")

# funcabization <- function(dat, convert_to = "FunCaB"){

#   dic <- read_csv("raw_data/FunCab_Funder_blockID_dictionary.csv")

#   # convert to FunCaB
#   if(convert_to == "FunCaB"){
#     dat |>
#       # join with dicionary by funder blockID
#       left_join(dic, by = c("blockID" = "funder_blockID")) |>
#       mutate(blockID = coalesce(funcab_blockID, blockID),
#              plotID = paste0(blockID, treatment)) |>
#       select(-funcab_blockID)
#     # convert to Funder
#   } else if(convert_to == "Funder") {
#     dat |>
#       # join with dictionary by funcab blockID
#       left_join(dic, c("blockID" = "funcab_blockID")) |>
#       mutate(blockID = coalesce(funder_blockID, blockID),
#              plotID = paste0(blockID, treatment)) |>
#       select(-funder_blockID)
#   }

# }
