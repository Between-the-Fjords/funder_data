apply_turf_map_corrections <- function(community_clean, turf_map_corrections_fixed, fun_gr = NULL) {
  
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
    # Check which deletions actually match existing species
    deletion_matches <- community_corrected |>
      semi_join(deletions, by = c("siteID", "blockID", "plotID", "treatment", "year", "species"))
    
    # Check for deletions that don't match any existing species
    deletion_non_matches <- deletions |>
      anti_join(community_corrected, by = c("siteID", "blockID", "plotID", "treatment", "year", "species"))
    
    if (nrow(deletion_non_matches) > 0) {
      warning(
        "Found ", nrow(deletion_non_matches), 
        " deletion instruction(s) for species that don't exist in the data. ",
        "These will be ignored. Use attr(result, 'deletion_non_matches') to review."
      )
      attr(community_corrected, "deletion_non_matches") <- deletion_non_matches
    }
    
    # Remove deleted species (only affects species that actually exist)
    community_corrected <- community_corrected |>
      tidylog::anti_join(deletions, by = c("siteID", "blockID", "plotID", "treatment", "year", "species"))
  }
  
  # Initialize tracking columns
  community_corrected <- community_corrected |>
    mutate(
      is_merged_species = FALSE,
      is_added_increase_row = FALSE
    )
  
  # Rule 1: Change species name (both from_species and to_species exist)
  # year_expanded: Each year from the original year range gets its own row, so corrections apply to all years
  species_changes <- turf_map_corrections_fixed |> 
    filter(!is.na(from_species) & !is.na(to_species)) |> 
    # Exclude deletions: handle NA comments properly (str_detect returns NA for NA values)
    filter(!coalesce(str_detect(comment, regex("delete", ignore_case = TRUE)), FALSE)) |>  
    select(siteID, blockID, plotID, treatment, year = year_expanded, from_species, to_species, is_merge_case) |>
    distinct()
  
  # Apply species name changes
  if (nrow(species_changes) > 0) {
    community_corrected <- community_corrected |>
      # Store original species name before join (for comment tracking)
      mutate(original_species = species) |>
      left_join(species_changes, by = c("siteID", "blockID", "plotID", "treatment", "year", "species" = "from_species")) |>
      mutate(
        # Change species name if to_species is present (join matched)
        species = if_else(!is.na(to_species), to_species, species),
        # Track merge cases: mark rows where species name was changed from a merge case
        is_merged_species = if_else(!is.na(to_species) & coalesce(is_merge_case, FALSE), TRUE, is_merged_species),
        # Add comment tracking species name change (preserve existing comments)
        # Use original_species (before change) and to_species (new name) for the comment
        comments = if_else(
          !is.na(to_species),
          if_else(
            is.na(comments) | comments == "",
            paste0("Species name changed from ", original_species, " to ", to_species, " (turf map correction)"),
            paste0(comments, "; Species name changed from ", original_species, " to ", to_species, " (turf map correction)")
          ),
          comments
        )
      ) |>
      select(-to_species, -is_merge_case, -original_species)
    
    # Merge duplicate species created by renaming
    # (when species A is renamed to species B, but species B already exists)
    key_cols <- c("siteID", "blockID", "plotID", "treatment", "year", "species")
    
    # Find duplicates
    duplicate_keys <- community_corrected |>
      group_by(across(all_of(key_cols))) |>
      filter(n() > 1) |>
      ungroup() |>
      select(all_of(key_cols)) |>
      distinct()
    
    if (nrow(duplicate_keys) > 0) {
      message("Merging ", nrow(duplicate_keys), " duplicate species groups created by renaming")
      
      # Separate duplicates from non-duplicates
      duplicates <- community_corrected |>
        semi_join(duplicate_keys, by = key_cols)
      
      non_duplicates <- community_corrected |>
        anti_join(duplicate_keys, by = key_cols)
      
      # Merge duplicates: sum cover, combine comments, take first value of other columns
      # Get all column names except the ones we handle specially
      other_cols <- setdiff(names(community_corrected), c(key_cols, "cover", "comments", "is_merged_species"))
      
      merged_duplicates <- duplicates |>
        group_by(across(all_of(key_cols))) |>
        summarise(
          cover = sum(cover, na.rm = TRUE),
          # Combine unique non-NA comments, then add merge note
          comments = {
            unique_comments <- unique(na.omit(comments))
            unique_comments <- unique_comments[unique_comments != ""]
            combined <- if (length(unique_comments) > 0) paste(unique_comments, collapse = "; ") else ""
            # Add merge note
            merge_note <- "Species merged (duplicate after renaming, covers summed)"
            if (combined != "") paste0(combined, "; ", merge_note) else merge_note
          },
          is_merged_species = TRUE,
          # Take first non-NA value for all other columns
          across(all_of(other_cols), ~ first(na.omit(.x))),
          .groups = "drop"
        )
      
      # Recombine
      community_corrected <- bind_rows(non_duplicates, merged_duplicates)
    }
  }
  
  # Rule 2: Adjust cover for to_species (increase or decrease)
  # year_expanded: Each year from the original year range gets its own row, so corrections apply to all years
  cover_adjustments_to <- turf_map_corrections_fixed |>
    # Exclude deletions; keep rows with NA comments
    filter(!coalesce(str_detect(comment, regex("delete", ignore_case = TRUE)), FALSE)) |>  # Exclude deletions
    filter(!(is.na(increase_to_cover) & is.na(decrease_to_cover))) |>  # Both NA = skip
    mutate(
      # Determine which species to adjust:
      # - Prefer to_species when present
      # - Fall back to from_species when to_species is missing but we still have a decrease_to_cover
      species_target = case_when(
        !is.na(to_species) ~ to_species,
        is.na(to_species) & !is.na(decrease_to_cover) & !is.na(from_species) ~ from_species,
        TRUE ~ NA_character_
      )
    ) |>
    # Drop rows where we still don't know which species to adjust
    filter(!is.na(species_target)) |>
    select(siteID, blockID, plotID, treatment, year = year_expanded,
           species_target, increase_to_cover, decrease_to_cover) |>
    mutate(
      # Target value for decreases; additive adjustment for increases
      cover_target = decrease_to_cover,
      cover_adjustment = case_when(
        !is.na(increase_to_cover) ~ increase_to_cover,
        !is.na(decrease_to_cover) ~ -decrease_to_cover,
        TRUE ~ NA_real_
      )
    ) |>
    select(siteID, blockID, plotID, treatment, year,
           species = species_target, cover_adjustment, cover_target, increase_to_cover) |>
    distinct()
  
  # For increase_to_cover: add missing species rows (species absent in that year)
  if (nrow(cover_adjustments_to) > 0) {
    keys <- c("siteID", "blockID", "plotID", "treatment", "year", "species")
    
    existing_keys <- community_corrected |>
      select(all_of(keys)) |>
      distinct()
    
    missing_additions <- cover_adjustments_to |>
      filter(!is.na(increase_to_cover)) |>
      select(all_of(keys), increase_to_cover) |>
      anti_join(existing_keys, by = keys)
    
    if (nrow(missing_additions) > 0) {
      # Create new rows with cover starting at 0; Rule 2 below will then add increase_to_cover
      new_rows <- missing_additions |>
        transmute(
          siteID,
          blockID,
          plotID,
          treatment,
          year,
          species,
          cover = 0,
          is_merged_species = FALSE,
          is_added_increase_row = TRUE
        )
      
      community_corrected <- bind_rows(community_corrected, new_rows)
    }
  }
  
  # Track cases where cover would go below 0
  negative_cover_cases_to <- NULL
  
  # Apply cover adjustments for to_species
  if (nrow(cover_adjustments_to) > 0) {
    community_corrected <- community_corrected |>
      left_join(cover_adjustments_to, by = c("siteID", "blockID", "plotID", "treatment", "year", "species")) |>
      mutate(
        cover_original = cover,
        cover = case_when(
          !is.na(cover_target) ~ cover_target,
          !is.na(cover_adjustment) ~ cover + cover_adjustment,
          TRUE ~ cover
        ),
        comments = case_when(
          !is.na(cover_target) ~ if_else(
            is.na(comments) | comments == "",
            paste0("Cover set to ", cover_target, "% (from ", cover_original, "%) (turf map correction)"),
            paste0(comments, "; Cover set to ", cover_target, "% (from ", cover_original, "%) (turf map correction)")
          ),
          # Newly added species/year rows for increase_to_cover
          !is.na(cover_adjustment) & is_added_increase_row ~ if_else(
            is.na(comments) | comments == "",
            paste0("Species/year added and cover set to ", cover, "% (turf map correction)"),
            paste0(comments, "; Species/year added and cover set to ", cover, "% (turf map correction)")
          ),
          # Existing species with increase_to_cover
          !is.na(cover_adjustment) ~ if_else(
            is.na(comments) | comments == "",
            paste0("Cover adjusted by ", cover_adjustment, "% (from ", cover_original, "%) (turf map correction)"),
            paste0(comments, "; Cover adjusted by ", cover_adjustment, "% (from ", cover_original, "%) (turf map correction)")
          ),
          TRUE ~ comments
        )
      )
    
    # Track cases where cover goes below 0
    negative_cover_cases_to <- community_corrected |>
      filter((!is.na(cover_adjustment) | !is.na(cover_target)) & cover < 0) |>
      select(siteID, blockID, plotID, treatment, year, species, cover_original, cover_adjustment, cover_target, cover_new = cover) |>
      mutate(rule = "Rule 2 (to_species cover adjustment)")
    
    # Set cover to 0 if it goes below 0 (temporary - user will review)
    community_corrected <- community_corrected |>
      mutate(cover = if_else(cover < 0, 0, cover)) |>
      select(-cover_original, -cover_adjustment, -cover_target, -increase_to_cover)
  }
  
  # Rule 3: Decrease cover from from_species (only if decrease_from_cover is not NA)
  # year_expanded: Each year from the original year range gets its own row, so corrections apply to all years
  cover_adjustments_from <- turf_map_corrections_fixed |>
    filter(!is.na(from_species) & is.na(to_species)) |>
    filter(!is.na(decrease_from_cover)) |>  # Only if decrease_from_cover has a value
    # Exclude deletions: handle NA comments properly (str_detect returns NA for NA values)
    filter(!coalesce(str_detect(comment, regex("delete", ignore_case = TRUE)), FALSE)) |>
    select(siteID, blockID, plotID, treatment, year = year_expanded, from_species, decrease_from_cover) |>
    # Set explicit target cover (not a delta)
    mutate(cover_target = decrease_from_cover) |>
    select(siteID, blockID, plotID, treatment, year, species = from_species, cover_target) |>
    distinct()
  
  # Track cases where cover would go below 0
  negative_cover_cases_from <- NULL
  
  # Apply cover adjustments for from_species
  if (nrow(cover_adjustments_from) > 0) {
    community_corrected <- community_corrected |>
      left_join(cover_adjustments_from, by = c("siteID", "blockID", "plotID", "treatment", "year", "species")) |>
      mutate(
        cover_original = cover,
        cover = if_else(!is.na(cover_target), cover_target, cover),
        # Add comment tracking cover set action (preserve existing comments)
        comments = if_else(
          !is.na(cover_target),
          if_else(
            is.na(comments) | comments == "",
            paste0("Cover set to ", cover_target, "% (from ", cover_original, "%) (turf map correction)"),
            paste0(comments, "; Cover set to ", cover_target, "% (from ", cover_original, "%) (turf map correction)")
          ),
          comments
        )
      )
    
    # Track cases where cover goes below 0
    negative_cover_cases_from <- community_corrected |>
      filter(!is.na(cover_target) & cover < 0) |>
      select(siteID, blockID, plotID, treatment, year, species, cover_original, cover_target, cover_new = cover) |>
      mutate(rule = "Rule 3 (from_species cover set to target)")
    
    # Set cover to 0 if it goes below 0 (temporary - user will review)
    community_corrected <- community_corrected |>
      mutate(cover = if_else(cover < 0, 0, cover)) |>
      select(-cover_original, -cover_target)
  }
  
  # Combine and report negative cover cases
  negative_cover_cases <- bind_rows(
    negative_cover_cases_to,
    negative_cover_cases_from
  )

  # For rows added for increase_to_cover, rescue plot/year-level info
  # from other species in the same siteID/blockID/plotID/treatment/year
  plot_year_keys <- c("siteID", "blockID", "plotID", "treatment", "year")
  fill_cols <- c(
    "removal", "total_graminoids", "total_forbs", "total_bryophytes",
    "vegetation_height", "moss_height", "litter", "sumcover", "recorder",
    "turfID", "total_lichen", "total_rock", "total_soil", "weather"
  )
  
  # Build a template of non-added rows with plot/year-level information
  template_info <- community_corrected |>
    filter(!is_added_increase_row) |>
    group_by(across(all_of(plot_year_keys))) |>
    summarise(across(all_of(fill_cols), ~ dplyr::first(na.omit(.x)), .names = "{.col}"), .groups = "drop")
  
  # Join template back and fill missing values for added rows only
  if (nrow(template_info) > 0) {
    community_corrected <- community_corrected |>
      left_join(template_info, by = plot_year_keys, suffix = c("", "_tmpl")) |>
      mutate(across(
        all_of(fill_cols),
        ~ if_else(
          is_added_increase_row & (is.na(.x) | (.x == "")),
          coalesce(get(paste0(cur_column(), "_tmpl")), .x),
          .x
        )
      )) |>
      select(-ends_with("_tmpl"))
  }
  
  # Fill missing functional_group values from fun_gr lookup
  if (!is.null(fun_gr) && "functional_group" %in% names(community_corrected)) {
    community_corrected <- community_corrected |>
      left_join(fun_gr, by = "species", suffix = c("", "_lookup")) |>
      mutate(
        functional_group = if_else(
          is.na(functional_group) | functional_group == "",
          functional_group_lookup,
          functional_group
        )
      ) |>
      select(-functional_group_lookup)
  }
  
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

