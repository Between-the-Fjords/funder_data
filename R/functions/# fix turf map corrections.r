# Helper function to parse year strings into a list of years
parse_years <- function(year_str, comment_str = NULL) {
  # Convert to character if numeric
  if (is.numeric(year_str)) {
    year_str <- as.character(year_str)
  }
  
  # If year is empty/NA, try to extract from comment
  if (is.na(year_str) || year_str == "" || str_trim(year_str) == "") {
    if (!is.null(comment_str) && !is.na(comment_str) && comment_str != "") {
      # Extract 4-digit years from comment
      years_in_comment <- str_extract_all(comment_str, "\\b(19|20)\\d{2}\\b")[[1]]
      if (length(years_in_comment) > 0) {
        year_str <- paste(years_in_comment, collapse = ", ")
      } else {
        return(list(years = integer(0), years_str = NA_character_))
      }
    } else {
      return(list(years = integer(0), years_str = NA_character_))
    }
  }
  
  # Normalize the string
  year_str <- str_to_lower(str_trim(year_str))
  
  # Handle "all", "all years", "in general" - return all years from 2015-2022
  if (year_str %in% c("all", "all years", "in general", "general")) {
    years <- 2015:2022
    years_str <- paste(years, collapse = ", ")
    return(list(years = years, years_str = years_str))
  }
  
  # Remove common words like "og" (Norwegian for "and")
  year_str <- str_replace_all(year_str, "\\bog\\b", ",")
  
  # Handle ranges like "2015-2022" or "2015-22"
  # First try 4-digit to 4-digit range
  if (str_detect(year_str, "\\d{4}-\\d{4}")) {
    range_match <- str_extract(year_str, "(\\d{4})-(\\d{4})")
    if (!is.na(range_match)) {
      parts <- str_split(range_match, "-")[[1]]
      start_year <- as.integer(parts[1])
      end_year <- as.integer(parts[2])
      years <- seq(start_year, end_year)
      years_str <- paste(years, collapse = ", ")
      return(list(years = years, years_str = years_str))
    }
  }
  # Then try 4-digit to 2-digit range (e.g., "2015-22" → 2015-2022)
  if (str_detect(year_str, "\\d{4}-\\d{2}")) {
    range_match <- str_extract(year_str, "(\\d{4})-(\\d{2})")
    if (!is.na(range_match)) {
      parts <- str_split(range_match, "-")[[1]]
      start_year <- as.integer(parts[1])
      end_year_short <- as.integer(parts[2])
      # Convert 2-digit year to 4-digit (assume 20xx if < 50, else 19xx)
      if (end_year_short < 50) {
        end_year <- 2000 + end_year_short
      } else {
        end_year <- 1900 + end_year_short
      }
      years <- seq(start_year, end_year)
      years_str <- paste(years, collapse = ", ")
      return(list(years = years, years_str = years_str))
    }
  }
  
  # Split by comma, semicolon, or period
  year_parts <- str_split(year_str, "[,;.]")[[1]] |>
    str_trim() |>
    str_remove_all("\\s+")
  
  # Parse each part
  years <- integer(0)
  for (part in year_parts) {
    if (part == "" || is.na(part)) next
    
    # Handle 2-digit years (assume 20xx if < 50, else 19xx)
    if (str_detect(part, "^\\d{2}$")) {
      year_val <- as.integer(part)
      if (year_val < 50) {
        year_val <- 2000 + year_val
      } else {
        year_val <- 1900 + year_val
      }
      years <- c(years, year_val)
    }
    # Handle 4-digit years
    else if (str_detect(part, "^\\d{4}$")) {
      years <- c(years, as.integer(part))
    }
    # Handle abbreviated years like "2015.17" (2015 and 2017)
    else if (str_detect(part, "^\\d{4}\\.\\d{2}$")) {
      parts <- str_split(part, "\\.")[[1]]
      base_year <- as.integer(parts[1])
      short_year <- as.integer(parts[2])
      if (short_year < 50) {
        short_year <- 2000 + short_year
      } else {
        short_year <- 1900 + short_year
      }
      years <- c(years, base_year, short_year)
    }
  }
  
  # Remove duplicates and sort
  years <- sort(unique(years))
  
  # Create standardized string format
  if (length(years) > 0) {
    years_str <- paste(years, collapse = ", ")
  } else {
    years_str <- NA_character_
  }
  
  return(list(years = years, years_str = years_str))
}

# fix turf map corrections
fix_turf_map_corrections <- function(turf_map_corrections, funder_meta = NULL) {

  # Fix column names in turf_map_corrections
  turf_map_corrections_fixed <- turf_map_corrections |>
    slice(-1) |>
    clean_names() |>
    rename(
      siteID = site,
      plotID = plot
    ) |>
    # Standardize siteID to match community_clean format (full names, no special characters)
    mutate(
      # First normalize case and handle special characters
      siteID = str_to_title(siteID),
      siteID = str_replace_all(siteID, c("Å" = "A", "Ø" = "O", "Æ" = "AE", "å" = "a", "ø" = "o", "æ" = "ae")),
      # Then map to standard full names (handles both abbreviations and full names)
      siteID = recode(
        siteID,
        # 3-letter abbreviations
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
      ),
      # Get 3-letter site abbreviation for constructing plotID
      site_abbrev = str_to_title(str_sub(siteID, 1, 3)),
      # Parse plotID to extract block number and treatment
      # Handle various formats: "Gud1C", "gud1c", "1C", "Gudmedalen1C", etc.
      plotID_clean = str_to_title(plotID),  # Normalize case first
      # Remove site name if present (full name or abbreviation)
      plotID_clean = str_remove_all(plotID_clean, paste0("^", site_abbrev, "|^", siteID)),
      # Extract block number (first digit sequence)
      block_num = str_extract(plotID_clean, "^[0-9]+"),
      # Extract treatment (letters at the end, after block number)
      treatment_raw = str_extract(plotID_clean, "[A-Za-z]+$"),
      # Standardize treatment to uppercase
      treatment = str_to_upper(treatment_raw),
      # Create blockID: site abbreviation + block number
      blockID = paste0(site_abbrev, block_num),
      # Reconstruct plotID correctly: blockID + treatment
      plotID = paste0(blockID, treatment)
    ) |>
    # Remove temporary columns
    select(-site_abbrev, -plotID_clean, -block_num, -treatment_raw) |>
    # Parse and standardize year column
    # Note: Assumes clean_names() created "year" and "comment" columns
    # Adjust column names if they differ
    mutate(year = if_else(year == "2028", "2018", year),
    year = if_else(year == "2019.2021999999999", "2019, 2022", year)) |>
    mutate(
      # Keep original year column for comparison
      year_original = year,
      # Convert comment to character, handling NAs
      comment_char = as.character(ifelse(is.na(comment), "", comment)),
      # Parse years for each row - pass original year value (can be numeric or character)
      # parse_years will handle the conversion
      year_parsed = map2(year, comment_char, ~ parse_years(year_str = .x, comment_str = .y)),
      # Extract years list and standardized string
      years_list = map(year_parsed, ~ .x$years),
      year_standardized = map_chr(year_parsed, ~ {
        if(!is.na(.x$years_str) && .x$years_str != "") .x$years_str else NA_character_
      })
    ) |>
    select(-year_parsed, -year) |>
    # Remove original year column (we have year_original for comparison)
    # Keep comment column (only removed comment_char which was temporary)
    rename(year = year_standardized) |>
    # Expand rows with multiple years (one row per year)
    # This makes it easier to join with community_clean later by year
    unnest(years_list, keep_empty = TRUE) |>
    rename(year_expanded = years_list) |>
    # Keep both the standardized year string and the expanded year for flexibility
    mutate(
      year_expanded = as.integer(year_expanded)
    ) |>
    # Remove rows where year_expanded is NA (these are "all" or "in general" cases that couldn't be parsed)
    filter(!is.na(year_expanded)) |>
    # Parse from_species and to_species to extract cover percentages
    mutate(
      # Keep original columns for verification
      from_species_original = from_species,
      to_species_original = to_species,
      # Parse from_species: extract "reduced to X%" → decrease_from_cover
      # Allow case-insensitive matching and whitespace between number and %
      decrease_from_cover = if_else(
        str_detect(from_species, regex("reduced\\s+to\\s+\\d+\\s*%", ignore_case = TRUE)),
        as.numeric(str_extract(from_species, "\\d+(?=\\s*%)")),
        NA_real_
      ),
      # Clean from_species: remove "reduced to X%" pattern (case-insensitive, allow whitespace)
      from_species = str_remove_all(from_species, regex("reduced\\s+to\\s+\\d+\\s*%", ignore_case = TRUE)) |> str_trim(),
      
      # Parse to_species: extract "decrease X%" or "reduced to X%" → decrease_to_cover (check this first)
      # Allow whitespace between number and %
      decrease_to_cover = if_else(
        str_detect(to_species, regex("(?:decrease|reduced\\s+to)\\s+\\d+\\s*%", ignore_case = TRUE)),
        as.numeric(str_extract(to_species, "\\d+(?=\\s*%)")),
        NA_real_
      ),
      # Parse to_species: extract "+ X%" or "X%" → increase_to_cover (only if not a decrease or reduced)
      # Allow whitespace between number and %
      increase_to_cover = if_else(
        str_detect(to_species, regex("(?:\\+\\s*)?\\d+\\s*%", ignore_case = FALSE)) & 
        !str_detect(to_species, regex("(?:decrease|reduced)", ignore_case = TRUE)),
        as.numeric(str_extract(to_species, "\\d+(?=\\s*%)")),
        NA_real_
      ),
      # Clean to_species: remove "+ X%", "X%", "decrease X%", and "reduced to X%" patterns (allow whitespace)
      to_species = str_remove_all(to_species, regex("(?:\\+\\s*)?\\d+\\s*%|(?:decrease|reduced\\s+to)\\s+\\d+\\s*%", ignore_case = TRUE)) |> str_trim()
    ) |>
    # Remove rows where all information columns are NA
    filter(!(is.na(from_species_original) & is.na(to_species_original) & is.na(comment) & is.na(general_comment))) |>
    # Fix typos and standardize species names in from_species
    mutate(
      from_species = str_trim(from_species),
      # Fix specific typos first
      from_species = str_replace_all(from_species, "ave flex", "ave fle"),
      from_species = str_replace_all(from_species, "nar stri", "nar str"),
      from_species = str_replace_all(from_species, "tarax", "tar sp"),
      from_species = str_replace_all(from_species, "des cesp", "des ces"),
      from_species = str_replace_all(from_species, "car atr og atro", "car atr og car atro"),
      # Fix specific issues
      from_species = str_replace_all(from_species, "gym  dry", "gym dry"),  # Fix double space
      from_species = str_replace_all(from_species, "hypsp", "hyp sp"),  # Add space
      # Handle multiple species separated by "og" (Norwegian for "and")
      # Replace "og" with comma to separate species
      from_species = str_replace_all(from_species, "\\s+og\\s+", ", "),
      # Handle cases where species are listed without "og" (e.g., "vio pal vio bif")
      from_species = str_replace_all(from_species, "vio pal vio bif", "vio pal, vio bif"),
      # Normalize multiple spaces to single space
      from_species = str_replace_all(from_species, "\\s+", " "),
      # Format species names: split by comma, format each, then rejoin
      from_species = map_chr(from_species, function(s) {
        if (is.na(s) || s == "") return(s)
        # Split by comma
        species_list <- str_split(s, ",")[[1]] |> str_trim()
        # Format each species: "ach mil" → "Ach.mil"
        # Only first letter of entire species name is uppercase, rest lowercase
        formatted <- map_chr(species_list, function(sp) {
          if (is.na(sp) || sp == "") return(sp)
          # Convert to lowercase first
          sp_lower <- str_to_lower(sp)
          # Split by space
          parts <- str_split(sp_lower, "\\s+")[[1]] |> str_trim()
          # Capitalize only first letter of first part
          if (length(parts) > 0 && nchar(parts[1]) > 0) {
            parts[1] <- paste0(str_to_upper(str_sub(parts[1], 1, 1)), str_sub(parts[1], 2))
          }
          # Join with dot
          paste(parts, collapse = ".")
        })
        # Rejoin with comma and space
        paste(formatted, collapse = ", ")
      })
    ) |>
    # Fix typos and standardize species names in to_species
    mutate(
      to_species = str_trim(to_species),
      # Handle "delete" - preserve "delete" in comment if from_species is not NA
      # If from_species is not NA and "delete" is in comment, keep both (don't modify comment)
      # If to_species has "delete": move it to comment, set to_species to NA
      # If to_species doesn't have "delete": leave comment unchanged (can be NA, empty, or have content)
      comment = if_else(
        # If from_species is not NA and comment already has "delete", preserve it
        !is.na(from_species) & str_detect(comment, regex("delete", ignore_case = TRUE)),
        comment,  # Keep original comment with "delete"
        # Otherwise, handle "delete" in to_species
        if_else(
          str_detect(to_species, regex("delete", ignore_case = TRUE)),
          if_else(
            is.na(comment) | comment == "",
            "delete",
            paste0(comment, "; delete")
          ),
          comment  # Keep original comment if no "delete" in to_species
        )
      ),
      # Remove "delete" from to_species
      to_species = str_remove_all(to_species, regex("delete", ignore_case = TRUE)) |> str_trim(),
      # Set to NA if empty after removing delete (so "delete" becomes NA)
      to_species = if_else(to_species == "", NA_character_, to_species),
      # Handle multiple species separated by "og" (Norwegian for "and")
      to_species = str_replace_all(to_species, "\\s+og\\s+", ", "),
      # Fix specific typos
      to_species = str_replace_all(to_species, "fes rubr", "fes rub"),
      to_species = str_replace_all(to_species, "agr ca", "agr cap"),
      to_species = str_replace_all(to_species, "agr capp", "agr cap"),  # Fix capp typo
      to_species = str_replace_all(to_species, "phle alp", "phl alp"),
      to_species = str_replace_all(to_species, "hievul", "hie vul"),
      to_species = str_replace_all(to_species, "pimp sax", "pim sax"),
      to_species = str_replace_all(to_species, "knau arv", "kna arv"),
      to_species = str_replace_all(to_species, "ave flex", "ave fle"),
      to_species = str_replace_all(to_species, "nar stri", "nar str"),
      to_species = str_replace_all(to_species, "rub idea", "rub ida"),
      to_species = str_replace_all(to_species, "ver chae", "rub cha"),
      to_species = str_replace_all(to_species, "valeriana", "val sam"),
      to_species = str_replace_all(to_species, "rubus idae", "rub ida"),
      to_species = str_replace_all(to_species, regex("agr cap increased to", ignore_case = TRUE), "agr cap"),
      # Normalize multiple spaces to single space
      to_species = str_replace_all(to_species, "\\s+", " "),
      # Format species names: split by comma, format each, then rejoin
      to_species = map_chr(to_species, function(s) {
        if (is.na(s) || s == "") return(NA_character_)
        # Split by comma
        species_list <- str_split(s, ",")[[1]] |> str_trim()
        # Format each species: "agr cap" → "Agr.cap"
        # Only first letter of entire species name is uppercase, rest lowercase
        formatted <- map_chr(species_list, function(sp) {
          if (is.na(sp) || sp == "") return(sp)
          # Convert to lowercase first
          sp_lower <- str_to_lower(sp)
          # Split by space
          parts <- str_split(sp_lower, "\\s+")[[1]] |> str_trim()
          # Capitalize only first letter of first part
          if (length(parts) > 0 && nchar(parts[1]) > 0) {
            parts[1] <- paste0(str_to_upper(str_sub(parts[1], 1, 1)), str_sub(parts[1], 2))
          }
          # Join with dot
          paste(parts, collapse = ".")
        })
        # Rejoin with comma and space
        paste(formatted, collapse = ", ")
      })
    ) |>
    select(siteID, blockID, plotID, treatment, year_original, year, year_expanded, from_species_original, from_species, to_species_original, to_species, decrease_from_cover, increase_to_cover, decrease_to_cover, comment, general_comment)
  


  # Validate against funder_meta (if provided)
  # Check if plotID, blockID, treatment combinations exist in metadata
#   if (!is.null(funder_meta)) {
#     # Create a validation key from funder_meta
#     meta_key <- funder_meta |>
#       select(siteID, blockID, plotID, treatment) |>
#       distinct()
    
#     # Find rows that don't match metadata
#     mismatches <- turf_map_corrections_fixed |>
#       anti_join(meta_key, by = c("siteID", "blockID", "plotID", "treatment"))
    
#     if (nrow(mismatches) > 0) {
#       warning(
#         "Found ", nrow(mismatches), 
#         " plotID/blockID/treatment combinations that don't match funder_meta. ",
#         "These rows will be removed.\n",
#         "Mismatched rows:\n",
#         paste(capture.output(print(mismatches |> select(siteID, blockID, plotID, treatment))), collapse = "\n")
#       )
#     }
    
#     # Keep only rows that match metadata
#     turf_map_corrections_fixed <- turf_map_corrections_fixed |>
#       semi_join(meta_key, by = c("siteID", "blockID", "plotID", "treatment"))
#   }
    
}