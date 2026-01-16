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
    select(siteID, blockID, plotID, treatment, year_original, year, year_expanded, from_species, to_species, comment, general_comment, comment_char)
  


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