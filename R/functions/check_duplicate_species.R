# Check for duplicate species in community data
# Duplicates are defined as multiple rows with the same 
# siteID, blockID, plotID, treatment, year, and species

check_duplicate_species <- function(community_data) {
  # Key columns that should uniquely identify a species observation
  key_cols <- c("siteID", "blockID", "plotID", "treatment", "year", "species")
  
  # Check if all key columns exist
  missing_cols <- setdiff(key_cols, names(community_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Find duplicates
  duplicates <- community_data |>
    group_by(across(all_of(key_cols))) |>
    summarise(
      n_rows = n(),
      cover_values = paste(cover, collapse = ", "),
      .groups = "drop"
    ) |>
    filter(n_rows > 1)
  
  if (nrow(duplicates) > 0) {
    # Get full details of duplicate rows
    duplicate_details <- community_data |>
      semi_join(duplicates, by = key_cols) |>
      arrange(across(all_of(key_cols)), cover)
    
    return(list(
      has_duplicates = TRUE,
      n_duplicate_groups = nrow(duplicates),
      duplicate_summary = duplicates,
      duplicate_details = duplicate_details
    ))
  } else {
    return(list(
      has_duplicates = FALSE,
      n_duplicate_groups = 0,
      duplicate_summary = NULL,
      duplicate_details = NULL
    ))
  }
}
