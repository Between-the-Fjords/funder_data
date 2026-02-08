# Process manually downloaded NGCD files
# Use this if you downloaded files manually from the CDS website
# Run with: source("other_code/process_downloaded_ngcd.R") from project root

library(tidyverse)
library(terra)
library(ncdf4)
library(lubridate)

source("R/functions/clean_ngcd_climate.R")

cat("=== Processing Manually Downloaded NGCD Files ===\n\n")

# =============================================================================
# Check for downloaded files
# =============================================================================

cat("Checking for downloaded files...\n")

# Look for zip or .nc files in raw_data/NGCD/
if (!dir.exists("raw_data/NGCD")) {
  dir.create("raw_data/NGCD", recursive = TRUE)
  stop("Created raw_data/NGCD/ directory.\n",
       "Please download files manually and place them here.\n",
       "See: other_code/download_ngcd_manual_guide.md")
}

# Prefer zip files; if none, use .nc files (already unzipped)
nc_files <- list.files("raw_data/NGCD", pattern = "\\.zip$", full.names = TRUE)
if (length(nc_files) == 0) {
  nc_files <- list.files("raw_data/NGCD", pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
}
if (length(nc_files) == 0) {
  stop("No .zip or .nc files found in raw_data/NGCD/\n",
       "Please download from CDS or add unzipped .nc files.\n",
       "See: other_code/download_ngcd_manual_guide.md")
}

cat("Found", length(nc_files), "file(s)\n")
if (length(nc_files) <= 20) {
  for (f in nc_files) cat("  -", basename(f), "\n")
} else {
  for (f in nc_files[1:5]) cat("  -", basename(f), "\n")
  cat("  ... and", length(nc_files) - 5, "more\n")
}
cat("\n")

# =============================================================================
# Load site coordinates
# =============================================================================

coordinates <- read_csv("raw_data/coordinates.csv")

cat("Sites to extract:\n")
print(coordinates %>% select(siteID, latitude_N, longitude_E))
cat("\n")

# =============================================================================
# Extract data for sites
# =============================================================================

cat("Extracting data for your sites...\n")
cat("This may take 5-10 minutes...\n\n")

ngcd_extracted <- extract_ngcd_for_sites(
  nc_files = nc_files,
  coordinates = coordinates
)

cat("\n✓ Extraction complete!\n\n")

# =============================================================================
# Clean data
# =============================================================================

cat("Cleaning data...\n")

ngcd_clean <- clean_ngcd(ngcd_extracted)

cat("✓ Data cleaned!\n\n")

# =============================================================================
# Save results
# =============================================================================

output_dir <- "clean_data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "FUNDER_clean_NGCD_climate_2020_2024.csv")

write_csv(ngcd_clean, output_file)

cat("✓ Data saved to:", output_file, "\n\n")

# =============================================================================
# Summary
# =============================================================================

cat("=== Summary ===\n")

summary_stats <- ngcd_clean %>%
  group_by(siteID, variable) %>%
  summarise(
    n_obs = n(),
    min_value = min(value, na.rm = TRUE),
    mean_value = mean(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    first_date = min(date),
    last_date = max(date),
    .groups = "drop"
  )

print(summary_stats)

cat("\n✓✓✓ All done! ✓✓✓\n")
cat("\nYour climate data is ready to use.\n")
