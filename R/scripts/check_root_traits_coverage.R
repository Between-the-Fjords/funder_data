# Check which plots/sites are missing in root traits datasets
# Run from project root: source("R/scripts/check_root_traits_coverage.R")

library(tidyverse)
library(janitor)

# Full design
SITES <- c("Alr", "Arh", "Fau", "Gud", "Hog", "Lav", "Ovs", "Ram", "Skj", "Ulv", "Ves", "Vik")
BLOCKS <- 1:4
TREATMENTS <- c("C", "F", "G", "B", "FB", "GB", "GF", "FGB")

full_biomass_grid <- expand_grid(
  siteID = SITES,
  block = BLOCKS,
  treatment = TREATMENTS
) |>
  mutate(
    blockID = paste0(siteID, block),
    plotID = paste0(blockID, treatment)
  )

full_C_FGB_grid <- full_biomass_grid |>
  filter(treatment %in% c("C", "FGB"))

# ---------------------------------------------------------------------------
# 1. root_ric_biomass_clean: why 309 vs 384? Which plots are missing?
# ---------------------------------------------------------------------------
# Use distinct names to avoid conflicting with targets:: biomass_raw, biomass_clean (vegetation_plan)
ric_biomass_raw <- read_csv(here::here("raw_data/root_traits/BIOMASS_DATASHEET_ALL.csv"), show_col_types = FALSE)
ric_biomass_clean <- ric_biomass_raw |>
  select(site, blockID, plotID, treatment, `ric_length (m)`, `ric_volume (m3)`, `wet_root_biomass (g)`, `dry_root_biomass (g)`) |>
  janitor::clean_names() |>
  rename(plotID = plot_id, blockID = block_id, siteID = site) |>
  filter(!is.na(ric_length_m))

n_biomass <- nrow(ric_biomass_clean)
n_expected_biomass <- 12 * 4 * 8  # 384

ric_biomass_plotIDs <- ric_biomass_clean |> distinct(siteID, blockID, plotID, treatment)
missing_biomass <- full_biomass_grid |>
  anti_join(ric_biomass_plotIDs, by = c("siteID", "blockID", "plotID", "treatment"))

# Also check: in raw but dropped by NA filter?
in_raw_not_clean <- ric_biomass_raw |>
  janitor::clean_names() |>
  rename(plotID = plot_id, blockID = block_id, siteID = site) |>
  filter(is.na(ric_length_m)) |>
  distinct(siteID, blockID, plotID, treatment)

n_unique_biomass <- nrow(ric_biomass_plotIDs)
cat("--- 1. root_ric_biomass_clean (expected 384) ---\n")
cat("Rows (observations) after cleaning:", n_biomass, "\n")
cat("Unique plot-treatment combinations:", n_unique_biomass, "\n")
cat("Missing from full grid (384 - unique):", 384 - n_unique_biomass, "\n\n")
cat("Missing plotIDs (in full grid but not in cleaned data):\n")
print(missing_biomass |> select(siteID, blockID, treatment, plotID) |> arrange(siteID, blockID, treatment))
cat("\nPlots in raw data but dropped by filter (NA ric_length_m):\n")
print(in_raw_not_clean |> filter(!is.na(plotID)) |> select(siteID, blockID, treatment, plotID) |> arrange(siteID, blockID, treatment))

# ---------------------------------------------------------------------------
# 2. Double-removal scan file: which 3 sites are missing?
# ---------------------------------------------------------------------------
double_removal_raw <- read_delim(
  here::here("raw_data/root_traits/double-removal_treatments-SCAN_RESULTS_2024.txt"),
  delim = "\t",
  col_types = cols(.default = col_character())
) |>
  slice(-1:-4)  # drop header rows

# First column is plot ID (RHIZO 2022b, renamed in pipeline)
plot_col <- names(double_removal_raw)[1]
double_removal_plotIDs <- double_removal_raw |> pull(1) |> unique()
double_removal_sites <- double_removal_plotIDs |>
  str_sub(1, 3) |>
  str_to_title() |>
  unique() |>
  sort()

sites_in_double_removal <- tibble(site_abbrev = double_removal_sites)
missing_sites_double_removal <- tibble(site_abbrev = SITES) |>
  anti_join(sites_in_double_removal, by = "site_abbrev")

cat("\n--- 2. Double-removal scan (9 sites vs 12) ---\n")
cat("Sites present in double-removal file:", paste(double_removal_sites, collapse = ", "), "\n")
cat("Missing sites (not in double-removal file):", paste(missing_sites_double_removal$site_abbrev, collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# 3. C_FGB file: why 72 vs 96? Which plots are missing?
# ---------------------------------------------------------------------------
c_fgb_raw <- read_csv(here::here("raw_data/root_traits/FUNDER_clean_Roots_C_FGB_2023.csv"), show_col_types = FALSE)
c_fgb_plots <- c_fgb_raw |>
  distinct(plotID, treatment) |>
  filter(treatment %in% c("C", "FGB"))

n_c_fgb <- nrow(c_fgb_plots)
n_expected_c_fgb <- 12 * 4 * 2  # 96

missing_c_fgb <- full_C_FGB_grid |>
  anti_join(c_fgb_plots, by = c("plotID", "treatment"))

cat("\n--- 3. C_FGB file (expected 96) ---\n")
cat("Observations:", n_c_fgb, "\n")
cat("Missing plots:", n_expected_c_fgb - n_c_fgb, "\n\n")
cat("Missing plotIDs:\n")
print(missing_c_fgb |> select(siteID, blockID, treatment, plotID) |> arrange(siteID, blockID, treatment))
