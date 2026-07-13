# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "tidylog",
    "janitor",
    "lubridate",
    "tibble",
    "dataDocumentation",
    "dataDownloader",
    "PFTCFunctions",
    "readxl",
    "writexl",
    "here",
    "terra",
    "ncdf4",
    "fs",
    "hms",
    "fluxible"
  ) # packages that your targets need to run
)
#"TNRS"
# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

#Combine target plans
combined_plan <- c(
  vegetation_plan,
  root_plan,
  mesofauna_plan,
  nutrient_cycling_plan,
  environmenet_plan,
  flux_plan,
  data_dic_plan,
  fungal_community_plan,
  plant_litter_decomposition_plan
)
