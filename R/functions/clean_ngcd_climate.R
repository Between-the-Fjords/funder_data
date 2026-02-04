# Download and clean NGCD climate data
# Nordic Gridded Climate Dataset from Copernicus Climate Data Store

library(ecmwfr)
library(ncdf4)
library(terra)
library(tidyverse)
library(lubridate)


#' Download NGCD climate data from Copernicus CDS
#' 
#' @param years Vector of years to download
#' @param variables Vector of variables to download
#' @param output_path Path to save downloaded files
#' @return Path to downloaded file
download_ngcd <- function(years = 1961:2024,
                          variables = c("mean_temperature", 
                                       "minimum_temperature",
                                       "maximum_temperature", 
                                       "precipitation"),
                          output_path = "raw_data/NGCD") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  # Load credentials from .Renviron
  renviron_path <- file.path(getwd(), ".Renviron")
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
  }
  
  user_uid <- Sys.getenv("CDS_UID")
  user_key <- Sys.getenv("CDS_API_KEY")
  
  if (user_uid == "" || user_key == "") {
    stop("CDS API credentials not found in .Renviron.\n",
         "Run source('other_code/setup_ngcd_simple.R') first.")
  }
  
  # Set credentials for ecmwfr (bypass keychain issues)
  tryCatch({
    wf_set_key(
      user = user_uid,
      key = user_key,
      service = "cds"
    )
    cat("✓ Credentials loaded and configured\n")
  }, error = function(e) {
    # If keychain fails, set environment variables directly
    Sys.setenv(cds_user = user_uid)
    Sys.setenv(cds_key = user_key)
    cat("✓ Credentials loaded from .Renviron (keychain bypassed)\n")
  })
  
  # Download each variable separately (CDS limitation)
  downloaded_files <- list()
  
  for (var in variables) {
    cat("Downloading", var, "...\n")
    
    filename <- file.path(output_path, paste0("ngcd_", var, ".zip"))
    
    # Skip if already downloaded
    if (file.exists(filename)) {
      cat("  File already exists, skipping download\n")
      downloaded_files[[var]] <- filename
      next
    }
    
    request <- list(
      version = "25.09",
      variable = var,
      year = as.character(years),
      month = sprintf("%02d", 1:12),
      day = sprintf("%02d", 1:31),
      format = "zip",
      dataset_short_name = "insitu-gridded-observations-nordic",
      target = filename
    )
    
    tryCatch({
      # Try download with credentials from environment
      result <- wf_request(
        user = user_uid,
        request = request,
        transfer = TRUE,
        path = dirname(filename),
        verbose = TRUE,
        time_out = 3600
      )
      downloaded_files[[var]] <- filename
      cat("✓ Successfully downloaded", var, "\n")
    }, error = function(e) {
      warning("Failed to download ", var, ": ", e$message, "\n")
      cat("\nTrying alternative download method...\n")
      
      # Alternative: construct URL and download directly
      # This is a fallback if ecmwfr doesn't work
      warning("Please download manually from CDS website if automatic download fails.")
    })
  }
  
  return(downloaded_files)
}


#' Extract NGCD climate data for specific coordinates
#' 
#' @param nc_files Vector of paths to NetCDF files
#' @param coordinates Dataframe with columns: siteID, latitude, longitude
#' @return Dataframe with extracted climate data
extract_ngcd_for_sites <- function(nc_files, coordinates) {
  
  # Prepare coordinates
  coords_sf <- coordinates %>%
    # Keep all original columns, just standardize lon/lat names
    rename(
      longitude = longitude_E,
      latitude = latitude_N
    ) %>%
    mutate(
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude)
    )
  
  # ---------------------------------------------------------------------------
  # Expand any zip files into a list of individual NetCDF files
  # In NGCD, each NetCDF is typically ONE day (single-layer raster),
  # and the time is stored as metadata (e.g. 2023-01-01).
  # ---------------------------------------------------------------------------
  all_nc_files <- c()
  
  if (length(nc_files) == 0 || all(is.na(nc_files))) {
    stop("No NetCDF/zip files provided.")
  }
  
  for (f in nc_files) {
    if (is.na(f) || !file.exists(f)) {
      warning("File not found: ", f)
      next
    }
    
    if (tolower(tools::file_ext(f)) == "zip") {
      # Unzip into a per-file directory to avoid mixing contents
      unzip_dir <- file.path(
        dirname(f),
        tools::file_path_sans_ext(basename(f))
      )
      if (!dir.exists(unzip_dir)) {
        dir.create(unzip_dir, recursive = TRUE)
        unzip(f, exdir = unzip_dir)
      }
      nc_extracted <- list.files(
        unzip_dir,
        pattern = "\\.nc$",
        full.names = TRUE,
        recursive = TRUE
      )
      if (length(nc_extracted) == 0) {
        warning("No .nc files found inside ", f)
      } else {
        all_nc_files <- c(all_nc_files, nc_extracted)
      }
    } else if (grepl("\\.nc$", f, ignore.case = TRUE)) {
      all_nc_files <- c(all_nc_files, f)
    }
  }
  
  all_nc_files <- unique(all_nc_files)
  
  if (length(all_nc_files) == 0) {
    stop("No NetCDF files found after unzipping. Check your downloads.")
  }
  
  # ---------------------------------------------------------------------------
  # Process each NetCDF file (typically one day per file)
  # ---------------------------------------------------------------------------
  all_data <- list()
  
  for (nc_file in all_nc_files) {
    cat("Processing", basename(nc_file), "...\n")
    
    tryCatch({
      # Open with terra
      climate_rast <- rast(nc_file)
      
      # Skip if raster has no layers
      n_layers <- nlyr(climate_rast)
      if (n_layers == 0) {
        warning("No layers found in ", nc_file)
        next
      }
      
      # Get variable name from file (first layer name or NetCDF varname)
      var_name <- names(climate_rast)[1]
      
      # Coordinates as spatial vector
      coords_vect <- vect(
        coords_sf,
        geom = c("longitude", "latitude"),
        crs = "EPSG:4326"
      )
      
      # Extract values for each site (one column per layer)
      extracted <- terra::extract(climate_rast, coords_vect, ID = FALSE)
      
      if (is.null(extracted) || ncol(extracted) == 0) {
        warning("Extraction returned no data for ", nc_file)
        next
      }
      
      # -------------------------------------------------------------------
      # Get dates for each layer
      # -------------------------------------------------------------------
      dates <- NULL
      
      # Preferred: use terra::time if present
      time_vals <- tryCatch(terra::time(climate_rast), error = function(e) NULL)
      if (!is.null(time_vals) && length(time_vals) == n_layers) {
        dates <- as.Date(time_vals)
      } else {
        # Fallback: read time-like dimension with ncdf4
        nc <- nc_open(nc_file)
        on.exit(nc_close(nc), add = TRUE)
        
        dim_names <- names(nc$dim)
        time_idx <- which(grepl("time", dim_names, ignore.case = TRUE))[1]
        
        if (!is.na(time_idx)) {
          time_dim <- nc$dim[[time_idx]]
          time_var <- time_dim$vals
          time_units <- time_dim$units
          
          origin_date <- str_extract(time_units, "\\d{4}-\\d{2}-\\d{2}")
          if (!is.na(origin_date)) {
            dates <- as.Date(origin_date) + days(time_var)
          }
        }
        
        if (is.null(dates) || length(dates) != n_layers) {
          warning(
            "Could not reliably determine time axis for ", nc_file,
            "; creating a simple sequence of dates starting from 1961-01-01."
          )
          dates <- seq.Date(
            from = as.Date("1961-01-01"),
            by = "day",
            length.out = n_layers
          )
        }
      }
      
      # Ensure dates length matches number of layers/columns
      layer_names <- colnames(extracted)
      if (length(dates) != length(layer_names)) {
        warning(
          "Length mismatch between dates (", length(dates),
          ") and extracted layers (", length(layer_names), ") for ", nc_file
        )
        min_len <- min(length(dates), length(layer_names))
        dates <- dates[seq_len(min_len)]
        extracted <- extracted[, seq_len(min_len), drop = FALSE]
        layer_names <- layer_names[seq_len(min_len)]
      }
      
      # -------------------------------------------------------------------
      # Combine coordinates, values, and dates
      # -------------------------------------------------------------------
      extracted_tbl <- as_tibble(extracted)
      colnames(extracted_tbl) <- layer_names
      
      site_data <- bind_cols(coords_sf, extracted_tbl) %>%
        pivot_longer(
          cols = all_of(layer_names),
          names_to = "layer",
          values_to = "value"
        ) %>%
        mutate(
          layer_index = match(layer, layer_names),
          date = dates[layer_index],
          variable = var_name
        ) %>%
        # Drop helper columns, keep all original coordinate/meta columns
        select(-layer_index)
      
      all_data[[nc_file]] <- site_data
      
    }, error = function(e) {
      warning("Failed to process ", nc_file, ": ", e$message)
    })
  }
  
  # Combine all variables
  if (length(all_data) == 0) {
    stop("No data was extracted. Check that NetCDF files exist and are valid.")
  }
  
  climate_data <- bind_rows(all_data)
  
  return(climate_data)
}


#' Clean NGCD climate data
#' 
#' @param ngcd_raw Raw extracted NGCD data
#' @return Cleaned and formatted climate data
clean_ngcd <- function(ngcd_raw) {
  
  ngcd_clean <- ngcd_raw %>%
    # Rename and standardize variables
    mutate(
      variable = case_when(
        # NGCD variable codes:
        # TG = daily mean temperature
        # TN = daily minimum temperature
        # TX = daily maximum temperature
        # RR = daily precipitation
        variable %in% c("TG", "mean_temperature") ~ "mean_temperature",
        variable %in% c("TN", "min_temperature") ~ "min_temperature",
        variable %in% c("TX", "max_temperature") ~ "max_temperature",
        variable %in% c("RR", "precipitation") ~ "precipitation",
        TRUE ~ variable
      ),
      unit = case_when(
        variable %in% c("mean_temperature", "min_temperature", "max_temperature") ~ "degree_celsius",
        variable == "precipitation" ~ "mm",
        TRUE ~ NA_character_
      ),
      # Convert temperature from Kelvin to °C; precipitation (kg m-2) = mm, no conversion
      value = case_when(
        variable %in% c("mean_temperature", "min_temperature", "max_temperature") ~ value - 273.15,
        TRUE ~ value
      )
    ) %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Add year, month, day columns for easier filtering
    mutate(
      year = year(date),
      month = month(date),
      day = day(date),
      doy = yday(date)
    ) %>%
    # Bring key columns to the front but keep all extra metadata columns
    relocate(siteID, longitude, latitude, date, year, month, day, doy,
             variable, value, unit, .before = 1) %>%
    arrange(siteID, date, variable)
  
  return(ngcd_clean)
}


#' Complete workflow to download and process NGCD data
#' 
#' @param coordinates Dataframe with site coordinates
#' @param years Vector of years to download
#' @return Cleaned climate data
process_ngcd_climate <- function(coordinates, 
                                 years = 2020:2024,
                                 force_download = FALSE) {
  
  cat("=== Processing NGCD Climate Data ===\n\n")
  
  # Step 1: Download data (if needed)
  cat("Step 1: Downloading data...\n")
  if (force_download || !dir.exists("raw_data/NGCD")) {
    downloaded_files <- download_ngcd(years = years)
  } else {
    cat("Using existing downloaded files\n")
    downloaded_files <- list.files("raw_data/NGCD", 
                                   pattern = "\\.zip$", 
                                   full.names = TRUE)
  }
  
  # Step 2: Extract for sites
  cat("\nStep 2: Extracting data for sites...\n")
  ngcd_raw <- extract_ngcd_for_sites(
    nc_files = downloaded_files,
    coordinates = coordinates
  )
  
  # Step 3: Clean data
  cat("\nStep 3: Cleaning data...\n")
  ngcd_clean <- clean_ngcd(ngcd_raw)
  
  cat("\n=== Processing complete ===\n")
  cat("Sites:", unique(ngcd_clean$siteID), "\n")
  cat("Date range:", min(ngcd_clean$date), "to", max(ngcd_clean$date), "\n")
  cat("Variables:", unique(ngcd_clean$variable), "\n")
  cat("Total observations:", nrow(ngcd_clean), "\n")
  
  return(ngcd_clean)
}
