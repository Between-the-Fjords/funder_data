clean_cflux <- function(output_dir = "raw_data", make_plots = FALSE) {

  metadata_dir <- file.path(output_dir, "C-flux site metadata")
  conc_dir <- file.path(output_dir, "CO2_H20_PAR_Squirrel")
  temp_dir <- file.path(output_dir, "Temperature_iButton")

  # Metadata ---------------------------------------------------------------
  cflux_funder_metadata <- fs::dir_ls(metadata_dir) |>
    map_dfr(
      read_excel,
      range = cell_cols("A:K"),
      na = "NA",
      col_types = c("text", rep("guess", 10))
    ) |>
    mutate(
      Starttime = as_hms(Starttime),
      Starttime = dmy_hms(paste(Date, Starttime)),
      Stoptime = as_hms(Stoptime),
      Stoptime = dmy_hms(paste(Date, Stoptime)),
      Date = dmy(Date)
    ) |>
    filter(
      !is.na(Starttime)
    ) |>
    select(!PAR)

  # Concentration data -----------------------------------------------------
  conc_files <- fs::dir_ls(conc_dir, regexp = ".#01")

  empty_conc_files <- conc_files[file.size(conc_files) == 0]
  if (length(empty_conc_files) > 0) {
    warning(
      "The following CO2/H2O/PAR files are empty: ",
      paste(basename(empty_conc_files), collapse = ", ")
    )
  }

  conc_og <- conc_files |>
    map_dfr(
      ~ read_csv(.x, na = c("#N/A")) |>
        mutate(source_file = basename(.x))
    ) |>
    transmute(
      source_file,
      datetime = dmy_hms(`Date/Time`),
      CO2 = `CO2 (ppm)`,
      H2O = `H2O (ppt)`,
      PAR = `PAR (micromol)`
    ) |>
    fill(H2O)

    #conc_og |> filter(is.na(CO2)) |> count(source_file)

  # Temperature data -------------------------------------------------------
  temp_df <- fs::dir_ls(temp_dir) |>
    map_dfr(
      read_delim,
      skip = 14,
      delim = ",",
      col_types = "ccc"
    ) |>
    mutate(
      datetime = dmy_hms(`Date/Time`),
      air_temp = as.numeric(str_replace_all(Value, ",", ".")),
      .keep = "none"
    )

  # Join concentration and temperature -------------------------------------
  conc_df <- conc_og |>
    left_join(
      temp_df,
      by = "datetime"
    )

  # Flux processing --------------------------------------------------------
  conc_funder <- flux_match(
    raw_conc = conc_df,
    field_record = cflux_funder_metadata,
    f_datetime = datetime,
    start_col = Starttime,
    end_col = Stoptime,
    time_diff = 0
  )

  # wet air correction
  conc_funder_dry <- flux_drygas(conc_funder, CO2, H2O)

  slopes_funder <- flux_fitting(
    conc_df = conc_funder_dry,
    f_conc = CO2_dry,
    f_datetime = datetime,
    f_start = f_start,
    f_end = f_end,
    f_fluxid = f_fluxid,
    fit_type = "exp_zhao18",
    start_cut = 0,
    end_cut = 0
  )

  flags_funder <- flux_quality(
    slopes_df = slopes_funder,
    f_conc = CO2,
    force_discard = c(
      # not zero, measurement interupted early but no note in metadata
      202,
      266,
      917
    ),
    force_lm = c(
      # linear flux, exp model artificially steep
      479
    ),
    ambient_conc = 421,
    error = 100,
    instr_error = 5
  )

  fluxes_funder <- flux_calc(
    slopes_df = flags_funder,
    slope_col = f_slope_corr,
    f_datetime = datetime,
    temp_air_col = air_temp,
    conc_unit = "ppm",
    flux_unit = "umol/m2/s",
    temp_air_unit = "celsius",
    setup_volume = 25,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = c("Site", "Block", "Treatment", "Chamber", "Cover", "Date"),
    cols_ave = c("PAR")
  ) |>
    mutate(
      type = str_replace_all(Cover, c("L" = "NEE", "D" = "Reco"))
    ) |>
    filter(
      type %in% c("NEE", "Reco")
    )

  # Create diagnostic plots (saved as PDF pages, like in the Qmd) ----------
  if (isTRUE(make_plots)) {
    flux_plot(
      flags_funder,
      f_conc = CO2,
      f_datetime = datetime,
      output = "pdfpages",
      f_ylim_lower = 350,
      f_plotname = "funder_fluxes"
    )
  }

  fluxes_funder |>
    arrange(f_fluxid) |>
    mutate(
      .by = c(Site, Block, Treatment, Chamber, Cover),
      replicate = row_number()
    ) |>
    flux_diff(
      type_col = type,
      id_cols = c("Site", "Block", "Treatment", "Chamber", "Date", "replicate"),
      cols_keep = c("PAR_ave", "datetime"),
      type_a = "NEE",
      type_b = "Reco",
      diff_name = "GPP"
    ) |> 
    tidylog::select(datetime, Site, Block, Treatment, replicate, type, f_flux, PAR_ave, chamber = Chamber)
}

