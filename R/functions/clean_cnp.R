# Clean CNP data


clean_cnp <- function(cnp_depth_raw, cnp_ram_depth_raw, funder_meta){

  # sampling for Rambera was done differently and blockIDs are different Ram1_2
  # has no treatment!
  # date is missing!!!
  cnp_ram_depth <- cnp_ram_depth_raw |>
    mutate(site = factor(site),
           site = str_to_title(tolower(site))) |>
    mutate(site = recode(site, 'Ram' = "Rambera"),
           block = paste0(substr(site, 1, 3), block),
           unit = "cm") |>
    rename(siteID = site, blockID = block, depth = depth_cm) |>
    mutate(blockID = if_else(blockID == "Ram1_2", "Ram1", blockID))


  # Fix column names and variables in accordance with FUNDER naming convention
  cnp_depth_raw |>
    mutate(blockID = paste(siteID, block, sep = "")) |>
    # Replace "," with "." as decimal deliminator and make numeric.
    # some NAs introduced but ok, because "na" from before
    mutate(depth_cm = as.numeric(gsub(",", ".", depth_cm))) |>
    # Sample Fau2GF has one recorded depth of 66 cm clearly a typo from data entry
    # Checked field noter and value should be 6 cm
    # Manually fix typo:
    mutate(depth_cm = if_else(depth_cm == 66, 6, depth_cm),
           unit = "cm") |>
    # Remove rows where depth is NA
    drop_na(depth_cm) |>
    select(-siteID) |>
    left_join(funder_meta) |>
    select(date, siteID, blockID, plotID, treatment, core_nr = core_no, depth = depth_cm, unit, comment) |>
    bind_rows(cnp_ram_depth)

}

