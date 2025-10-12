# clean bryophyte data

clean_bryophyte_structure <- function(bryophyte_raw, funder_meta){

  bryophyte <- bryophyte_raw |> 
    mutate(blockID = paste0(str_sub(siteID, 1, 3), blockID)) |>
    tidylog::left_join(funder_meta, by = c("siteID", "blockID", "treatment")) |>
    rename(date = `date (yyyy-mm-dd)`)

bryophyte |>
  rowwise() |>
  mutate(vegetation_height = mean(c_across(starts_with("veg_height_")), na.rm = TRUE),
         bryophyte_depth = mean(c_across(starts_with("bryo_height_")), na.rm = TRUE)) |>
  ungroup() |>
  select(date:blockID, plotID, treatment, graminoid:soil, vegetation_height, bryophyte_depth, comments) |>
  mutate(across(graminoid:bryophyte_depth, 
                ~if_else(. == "<1", "0.5", as.character(.)))) |>
  pivot_longer(cols = c(graminoid:bryophyte_depth), names_to = "functional_group", values_to = "value") |>
  mutate(value = as.numeric(value)) |>
  filter(!is.na(value)) |>
  mutate(variable = if_else(functional_group %in% c("vegetation_height", "bryophyte_depth"), "height", "cover")) |>
  select(date:treatment, variable, functional_group, value)

}

clean_bryophyte <- function(bryophyte_raw, bryophyte_dictionary, funder_meta){

    bryophyte <- bryophyte_raw |> 
    mutate(blockID = paste0(str_sub(siteID, 1, 3), blockID)) |>
    tidylog::left_join(funder_meta, by = c("siteID", "blockID", "treatment")) |>
    rename(date = `date (yyyy-mm-dd)`) |> 
    mutate(voucherID = str_replace_all(voucherID, c("Å" = "A", "Ø" = "O", "Æ" = "AE")))

    bryo_dic <- bryophyte_dictionary |>
      rename(siteID = SITE,
             voucherID = `voucher ID`) |>
      mutate(voucherID = str_replace_all(voucherID, c("Å" = "A", "Ø" = "O", "Æ" = "AE")),
             siteID = str_replace_all(siteID, c("Å" = "A", "Ø" = "O", "Æ" = "AE")),
             siteID = case_when(
               siteID == "ARH" ~ "Arhelleren",
               siteID == "FAU" ~ "Fauske",
               siteID == "GUD" ~ "Gudmedalen",
               siteID == "HOG" ~ "Hogsete",
               siteID == "LAV" ~ "Lavisdalen",
               siteID == "RAM" ~ "Rambera",
               siteID == "SKJ" ~ "Skjelingahaugen",
               siteID == "ULV" ~ "Ulvehaugen",
               siteID == "VES" ~ "Veskre",
               siteID == "VIK" ~ "Vikesland",
               siteID == "OVS" ~ "Ovstedalen",
               siteID == "ALR" ~ "Alrust",
               TRUE ~ siteID
             )) |>
      # First, split multiple species into separate rows
      separate_longer_delim(species_correction_Kristian_Hassel, delim = regex("[+&]")) |>
      mutate(species_correction_Kristian_Hassel = str_trim(species_correction_Kristian_Hassel)) |>
      mutate(
        # Extract check status - handle both "Confirmed" and "Confirmed -" patterns
        check = case_when(
          str_detect(species_correction_Kristian_Hassel, "^Confirmed") ~ "Confirmed",
          TRUE ~ NA_character_
        ),
        # Clean up the string by removing "Confirmed" and "Confirmed -" prefixes
        temp_species_string = str_remove(species_correction_Kristian_Hassel, "^Confirmed:?\\s*-?\\s*"),
        temp_species_string = str_trim(temp_species_string),
        # Only extract names if there's actual content after removing "Confirmed"
        norwegian_name = case_when(
          !is.na(temp_species_string) & temp_species_string != "" & str_detect(temp_species_string, "/") ~ 
            str_trim(str_extract(temp_species_string, "^[^/]+")),
          TRUE ~ NA_character_
        ),
        species = case_when(
          !is.na(temp_species_string) & temp_species_string != "" & str_detect(temp_species_string, "/") ~ 
            str_trim(str_extract(temp_species_string, "[^/]+$")),
          !is.na(temp_species_string) & temp_species_string != "" ~ temp_species_string,
          TRUE ~ NA_character_
        )
      ) |>
      select(-temp_species_string) |>
      select(siteID:comments_data_entering, check, norwegian_name, species)

      bryo_dic |> write_csv("bryo_dic.csv")

    ddd <- bryophyte |>
      select(date:blockID, plotID, treatment:weather, comments) |>
      pivot_longer(cols = c(`1`, `2`, `3`, `4`, `5`), 
                   names_to = "subplot", values_to = "subplot_coverage") |>
      tidylog::left_join(bryo_dic, by = c("siteID", "voucherID"))

# confirmed species
      ddd |>
      filter(species_correction_Kristian_Hassel.y == "Confirmed")


      ddd |>
      distinct(species, species_correction_Kristian_Hassel.x, vernacular, scientific, species_correction_Kristian_Hassel.y, `level of certainty`) |>
      filter(species_correction_Kristian_Hassel.y != "Confirmed", `level of certainty` == "1") |> write_csv("bryo_level_1.csv")
    

}