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
    rename(date = `date (yyyy-mm-dd)`)

    bryophyte |>
    select(date:blockID, plotID, treatment:weather, comments) |>
    

}