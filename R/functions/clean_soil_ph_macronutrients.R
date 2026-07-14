

clean_ph_and_macronutrients <- function(bioner, uib) {

  bioner_clean <- bioner |>
    select(1, 8:11, 14) |>
    janitor::clean_names() |>
    rename(
      plotID = prove_id,
      Mg = mg_al_mg_100g_jord,
      P = p_al_mg_100g_jord,
      K = k_al_mg_100g_jord,
      Ca = ca_al_mg_100g_jord,
      pH = p_h
    ) |>
    filter(!is.na(Mg))

  ph_complete <- bioner_clean |>
    select(plotID, pH) |>
    filter(!is.na(pH)) |>
    bind_rows(uib) |>
    arrange(plotID)

  clean <- bioner_clean |>
    left_join(ph_complete) |>
  mutate(
    siteID = substr(plotID, 1, 3),
    siteID = recode_values(
      siteID,
      "Gud" ~ "Gudmedalen",
      "Lav" ~ "Lavisdalen",
      "Ram" ~ "Rambera",
      "Ulv" ~ "Ulvehaugen",
      "Skj" ~ "Skjelingahaugen",
      "Alr" ~ "Alrust",
      "Arh" ~ "Arhelleren",
      "Fau" ~ "Fauske",
      "Hog" ~ "Hogsete",
      "Ovs" ~ "Ovstedalen",
      "Vik" ~ "Vikesland",
      "Ves" ~ "Veskre"
    ),
    blockID = substr(plotID, 1, 4),
    treatment = substr(plotID, 5, 7),
    year = "2022"
  ) |>
    select(year, siteID, blockID, treatment, plotID, pH, Mg, P, K, Ca) |>
    pivot_longer(
      cols = c(pH, Mg, P, K, Ca),
      names_to = "name",
      values_to = "value"
    ) |>
    mutate(
      value = round(value, digits = 2),
      unit = recode_values(
        name,
        "pH" ~ "pH",
        "Mg" ~ "mg_per_100_g_soil",
        "P" ~ "mg_per_100_g_soil",
        "K" ~ "mg_per_100_g_soil",
        "Ca" ~ "mg_per_100_g_soil"
      )) |>
    funcabization(convert_to = "FunCaB")

  return(clean)

}

