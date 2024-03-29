# clean microclimate data

clean_climate <- function(climate_ID_raw){

  # Metadata for TOMST climate data
  climate_ID <- climate_ID_raw %>%
    rename(
      loggerID = `TOMST number`,
      Date_in = `Date in`,
      Date_out = `Date out`,
      siteID = SiteID,
      blockID = BlockID,
      treatment = TreatmentID
    ) %>%
    mutate(Date_out = dmy(Date_out)) %>%
    mutate(Date_in = dmy(Date_in)) %>%

    #Fixing name treatment at Hogsete
    mutate(
      treatment = case_when(
        treatment == "B?" ~ "C",
        treatment == "F?" ~ "FGB",
        treatment == "FG" ~ "GF",
        loggerID == "95221003" ~ "GB",
        TRUE ~ treatment
      )
    ) %>%
    # code to clean site names
    mutate(
      siteID = recode(
        siteID,
        # old name (replace) = valid name (do not change)
        'GUD' = "Gudmedalen",
        'LAV' = "Lavisdalen",
        'RAM' = "Rambera",
        'ULV' = "Ulvehaugen",
        'SKJ' = "Skjelingahaugen",
        'ALR' = "Alrust",
        'ARH' = "Arhelleren",
        'FAU' = "Fauske",
        'HOG' = "Hogsete",
        'OVS' = "Ovstedalen",
        'VIK' = "Vikesland",
        'VES' = "Veskre"
      ),
      blockID = paste0(substr(siteID, 1, 3), blockID),
      plotID = paste0(blockID, treatment)
    ) |>
    relocate(plotID, .before = treatment)

  #List of all the files
  files <- dir(
    path = "raw_data/FUNDER_raw_climate_TOMST",
    pattern = "^data.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )


  #Import data
  temp_raw <- map_df(set_names(files), function(file) {
    column_names <-
      c(
        "ID",
        "date_time",
        "time_zone",
        "soil_temperature",
        "ground_temperature",
        "air_temperature",
        "raw_soilmoisture",
        "shake",
        "error_flag"
      )
    file %>%
      set_names() %>%
      map_dfr( ~ read_csv2(file = file, col_names = column_names))
  }, .id = "file")

  temp <- temp_raw %>%
    # rename column names
    mutate(date_time = ymd_hm(date_time)) %>%
    mutate(loggerID = as.numeric(substr(file, nchar(file) - 24, nchar(file) -
                                          17))) %>%
    left_join(climate_ID, by = "loggerID") %>%
    # Remove data before initial date time
    filter(date_time < Date_out, date_time > Date_in + 1) %>%
    # Soil moisture correction using function
    mutate(
      soilmoisture = soil.moist(
        rawsoilmoist = raw_soilmoisture,
        soil_temp = soil_temperature,
        soilclass = "loamy_sand_A"
      )
    )


  #soil moisture: Skj4FB varies a lot -> sent an email to tomst@tomst.com, nothing anormal with the logger (min 0.1), Vik4C and Vik4FB have been moved for several days because of the goats?, Gud min 0.1, Lav min 0.1, Ram min 0.1,  Ulv min 0.1 max 0.45, Alr min 0 and max 0.4, Arh min 0.1, Fau min 0 max 0.4, Hog min 0.1, Ovs min 0.1, Vik min 0 and cut dates, Ves min 0.3

  #Soil temperature Ves between 5 and 25, Alr max 30, lav max 20, Ulv4FGB very exposed (too many rocks), Ulv4GB max 20, Skj max 25, Alr 4FB more higher than the others(exposed ?)

  #Air temperature:Vik4C cut dates
  #Ground temperature: Vik4C cut dates


  # Soil moisture
  microclimate <- temp %>%
    mutate(soilmoisture = case_when(siteID %in% c("Lavisdalen", "Gudmedalen", "Rambera", "Ulvehaugen", "Arhelleren",
        "Hogsete", "Ovstedalen") & soilmoisture < 0.1 ~ NA_real_,
        siteID %in% c("Alrust", "Fauske", "Vikesland", "Skjelingahaugen") & soilmoisture < 0 ~ NA_real_,
      siteID == "Veskre" & soilmoisture < 0.3 ~ NA_real_,
      siteID == "Ulvehaugen" & soilmoisture > 0.45 ~ NA_real_,
      siteID %in% c("Alrust", "Fauske") &
        soilmoisture > 0.4 ~ NA_real_,
      plotID == "Vik4C" &
        date_time > "2022-06-24 00:15:00" &
        date_time < "2022-06-30 00:15:00" ~ NA_real_,
      plotID == "Vik4FB" &
        date_time > "2022-07-03 00:15:00" &
        date_time < "2022-07-11 00:15:00" ~ NA_real_,
      siteID == "Fauske" &
        date_time > "2022-06-15 00:15:00" &
        date_time < "2022-07-22 00:15:00" & soilmoisture < 0.075 ~ NA_real_,
      plotID %in% c("Alr4FGB", "Alr4GB") &
        date_time > "2022-06-15 00:15:00" &
        date_time < "2022-07-01 00:15:00" & soilmoisture < 0.2 ~ NA_real_,
      plotID == "Ulv4FGB" &
        date_time > "2022-07-13 00:15:00" &
        date_time < "2022-07-16 00:15:00" & soilmoisture < 0.2 ~ NA_real_,
      plotID %in% c("Arh4FB", "Arh4GB") & soilmoisture < 0.175 ~ NA_real_,
      plotID == "Gud4GB" & soilmoisture < 0.25 ~ NA_real_,
      TRUE ~ soilmoisture
    )
  )


  #Soil temperature
  microclimate <- microclimate %>% mutate(
      soil_temperature = case_when(
        plotID == "Vik4C" &
          date_time > "2022-06-24 00:15:00" &
          date_time < "2022-06-30 00:15:00" ~ NA_real_,
        plotID == "Vik4FB" &
          date_time > "2022-07-03 00:15:00" &
          date_time < "2022-07-11 00:15:00" ~ NA_real_,
        siteID == "Vikesland" & date_time > "2022-08-22 00:15:00" ~ NA_real_,
        siteID == "Veskre" & soil_temperature > 25 ~ NA_real_,
        siteID == "Veskre" & soil_temperature < 5 ~ NA_real_,
        siteID == "Alrust" & soil_temperature > 30 ~ NA_real_,
        siteID == "Lavisdalen" & soil_temperature > 20 ~ NA_real_,
        plotID == "Ulv4GB" & soil_temperature > 20 ~ NA_real_,
        siteID == "Skjelingahaugen" & soil_temperature > 25 ~ NA_real_,
        siteID == "Hogsete" & soil_temperature > 27 ~ NA_real_,
        siteID == "Vikesland" & soil_temperature > 26 ~ NA_real_,
        TRUE ~ soil_temperature
      )
    )


  #Air temperature
  microclimate <- microclimate %>% mutate(
    air_temperature = case_when(
      plotID == "Vik4C" &
        date_time > "2022-06-24 00:15:00" &
        date_time < "2022-06-30 00:15:00" ~ NA_real_,
      plotID == "Vik4FB" &
        date_time > "2022-07-03 00:15:00" &
        date_time < "2022-07-11 00:15:00" ~ NA_real_,
      TRUE ~ air_temperature
    )
  )

  #Ground temperature
  microclimate <- microclimate %>% mutate(
    ground_temperature = case_when(
      plotID == "Vik4C" &
        date_time > "2022-06-24 00:15:00" &
        date_time < "2022-06-30 00:15:00" ~ NA_real_,
      plotID == "Vik4FB" &
        date_time > "2022-07-03 00:15:00" &
        date_time < "2022-07-11 00:15:00" ~ NA_real_,
      TRUE ~ ground_temperature
    )
  )



  #Comments column
  microclimate <- microclimate %>%
    mutate(comments = case_when (
      plotID %in% c ("Ulv4FGB", "Alr4FB") ~ "very high variations of soil temperature maybe due to exposed logger to air",
      plotID %in% c ("Vik4C", "Vik4FB") ~ "loggers out of the soil during few days",
      plotID %in% c ("Skj4FB", "Skj4GB") ~ "FB and GB could have been mixed up, be careful with soil moisture",
      TRUE ~ NA_character_
    )
  )

  #Reordering columns
  microclimate <- microclimate %>%
    pivot_longer(cols = c(soil_temperature:air_temperature, soilmoisture), names_to = "variable", values_to = "value") |>
    # remove NAs in the data
    filter(!is.na(value)) |>
    mutate(unit = if_else(variable == "soilmoisture", "percentage", "degree celsius")) |>
    select(date_time, siteID:treatment, loggerID, variable, value, unit, comments)

}


#Test to find out how to clean the data
# microclimate %>%
#   filter(siteID == "Skjelingahaugen") %>%
#   ggplot(aes(x = date_time, y = soilmoisture, color = treatment)) +
#   geom_line() +
#   facet_wrap(vars(plotID))

# microclimate %>%
#   filter(
#     siteID %in% c("Skjelingahaugen", "Ulvehaugen", "Gudmedalen", "Lavisdalen"),
#     treatment %in% c("FB", "GB")) %>%
#   ggplot(aes(x = date_time, y = ground_temperature, color = siteID)) +
#   geom_line() +
#   facet_grid(treatment ~ siteID)

# microclimate %>%
#   filter(
#     siteID %in% c("Skjelingahaugen", "Ulvehaugen", "Gudmedalen", "Lavisdalen"),
#     treatment %in% c("FB", "GB")) |>
#   group_by(siteID, treatment) |>
#   summarise(mean(soilmoisture, na.rm = TRUE))

# #Test to find out how to clean the data
# microclimate %>%
#   filter(
#     siteID == "Vikesland",
#     date_time > "2022-06-01 00:15:00",
#     date_time < "2022-09-07 01:15:00") %>%
#   ggplot(aes(x = date_time, y = soil_temperature, color = treatment)) +
#   geom_line() +
#   facet_wrap( ~ plotID)
#
# microclimate %>%
#   filter(siteID == "Ar") %>%
#   group_by(loggerID, plotID) %>%
#   summarise(mean = mean(soil_temperature),
#             var = var(ground_temperature))


#TEST
# microclimate %>%
#   filter(plotID == "Vik4C") %>%
#   ggplot(aes(x = date_time, y = air_temperature, color = treatment)) +
#   geom_line() +
#   facet_wrap(vars(plotID))


# microclimate %>%
#   filter(variable == "soilmoisture") |>
#   ggplot(aes(x = date_time,
#              y = value,
#              color = treatment)) +
#   geom_line() +
#   facet_wrap(vars(siteID))
