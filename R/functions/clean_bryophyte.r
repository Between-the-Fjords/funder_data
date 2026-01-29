# clean bryophyte data

clean_bryophyte_structure <- function(bryophyte_raw, funder_meta) {
  bryophyte <- bryophyte_raw |>
    mutate(blockID = paste0(str_sub(siteID, 1, 3), blockID)) |>
    tidylog::left_join(funder_meta, by = c("siteID", "blockID", "treatment")) |>
    rename(date = `date (yyyy-mm-dd)`)

  bryophyte |>
    rowwise() |>
    mutate(
      vegetation_height = mean(c_across(starts_with("veg_height_")), na.rm = TRUE),
      bryophyte_depth = mean(c_across(starts_with("bryo_height_")), na.rm = TRUE)
    ) |>
    ungroup() |>
    select(date:blockID, plotID, treatment, graminoid:soil, vegetation_height, bryophyte_depth, comments) |>
    mutate(across(
      graminoid:bryophyte_depth,
      ~ if_else(. == "<1", "0.5", as.character(.))
    )) |>
    pivot_longer(cols = c(graminoid:bryophyte_depth), names_to = "functional_group", values_to = "value") |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value)) |>
    mutate(variable = if_else(functional_group %in% c("vegetation_height", "bryophyte_depth"), "height", "cover")) |>
    select(date:treatment, variable, functional_group, value)
}


# Step 1: Call OpenAI API to get raw JSON for unique texts
# Returns a tibble with: original_text, json_response (one row per unique text)
get_species_from_llm <- function(unique_texts,
                                 model = Sys.getenv("OPENAI_MODEL", unset = "gpt-4o"),
                                 api_key = "REDACTED") {
  if (is.null(api_key) || identical(api_key, "")) {
    stop("OPENAI_API_KEY is not set. Please set it in your .Renviron or via Sys.setenv().")
  }

  # Local helpers -------------------------------------------------------------
  build_prompt <- function(cell_text) {
    txt <- cell_text
    if (is.na(txt)) txt <- ""
    txt <- as.character(txt)
    paste0(
      "You are extracting species information from bryophyte specimen data. ",
      "Parse the text and return a JSON array with one object per species. ",
      "Each object should have these fields: scientificName, vernacularName, confirmed (boolean), comment.\n\n",
      "CRITICAL: If multiple species are mentioned (separated by + or &), you MUST create separate objects for each one.\n\n",
      "Rules:\n",
      "- Create a separate object for EACH species mentioned. Multiple species are often separated by + or &\n",
      "- For each species, the vernacular (Norwegian) name and scientific (Latin) name are often separated by a forward slash (/)\n",
      "- Always include scientificName (even if empty string). Always include vernacularName (even if empty string).\n",
      "- If the text explicitly contains the word 'Confirmed' (case-insensitive), set confirmed to true for all species objects. This is important even if no other information is present.\n",
      "- Optional field comment: include any additional information such as life stage (e.g., 'juvenile'), notes (e.g., 'Ingen mose'), codes in parentheses (e.g., '(GUD1221)'), or other descriptive text. If no additional information, omit this field.\n",
      "- If the text is just 'Confirmed' with no species names, return one object with confirmed=true and empty strings for names.\n\n",
      "Examples:\n",
      "- 'Bryum sp.' <U+2192> [{\"scientificName\":\"Bryum sp.\",\"vernacularName\":\"\"}]\n",
      "- 'Pohlia sp.' <U+2192> [{\"scientificName\":\"Pohlia sp.\",\"vernacularName\":\"\"}]\n",
      "- 'mose / Bryum sp.' <U+2192> [{\"scientificName\":\"Bryum sp.\",\"vernacularName\":\"mose\"}]\n",
      "- 'Species A / Name A & Species B / Name B' <U+2192> [{\"scientificName\":\"Species A\",\"vernacularName\":\"Name A\"},{\"scientificName\":\"Species B\",\"vernacularName\":\"Name B\"}]\n",
      "- 'Confirmed - Name1 / Species1 & Name2 / Species2' <U+2192> [{\"scientificName\":\"Species1\",\"vernacularName\":\"Name1\",\"confirmed\":true},{\"scientificName\":\"Species2\",\"vernacularName\":\"Name2\",\"confirmed\":true}]\n",
      "- 'Confirmed' <U+2192> [{\"scientificName\":\"\",\"vernacularName\":\"\",\"confirmed\":true}]\n\n",
      "Text to parse: \"", txt, "\"\n\n",
      "Return the JSON array:"
    )
  }

  call_openai_json <- function(prompt_text) {
    req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
      httr2::req_body_json(list(
        model = model,
        temperature = 0,
        response_format = list(type = "json_object"),
        messages = list(
          list(role = "system", content = "You are a precise information extraction assistant. Always return only valid JSON, no prose."),
          list(role = "user", content = prompt_text)
        )
      ), auto_unbox = TRUE)

    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
      cat("API request failed:", attr(resp, "condition")$message, "\n")
      return("[]")
    }

    content <- try(httr2::resp_body_string(resp), silent = TRUE)
    if (inherits(content, "try-error")) {
      cat("Failed to get response body\n")
      return("[]")
    }

    json_text <- try(jsonlite::fromJSON(content, simplifyVector = FALSE), silent = TRUE)
    if (inherits(json_text, "try-error")) {
      return("[]")
    }

    out <- try(json_text$choices[[1]]$message$content, silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) {
      return("[]")
    }

    # Strip code fences and extract JSON array
    out <- gsub("```[a-zA-Z]*", "", out)
    out <- gsub("```", "", out)
    m <- regmatches(out, regexpr("\\[.*\\]", out, perl = TRUE))
    if (length(m) == 1 && jsonlite::validate(m)) {
      return(m)
    }

    # If a JSON object, try common array containers
    if (jsonlite::validate(out)) {
      obj <- try(jsonlite::fromJSON(out, simplifyVector = FALSE), silent = TRUE)
      if (!inherits(obj, "try-error") && is.list(obj)) {
        # If it's an empty object {}, return empty array
        if (length(obj) == 0) {
          return("[]")
        }

        # If it's a single species object, wrap it in an array
        if ("scientificName" %in% names(obj)) {
          arr_txt <- jsonlite::toJSON(list(obj), auto_unbox = TRUE)
          return(arr_txt)
        }

        for (k in c("items", "data", "result", "output")) {
          if (!is.null(obj[[k]])) {
            arr_txt <- jsonlite::toJSON(obj[[k]], auto_unbox = TRUE)
            if (jsonlite::validate(arr_txt)) {
              return(arr_txt)
            }
          }
        }
      }
    }
    "[]"
  }

  # Filter and process unique texts ----------------------------------
  unique_texts <- unique_texts[!is.na(unique_texts) & trimws(unique_texts) != ""]

  cat("Processing", length(unique_texts), "unique texts\n")

  # Call API for each unique text and return RAW JSON (one row per original text)
  results_map <- purrr::map_dfr(unique_texts, function(tx) {
    prompt <- build_prompt(tx)
    json_txt <- call_openai_json(prompt)

    tibble::tibble(
      original_text = tx,
      json_response = as.character(json_txt)
    )
  })

  return(results_map)
}


# Step 2: Parse JSON and join with original dictionary
# Takes the dictionary and raw LLM JSON results, parses JSON into multiple rows, then joins
join_bryophyte_with_llm <- function(bryophyte_dictionary, bryophyte_llm_results) {
  # Helper to parse JSON array into tibble (returns multiple rows if multiple species)
  parse_json_to_rows <- function(json_text) {
    items <- try(jsonlite::fromJSON(json_text, simplifyVector = TRUE), silent = TRUE)
    if (inherits(items, "try-error") || is.null(items) || length(items) == 0) {
      return(tibble::tibble(
        scientificName = NA_character_,
        vernacularName = NA_character_,
        confirmed = NA,
        comment = NA_character_
      ))
    }

    # Convert to tibble
    if (is.list(items) && !is.data.frame(items)) {
      df <- tibble::as_tibble(items)
    } else if (is.data.frame(items)) {
      df <- tibble::as_tibble(items)
    } else {
      return(tibble::tibble(
        scientificName = NA_character_,
        vernacularName = NA_character_,
        confirmed = NA,
        comment = NA_character_
      ))
    }

    if (nrow(df) == 0) {
      return(tibble::tibble(
        scientificName = NA_character_,
        vernacularName = NA_character_,
        confirmed = NA,
        comment = NA_character_
      ))
    }

    # Ensure all columns exist
    if (!"scientificName" %in% names(df)) df$scientificName <- NA_character_
    if (!"vernacularName" %in% names(df)) df$vernacularName <- NA_character_
    if (!"confirmed" %in% names(df)) df$confirmed <- NA
    if (!"comment" %in% names(df)) df$comment <- NA_character_

    # Return only the columns we need with correct types
    df |>
      dplyr::select(scientificName, vernacularName, confirmed, comment) |>
      dplyr::mutate(
        scientificName = as.character(scientificName),
        vernacularName = as.character(vernacularName),
        confirmed = as.logical(confirmed),
        comment = as.character(comment)
      )
  }

  # Parse JSON for each row (this expands rows with multiple species)
  llm_parsed <- bryophyte_llm_results |>
    dplyr::rowwise() |>
    dplyr::mutate(parsed = list(parse_json_to_rows(json_response))) |>
    dplyr::ungroup() |>
    tidyr::unnest(parsed)

  # Prepare dictionary: clean IDs and site names
  dic <- bryophyte_dictionary |>
    dplyr::rename(
      siteID = dplyr::any_of(c("SITE", "siteID")),
      voucherID = dplyr::any_of(c("voucher ID", "voucherID"))
    ) |>
    dplyr::mutate(
      voucherID = stringr::str_replace_all(voucherID, c("<U+00C5>" = "A", "<U+00D8>" = "O", "<U+00C6>" = "AE")),
      siteID = stringr::str_replace_all(siteID, c("<U+00C5>" = "A", "<U+00D8>" = "O", "<U+00C6>" = "AE")),
      siteID = dplyr::case_when(
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
      ),
      species_correction_Kristian_Hassel = stringr::str_squish(as.character(species_correction_Kristian_Hassel))
    )

  # Manual fix for specific problematic entries that the LLM didn't parse correctly
  # These are hardcoded fixes for entries where multiple species weren't properly separated

  # Fix 1: "Einerbj<U+00F8>rnemose confirmed + Polytrichum alpinum"
  lav704_base <- llm_parsed |>
    dplyr::filter(stringr::str_detect(original_text, "Einerbj<U+00F8>rnemose confirmed \\+ Polytrichum alpinum")) |>
    dplyr::slice(1)

  lav704_fix <- dplyr::bind_rows(
    lav704_base |> dplyr::mutate(scientificName = "", vernacularName = "Einerbj<U+00F8>rnemose", confirmed = TRUE, comment = NA_character_),
    lav704_base |> dplyr::mutate(scientificName = "Polytrichum alpinum", vernacularName = "", confirmed = TRUE, comment = NA_character_)
  )

  # Fix 2: "Beitegr<U+00E5>mose / Racomitrium elongatum + Ribbesigd / Dicranum scoparium + Grynskjeggmose / Barbilophozia hatcheri + Furumose / Pleurozium schreberi"
  ulv823_base <- llm_parsed |>
    dplyr::filter(stringr::str_detect(original_text, "Beitegr<U+00E5>mose.*Racomitrium elongatum.*Ribbesigd")) |>
    dplyr::slice(1)

  ulv823_fix <- dplyr::bind_rows(
    ulv823_base |> dplyr::mutate(scientificName = "Racomitrium elongatum", vernacularName = "Beitegr<U+00E5>mose", confirmed = NA, comment = NA_character_),
    ulv823_base |> dplyr::mutate(scientificName = "Dicranum scoparium", vernacularName = "Ribbesigd", confirmed = NA, comment = NA_character_),
    ulv823_base |> dplyr::mutate(scientificName = "Barbilophozia hatcheri", vernacularName = "Grynskjeggmose", confirmed = NA, comment = NA_character_),
    ulv823_base |> dplyr::mutate(scientificName = "Pleurozium schreberi", vernacularName = "Furumose", confirmed = NA, comment = NA_character_)
  )

  # Fix 3: "Ptychodium plicatum + Sumptvebladmose / Scaparia irrigua"
  gud1223_base <- llm_parsed |>
    dplyr::filter(stringr::str_detect(original_text, "Ptychodium plicatum.*Sumptvebladmose.*Scaparia irrigua")) |>
    dplyr::slice(1)

  gud1223_fix <- dplyr::bind_rows(
    gud1223_base |> dplyr::mutate(scientificName = "Ptychodium plicatum", vernacularName = "", confirmed = NA, comment = NA_character_),
    gud1223_base |> dplyr::mutate(scientificName = "Scaparia irrigua", vernacularName = "Sumptvebladmose", confirmed = NA, comment = NA_character_)
  )

  # Fix 4: "Bryum sp. - muligens en liten B. pseudotriquetrum" - add cf to indicate uncertainty
  fau104_base <- llm_parsed |>
    dplyr::filter(stringr::str_detect(original_text, "Bryum sp.*muligens.*B\\. pseudotriquetrum")) |>
    dplyr::slice(1)

  fau104_fix <- fau104_base |>
    dplyr::mutate(scientificName = "Bryum cf. pseudotriquetrum", vernacularName = "", confirmed = NA, comment = "muligens en liten B. pseudotriquetrum")

  # Remove the problematic entries from llm_parsed and add the fixes
  llm_parsed_fixed <- llm_parsed |>
    dplyr::filter(
      !stringr::str_detect(original_text, "Einerbj<U+00F8>rnemose confirmed \\+ Polytrichum alpinum"),
      !stringr::str_detect(original_text, "Beitegr<U+00E5>mose.*Racomitrium elongatum.*Ribbesigd"),
      !stringr::str_detect(original_text, "Ptychodium plicatum.*Sumptvebladmose.*Scaparia irrigua"),
      !stringr::str_detect(original_text, "Bryum sp.*muligens.*B\\. pseudotriquetrum")
    ) |>
    dplyr::bind_rows(lav704_fix, ulv823_fix, gud1223_fix, fau104_fix)

  # Join parsed results with dictionary (many-to-many because multiple species per text)
  out <- dic |>
    dplyr::left_join(llm_parsed_fixed,
      by = dplyr::join_by(species_correction_Kristian_Hassel == original_text),
      relationship = "many-to-many"
    )

  # Format output columns
  out |>
    dplyr::mutate(
      check = dplyr::if_else(confirmed == TRUE, "confirmed", NA_character_),
      norwegian_name = vernacularName,
      species_llm = scientificName,
      comment_llm = comment,
      # Merge species names: prefer checked LLM columns, fall back to original if empty
      vernacular_final = dplyr::case_when(
        !is.na(norwegian_name) & norwegian_name != "" & norwegian_name != "?" ~ norwegian_name,
        !is.na(vernacular) & vernacular != "" & vernacular != "?" ~ vernacular,
        TRUE ~ NA_character_
      ),
      scientific_final = dplyr::case_when(
        !is.na(species_llm) & species_llm != "" ~ species_llm,
        !is.na(scientific) & scientific != "" ~ scientific,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(siteID, voucherID, vernacular_final, scientific_final, species_correction_Kristian_Hassel, json_response, check, norwegian_name, species_llm, comment_llm, confirmed, `level of certainty`:comments_data_entering, vernacularName, scientificName)
}

# Rhytidiadelphus (Hylocomiadelphus) triquetrus
# joined_bryophyte |> filter(scientific_final == "Rhytidiadelphus (Hylocomiadelphus) triquetrus")

clean_bryophyte <- function(bryophyte_raw, joined_bryophyte, funder_meta) {
  bryophyte <- bryophyte_raw |>
    mutate(blockID = paste0(str_sub(siteID, 1, 3), blockID)) |>
    # Fix: Change some F treatments to C for Veskre blockID 2
    # This is needed because there are 2x F treatments and no C treatment
    # Convert cover_percent to numeric, handling "<1" as 0.5
    mutate(
      cover_percent = if_else(cover_percent == "<1", "0.5", as.character(cover_percent)),
      cover_percent = as.numeric(cover_percent),
      treatment = case_when(
        siteID == "Veskre" & blockID == "Ves2" & treatment == "F" &
          species == "Etasjemose" & cover_percent == 20 ~ "C",
        siteID == "Veskre" & blockID == "Ves2" & treatment == "F" &
          species == "Engkransmose" & cover_percent == 2 ~ "C",
        siteID == "Veskre" & blockID == "Ves2" & treatment == "F" &
          species == "Storkransmose" & cover_percent == 2 ~ "C",
        TRUE ~ treatment
      )
    ) |>
    tidylog::left_join(funder_meta, by = c("siteID", "blockID", "treatment")) |>
    rename(date = `date (yyyy-mm-dd)`) |>
    mutate(voucherID = str_replace_all(voucherID, c("<U+00C5>" = "A", "<U+00D8>" = "O", "<U+00C6>" = "AE"))) |>
    # Remove incorrect rows that should not be in the data
    # Handle case-insensitive matching and potential whitespace
    tidylog::filter(!(plotID == "Gud4G" & str_to_lower(str_trim(species)) == "racomitrium ericoides")) |>
    # Fill missing cover_percent values for specific plotID and species combinations
    mutate(cover_percent = case_when(
      is.na(cover_percent) & plotID == "Skj1C" & species == "Rose" ~ 0.5,
      is.na(cover_percent) & plotID == "Gud1GF" & species == "\"Bronze\"" ~ 0.5, # Update Bronze regardless of NA status (2 rows)
      is.na(cover_percent) & plotID == "Arh3G" & species == "Storbj<U+00F8>rnemose" ~ 0.5,
      TRUE ~ cover_percent
    ))

  # check if all treatments are present in bryophyte
  # Ves2 has only 3 treatments
  # bryophyte |>
  # distinct(siteID, blockID, plotID,treatment) |>
  # group_by(siteID, blockID) |>
  # count() |> filter(n < 4)

  bryo_dictionary <- joined_bryophyte |>
    # Apply vernacular_final changes
    mutate(vernacular_final = case_when(
      vernacular_final == "Bakkefrynse" ~ "Bakkefrynsemose",
      vernacular_final == "Einerbj<U+00F8>rnemose|Storbj<U+00F8>rnemose" & scientific_final == "Polytrichum alpinum" ~ "Fjellbinnemose",
      vernacular_final == "Einerbj<U+00F8>rnemose" & scientific_final == "Polytrichum alpinum" ~ "Fjellbinnemose",
      vernacular_final == "Heigr<U+00E5>mose" & scientific_final == "Oncophorus integerrimus" ~ "Glattsprikemose",
      vernacular_final == "Sumptveblad" ~ "Sumptvebladmose",
      is.na(vernacular_final) & scientific_final == "Ptychodium plicatum" ~ "Storraspmose",
      is.na(vernacular_final) & scientific_final == "Racomitrium canescens ssp. elongatum" ~ NA_character_, ### need vernacular name
      is.na(vernacular_final) & scientific_final == "Racomitrium canescens ssp. canescens" ~ NA_character_, ### need vernacular name
      scientific_final == "Bryum sp." ~ NA_character_,
      TRUE ~ vernacular_final
    )) |>
    # Apply scientific_final changes
    mutate(scientific_final = case_when(
      voucherID == "LAV704" & vernacular_final == "Einerbj<U+00F8>rnemose" & is.na(scientific_final) ~ "Polytrichum juniperinum",
      scientific_final == "R. lanuginosum" ~ "Racomitrium lanuginosum",
      scientific_final == "Polytrichum alpinum" ~ "Polytrichastrum alpinum",
      scientific_final == "Rhytidiadelphus triquetrus" ~ "Hylocomiadelphus triquetrus",
      scientific_final == "Rhytidiadelphus (Hylocomiadelphus) triquetrus" ~ "Hylocomiadelphus triquetrus",
      scientific_final == "Scaparia irrigua" ~ "Scapania irrigua",
      scientific_final == "Ptychodium plicatum" ~ "Lescuraea plicata",
      scientific_final == "Polia sp." ~ "Pohlia sp.",
      TRUE ~ scientific_final
    )) |>
    # Set vernacular_final to NA for Pohlia sp. (after scientific_final changes)
    mutate(vernacular_final = if_else(scientific_final == "Pohlia sp.", NA_character_, vernacular_final)) |>
    # remove entries where species_correction_Kristian_Hassel is "Ingen mose!"
    filter(species_correction_Kristian_Hassel != "Ingen mose!") |>
    # remove rows where scientific_final is NA (no species found)
    filter(!is.na(scientific_final))

  # joined_bryophyte2 |>
  # distinct(vernacular_final, scientific_final, species_correction_Kristian_Hassel, comments_data_entering) |>
  # arrange(vernacular_final) |>
  # writexl::write_xlsx("bryo_species_list2.xlsx")


  # Join bryophyte data with the species dictionary
  # left_join will duplicate bryophyte rows when a voucherID has multiple species in bryo_dictionary
  bryophyte_joined <- bryophyte |>
    select(date:blockID, plotID, treatment:weather, comments) |>
    tidylog::left_join(
      bryo_dictionary |>
        select(siteID, voucherID, vernacular_final, scientific_final, comments_data_entering),
      by = c("siteID", "voucherID")
    ) |>
    # Filter out rows where no match was found in bryo_dictionary (these would have NA in scientific_final)
    # This handles cases where voucherID exists in bryophyte but not in bryo_dictionary
    # 18 observations do not join because they are in the bryo_dictionary but not in the bryophyte data. This is ok.
    tidylog::filter(!is.na(scientific_final)) |>
    # Add a column to count how many species each voucherID has (for splitting cover values later)
    group_by(siteID, plotID, voucherID) |>
    mutate(n_species = n()) |>
    ungroup()
  
  # Track species that were split (n_species > 1) before modifying cover
  split_species_cases <- bryophyte_joined |>
    filter(n_species > 1) |>
    select(siteID, blockID, plotID, voucherID, vernacular_final, scientific_final, 
           cover_percent_original = cover_percent, n_species) |>
    mutate(cover_percent_split = cover_percent_original / n_species)
  
  if (nrow(split_species_cases) > 0) {
    n_vouchers <- split_species_cases |> distinct(siteID, plotID, voucherID) |> nrow()
    message(
      "Found ", nrow(split_species_cases), " species rows from ", n_vouchers, 
      " voucherIDs where cover was split among multiple species. ",
      "Use attr(result, 'split_species_cases') to review."
    )
  }
  
  # Split cover values when multiple species were observed for the same voucherID
  # Divide cover by n_species to split it proportionally across all species
  bryophyte_joined <- bryophyte_joined |>
    mutate(cover_percent = if_else(n_species > 1, cover_percent / n_species, cover_percent))

  # Attach split species cases as attribute for review
  if (nrow(split_species_cases) > 0) {
    attr(bryophyte_joined, "split_species_cases") <- split_species_cases
  }
  
  return(bryophyte_joined)
}
