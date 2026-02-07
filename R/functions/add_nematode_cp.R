# Add Nemaplex c-p and feeding data to nematode feeding file
# Uses embedded lookup (Approach 1) for offline reproducibility

library(tidyverse)

# Clean newlines, tabs and collapse spaces in text (Nemaplex HTML can have \r\n\t in cells)
clean_lookup_text <- function(x) {
  if (!is.character(x)) return(x)
  trimws(gsub(" +", " ", gsub("[\r\n\t]+", " ", x)))
}

# ------------------------------------------------------------------------------
# Embedded Nemaplex lookup (genus level)
# Loaded from data/nemaplex_genus_lookup.csv when present (created by build_nemaplex_lookup)
# Run build_nemaplex_lookup(save_path = "data/nemaplex_genus_lookup.csv") to refresh
# ------------------------------------------------------------------------------
NEMAPLEX_LOOKUP_PATH <- "data/nemaplex_genus_lookup.csv"
NEMAPLEX_LOOKUP <- if (file.exists(NEMAPLEX_LOOKUP_PATH)) {
  readr::read_csv(NEMAPLEX_LOOKUP_PATH, show_col_types = FALSE) |>
    mutate(across(where(is.character), clean_lookup_text)) |>
    mutate(across(c(putative_feeding, feeding_group, functional_guild), ~ if_else(is.na(.), NA_character_, tolower(.))))
} else {
  tibble(
    genus = character(),
    family = character(),
    putative_feeding = character(),
    cp_group = character(),
    feeding_group = character(),
    functional_guild = character()
  )
}

# ------------------------------------------------------------------------------
# build_nemaplex_lookup() - optional helper to refresh lookup from Nemaplex
# ------------------------------------------------------------------------------
#' Build Nemaplex genus-level lookup from 6 HTML index pages
#'
#' Fetches and parses Nemaplex index pages (atob, ctod, etol, mtoo, ptor, stoz).
#' Returns a tibble with one row per genus. Requires rvest and network access.
#'
#' @param save_path Optional path to save CSV (e.g. "data/nemaplex_genus_lookup.csv")
#' @return Tibble with columns: genus, family, putative_feeding, cp_group, feeding_group, functional_guild
#' @export
build_nemaplex_lookup <- function(save_path = NULL) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("rvest is required for build_nemaplex_lookup(). Install with: install.packages(\"rvest\")")
  }

  base_url <- "https://nemaplex.ucdavis.edu/IndexFiles"
  pages <- c("atob", "ctod", "etol", "mtoo", "ptor", "stoz")

  out <- vector("list", length(pages))

  for (i in seq_along(pages)) {
    url <- paste0(base_url, "/", pages[i], ".html")
    html <- try(rvest::read_html(url), silent = TRUE)
    if (inherits(html, "try-error")) {
      warning("Could not fetch ", url)
      next
    }

    tbl <- rvest::html_table(html, fill = TRUE)
    if (length(tbl) == 0) {
      warning("No tables found in ", url)
      next
    }

    d <- tbl[[1]]
    # Nemaplex: X1=Genus, X2=Family, X3=Putative Feeding, X4=c-p, X5=Feeding Group, X6=Functional Guild
    # Skip header rows (first 3: menu, column names, blank)
    d <- d |>
      slice(-(1:3)) |>
      rename(genus = 1, family = 2, putative_feeding = 3, cp_group = 4, feeding_group = 5, functional_guild = 6) |>
      select(genus, family, putative_feeding, cp_group, feeding_group, functional_guild) |>
      mutate(across(everything(), ~ clean_lookup_text(as.character(.))))
    out[[i]] <- d
  }

  result <- bind_rows(out) |>
    filter(!is.na(genus), nzchar(trimws(genus))) |>
    mutate(across(everything(), ~ trimws(as.character(.)))) |>
    mutate(across(c(putative_feeding, feeding_group, functional_guild), ~ if_else(is.na(.), NA_character_, tolower(.)))) |>
    distinct(genus, family, .keep_all = TRUE)

  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(result, save_path)
    message("Saved Nemaplex lookup to ", save_path)
  }

  result
}

# ------------------------------------------------------------------------------
# add_nematode_cp() - add Nemaplex data to nematode feeding file
# ------------------------------------------------------------------------------
#' Add Nemaplex c-p and feeding data to nematode feeding file
#'
#' Reads the nematode feeding CSV, trims Family, and left-joins Nemaplex
#' genus-level data on exact (case-insensitive) family match. One row per
#' (family, genus) for matched families; one row per family with genus=NA
#' for unmatched families.
#'
#' @param nematode_feeder_path Path to semicolon-separated CSV (e.g. FUNDER_raw_Nematode_feeding_group_2023.csv)
#' @param nemaplex_lookup Optional tibble from build_nemaplex_lookup(); if NULL, uses embedded NEMAPLEX_LOOKUP
#' @return Tibble with Family, Functional Group, genus, putative_feeding, cp_group, feeding_group, functional_guild, family_matched
#' @export
add_nematode_cp <- function(nematode_feeder_path, nemaplex_lookup = NULL) {
  lookup <- (nemaplex_lookup %||% NEMAPLEX_LOOKUP) |>
    mutate(across(where(is.character), clean_lookup_text)) |>
    mutate(across(c(putative_feeding, feeding_group, functional_guild), ~ if_else(is.na(.), NA_character_, tolower(.))))

  feeder <- readr::read_csv2(nematode_feeder_path, show_col_types = FALSE) |>
    mutate(Family = stringr::str_trim(Family))

  if (nrow(lookup) == 0) {
    warning("Nemaplex lookup is empty. Run build_nemaplex_lookup() when online and pass the result.")
  }

  lookup_lower <- lookup |>
    mutate(family_lower = tolower(trimws(family)))

  feeder_lower <- feeder |>
    mutate(family_lower = tolower(Family)) |>
    distinct(Family, `Functional Group`, family_lower)

  matched <- feeder_lower |>
    left_join(lookup_lower |> select(-family), by = "family_lower") |>
    mutate(family_matched = !is.na(genus))

  out <- matched |>
    select(Family, `Functional Group`, genus, putative_feeding, cp_group, feeding_group, functional_guild, family_matched)

  unmatched <- feeder_lower |>
    filter(!family_lower %in% lookup_lower$family_lower) |>
    pull(Family) |>
    unique()

  if (length(unmatched) > 0) {
    message("Unmatched families (no Nemaplex data): ", paste(unmatched, collapse = ", "))
  }

  out
}

# ------------------------------------------------------------------------------
# select_nematode_family_row() - one row per family, choosing by Functional Group match
# ------------------------------------------------------------------------------
#' Reduce nematode feeder cp to one row per family
#'
#' For families with multiple Nemaplex rows, selects the row where Functional Group
#' matches putative_feeding (Plant↔plant feeders, Fungi↔fungus feeders, etc.).
#' Rhabditidae: explicitly chooses first bacteria feeders. Fallback: first row.
#'
#' @param feeder_cp Output from add_nematode_cp (genus-expanded)
#' @return Tibble with one row per family
#' @export
select_nematode_family_row <- function(feeder_cp) {
  fg_to_putative <- list(
    Plant = "plant feeders",
    Fungi = "fungus feeders",
    Bacteria = "bacteria feeders",
    Predator = "predators",
    Omnivor = "omnivores"
  )

  feeder_cp |>
    distinct(Family, `Functional Group`, genus, putative_feeding, cp_group, feeding_group, functional_guild, family_matched) |>
    group_by(Family) |>
    filter(n() == 1L | {
      fg <- first(`Functional Group`)
      # Rhabditidae: choose first bacteria feeders (most common)
      if (identical(Family[1], "Rhabditidae")) {
        idx <- which(putative_feeding == "bacteria feeders")[1]
        seq_along(Family) == replace(idx, is.na(idx), 1L)
      } else {
        # Prefer row where Functional Group matches putative_feeding
        expected <- fg_to_putative[[fg]]
        match_idx <- which(putative_feeding == expected)[1]
        seq_along(Family) == replace(match_idx, is.na(match_idx), 1L)
      }
    }) |>
    ungroup() |>
    select(Family, `Functional Group`, putative_feeding, cp_group)
}

# NULL coalesce helper
`%||%` <- function(x, y) if (is.null(x)) y else x
