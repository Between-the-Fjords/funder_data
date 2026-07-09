# Operator bias correction for root traits
#
# Subset-based approach:
# 1. Reference operator (e.g. Michaela): their values are uncorrected (baseline).
# 2. For each other operator: estimate their effect only from treatments where both
#    that operator and the reference measured. Fit model on shared treatments only
#    to avoid extrapolation and confounding.
# 3. Store in <trait>_corrected. Rows without operator or in exclude_operators get NA.
#
# By default uses treatment × operator interaction so corrections can vary by treatment.
# Use operator_model = "additive" for a single offset per operator.

# Traits from the 2022 RIC ingrowth workflow (lab + winRhIZO). Operator bias
# correction applies to these only.
RIC_TRAIT_COLS <- c(
  "dry_root_turnover_g",
  "root_length_m",
  "avg_root_diameter_m",
  "specific_root_length_m_per_g",
  "root_tissue_density_g_per_m3",
  "root_dry_matter_content",
  "root_productivity_g_per_m3_per_year"
)

# Units for exported long-format trait column.
# root_biomass: raw file has no unit column; values are dry mass per soil sample
# (core/monolith), not normalized to area or volume — same order of magnitude as RIC turnover (g).
ROOT_TRAIT_UNITS <- c(
  root_biomass = "g",
  dry_root_turnover_g = "g",
  root_length_m = "m",
  avg_root_diameter_m = "m",
  specific_root_length_m_per_g = "m/g",
  root_tissue_density_g_per_m3 = "g/m3",
  root_dry_matter_content = "1",
  root_productivity_g_per_m3_per_year = "g/m3/year"
)

# Legacy aliases included for old workflow compatibility.
TRAIT_COLS_ROOT <- c(
  RIC_TRAIT_COLS,
  "dry_root_biomass_g",
  "average_root_diameter_m"
)

#' Apply operator bias correction to root traits (subset-based approach)
#'
#' For each other operator, estimates their effect relative to the reference only
#' from treatments where both operator and reference measured. Avoids extrapolation
#' and confounding between operator and treatment. Correction = raw - estimated_effect.
#' Reference operator's values are unchanged.
#'
#' @param data Data frame with root_traits_clean structure (siteID, treatment, operator, trait columns).
#' @param trait_names Character vector of trait column names; default is RIC_TRAIT_COLS.
#' @param reference_operator Operator used as baseline (e.g. "Michaela"). Their values
#'   are uncorrected. Other operators are corrected by subtracting their estimated effect.
#' @param exclude_operators Character vector of operator names to exclude from fitting.
#' @param min_subset_n Minimum rows required in shared-treatment subset to estimate effect (default 5).
#' @param operator_model `"interaction"` fits `trait ~ siteID * treatment + treatment:operator`
#'   and subtracts the treatment-specific operator offset per row. `"additive"` fits
#'   `trait ~ siteID * treatment + operator` and subtracts one offset per operator.
#' @return Data frame with extra columns \code{<trait>_corrected}; attributes
#'   \code{operator_correction_effects} and \code{reference_operator}.
#' @export
apply_operator_correction <- function(data,
                                     trait_names = RIC_TRAIT_COLS,
                                     reference_operator = NULL,
                                     exclude_operators = character(0),
                                     min_subset_n = 5L,
                                     operator_model = c("interaction", "additive")) {
  operator_model <- match.arg(operator_model)
  op_col <- intersect(c("operator", "Operator"), names(data))[1]
  if (is.na(op_col)) return(data)
  if (op_col == "Operator") data$operator <- data$Operator

  has_op <- !is.na(data$operator) & !(data$operator %in% exclude_operators)
  data$operator <- factor(data$operator)
  nop <- length(unique(data$operator[has_op]))
  if (nop < 2L) return(data)
  if (is.null(reference_operator)) reference_operator <- levels(factor(data$operator[has_op]))[1]
  if (!reference_operator %in% levels(data$operator)) return(data)

  other_operators <- setdiff(levels(data$operator), reference_operator)
  out <- data
  all_effects <- list()

  for (tr in trait_names) {
    if (!tr %in% names(data)) next
    corr_name <- paste0(tr, "_corrected")
    out[[corr_name]] <- NA_real_

    # Reference operator: uncorrected
    ref_idx <- which(has_op & data$operator == reference_operator & !is.na(data[[tr]]))
    out[[corr_name]][ref_idx] <- data[[tr]][ref_idx]

    trait_effects <- list()

    for (op in other_operators) {
      op_idx <- which(has_op & data$operator == op & !is.na(data[[tr]]))
      if (length(op_idx) == 0) next

      # Treatments where both operator and reference measured
      op_treatments <- unique(data$treatment[op_idx])
      ref_treatments <- unique(data$treatment[has_op & data$operator == reference_operator & !is.na(data[[tr]])])
      shared_treatments <- intersect(op_treatments, ref_treatments)
      if (length(shared_treatments) == 0) next

      subset_data <- data %>%
        dplyr::filter(
          .data$treatment %in% shared_treatments,
          .data$operator %in% c(op, reference_operator),
          !is.na(.data[[tr]])
        ) %>%
        dplyr::mutate(operator = relevel(.data$operator, ref = reference_operator))

      if (nrow(subset_data) < min_subset_n) next

      form <- switch(
        operator_model,
        interaction = stats::as.formula(paste(tr, "~ siteID * treatment + treatment:operator")),
        additive = stats::as.formula(paste(tr, "~ siteID * treatment + operator"))
      )
      m <- tryCatch(stats::lm(form, data = subset_data), error = function(e) NULL)
      if (is.null(m)) next

      if (operator_model == "additive") {
        coef_name <- paste0("operator", op)
        if (!coef_name %in% names(stats::coef(m))) next
        op_effect <- stats::coef(m)[coef_name]
        out[[corr_name]][op_idx] <- data[[tr]][op_idx] - op_effect
        trait_effects[[op]] <- op_effect
      } else {
        op_effects_by_treatment <- list()
        for (idx in op_idx) {
          row <- out[idx, , drop = FALSE]
          pred_op <- stats::predict(m, newdata = row)
          row_ref <- row
          row_ref$operator <- reference_operator
          pred_ref <- stats::predict(m, newdata = row_ref)
          operator_offset <- pred_op - pred_ref
          out[[corr_name]][idx] <- out[[tr]][idx] - operator_offset
          trt <- as.character(row$treatment[[1]])
          op_effects_by_treatment[[trt]] <- operator_offset
        }
        trait_effects[[op]] <- op_effects_by_treatment
      }
    }

    all_effects[[tr]] <- trait_effects
  }

  attr(out, "operator_correction_effects") <- all_effects
  attr(out, "reference_operator") <- reference_operator
  attr(out, "operator_correction_model") <- operator_model
  out
}

#' Pivot root traits to long format with raw and operator-corrected values
#'
#' `root_biomass` (2021 standing biomass) is exported with `value` only;
#' `value_corrected` is NA because that dataset has no operator metadata.
#'
#' @param data Wide root traits table with optional `<trait>_corrected` columns.
#' @param ric_trait_cols Traits that may have corrected columns.
#' @return Long data frame with `trait`, `year`, `unit`, `value`, and `value_corrected`.
#' @export
make_root_traits_long <- function(data, ric_trait_cols = RIC_TRAIT_COLS) {
  id_cols <- c(
    "siteID", "blockID", "plotID", "treatment",
    "burial_date", "retrieval_date", "duration", "operator", "ric_volume_m3"
  )
  trait_cols <- intersect(c("root_biomass", ric_trait_cols), names(data))
  corr_cols <- intersect(paste0(ric_trait_cols, "_corrected"), names(data))

  add_meta_cols <- function(df) {
    df |>
      dplyr::mutate(
        year = dplyr::if_else(.data$trait == "root_biomass", 2021L, 2022L),
        unit = unname(ROOT_TRAIT_UNITS[.data$trait])
      )
  }

  raw_long <- data |>
    dplyr::select(dplyr::any_of(c(id_cols, trait_cols))) |>
    tidyr::pivot_longer(
      dplyr::all_of(trait_cols),
      names_to = "trait",
      values_to = "value"
    ) |>
    add_meta_cols()

  if (length(corr_cols) == 0) {
    raw_long$value_corrected <- NA_real_
    return(dplyr::relocate(raw_long, year, unit, .after = trait))
  }

  corr_long <- data |>
    dplyr::select(dplyr::any_of(c(id_cols, corr_cols))) |>
    tidyr::pivot_longer(
      dplyr::all_of(corr_cols),
      names_to = "trait",
      values_to = "value_corrected",
      names_pattern = "(.+)_corrected"
    )

  dplyr::left_join(raw_long, corr_long, by = c(id_cols, "trait")) |>
    dplyr::relocate(year, unit, .after = trait)
}
