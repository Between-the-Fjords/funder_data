# Operator bias correction for root traits
#
# Subset-based approach:
# 1. Reference operator (e.g. Michaela): their values are uncorrected (baseline).
# 2. For each other operator: estimate their effect only from treatments where both
#    that operator and the reference measured. Fit model on shared treatments only
#    to avoid extrapolation and confounding. Correction = raw - estimated_effect.
# 3. Store in <trait>_corrected. Rows without operator or in exclude_operators get NA.
#
# Use *_corrected columns for analyses when operator bias is a concern.

TRAIT_COLS_ROOT <- c(
  "dry_root_biomass_g",
  "root_length_m",
  "average_root_diameter_m",
  "specific_root_length_m_per_g",
  "root_tissue_density_g_per_m3",
  "root_dry_matter_content",
  "root_productivity_g_per_m3_per_year"
)

#' Apply operator bias correction to root traits (subset-based approach)
#'
#' For each other operator, estimates their effect relative to the reference only
#' from treatments where both operator and reference measured. Avoids extrapolation
#' and confounding between operator and treatment. Correction = raw - estimated_effect.
#' Reference operator's values are unchanged.
#'
#' @param data Data frame with root_traits_clean structure (siteID, treatment, operator, trait columns).
#' @param trait_names Character vector of trait column names; default is TRAIT_COLS_ROOT.
#' @param reference_operator Operator used as baseline (e.g. "Michaela"). Their values
#'   are uncorrected. Other operators are corrected by subtracting their estimated effect.
#' @param exclude_operators Character vector of operator names to exclude from fitting (e.g. "Peter").
#' @param min_subset_n Minimum rows required in shared-treatment subset to estimate effect (default 5).
#' @return Data frame with extra columns \code{<trait>_corrected}; attributes
#'   \code{operator_correction_effects} and \code{reference_operator}.
#' @export
apply_operator_correction <- function(data,
                                     trait_names = TRAIT_COLS_ROOT,
                                     reference_operator = NULL,
                                     exclude_operators = c("Peter"),
                                     min_subset_n = 5L) {
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

      form <- as.formula(paste(tr, "~ siteID * treatment + operator"))
      m <- tryCatch(lm(form, data = subset_data), error = function(e) NULL)
      if (is.null(m)) next

      coef_name <- paste0("operator", op)
      if (!coef_name %in% names(coef(m))) next
      op_effect <- coef(m)[coef_name]

      out[[corr_name]][op_idx] <- data[[tr]][op_idx] - op_effect
      trait_effects[[op]] <- op_effect
    }

    all_effects[[tr]] <- trait_effects
  }

  attr(out, "operator_correction_effects") <- all_effects
  attr(out, "reference_operator") <- reference_operator
  out
}
