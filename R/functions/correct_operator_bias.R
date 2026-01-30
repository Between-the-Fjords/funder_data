# Operator bias correction for root traits
#
# How the correction works:
# 1. Fit the model: trait ~ siteID * treatment + operator (treatment nested in site;
#    operator additive). Fitting uses only rows with non-missing operator and not in
#    exclude_operators.
# 2. Reference operator: the level to which we "correct" (default: first level
#    alphabetically among non-excluded operators). Pass reference_operator to change.
# 3. For each row: keep observed siteID and treatment, but set operator = reference_operator.
#    Predict from the fitted model with this newdata. That predicted value is the
#    corrected value (expected trait if the reference operator had measured that plot).
# 4. Store in <trait>_corrected. Rows without operator or in exclude_operators get NA.
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

#' Apply operator bias correction to root traits
#'
#' Fits the model \code{trait ~ siteID * treatment + operator} for each trait,
#' then for each row predicts the trait value using the same site and treatment
#' but with \code{operator} set to \code{reference_operator}. That predicted
#' value is the corrected value (expected value if the reference operator had
#' measured that plot). The operator effect is thus removed while keeping
#' site × treatment structure.
#'
#' @param data Data frame with root_traits_clean structure (siteID, treatment, operator, trait columns).
#' @param trait_names Character vector of trait column names; default is TRAIT_COLS_ROOT.
#' @param reference_operator Operator level used for correction (e.g. "Lou"). The value
#'   used for operator when predicting: all rows are predicted as if this operator
#'   had measured them. Default: first level alphabetically among non-excluded operators.
#' @param exclude_operators Character vector of operator names to exclude from fitting (e.g. "Peter" with one observation).
#' @return Data frame with extra columns \code{<trait>_corrected}; attributes \code{operator_correction_models} and \code{reference_operator}.
#' @export
apply_operator_correction <- function(data,
                                     trait_names = TRAIT_COLS_ROOT,
                                     reference_operator = NULL,
                                     exclude_operators = c("Peter")) {
  op_col <- intersect(c("operator", "Operator"), names(data))[1]
  if (is.na(op_col)) return(data)
  if (op_col == "Operator") data$operator <- data$Operator
  # Exclude operators with too few observations (e.g. Peter with one)
  has_op <- !is.na(data$operator) & !(data$operator %in% exclude_operators)
  data$operator <- factor(data$operator)
  nop <- length(unique(data$operator[has_op]))
  if (nop < 2L) return(data)
  if (is.null(reference_operator)) reference_operator <- levels(factor(data$operator[has_op]))[1]
  d <- data %>% dplyr::filter(has_op)

  out <- data
  fits <- list()
  for (tr in trait_names) {
    if (!tr %in% names(data)) next
    form <- as.formula(paste(tr, "~ siteID * treatment + operator"))
    d_tr <- d %>% dplyr::filter(!is.na(.data[[tr]]))
    if (nrow(d_tr) < 10L) next
    m <- lm(form, data = d_tr)
    fits[[tr]] <- m
    newdata <- d_tr %>% dplyr::mutate(operator = factor(reference_operator, levels = levels(data$operator)))
    pred <- predict(m, newdata = newdata)
    corr_name <- paste0(tr, "_corrected")
    out[[corr_name]] <- NA_real_
    idx <- which(has_op & !is.na(data[[tr]]))
    out[[corr_name]][idx] <- pred
  }
  attr(out, "operator_correction_models") <- fits
  attr(out, "reference_operator") <- reference_operator
  out
}
