# Root traits operator bias: exploration, variance decomposition, and correction
#
# Run after building and loading targets (e.g. run.R), or:
#   targets::tar_load(root_traits_clean)
# then source this file.
#
# Requires: tidyverse (dplyr, tidyr, ggplot2), broom (optional, for tidy model output)

library(tidyverse)

# Ensure data is loaded
if (!exists("root_traits_clean")) {
  if (requireNamespace("targets", quietly = TRUE)) {
    targets::tar_load(root_traits_clean)
  } else {
    stop("root_traits_clean not found. Run targets and load root_traits_clean, or load the object manually.")
  }
}

# Operators to exclude from bias analysis (e.g. too few observations)
OPERATORS_EXCLUDE <- c("Peter")

# Traits that are measured or derived and may show operator bias
# (winRhIZO: root_length_m, average_root_diameter_m; derived: SRL, RTD, etc.)
TRAIT_COLS <- c(
  "dry_root_biomass_g",
  "root_length_m",
  "average_root_diameter_m",
  "specific_root_length_m_per_g",
  "root_tissue_density_g_per_m3",
  "root_dry_matter_content",
  "root_productivity_g_per_m3_per_year"
)

# Restrict to rows with operator and non-NA trait (for modeling)
# Handle both 'operator' and 'Operator' column names from join
operator_col <- intersect(c("operator", "Operator"), names(root_traits_clean))[1]
if (is.na(operator_col)) stop("No operator/Operator column in root_traits_clean.")
if (operator_col == "Operator") root_traits_clean$operator <- root_traits_clean$Operator

dat <- root_traits_clean %>%
  filter(!is.na(operator)) %>%
  filter(!(operator %in% OPERATORS_EXCLUDE)) %>%
  mutate(
    treatment = factor(treatment, levels = c("C", "G", "F", "B", "FB", "GB", "GF", "FGB")),
    operator = factor(operator)
  )

if (nlevels(dat$operator) < 2L) {
  message("Only one operator level in data; variance decomposition and correction skipped.")
}

# ---- 1. Summary: operator × site × treatment ----
operator_summary <- dat %>%
  count(operator, name = "n_plots") %>%
  mutate(operator = as.character(operator))

operator_by_site <- dat %>%
  count(operator, siteID) %>%
  pivot_wider(names_from = operator, values_from = n, values_fill = 0)

operator_by_treatment <- dat %>%
  count(operator, treatment, .drop = FALSE) %>%
  pivot_wider(names_from = operator, values_from = n, values_fill = 0)

message("Operator sample sizes:")
print(operator_summary)
message("\nOperator × site (counts):")
print(operator_by_site)
message("\nOperator × treatment (counts):")
print(operator_by_treatment)

# Only run decomposition, effects, correction and bias plots if we have 2+ operators
run_operator_analysis <- nlevels(dat$operator) >= 2L

if (!run_operator_analysis) {
  message("\nSkipping variance decomposition, operator effects, correction and bias plots (need 2+ operators).")
  var_table <- tibble()
  effects_table <- tibble()
  dat_corrected <- dat
  ref_operator <- levels(dat$operator)[1]
  plot_by_operator <- plot_treatment_operator <- plot_site_operator <- NULL
} else {

# ---- 2. Variance decomposition: how much is operator vs site vs treatment? ----
# For each trait, fit:
#   (a) trait ~ operator
#   (b) trait ~ siteID + treatment
#   (c) trait ~ siteID + treatment + operator
# Compare R² and partial variance (Type II SS or drop1).

variance_decomp <- function(trait_name) {
  form_op <- as.formula(paste(trait_name, "~ operator"))
  form_st <- as.formula(paste(trait_name, "~ siteID + treatment"))
  form_full <- as.formula(paste(trait_name, "~ siteID + treatment + operator"))

  d <- dat %>% filter(!is.na(.data[[trait_name]]))
  if (nrow(d) < 10L) return(NULL)

  m_op   <- lm(form_op,   data = d)
  m_st   <- lm(form_st,   data = d)
  m_full <- lm(form_full, data = d)

  r2_op   <- summary(m_op)$r.squared
  r2_st   <- summary(m_st)$r.squared
  r2_full <- summary(m_full)$r.squared
  # Partial R² for operator when added to site + treatment
  r2_partial_operator <- r2_full - r2_st

  tibble(
    trait = trait_name,
    n = nrow(d),
    R2_operator_only = r2_op,
    R2_site_treatment = r2_st,
    R2_full = r2_full,
    R2_partial_operator = r2_partial_operator
  )
}

var_table <- TRAIT_COLS %>%
  map_dfr(variance_decomp) %>%
  filter(!is.na(R2_full))

message("\nVariance decomposition (R²):")
print(var_table, n = Inf)

# ---- 3. Plots: operator bias ----
# Boxplot: trait by operator (key traits only for readability)
traits_for_plot <- intersect(
  c("dry_root_biomass_g", "root_length_m", "average_root_diameter_m",
    "specific_root_length_m_per_g", "root_productivity_g_per_m3_per_year"),
  names(dat)
)

plot_by_operator <- dat %>%
  select(siteID, blockID, plotID, treatment, operator, any_of(traits_for_plot)) %>%
  pivot_longer(any_of(traits_for_plot), names_to = "trait", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = operator, y = value, fill = operator)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.5) +
  facet_wrap(vars(trait), scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Root traits by operator", x = "Operator", y = "Value")

print(plot_by_operator)

# Treatment × operator: dry_root_biomass_g (main response)
plot_treatment_operator <- dat %>%
  filter(!is.na(dry_root_biomass_g)) %>%
  ggplot(aes(x = treatment, y = dry_root_biomass_g, fill = operator)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.8, outlier.alpha = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Dry root biomass by treatment and operator",
    x = "Treatment", y = "Dry root biomass (g)"
  )

print(plot_treatment_operator)

# Site × operator: same trait
plot_site_operator <- dat %>%
  filter(!is.na(dry_root_biomass_g)) %>%
  ggplot(aes(x = siteID, y = dry_root_biomass_g, fill = operator)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.8, outlier.alpha = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Dry root biomass by site and operator",
    x = "Site", y = "Dry root biomass (g)"
  )

print(plot_site_operator)

# ---- 4. Operator effects (coefficients) ----
# Fit trait ~ siteID + treatment + operator; report operator coefficients
# (effect of each operator relative to reference level)
ref_operator <- levels(dat$operator)[1]
message("\nReference operator for correction: ", ref_operator)

operator_effects <- function(trait_name) {
  form <- as.formula(paste(trait_name, "~ siteID + treatment + operator"))
  d <- dat %>% filter(!is.na(.data[[trait_name]]))
  if (nrow(d) < 10L) return(NULL)
  m <- lm(form, data = d)
  co <- coef(m)
  op_coefs <- co[grepl("^operator", names(co))]
  tibble(
    trait = trait_name,
    operator = gsub("operator", "", names(op_coefs), fixed = TRUE),
    effect_vs_reference = unname(op_coefs)
  )
}

effects_table <- TRAIT_COLS %>%
  map_dfr(operator_effects) %>%
  filter(!is.na(effect_vs_reference))

message("\nOperator effects (vs reference '", ref_operator, "'):")
print(effects_table, n = Inf)

# ---- 5. Correction: remove operator effect, keep site + treatment ----
# Corrected value = predicted value from full model with operator set to reference.
# So: fit trait ~ siteID + treatment + operator; then
#     corrected = predict(model, newdata = data with operator = ref_operator)

apply_operator_correction <- function(data, trait_names, reference_operator = NULL) {
  if (is.null(reference_operator)) reference_operator <- levels(data$operator)[1]
  out <- data
  fits <- list()
  for (tr in trait_names) {
    if (!tr %in% names(data)) next
    form <- as.formula(paste(tr, "~ siteID + treatment + operator"))
    d <- data %>% filter(!is.na(.data[[tr]]))
    if (nrow(d) < 10L) next
    m <- lm(form, data = d)
    fits[[tr]] <- m
    newdata <- d %>% mutate(operator = factor(reference_operator, levels = levels(data$operator)))
    pred <- predict(m, newdata = newdata)
    corr_name <- paste0(tr, "_corrected")
    out[[corr_name]] <- NA_real_
    out[[corr_name]][which(!is.na(data[[tr]]))] <- pred
  }
  attr(out, "operator_correction_models") <- fits
  attr(out, "reference_operator") <- reference_operator
  out
}

traits_to_correct <- intersect(TRAIT_COLS, names(dat))
dat_corrected <- apply_operator_correction(dat, traits_to_correct, reference_operator = ref_operator)

# Compare raw vs corrected (e.g. dry_root_biomass_g)
if ("dry_root_biomass_g_corrected" %in% names(dat_corrected)) {
  plot_raw_vs_corrected <- dat_corrected %>%
    filter(!is.na(dry_root_biomass_g)) %>%
    ggplot(aes(x = dry_root_biomass_g, y = dry_root_biomass_g_corrected, colour = operator)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "gray50") +
    geom_point(alpha = 0.8) +
    theme_bw() +
    labs(
      title = "Raw vs operator-corrected dry root biomass",
      subtitle = paste0("Corrected to reference operator: ", ref_operator),
      x = "Raw (g)", y = "Corrected (g)"
    )
  print(plot_raw_vs_corrected)
}

}  # end if (run_operator_analysis)

# ---- 6. Summary and recommendation ----
message("\n--------- Recommendation ---------")
if (run_operator_analysis) {
  message("1. Variance table (above) shows how much R² is due to operator vs site+treatment.")
  message("2. Corrected values: use data_corrected (columns <trait>_corrected).")
  message("3. Correction = predicted value from lm(trait ~ siteID + treatment + operator) with operator set to reference.")
  message("4. For downstream analyses, use *_corrected columns when operator bias is a concern.")
  message("5. Reference operator used: ", ref_operator, " (change via reference_operator in apply_operator_correction() if needed).")
} else {
  message("With only one operator, no correction applied. data_corrected = dat.")
}

# Optional: return list for use in reports
invisible(list(
  data = dat,
  data_corrected = dat_corrected,
  variance_decomp = var_table,
  operator_effects = effects_table,
  reference_operator = ref_operator,
  plots = list(
    by_operator = plot_by_operator,
    treatment_operator = plot_treatment_operator,
    site_operator = plot_site_operator,
    raw_vs_corrected = if (exists("plot_raw_vs_corrected")) plot_raw_vs_corrected else NULL
  )
))
