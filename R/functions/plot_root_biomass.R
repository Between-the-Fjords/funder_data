# Visualize root biomass data for outlier detection

plot_root_biomass <- function(root_biomass_data) {
  # Load required libraries
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  library(ggplot2)
  # Identify the biomass column (first numeric column that's not an ID or year)
  numeric_cols <- root_biomass_data |>
    select(where(is.numeric)) |>
    names()
  
  # Exclude ID columns and common metadata
  exclude_cols <- c("year", "block", "depth", "depth_cm", "layer")
  biomass_col <- setdiff(numeric_cols, exclude_cols)[1]
  
  if (is.na(biomass_col)) {
    stop("Could not identify biomass column. Please specify manually.")
  }
  
  # Set treatment factor levels for consistent ordering
  treatment_order <- c("C", "G", "F", "B", "FB", "GB", "GF", "FGB")
  
  # Add temperature and precipitation level columns early for sorting
  root_biomass_data <- root_biomass_data |>
    mutate(
      treatment = factor(treatment, levels = treatment_order),
      temp_level = case_when(
        siteID %in% c("Ovstedalen", "Arhelleren", "Vikesland", "Fauske") ~ "1",
        siteID %in% c("Veskre", "Hogsete", "Rambera", "Alrust") ~ "2",
        siteID %in% c("Skjelingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen") ~ "4",
        TRUE ~ NA_character_
      ),
      prec_level = case_when(
        siteID %in% c("Fauske", "Alrust", "Ulvehaugen") ~ "1",
        siteID %in% c("Vikesland", "Hogsete", "Lavisdalen") ~ "2",
        siteID %in% c("Arhelleren", "Rambera", "Gudmedalen") ~ "3",
        siteID %in% c("Ovstedalen", "Veskre", "Skjelingahaugen") ~ "4",
        TRUE ~ NA_character_
      ),
      temp_level = factor(temp_level, levels = c("1", "2", "4")),
      prec_level = factor(prec_level, levels = c("1", "2", "3", "4")),
      # Create a sorting variable: temp_level then prec_level
      site_sort_order = as.numeric(temp_level) * 10 + as.numeric(prec_level)
    )
  
  # Calculate outlier statistics
  biomass_values <- root_biomass_data[[biomass_col]]
  mean_val <- mean(biomass_values, na.rm = TRUE)
  sd_val <- sd(biomass_values, na.rm = TRUE)
  q1 <- quantile(biomass_values, 0.25, na.rm = TRUE)
  q3 <- quantile(biomass_values, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  # Identify outliers (using IQR method: > Q3 + 1.5*IQR or < Q1 - 1.5*IQR)
  root_biomass_data <- root_biomass_data |>
    mutate(
      outlier_iqr = if_else(
        .data[[biomass_col]] > (q3 + 1.5 * iqr) | .data[[biomass_col]] < (q1 - 1.5 * iqr),
        TRUE, FALSE
      ),
      outlier_zscore = if_else(
        abs((.data[[biomass_col]] - mean_val) / sd_val) > 3,
        TRUE, FALSE
      )
    )
  
  plots <- list()
  
  # 1. Distribution histogram
  plots$distribution <- root_biomass_data |>
    ggplot(aes(x = .data[[biomass_col]])) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
    geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = c(q1, q3), color = "orange", linetype = "dashed", linewidth = 0.8) +
    labs(
      title = paste("Distribution of", biomass_col),
      x = biomass_col,
      y = "Frequency"
    ) +
    theme_bw()
  
  # 2. Boxplot by treatment
  plots$treatment <- root_biomass_data |>
    ggplot(aes(x = treatment, y = .data[[biomass_col]], fill = treatment)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2, alpha = 0.7, outlier.color = "red", outlier.size = 2) +
    labs(
      title = paste("Root Biomass by Treatment"),
      x = "Treatment",
      y = biomass_col
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # 3. Boxplot by site (sorted by temp level then prec level)
  plots$site <- root_biomass_data |>
    ggplot(aes(x = reorder(siteID, site_sort_order),
               y = .data[[biomass_col]], fill = siteID)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2, alpha = 0.7, outlier.color = "red", outlier.size = 2) +
    labs(
      title = paste("Root Biomass by Site (sorted by Temp Level, then Prec Level)"),
      x = "Site",
      y = biomass_col
    ) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 3b. All sites together (overview)
  plots$all_sites <- root_biomass_data |>
    ggplot(aes(x = reorder(siteID, .data[[biomass_col]], median, na.rm = TRUE),
               y = .data[[biomass_col]])) +
    geom_boxplot(aes(fill = siteID), outlier.color = "red", outlier.size = 2, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
    labs(
      title = paste("Root Biomass - All Sites Overview"),
      x = "Site",
      y = biomass_col,
      fill = "Site"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  # 4. Boxplot by site and treatment
  plots$site_treatment <- root_biomass_data |>
    ggplot(aes(x = treatment, y = .data[[biomass_col]], fill = treatment)) +
    geom_boxplot(outlier.color = "red", outlier.size = 2) +
    facet_wrap(~ siteID, scales = "free_y") +
    labs(
      title = paste("Root Biomass by Site and Treatment"),
      x = "Treatment",
      y = biomass_col
    ) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4b. Root biomass by treatment, faceted by temperature and precipitation levels
  plots$temp_prec_treatment <- root_biomass_data |>
    filter(!is.na(temp_level) & !is.na(prec_level)) |>
    ggplot(aes(x = treatment, y = .data[[biomass_col]], fill = treatment)) +
    geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
    facet_grid(temp_level ~ prec_level, 
               labeller = labeller(
                 temp_level = c("1" = "Temp Level 1", "2" = "Temp Level 2", "4" = "Temp Level 4"),
                 prec_level = c("1" = "Prec Level 1", "2" = "Prec Level 2", 
                               "3" = "Prec Level 3", "4" = "Prec Level 4")
               )) +
    labs(
      title = paste("Root Biomass by Treatment, Temperature and Precipitation Levels"),
      x = "Treatment",
      y = biomass_col,
      fill = "Treatment"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  # 5. Outlier identification plot (IQR method)
  plots$outliers_iqr <- root_biomass_data |>
    ggplot(aes(x = seq_along(.data[[biomass_col]]), y = .data[[biomass_col]],
               color = outlier_iqr, shape = outlier_iqr)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_hline(yintercept = q3 + 1.5 * iqr, color = "red", linetype = "dashed") +
    geom_hline(yintercept = q1 - 1.5 * iqr, color = "red", linetype = "dashed") +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                      labels = c("FALSE" = "Normal", "TRUE" = "Outlier")) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                       labels = c("FALSE" = "Normal", "TRUE" = "Outlier")) +
    labs(
      title = "Outlier Detection (IQR Method)",
      x = "Observation",
      y = biomass_col,
      color = "Status",
      shape = "Status"
    ) +
    theme_bw()
  
  # 6. Outlier identification plot (Z-score method)
  plots$outliers_zscore <- root_biomass_data |>
    mutate(z_score = abs((.data[[biomass_col]] - mean_val) / sd_val)) |>
    ggplot(aes(x = seq_along(.data[[biomass_col]]), y = z_score,
               color = outlier_zscore, shape = outlier_zscore)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_hline(yintercept = 3, color = "red", linetype = "dashed") +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                      labels = c("FALSE" = "Normal", "TRUE" = "Outlier")) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                       labels = c("FALSE" = "Normal", "TRUE" = "Outlier")) +
    labs(
      title = "Outlier Detection (Z-score Method, |z| > 3)",
      x = "Observation",
      y = "Absolute Z-score",
      color = "Status",
      shape = "Status"
    ) +
    theme_bw()
  
  # 7. If date column exists, time series plot
  date_cols <- c("date", "sampling_date", "collection_date", "date_in_oven", "retrieval_date")
  date_col <- intersect(date_cols, names(root_biomass_data))[1]
  
  if (!is.na(date_col)) {
    plots$time_series <- root_biomass_data |>
      ggplot(aes(x = .data[[date_col]], y = .data[[biomass_col]], color = siteID)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = TRUE, color = "black") +
      labs(
        title = paste("Root Biomass Over Time"),
        x = date_col,
        y = biomass_col,
        color = "Site"
      ) +
      theme_bw()
  }
  
  # 8. Summary statistics table
  outlier_summary <- root_biomass_data |>
    summarise(
      n_total = n(),
      n_outliers_iqr = sum(outlier_iqr, na.rm = TRUE),
      n_outliers_zscore = sum(outlier_zscore, na.rm = TRUE),
      mean = mean(.data[[biomass_col]], na.rm = TRUE),
      median = median(.data[[biomass_col]], na.rm = TRUE),
      sd = sd(.data[[biomass_col]], na.rm = TRUE),
      min = min(.data[[biomass_col]], na.rm = TRUE),
      max = max(.data[[biomass_col]], na.rm = TRUE),
      q1 = quantile(.data[[biomass_col]], 0.25, na.rm = TRUE),
      q3 = quantile(.data[[biomass_col]], 0.75, na.rm = TRUE)
    )
  
  # Print summary
  cat("\n=== Root Biomass Summary Statistics ===\n")
  print(outlier_summary)
  cat("\n=== Outlier Details ===\n")
  
  outliers_iqr_data <- root_biomass_data |>
    filter(outlier_iqr) |>
    select(any_of(c("siteID", "blockID", "plotID", "treatment", biomass_col, date_col, "comments", "comment", "notes", "note")))
  
  if (nrow(outliers_iqr_data) > 0) {
    cat("\nOutliers detected by IQR method (", nrow(outliers_iqr_data), "):\n")
    print(outliers_iqr_data)
  } else {
    cat("\nNo outliers detected by IQR method.\n")
  }
  
  # Save specified plots
  if (!dir.exists("Figures")) {
    dir.create("Figures", recursive = TRUE)
  }
  
  # Save treatment plot
  ggsave(
    filename = "Figures/root_biomass_treatment.png",
    plot = plots$treatment,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # Save site plot
  ggsave(
    filename = "Figures/root_biomass_site.png",
    plot = plots$site,
    width = 14,
    height = 6,
    dpi = 300
  )
  
  # Save temp_prec_treatment plot
  ggsave(
    filename = "Figures/root_biomass_temp_prec_treatment.png",
    plot = plots$temp_prec_treatment,
    width = 16,
    height = 10,
    dpi = 300
  )
  
  cat("\n=== Plots saved ===\n")
  cat("Saved to Figures/:\n")
  cat("  - root_biomass_treatment.png\n")
  cat("  - root_biomass_site.png\n")
  cat("  - root_biomass_temp_prec_treatment.png\n")
  
  # Return plots and outlier data
  return(list(
    plots = plots,
    outlier_summary = outlier_summary,
    outliers_iqr = outliers_iqr_data,
    biomass_column = biomass_col
  ))
}
