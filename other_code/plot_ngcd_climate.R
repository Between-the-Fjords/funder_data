# Plot NGCD climate data
# - Mean monthly temperature & total monthly precipitation (Durin sites)
# - Viridis colours by bioclimatic zone, faceted by habitat
# - Summary table: mean summer temperature (June–August) by site
# - Table: monthly climate for Hogsete and Vikesland

library(tidyverse)
library(lubridate)

# -----------------------------------------------------------------------------
# Find and load cleaned NGCD data
# -----------------------------------------------------------------------------

candidates <- list.files("clean_data", pattern = "FUNDER_clean_NGCD.*\\.csv", full.names = TRUE)
if (length(candidates) == 0) {
  stop(
    "No cleaned NGCD file found in clean_data/.\n",
    "Run the pipeline (tar_make) or process_downloaded_ngcd.R first."
  )
}
climate_file <- candidates[1]
cat("Using:", climate_file, "\n\n")

ngcd <- read_csv(climate_file, show_col_types = FALSE)

fig_dir <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Mean monthly temperature (Durin) – viridis, facet by habitat
# -----------------------------------------------------------------------------

durin_monthly_temp <- ngcd %>%
  filter(project == "Durin", variable == "mean_temperature") %>%
  group_by(siteID, bioclimatic_zone, habitat, year, month) %>%
  summarise(mean_monthly_temp = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(yearmonth = make_date(year, month, 1))

p_temp <- ggplot(
  durin_monthly_temp,
  aes(x = yearmonth, y = mean_monthly_temp, colour = bioclimatic_zone, group = siteID)
) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  facet_wrap(~ habitat, ncol = 1, strip.position = "top") +
  scale_colour_viridis_d(option = "viridis", name = "Bioclimatic zone") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(x = "Date", y = "Mean monthly temperature (°C)", title = "Mean monthly temperature at Durin sites") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey92"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "ngcd_durin_mean_monthly_temperature.png"), p_temp, width = 10, height = 7, dpi = 300, bg = "white")
cat("Saved:", file.path(fig_dir, "ngcd_durin_mean_monthly_temperature.png"), "\n")

# -----------------------------------------------------------------------------
# 2. Total monthly precipitation (Durin) – viridis, facet by habitat
# -----------------------------------------------------------------------------

durin_monthly_precip <- ngcd %>%
  filter(project == "Durin", variable == "precipitation") %>%
  group_by(siteID, bioclimatic_zone, habitat, year, month) %>%
  summarise(total_monthly_precip = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(yearmonth = make_date(year, month, 1))

p_precip <- ggplot(
  durin_monthly_precip,
  aes(x = yearmonth, y = total_monthly_precip, colour = bioclimatic_zone, group = siteID)
) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  facet_wrap(~ habitat, ncol = 1, strip.position = "top") +
  scale_colour_viridis_d(option = "viridis", name = "Bioclimatic zone") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(x = "Date", y = "Total monthly precipitation (mm)", title = "Total monthly precipitation at Durin sites") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey92"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "ngcd_durin_monthly_precipitation.png"), p_precip, width = 10, height = 7, dpi = 300, bg = "white")
cat("Saved:", file.path(fig_dir, "ngcd_durin_monthly_precipitation.png"), "\n")

# -----------------------------------------------------------------------------
# 3. Summary table: mean summer temperature (June–August) by site (Durin)
# -----------------------------------------------------------------------------

durin_summer_temp <- ngcd %>%
  filter(project == "Durin", variable == "mean_temperature", month %in% 6:8) %>%
  group_by(siteID, bioclimatic_zone, habitat, elevation_m_asl) %>%
  summarise(
    mean_summer_temp_c = mean(value, na.rm = TRUE),
    sd_summer_temp_c   = sd(value, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  arrange(mean_summer_temp_c)

write_csv(durin_summer_temp, "clean_data/ngcd_durin_mean_summer_temperature_by_site.csv")
cat("\n=== Mean summer temperature (June–August) by site (Durin) ===\n")
print(durin_summer_temp, n = Inf)

# -----------------------------------------------------------------------------
# 4. Monthly climate table: Hogsete & Vikesland (Durin)
# -----------------------------------------------------------------------------

durin_monthly_hogsete_vikesland <- ngcd %>%
  filter(project == "Durin", siteID %in% c("Hogsete", "Vikesland")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  group_by(siteID, habitat, year, month) %>%
  summarise(
    mean_temp_c     = mean(mean_temperature, na.rm = TRUE),
    total_precip_mm = sum(precipitation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_name = month.abb[month]) %>%
  arrange(siteID, habitat, year, month)

write_csv(durin_monthly_hogsete_vikesland, "clean_data/ngcd_durin_monthly_hogsete_vikesland.csv")
cat("\n=== Monthly climate: Hogsete & Vikesland (Durin) ===\n")
print(durin_monthly_hogsete_vikesland, n = Inf)

# -----------------------------------------------------------------------------
# Show plots
# -----------------------------------------------------------------------------
print(p_temp)
print(p_precip)
