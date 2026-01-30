# Visualize bryophyte cover data

library(tidyverse)
library(patchwork)
library(targets)

# Load the bryophyte_cover target
tar_load(bryophyte_cover)

# Create summary plots
plots <- list()

# 1. Distribution of cover_percent
plots$cover_dist <- bryophyte_cover |>
  ggplot(aes(x = cover_percent)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
  labs(title = "Distribution of Bryophyte Cover (%)",
       x = "Cover (%)",
       y = "Frequency") +
  theme_bw()

# 2. Cover by treatment
plots$cover_treatment <- bryophyte_cover |>
  ggplot(aes(x = treatment, y = cover_percent, fill = treatment)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  labs(title = "Bryophyte Cover by Treatment",
       x = "Treatment",
       y = "Cover (%)") +
  theme_bw() +
  theme(legend.position = "none")

# 3. Cover by site
plots$cover_site <- bryophyte_cover |>
  ggplot(aes(x = reorder(siteID, cover_percent, median, na.rm = TRUE), 
             y = cover_percent, fill = siteID)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  labs(title = "Bryophyte Cover by Site",
       x = "Site",
       y = "Cover (%)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Top 20 species by total cover
top_species <- bryophyte_cover |>
  group_by(species, vernacular_species) |>
  summarise(total_cover = sum(cover_percent, na.rm = TRUE),
            n_observations = n(),
            .groups = "drop") |>
  arrange(desc(total_cover)) |>
  slice_head(n = 20) |>
  mutate(species_label = if_else(!is.na(vernacular_species), 
                                  paste0(vernacular_species, "\n(", species, ")"),
                                  species))

plots$top_species <- top_species |>
  ggplot(aes(x = reorder(species_label, total_cover), y = total_cover)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 20 Species by Total Cover",
       x = "Species",
       y = "Total Cover (%)") +
  theme_bw()

# 5. Cover by treatment and site (heatmap)
plots$cover_heatmap <- bryophyte_cover |>
  group_by(siteID, treatment) |>
  summarise(mean_cover = mean(cover_percent, na.rm = TRUE),
            .groups = "drop") |>
  ggplot(aes(x = treatment, y = siteID, fill = mean_cover)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Mean\nCover (%)") +
  labs(title = "Mean Bryophyte Cover by Site and Treatment",
       x = "Treatment",
       y = "Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Number of species per plot
plots$species_richness <- bryophyte_cover |>
  group_by(plotID, siteID, treatment) |>
  summarise(n_species = n_distinct(species, na.rm = TRUE),
            .groups = "drop") |>
  ggplot(aes(x = treatment, y = n_species, fill = treatment)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  labs(title = "Species Richness by Treatment",
       x = "Treatment",
       y = "Number of Species per Plot") +
  theme_bw() +
  theme(legend.position = "none")

# Combine plots
combined_plot <- (plots$cover_dist | plots$cover_treatment) /
  (plots$cover_site | plots$species_richness) /
  plots$top_species /
  plots$cover_heatmap

# Save combined plot
ggsave("Figures/bryophyte_cover_summary.png", combined_plot, 
       width = 16, height = 20, dpi = 300)

# Print individual plots
print(plots$cover_dist)
print(plots$cover_treatment)
print(plots$cover_site)
print(plots$top_species)
print(plots$cover_heatmap)
print(plots$species_richness)

# Summary statistics
cat("\n=== Summary Statistics ===\n")
cat("Total observations:", nrow(bryophyte_cover), "\n")
cat("Total species:", n_distinct(bryophyte_cover$species, na.rm = TRUE), "\n")
cat("Total plots:", n_distinct(bryophyte_cover$plotID), "\n")
cat("Mean cover:", round(mean(bryophyte_cover$cover_percent, na.rm = TRUE), 2), "%\n")
cat("Median cover:", round(median(bryophyte_cover$cover_percent, na.rm = TRUE), 2), "%\n")
cat("Cover range:", round(min(bryophyte_cover$cover_percent, na.rm = TRUE), 2), 
    "-", round(max(bryophyte_cover$cover_percent, na.rm = TRUE), 2), "%\n")
