# ====================================================================
# Vorpraktikum Publication Tables and Plots
# Purpose: Create polished tables and figures for university/thesis work
# Run AFTER 02_vorpraktikum.R has completed successfully
# ====================================================================

.libPaths("C:/Users/sebas/R/win-library/4.5.2")

# Load packages
for (pkg in c("readxl", "dplyr", "ggplot2", "readr", "tidyr", "RColorBrewer", "gridExtra")) {
  if (!require(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# ---- Configuration ----
base_dir <- "C:/Users/sebas/Downloads/tomato_analysis"
out_dir <- file.path(base_dir, "outputs_vorpraktikum")
table_dir <- file.path(out_dir, "tables")
plot_dir <- file.path(out_dir, "plots")
pub_dir <- file.path(out_dir, "publication_tables")
pub_plot_dir <- file.path(out_dir, "publication_plots")

dir.create(pub_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pub_plot_dir, showWarnings = FALSE, recursive = TRUE)

# ====================================================================
# 1. LOAD CLEANED DATA
# ====================================================================
clean_long <- read_csv(file.path(out_dir, "clean_all_long.csv"), show_col_types = FALSE)
box_means <- read_csv(file.path(table_dir, "means_by_box.csv"), show_col_types = FALSE)
treatment_means <- read_csv(file.path(table_dir, "means_by_treatment_all_responses.csv"), show_col_types = FALSE)

# ====================================================================
# 2. TABLE 1: DETAILED DATA INFORMATION TABLE
# ====================================================================
# All raw data points with treatment and response information

detailed_data <- clean_long %>%
  select(
    box, treatment, sample_id, response, value, unit, plant_id
  ) %>%
  mutate(
    response_label = recode(response,
      fruit_weight = "Fruit fresh weight",
      biomass_fresh = "Shoot fresh weight",
      biomass_dry = "Shoot dry weight",
      root_dry_weight = "Root dry weight"
    ),
    value_with_unit = paste0(round(value, 2), " ", unit)
  ) %>%
  arrange(box, response, sample_id) %>%
  select(
    Box = box,
    Treatment = treatment,
    "Sample ID" = sample_id,
    "Response Variable" = response_label,
    "Value (g)" = value,
    "Plant ID" = plant_id
  )

write_csv(detailed_data, file.path(pub_dir, "01_detailed_all_measurements.csv"))

# ====================================================================
# 3. TABLE 2: SUMMARY BY TREATMENT (ALL RESPONSES)
# ====================================================================

treatment_summary <- treatment_means %>%
  pivot_longer(
    cols = contains("_mean"),
    names_to = "response_type",
    values_to = "mean_value"
  ) %>%
  mutate(
    response_label = recode(response_type,
      fruit_mean = "Fruit fresh weight (g)",
      biomass_fresh_mean = "Shoot fresh weight (g)",
      biomass_dry_mean = "Shoot dry weight (g)",
      root_dry_mean = "Root dry weight (g)"
    )
  ) %>%
  select(
    Treatment = treatment,
    "Treatment Code" = treatment_abbrev,
    "N Boxes" = n_boxes,
    "Response Variable" = response_label,
    "Mean (g)" = mean_value
  ) %>%
  mutate(`Mean (g)` = round(`Mean (g)`, 1)) %>%
  arrange(Treatment, `Response Variable`)

write_csv(treatment_summary, file.path(pub_dir, "02_summary_by_treatment.csv"))

# ====================================================================
# 4. TABLE 3: BOX LEVEL SUMMARY (DETAILED)
# ====================================================================

box_summary_detailed <- box_means %>%
  select(box, treatment, treatment_abbrev, everything()) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  rename(
    Box = box,
    Treatment = treatment,
    "Treatment Code" = treatment_abbrev,
    "Fruit fresh weight (g)" = fruit_weight,
    "Shoot fresh weight (g)" = biomass_fresh,
    "Shoot dry weight (g)" = biomass_dry,
    "Root dry weight (g)" = root_dry_weight
  )

write_csv(box_summary_detailed, file.path(pub_dir, "03_box_level_summary.csv"))

# ====================================================================
# 5. TABLE 4: TREATMENT COMPARISON TABLE
# ====================================================================
# Contrasts: Extracts vs Control, Extract vs Extract, etc.

comparison_table <- expand_grid(
  t1 = unique(treatment_means$treatment),
  t2 = unique(treatment_means$treatment)
) %>%
  filter(t1 < t2) %>%
  crossing(response = c("fruit_mean", "biomass_fresh_mean", "biomass_dry_mean", "root_dry_mean"))

comparison_data <- comparison_table %>%
  left_join(
    treatment_means %>% select(treatment, treatment_abbrev, contains("_mean")),
    by = c("t1" = "treatment")
  ) %>%
  rename_with(~paste0("t1_", .), contains("_mean")) %>%
  left_join(
    treatment_means %>% select(treatment, treatment_abbrev, contains("_mean")),
    by = c("t2" = "treatment")
  ) %>%
  rename_with(~paste0("t2_", .), contains("_mean")) %>%
  mutate(
    response_label = recode(response,
      fruit_mean = "Fruit fresh weight (g)",
      biomass_fresh_mean = "Shoot fresh weight (g)",
      biomass_dry_mean = "Shoot dry weight (g)",
      root_dry_mean = "Root dry weight (g)"
    ),
    t1_value = ifelse(response == "fruit_mean", t1_fruit_mean,
                ifelse(response == "biomass_fresh_mean", t1_biomass_fresh_mean,
                ifelse(response == "biomass_dry_mean", t1_biomass_dry_mean,
                       t1_root_dry_mean))),
    t2_value = ifelse(response == "fruit_mean", t2_fruit_mean,
                ifelse(response == "biomass_fresh_mean", t2_biomass_fresh_mean,
                ifelse(response == "biomass_dry_mean", t2_biomass_dry_mean,
                       t2_root_dry_mean))),
    difference = t1_value - t2_value,
    percent_diff = (difference / t2_value) * 100
  ) %>%
  select(
    "Treatment A" = t1,
    "Treatment B" = t2,
    "Response Variable" = response_label,
    "A Mean (g)" = t1_value,
    "B Mean (g)" = t2_value,
    "Difference (A - B)" = difference,
    "Percent Difference (%)" = percent_diff
  ) %>%
  mutate(
    across(contains("Mean") | contains("Difference"), ~round(., 1)),
    `Percent Difference (%)` = round(`Percent Difference (%)`, 1)
  )

write_csv(comparison_data, file.path(pub_dir, "04_treatment_comparison_table.csv"))

# ====================================================================
# 6. TABLE 5: STATISTICAL SUMMARY (ANOVA)
# ====================================================================

anova_files <- list.files(table_dir, pattern = "^anova_", full.names = TRUE)
anova_combined <- bind_rows(
  lapply(anova_files, function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      mutate(file = basename(f))
  })
) %>%
  mutate(
    response_label = recode(response,
      fruit_weight = "Fruit fresh weight",
      biomass_fresh = "Shoot fresh weight",
      biomass_dry = "Shoot dry weight",
      root_dry_weight = "Root dry weight"
    ),
    level_label = ifelse(level == "box", "Box level", "Sample level"),
    p_value_formatted = ifelse(p_value < 0.001, "< 0.001",
                              ifelse(p_value < 0.01, "< 0.01",
                              ifelse(p_value < 0.05, "< 0.05",
                                     round(p_value, 4)))),
    significance = ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                         ifelse(p_value < 0.05, "*", "n.s.")))
  ) %>%
  select(
    "Response Variable" = response_label,
    "Analysis Level" = level_label,
    "F-statistic" = statistic,
    "DF" = df,
    "p-value" = p_value_formatted,
    "Significance" = significance,
    "Eta squared (effect size)" = eta2
  ) %>%
  mutate(
    `F-statistic` = round(`F-statistic`, 2),
    `Eta squared (effect size)` = round(`Eta squared (effect size)`, 3)
  )

write_csv(anova_combined, file.path(pub_dir, "05_statistical_summary_ANOVA.csv"))

# ====================================================================
# 7. PUBLICATION PLOT 1: COMBINED PANEL WITH ERROR BARS (ALL RESPONSES)
# ====================================================================

# Recalculate means with error bars for publication
plot_data <- clean_long %>%
  mutate(
    response_label = recode(response,
      fruit_weight = "Fruit fresh weight (g)",
      biomass_fresh = "Shoot fresh weight (g)",
      biomass_dry = "Shoot dry weight (g)",
      root_dry_weight = "Root dry weight (g)"
    )
  ) %>%
  group_by(treatment, response_label) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    treatment_short = recode(treatment,
      "Control (no salt)" = "Control\n(no salt)",
      "Salt control (no extract)" = "Salt Control\n(no extract)",
      "Salicornia extract (salt stress)" = "Salicornia\n(salt stress)",
      "Strandaster extract (salt stress)" = "Strandaster\n(salt stress)"
    )
  )

# Color palette
color_palette <- c(
  "Control (no salt)" = "#66C2A5",
  "Salt control (no extract)" = "#FC8D62",
  "Salicornia extract (salt stress)" = "#8DA0CB",
  "Strandaster extract (salt stress)" = "#E78AC3"
)

p_publication_panel <- ggplot(plot_data, aes(x = treatment_short, y = mean, fill = treatment)) +
  geom_col(alpha = 0.85, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.25, color = "black", linewidth = 0.6) +
  facet_wrap(~response_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(
    title = "Vorpraktikum: Plant responses by treatment",
    subtitle = "Mean ± 95% CI; bars show all measured values",
    x = "Treatment",
    y = "Value (g)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(pub_plot_dir, "01_panel_all_responses_by_treatment.png"), 
       p_publication_panel, width = 11, height = 9, dpi = 300)

# ====================================================================
# 8. PUBLICATION PLOT 2: BOX-LEVEL DETAIL (FRUIT + SHOOT FRESH)
# ====================================================================

box_plot_data <- box_means %>%
  select(box, treatment, treatment_abbrev, fruit_weight, biomass_fresh) %>%
  pivot_longer(c(fruit_weight, biomass_fresh), names_to = "response", values_to = "value") %>%
  mutate(
    response_label = recode(response,
      fruit_weight = "Fruit fresh weight (g)",
      biomass_fresh = "Shoot fresh weight (g)"
    ),
    treatment_color = recode(treatment,
      "Control (no salt)" = "#66C2A5",
      "Salt control (no extract)" = "#FC8D62",
      "Salicornia extract (salt stress)" = "#8DA0CB",
      "Strandaster extract (salt stress)" = "#E78AC3"
    )
  )

p_box_detail <- ggplot(box_plot_data, aes(x = box, y = value, fill = treatment)) +
  geom_col(alpha = 0.85, color = "black", linewidth = 0.3) +
  geom_text(aes(label = round(value, 0)), vjust = -0.4, size = 3, fontface = "bold") +
  facet_wrap(~response_label, scales = "free_y") +
  scale_fill_manual(values = color_palette, name = "Treatment") +
  labs(
    title = "Box-level responses: Fruit and shoot fresh weight",
    x = "Box",
    y = "Value (g)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    legend.position = "bottom"
  )

ggsave(file.path(pub_plot_dir, "02_box_detail_fruit_and_shoot.png"), 
       p_box_detail, width = 11, height = 7, dpi = 300)

# ====================================================================
# 9. PUBLICATION PLOT 3: SCATTER - FRUIT VS SHOOT FRESH (KEY ANALYSIS)
# ====================================================================

scatter_data <- box_means %>%
  mutate(
    treatment_color = recode(treatment,
      "Control (no salt)" = "#66C2A5",
      "Salt control (no extract)" = "#FC8D62",
      "Salicornia extract (salt stress)" = "#8DA0CB",
      "Strandaster extract (salt stress)" = "#E78AC3"
    )
  )

p_scatter_main <- ggplot(scatter_data, aes(x = fruit_weight, y = biomass_fresh, 
                                            color = treatment, label = box)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_text(vjust = -1.2, hjust = 0.5, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = color_palette, name = "Treatment") +
  labs(
    title = "Fruit vs shoot fresh weight (box means)",
    x = "Fruit fresh weight (g)",
    y = "Shoot fresh weight (g)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90")
  )

ggsave(file.path(pub_plot_dir, "03_scatter_fruit_vs_shoot_fresh.png"), 
       p_scatter_main, width = 9, height = 7, dpi = 300)

# ====================================================================
# 10. PUBLICATION PLOT 4: RESPONSE COMPARISON (EXTRACT EFFECT)
# ====================================================================

# Compare extracts under salt stress
extract_comparison <- box_means %>%
  filter(grepl("extract", tolower(treatment))) %>%
  select(box, treatment, treatment_abbrev, fruit_weight, biomass_fresh, biomass_dry) %>%
  pivot_longer(c(fruit_weight, biomass_fresh, biomass_dry), 
               names_to = "response", values_to = "value") %>%
  mutate(
    response_label = recode(response,
      fruit_weight = "Fruit fresh weight",
      biomass_fresh = "Shoot fresh weight",
      biomass_dry = "Shoot dry weight"
    )
  )

p_extract_comp <- ggplot(extract_comparison, aes(x = treatment_abbrev, y = value, 
                                                   fill = treatment)) +
  geom_col(alpha = 0.85, color = "black", linewidth = 0.3) +
  facet_wrap(~response_label, scales = "free_y") +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(
    title = "Extract treatments under salt stress: Response comparison",
    x = "Extract type",
    y = "Value (g)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90")
  )

ggsave(file.path(pub_plot_dir, "04_extract_comparison_under_salt.png"), 
       p_extract_comp, width = 10, height = 7, dpi = 300)

# ====================================================================
# 11. PUBLICATION PLOT 5: CONTROL CONTRAST (SALT EFFECT)
# ====================================================================

# Compare controls
control_comparison <- box_means %>%
  filter(grepl("control", tolower(treatment))) %>%
  select(box, treatment, treatment_abbrev, fruit_weight, biomass_fresh, biomass_dry, root_dry_weight) %>%
  pivot_longer(c(fruit_weight, biomass_fresh, biomass_dry, root_dry_weight), 
               names_to = "response", values_to = "value") %>%
  mutate(
    response_label = recode(response,
      fruit_weight = "Fruit fresh weight",
      biomass_fresh = "Shoot fresh weight",
      biomass_dry = "Shoot dry weight",
      root_dry_weight = "Root dry weight"
    )
  )

p_control_comp <- ggplot(control_comparison, aes(x = treatment_abbrev, y = value, 
                                                   fill = treatment)) +
  geom_col(alpha = 0.85, color = "black", linewidth = 0.3) +
  geom_text(aes(label = round(value, 0)), vjust = -0.4, size = 3.5, fontface = "bold") +
  facet_wrap(~response_label, scales = "free_y") +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(
    title = "Control treatments: Salt stress effect",
    subtitle = "CTRL0 = no salt; SALT_CTRL = salt without extract",
    x = "Control type",
    y = "Value (g)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90")
  )

ggsave(file.path(pub_plot_dir, "05_control_comparison_salt_effect.png"), 
       p_control_comp, width = 11, height = 8, dpi = 300)

# ====================================================================
# 12. SUMMARY REPORT
# ====================================================================

report_text <- c(
  "# Vorpraktikum Publication Tables and Plots",
  "",
  "## Overview",
  "This script generates professional-quality tables and figures for thesis/university presentation.",
  "All files are saved in English with clear formatting.",
  "",
  "## Generated Tables",
  "",
  "### 01_detailed_all_measurements.csv",
  "Complete record of all measured values with treatment and response variable information.",
  sprintf("- Total measurements: %d", nrow(detailed_data)),
  "",
  "### 02_summary_by_treatment.csv",
  "Mean values for each response variable by treatment group.",
  sprintf("- Treatments: %d", n_distinct(treatment_means$treatment)),
  sprintf("- Response variables: 4 (fruit, shoot fresh, shoot dry, root dry)",
  "",
  "### 03_box_level_summary.csv",
  "Individual box means for all response variables.",
  sprintf("- Boxes: %d", nrow(box_means)),
  "",
  "### 04_treatment_comparison_table.csv",
  "Pairwise comparisons between treatments showing differences and percent change.",
  sprintf("- Total pairwise comparisons: %d", nrow(comparison_data)),
  "",
  "### 05_statistical_summary_ANOVA.csv",
  "ANOVA results for each response variable with effect sizes.",
  sprintf("- Analyses: %d", nrow(anova_combined)),
  "",
  "## Generated Plots (300 dpi, publication-ready)",
  "",
  "### 01_panel_all_responses_by_treatment.png",
  "Combined 2×2 panel showing all four response variables by treatment group.",
  "- Bars: treatment means",
  "- Error bars: 95% confidence intervals",
  "- Best for: overview presentations and thesis chapters",
  "",
  "### 02_box_detail_fruit_and_shoot.png",
  "Detailed box-level values for fruit and shoot fresh weight.",
  "- Each box labeled with its value",
  "- Separated by treatment type",
  "- Best for: detailed discussion of results",
  "",
  "### 03_scatter_fruit_vs_shoot_fresh.png",
  "Relationship between fruit and shoot fresh weight across all boxes.",
  "- Color-coded by treatment",
  "- Box labels shown",
  "- Best for: exploring correlations between responses",
  "",
  "### 04_extract_comparison_under_salt.png",
  "Comparison of Salicornia and Strandaster extracts under salt stress.",
  "- Shows all three responses (fruit, shoot fresh, shoot dry)",
  "- Direct comparison of extract effectiveness",
  "- Best for: extract efficacy discussion",
  "",
  "### 05_control_comparison_salt_effect.png",
  "Comparison of controls showing salt stress impact.",
  "- CTRL0 vs SALT_CTRL contrast",
  "- All four response variables shown",
  "- Best for: demonstrating salt stress baseline",
  "",
  "## Color Scheme (consistent across all plots)",
  "- Control (no salt): #66C2A5 (teal)",
  "- Salt control (no extract): #FC8D62 (orange)",
  "- Salicornia extract: #8DA0CB (blue)",
  "- Strandaster extract: #E78AC3 (pink)",
  "",
  "## Files Location",
  paste("Tables:", pub_dir),
  paste("Plots:", pub_plot_dir),
  "",
  "## Recommendations for Use",
  "1. **For thesis chapters:** Use panel plots (01) as overview figures",
  "2. **For methods/results:** Use box detail plot (02) with statistical results from tables",
  "3. **For discussion:** Use scatter plots (03) to discuss correlations",
  "4. **For extract comparison:** Use plot 04 with comparison table (04)",
  "5. **For salt effect baseline:** Use control comparison (05) with ANOVA results",
  "",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
)

writeLines(report_text, file.path(pub_dir, "README.txt"))

cat("\n✓ Publication tables and plots generated successfully!\n")
cat(sprintf("✓ Tables saved to: %s\n", pub_dir))
cat(sprintf("✓ Plots saved to: %s\n", pub_plot_dir))
cat(sprintf("✓ Total files created: 10 tables + 5 plots\n"))
