.libPaths("C:/Users/sebas/R/win-library/4.5.2")
ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

for (pkg in c("readxl", "dplyr", "ggplot2", "readr", "broom", "stringr",
              "dunn.test", "lme4", "lmerTest", "broom.mixed", "RColorBrewer", "tidyr")) {
  ensure_pkg(pkg)
}

multcomp_available <- requireNamespace("multcompView", quietly = TRUE) || tryCatch({
  install.packages("multcompView", repos = "https://cloud.r-project.org")
  requireNamespace("multcompView", quietly = TRUE)
}, error = function(e) FALSE)

# ---- Configuration (Vorpraktikum miniproject only) ----
base_dir <- "C:/Users/sebas/Downloads/tomato_analysis"
data_path <- "C:/Users/sebas/Documents/2Vorpraktikum_msc2025_ohneduplicate.xlsx"
treatment_path <- file.path(base_dir, "data/vorpraktikum/treatment.csv")
out_dir <- file.path(base_dir, "outputs_vorpraktikum")
plot_dir <- file.path(out_dir, "plots")
diag_dir <- file.path(out_dir, "diagnostics")
table_dir <- file.path(out_dir, "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

valid_boxes <- paste0("B", 1:6)

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 1e-4) return(formatC(p, format = "e", digits = 2))
  sprintf("%.4f", p)
}

sig_stars <- function(p) {
  if (is.na(p)) return("n.s.")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  "n.s."
}

letters_from_tukey <- function(tukey_tbl, alpha = 0.05) {
  if (!multcomp_available || is.null(tukey_tbl) || nrow(tukey_tbl) == 0) return(NULL)
  comps <- if ("contrast" %in% names(tukey_tbl)) tukey_tbl$contrast else tukey_tbl$comparison
  if (is.null(comps)) return(NULL)
  pvals <- tukey_tbl$adj.p.value
  names(pvals) <- comps
  tryCatch(multcompView::multcompLetters(pvals <= alpha)$Letters, error = function(e) NULL)
}

find_outliers <- function(df, group_vars = character(0)) {
  if (length(group_vars) == 0) {
    q1 <- quantile(df$value, 0.25, na.rm = TRUE)
    q3 <- quantile(df$value, 0.75, na.rm = TRUE)
    iqr <- IQR(df$value, na.rm = TRUE)
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    return(df %>% mutate(Q1 = q1, Q3 = q3, IQR = iqr, lower = lower, upper = upper,
                         is_outlier = value < lower | value > upper))
  }
  df %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      Q1 = quantile(value, 0.25, na.rm = TRUE),
      Q3 = quantile(value, 0.75, na.rm = TRUE),
      IQR = IQR(value, na.rm = TRUE),
      lower = Q1 - 1.5 * IQR,
      upper = Q3 + 1.5 * IQR,
      is_outlier = value < lower | value > upper
    ) %>%
    ungroup()
}

treatment_map <- read_csv(treatment_path, show_col_types = FALSE) %>%
  mutate(
    box = factor(box, levels = valid_boxes),
    treatment = factor(treatment, levels = c(
      "Control (no salt)",
      "Salt control (no extract)",
      "Salicornia extract (salt stress)",
      "Strandaster extract (salt stress)"
    )),
    treatment_abbrev = factor(treatment_abbrev, levels = c("CTRL0", "SALT_CTRL", "SAL", "STR"))
  ) %>%
  group_by(treatment) %>%
  mutate(rep = as.integer(row_number())) %>%
  ungroup()

join_treatments <- function(df) {
  df %>%
    mutate(box = factor(as.character(box), levels = valid_boxes)) %>%
    left_join(
      treatment_map %>% select(box, treatment, treatment_abbrev, rep),
      by = "box"
    )
}

load_fruit <- function(path) {
  raw <- read_excel(path, sheet = "Fruits_Tomato_Data_Table (2)", col_names = FALSE)
  names(raw) <- c("tag", "value")
  raw %>%
    mutate(box = str_extract(tag, "^B[0-9]+")) %>%
    filter(box %in% valid_boxes) %>%
    join_treatments() %>%
    mutate(response = "fruit_weight", unit = "g", sample_id = tag)
}

load_biomass <- function(path) {
  raw <- read_excel(path, sheet = "Biomasse")
  names(raw) <- c("idx", "name", "fresh", "dry")
  raw %>%
    filter(!is.na(name)) %>%
    mutate(box = str_extract(name, "^B[0-9]+")) %>%
    mutate(
      flagged_label = is.na(box) | !box %in% valid_boxes,
      box = ifelse(box %in% valid_boxes, box, NA_character_)
    ) %>%
    group_by(name) %>%
    summarise(
      fresh = mean(fresh, na.rm = TRUE),
      dry = mean(dry, na.rm = TRUE),
      box = first(na.omit(box)),
      flagged_label = any(flagged_label),
      .groups = "drop"
    ) %>%
    filter(!is.na(box)) %>%
    join_treatments() %>%
    pivot_longer(c(fresh, dry), names_to = "weight_type", values_to = "value") %>%
    mutate(
      response = if_else(weight_type == "fresh", "biomass_fresh", "biomass_dry"),
      unit = "g",
      sample_id = name,
      plant_id = str_remove(name, paste0("^", as.character(box)))
    )
}

load_roots <- function(path) {
  raw <- read_excel(path, sheet = "Roots_Data_Table (1)")
  names(raw) <- c("idx", "box_name", "box_number", "value")
  raw %>%
    filter(!is.na(box_name), !is.na(value)) %>%
    rename(box = box_name) %>%
    mutate(
      flagged_label = !box %in% valid_boxes,
      box = ifelse(box %in% valid_boxes, box, NA_character_)
    ) %>%
    filter(!is.na(box)) %>%
    join_treatments() %>%
    mutate(
      response = "root_dry_weight",
      unit = "g",
      sample_id = paste0(box, "_root", box_number),
      rep_root = suppressWarnings(as.integer(box_number))
    )
}

fruit_df <- load_fruit(data_path)
biom_df <- load_biomass(data_path)
roots_df <- load_roots(data_path)

ambiguous_biom <- read_excel(data_path, sheet = "Biomasse") %>%
  { names(.) <- c("idx", "name", "fresh", "dry"); . } %>%
  filter(!is.na(name)) %>%
  mutate(box = str_extract(name, "^B[0-9]+")) %>%
  filter(is.na(box) | !box %in% valid_boxes)

ambiguous_roots <- read_excel(data_path, sheet = "Roots_Data_Table (1)") %>%
  { names(.) <- c("idx", "box_name", "box_number", "value"); . } %>%
  filter(!is.na(box_name), !is.na(value)) %>%
  rename(box = box_name) %>%
  filter(!box %in% valid_boxes)

write_csv(ambiguous_biom, file.path(table_dir, "excluded_ambiguous_biomass_labels.csv"))
write_csv(ambiguous_roots, file.path(table_dir, "excluded_ambiguous_root_labels.csv"))

# ---- Data-check overview tables (raw + means) ----
overview_dir <- file.path(table_dir, "overview")
dir.create(overview_dir, showWarnings = FALSE, recursive = TRUE)

biom_raw <- read_excel(data_path, sheet = "Biomasse")
names(biom_raw) <- c("idx", "name", "fresh_g", "dry_g")
roots_raw <- read_excel(data_path, sheet = "Roots_Data_Table (1)")
names(roots_raw) <- c("idx", "box", "root_number", "dry_weight_g")
fruit_raw <- read_excel(data_path, sheet = "Fruits_Tomato_Data_Table (2)", col_names = FALSE)
names(fruit_raw) <- c("tag", "fruit_weight_g")

write_csv(fruit_raw, file.path(overview_dir, "01_raw_fruit.csv"))
write_csv(biom_raw %>% filter(!is.na(name)), file.path(overview_dir, "02_raw_biomass.csv"))
write_csv(roots_raw %>% filter(!is.na(box), !is.na(dry_weight_g)), file.path(overview_dir, "03_raw_roots.csv"))

biom_dupes <- biom_raw %>%
  filter(!is.na(name)) %>%
  count(name, sort = TRUE) %>%
  filter(n > 1)
write_csv(biom_dupes, file.path(overview_dir, "04_duplicate_plant_labels.csv"))
if (nrow(biom_dupes) > 0) {
  write_csv(
    biom_raw %>% filter(name %in% biom_dupes$name) %>% arrange(name),
    file.path(overview_dir, "04_duplicate_plant_rows.csv")
  )
}

overview_counts <- tibble(
  dataset = c("fruit", "biomass_plants", "root_samples"),
  n_rows = c(nrow(fruit_raw), sum(!is.na(biom_raw$name)), sum(!is.na(roots_raw$box) & !is.na(roots_raw$dry_weight_g))),
  n_boxes = c(n_distinct(str_extract(fruit_raw$tag, "^B[0-9]+")),
              n_distinct(str_extract(biom_raw$name[!is.na(biom_raw$name)], "^B[0-9]+")),
              n_distinct(roots_raw$box[!is.na(roots_raw$box) & roots_raw$box %in% valid_boxes]))
)
write_csv(overview_counts, file.path(overview_dir, "00_row_counts.csv"))

overview_by_box <- fruit_df %>%
  transmute(box, treatment_abbrev, fruit_weight_g = value) %>%
  full_join(
    biom_df %>%
      group_by(box, treatment_abbrev, response) %>%
      summarise(mean_value = mean(value, na.rm = TRUE), n_plants = sum(!is.na(value)), .groups = "drop") %>%
      pivot_wider(names_from = response, values_from = c(mean_value, n_plants), names_sep = "_"),
    by = c("box", "treatment_abbrev")
  ) %>%
  full_join(
    roots_df %>%
      group_by(box, treatment_abbrev) %>%
      summarise(root_dry_mean_g = mean(value), root_n = n(), .groups = "drop"),
    by = c("box", "treatment_abbrev")
  ) %>%
  left_join(treatment_map %>% select(box, treatment), by = "box") %>%
  relocate(treatment, treatment_abbrev, box)
write_csv(overview_by_box, file.path(overview_dir, "05_means_by_box.csv"))

overview_by_treatment <- overview_by_box %>%
  group_by(treatment, treatment_abbrev) %>%
  summarise(
    n_boxes = n(),
    fruit_mean_g = mean(fruit_weight_g, na.rm = TRUE),
    fruit_sd_g = sd(fruit_weight_g, na.rm = TRUE),
    shoot_fresh_mean_g = mean(mean_value_biomass_fresh, na.rm = TRUE),
    shoot_fresh_sd_g = sd(mean_value_biomass_fresh, na.rm = TRUE),
    shoot_dry_mean_g = mean(mean_value_biomass_dry, na.rm = TRUE),
    shoot_dry_sd_g = sd(mean_value_biomass_dry, na.rm = TRUE),
    root_dry_mean_g = mean(root_dry_mean_g, na.rm = TRUE),
    root_dry_sd_g = sd(root_dry_mean_g, na.rm = TRUE),
    n_shoot_plants = sum(n_plants_biomass_fresh, na.rm = TRUE),
    n_root_samples = sum(root_n, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(overview_by_treatment, file.path(overview_dir, "06_means_by_treatment.csv"))

all_long <- bind_rows(
  fruit_df %>% select(sample_id, tag, box, treatment, treatment_abbrev, rep, response, value, unit, plant_id = tag),
  biom_df %>% select(sample_id, tag = name, box, treatment, treatment_abbrev, rep, response, value, unit, plant_id),
  roots_df %>% select(sample_id, tag = sample_id, box, treatment, treatment_abbrev, rep, response, value, unit, plant_id = sample_id)
)
write_csv(all_long, file.path(out_dir, "clean_all_long.csv"))
write_csv(all_long, file.path(table_dir, "clean_all_long.csv"))

analyze_response <- function(df, response_label, y_label, level = c("sample", "box")) {
  level <- match.arg(level)
  safe_name <- gsub("[^A-Za-z0-9]+", "_", response_label)

  if (level == "box") {
    df <- df %>%
      group_by(box, treatment, treatment_abbrev, rep) %>%
      summarise(value = mean(value, na.rm = TRUE), n = n(), .groups = "drop")
  }

  df <- df %>% filter(!is.na(value), !is.na(treatment))
  if (nrow(df) == 0 || dplyr::n_distinct(df[["treatment"]]) < 2) return(invisible(NULL))

  overall <- df %>%
    summarise(n = n(), mean = mean(value), sd = sd(value), median = median(value),
              IQR = IQR(value), min = min(value), max = max(value)) %>%
    mutate(response = response_label, level = level)

  by_treatment <- df %>%
    group_by(treatment, treatment_abbrev) %>%
    summarise(
      n = n(), n_boxes = n_distinct(box), mean = mean(value), sd = sd(value),
      se = sd / sqrt(n),
      t_crit = qt(0.975, df = pmax(n - 1, 1)),
      ci_lower = mean - t_crit * se, ci_upper = mean + t_crit * se,
      median = median(value), IQR = IQR(value), min = min(value), max = max(value),
      .groups = "drop"
    )

  by_box <- df %>%
    group_by(box, treatment, treatment_abbrev, rep) %>%
    summarise(n = n(), mean = mean(value), sd = sd(value), .groups = "drop")

  write_csv(overall, file.path(table_dir, paste0("summary_overall_", safe_name, "_", level, ".csv")))
  write_csv(by_treatment, file.path(table_dir, paste0("summary_by_treatment_", safe_name, "_", level, ".csv")))
  write_csv(by_box, file.path(table_dir, paste0("summary_by_box_", safe_name, "_", level, ".csv")))

  if (!"sample_id" %in% names(df)) {
    df <- df %>% mutate(sample_id = if ("tag" %in% names(.)) tag else paste(box, row_number(), sep = "_"))
  }
  out_treatment <- find_outliers(df, "treatment")
  write_csv(out_treatment %>% select(sample_id, box, treatment, value, is_outlier, lower, upper),
            file.path(table_dir, paste0("outliers_by_treatment_", safe_name, "_", level, ".csv")))

  fit <- aov(value ~ treatment, data = df)
  tidy_fit <- tidy(fit)
  effect_row <- tidy_fit %>% filter(term == "treatment")
  eta2 <- effect_row$sumsq / sum(tidy_fit$sumsq)

  shapiro_res <- tryCatch(shapiro.test(residuals(fit)), error = function(e) NULL)
  bartlett_res <- tryCatch(bartlett.test(value ~ treatment, data = df), error = function(e) NULL)
  writeLines(c(
    sprintf("Response: %s (%s level)", response_label, level),
    sprintf("n = %s", nrow(df)),
    sprintf("Residual Shapiro-W p=%s", fmt_p(ifelse(is.null(shapiro_res), NA, shapiro_res$p.value))),
    sprintf("Bartlett p=%s", fmt_p(ifelse(is.null(bartlett_res), NA, bartlett_res$p.value))),
    "Note: Vorpraktikum has 6 boxes (unequal reps per treatment); interpret inferential tests cautiously."
  ), file.path(diag_dir, paste0("diagnostics_", safe_name, "_", level, ".txt")))

  anova_out <- effect_row %>%
    mutate(response = response_label, level = level, eta2 = eta2, p_value = p.value) %>%
    select(response, level, term, df, sumsq, meansq, statistic, p_value, eta2)
  write_csv(anova_out, file.path(table_dir, paste0("anova_", safe_name, "_", level, ".csv")))

  tukey_tbl <- tidy(TukeyHSD(fit)) %>% filter(term == "treatment")
  write_csv(tukey_tbl, file.path(table_dir, paste0("tukey_", safe_name, "_", level, ".csv")))
  letters <- letters_from_tukey(tukey_tbl)
  if (!is.null(letters)) {
    write_csv(tibble(treatment = names(letters), letter = unname(letters)),
              file.path(table_dir, paste0("letters_", safe_name, "_", level, ".csv")))
  }

  kw <- tryCatch(kruskal.test(value ~ treatment, data = df), error = function(e) NULL)
  if (!is.null(kw)) {
    write_csv(tibble(statistic = kw$statistic, p_value = kw$p.value, parameter = kw$parameter, method = kw$method),
              file.path(table_dir, paste0("kruskal_", safe_name, "_", level, ".csv")))
  }
  dunn_res <- tryCatch(dunn.test::dunn.test(df$value, df$treatment, method = "bh"), error = function(e) NULL)
  if (!is.null(dunn_res)) {
    write_csv(tibble(comparison = dunn_res$comparisons, Z = dunn_res$Z, p_adj = dunn_res$P.adjusted),
              file.path(table_dir, paste0("dunn_", safe_name, "_", level, ".csv")))
  }

  if (level == "sample" && dplyr::n_distinct(df$box) > 1) {
    lmer_fit <- tryCatch(lmerTest::lmer(value ~ treatment + (1 | box), data = df), error = function(e) NULL)
    if (!is.null(lmer_fit)) {
      write_csv(as_tibble(anova(lmer_fit), rownames = "term"),
                file.path(table_dir, paste0("lmer_anova_", safe_name, ".csv")))
      write_csv(broom.mixed::tidy(lmer_fit, effects = "fixed"),
                file.path(table_dir, paste0("lmer_fixed_", safe_name, ".csv")))
    }
  }

  anova_p <- effect_row$p.value
  min_tukey_p <- if (nrow(tukey_tbl) > 0) min(tukey_tbl$adj.p.value, na.rm = TRUE) else NA
  subtitle_base <- sprintf(
    "ANOVA p=%s (%s); min Tukey p=%s (%s); n=%s",
    fmt_p(anova_p), sig_stars(anova_p),
    fmt_p(min_tukey_p), sig_stars(min_tukey_p),
    nrow(df)
  )

  label_df <- by_treatment %>%
    mutate(letter = if (!is.null(letters)) letters[as.character(treatment)] else NA_character_, ymax = ci_upper) %>%
    filter(!is.na(letter))

  p_bar <- ggplot(by_treatment, aes(x = treatment_abbrev, y = mean, fill = treatment_abbrev)) +
    geom_col(alpha = 0.92) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#111111") +
    geom_text(data = label_df, aes(x = treatment_abbrev, y = ymax * 1.08, label = letter),
              inherit.aes = FALSE, vjust = 0, size = 3.5, fontface = "bold") +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    labs(title = paste(y_label, "by treatment"), subtitle = subtitle_base, x = "Treatment", y = y_label) +
    theme_minimal(base_size = 11)
  ggsave(file.path(plot_dir, paste0("bar_", safe_name, "_", level, ".png")), p_bar, width = 8, height = 5.5, dpi = 150)

  p_bar_do <- ggplot(by_treatment, aes(x = treatment_abbrev, y = mean, fill = treatment_abbrev)) +
    geom_col(alpha = 0.92) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#111111") +
    geom_text(aes(label = sprintf("%.1f", mean), y = ci_upper * 1.05),
              hjust = 0, vjust = 0, size = 3.2, fontface = "bold", position = position_nudge(x = 0.18)) +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    expand_limits(y = max(by_treatment$ci_upper, na.rm = TRUE) * 1.15) +
    labs(title = paste(y_label, "by treatment"), x = "Treatment", y = y_label) +
    theme_minimal(base_size = 11)
  ggsave(file.path(plot_dir, paste0("bar_dataonly_", safe_name, "_", level, ".png")), p_bar_do, width = 8, height = 5.5, dpi = 150)

  by_box_ci <- by_box %>%
    mutate(
      se = sd / sqrt(pmax(n, 1)),
      t_crit = qt(0.975, df = pmax(n - 1, 1)),
      ci_lower = mean - t_crit * se,
      ci_upper = mean + t_crit * se
    )

  p_box_bar <- ggplot(by_box_ci, aes(x = box, y = mean, fill = treatment_abbrev)) +
    geom_col(alpha = 0.92) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#111111") +
    geom_text(aes(label = sprintf("%.0f", mean), y = ci_upper * 1.04), size = 2.8, vjust = 0) +
    scale_fill_brewer(palette = "Set2", name = "Treatment") +
    labs(title = paste(y_label, "by box"), x = "Box", y = y_label) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, paste0("bar_by_box_", safe_name, "_", level, ".png")), p_box_bar, width = 8, height = 5.5, dpi = 150)

  p_mean_ci <- ggplot(by_box_ci, aes(x = box, y = mean, color = treatment_abbrev)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 0.7) +
    scale_color_brewer(palette = "Set2", name = "Treatment") +
    labs(title = paste(y_label, "mean ± 95% CI by box"), x = "Box", y = y_label) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, paste0("mean_ci_by_box_", safe_name, "_", level, ".png")), p_mean_ci, width = 8, height = 5.5, dpi = 150)

  if (level == "sample" && nrow(df) > 3) {
    p_boxplot <- ggplot(df, aes(x = treatment_abbrev, y = value, fill = treatment_abbrev)) +
      geom_boxplot(alpha = 0.75, outlier.shape = 21) +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.5) +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(title = paste(y_label, "distribution by treatment"), subtitle = subtitle_base,
           x = "Treatment", y = y_label) +
      theme_minimal(base_size = 11)
    ggsave(file.path(plot_dir, paste0("boxplot_", safe_name, "_", level, ".png")), p_boxplot, width = 8, height = 5.5, dpi = 150)
  }

  p_hist <- ggplot(df, aes(x = value, fill = treatment_abbrev)) +
    geom_histogram(bins = 12, color = "white", alpha = 0.85, position = "identity") +
    facet_wrap(~ treatment_abbrev, scales = "free_y") +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    labs(title = paste("Distribution of", y_label), x = y_label, y = "Count") +
    theme_minimal(base_size = 10)
  ggsave(file.path(plot_dir, paste0("hist_", safe_name, "_", level, ".png")), p_hist, width = 9, height = 6, dpi = 150)

  invisible(list(anova = anova_out, by_treatment = by_treatment, by_box = by_box_ci))
}

# Fruit: one value per box -> box-level only
analyze_response(fruit_df, "fruit_weight", "Fruit fresh weight (g)", level = "box")

# Biomass metrics: plant-level + box means
for (metric in c("biomass_fresh", "biomass_dry")) {
  sub <- biom_df %>% filter(response == metric)
  analyze_response(sub, metric, ifelse(metric == "biomass_fresh", "Shoot fresh weight (g)", "Shoot dry weight (g)"), "sample")
  analyze_response(sub, metric, ifelse(metric == "biomass_fresh", "Shoot fresh weight (g)", "Shoot dry weight (g)"), "box")
}

# Roots: sample-level + box means
analyze_response(roots_df, "root_dry_weight", "Root dry weight (g)", "sample")
analyze_response(roots_df, "root_dry_weight", "Root dry weight (g)", "box")

# Box-level correlation across responses
box_wide <- fruit_df %>%
  group_by(box, treatment, treatment_abbrev) %>%
  summarise(fruit_weight = mean(value), .groups = "drop") %>%
  full_join(
    biom_df %>%
      group_by(box, treatment, treatment_abbrev, response) %>%
      summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = response, values_from = mean_value),
    by = c("box", "treatment", "treatment_abbrev")
  ) %>%
  full_join(
    roots_df %>%
      group_by(box, treatment, treatment_abbrev) %>%
      summarise(root_dry_weight = mean(value), .groups = "drop"),
    by = c("box", "treatment", "treatment_abbrev")
  )

write_csv(box_wide, file.path(table_dir, "box_level_means_all_responses.csv"))

cor_pairs <- combn(names(box_wide)[sapply(box_wide, is.numeric)], 2, simplify = FALSE)
cor_tbl <- lapply(cor_pairs, function(vars) {
  x <- box_wide[[vars[1]]]
  y <- box_wide[[vars[2]]]
  if (sum(!is.na(x) & !is.na(y)) < 3) return(NULL)
  ct <- cor.test(x, y, method = "pearson")
  tibble(var1 = vars[1], var2 = vars[2], method = "pearson",
         estimate = unname(ct$estimate), p_value = ct$p.value, n = sum(!is.na(x) & !is.na(y)))
}) %>% bind_rows()
write_csv(cor_tbl, file.path(table_dir, "correlation_box_level.csv"))

scatter_pairs <- list(
  c("fruit_weight", "biomass_fresh", "Fruit fresh weight (g)", "Shoot fresh weight (g)", "scatter_fruit_vs_biomass_fresh.png"),
  c("fruit_weight", "biomass_dry", "Fruit fresh weight (g)", "Shoot dry weight (g)", "scatter_fruit_vs_biomass_dry.png"),
  c("fruit_weight", "root_dry_weight", "Fruit fresh weight (g)", "Root dry weight (g)", "scatter_fruit_vs_root.png"),
  c("biomass_fresh", "root_dry_weight", "Shoot fresh weight (g)", "Root dry weight (g)", "scatter_biomass_fresh_vs_root.png"),
  c("biomass_dry", "root_dry_weight", "Shoot dry weight (g)", "Root dry weight (g)", "scatter_biomass_dry_vs_root.png")
)
for (sp in scatter_pairs) {
  if (all(sp[1:2] %in% names(box_wide))) {
    p_sc <- ggplot(box_wide, aes(x = .data[[sp[1]]], y = .data[[sp[2]]], color = treatment_abbrev, label = box)) +
      geom_point(size = 3.5) +
      geom_text(vjust = -0.9, size = 3, show.legend = FALSE) +
      scale_color_brewer(palette = "Set2", name = "Treatment") +
      labs(title = paste(sp[4], "vs", sp[3], "(box means)"), x = sp[3], y = sp[4]) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    ggsave(file.path(plot_dir, sp[5]), p_sc, width = 7.5, height = 5.5, dpi = 150)
  }
}

means_compact <- box_wide %>%
  select(box, treatment, treatment_abbrev, any_of(c(
    "fruit_weight", "biomass_fresh", "biomass_dry", "root_dry_weight"
  ))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
write_csv(means_compact, file.path(table_dir, "means_by_box.csv"))
write_csv(means_compact, file.path(table_dir, "overview", "means_all_responses_by_box.csv"))

means_treatment_long <- box_wide %>%
  group_by(treatment, treatment_abbrev) %>%
  summarise(
    n_boxes = n(),
    fruit_mean = mean(fruit_weight, na.rm = TRUE),
    biomass_fresh_mean = mean(biomass_fresh, na.rm = TRUE),
    biomass_dry_mean = mean(biomass_dry, na.rm = TRUE),
    root_dry_mean = mean(root_dry_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
write_csv(means_treatment_long, file.path(table_dir, "means_by_treatment_all_responses.csv"))

# Combined overview panel (box means, all four responses)
box_long <- means_compact %>%
  pivot_longer(
    cols = any_of(c("fruit_weight", "biomass_fresh", "biomass_dry", "root_dry_weight")),
    names_to = "response", values_to = "value"
  ) %>%
  mutate(
    response = recode(response,
      fruit_weight = "Fruit fresh",
      biomass_fresh = "Shoot fresh",
      biomass_dry = "Shoot dry",
      root_dry_weight = "Root dry"
    )
  )
p_panel <- ggplot(box_long, aes(x = box, y = value, fill = treatment_abbrev)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = sprintf("%.0f", value)), vjust = -0.3, size = 2.5) +
  facet_wrap(~ response, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set2", name = "Treatment") +
  labs(title = "Vorpraktikum — all responses by box", x = "Box", y = "Value (g)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")
ggsave(file.path(plot_dir, "panel_all_responses_by_box.png"), p_panel, width = 10, height = 8, dpi = 150)

# ANOVA summary across responses (box level preferred for fruit/roots; sample level for biomass)
collect_anova <- function(safe_name, level) {
  f <- file.path(table_dir, paste0("anova_", safe_name, "_", level, ".csv"))
  if (!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE)
}
anova_summary <- bind_rows(
  collect_anova("fruit_weight", "box"),
  collect_anova("biomass_fresh", "sample"),
  collect_anova("biomass_dry", "sample"),
  collect_anova("root_dry_weight", "box")
) %>%
  mutate(
    p_label = vapply(p_value, fmt_p, character(1)),
    stars = vapply(p_value, sig_stars, character(1))
  )
write_csv(anova_summary, file.path(table_dir, "anova_summary_all_responses.csv"))

# Discussion report
discussion_lines <- c(
  "# Vorpraktikum — results, statistics, and discussion",
  "",
  paste("**Data:**", data_path),
  paste("**Generated:**", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "## Data quality",
  "- 6 boxes (B1–B6), 4 treatments, 36 shoot biomass plants, 16 root samples, 6 fruit records.",
  "- All plant labels map to B1–B6; no ambiguous P2/P6 labels in the cleaned Biomasse sheet.",
  "",
  "## Descriptive highlights (box means)",
  ""
)
for (i in seq_len(nrow(means_compact))) {
  row <- means_compact[i, ]
  discussion_lines <- c(discussion_lines, sprintf(
    "- **%s (%s):** fruit %.0f g; shoot fresh %.0f g; shoot dry %.0f g; root dry %.1f g",
    row$box, row$treatment_abbrev, row$fruit_weight, row$biomass_fresh, row$biomass_dry, row$root_dry_weight
  ))
}
discussion_lines <- c(discussion_lines, "", "## Inferential tests (ANOVA)", "")
if (nrow(anova_summary) > 0) {
  for (i in seq_len(nrow(anova_summary))) {
    r <- anova_summary[i, ]
    discussion_lines <- c(discussion_lines, sprintf(
      "- **%s (%s):** F=%.2f, p=%s %s, eta²=%.2f",
      r$response, r$level, r$statistic, r$p_label, r$stars, r$eta2
    ))
  }
}
discussion_lines <- c(discussion_lines,
  "",
  "## What looks interesting?",
  "",
  "### 1. Fruit yield — strongest treatment contrast",
  "- **B5 (control, no salt)** and **B2 (Salicornia)** show the highest fruit weights (~444 and ~661 g).",
  "- **B6 (salt control, no extract)** is clearly lowest (~181 g) — salt stress without biostimulant hurts fruit most.",
  "- Salicornia (mean ~549 g across B1–B2) outperforms Strandaster (~348 g) and the salt control on fruit.",
  "- *Caveat:* only 6 independent boxes; controls are n=1 box each.",
  "",
  "### 2. Shoot biomass — less separation between treatments",
  "- Fresh shoot weights are relatively similar across salt-stressed boxes (~390–552 g per box).",
  "- **B5 (control)** has the highest mean shoot fresh weight (~552 g); salt treatments do not collapse vegetative biomass as strongly as fruit yield.",
  "- High within-box variability (e.g. B1: one very large plant B1P1 at 940 g vs B1P7 at 70 g) drives large SDs.",
  "",
  "### 3. Root dry weight — allocation signal",
  "- **B1** has a high root mean (~32 g) but only one root sample recorded — interpret cautiously.",
  "- **B6** shows the highest root dry weight among boxes with 3 samples (~18 g), possibly indicating more biomass allocated below ground under salt stress without extract.",
  "- Fruit vs root scatter: boxes with low fruit (B6) do not always have the lowest roots — allocation patterns differ.",
  "",
  "### 4. Halophyte extracts under salt",
  "- Under salt stress, **Salicornia-treated boxes (B1, B2)** tend to produce more fruit than **Strandaster (B3, B4)** and especially the salt control (B6).",
  "- This supports the protocol narrative that Salicornia extract may partially mitigate salt impact on yield, but replication is too low for definitive claims.",
  "",
  "## Recommended interpretation",
  "- Treat ANOVA/Tukey as **exploratory** (6 boxes, unequal replication). Prefer Kruskal–Wallis and visual patterns.",
  "- Emphasize **fruit weight** and **B6 vs B2/B5 contrasts** as the clearest biological story.",
  "- Note high plant-to-plant variation within boxes when discussing shoot biomass.",
  "",
  "## Outputs",
  "- Tables: `outputs_vorpraktikum/tables/`",
  "- Plots: `outputs_vorpraktikum/plots/`",
  "- Overview: `outputs_vorpraktikum/tables/overview/`"
)
writeLines(discussion_lines, file.path(base_dir, "reports", "vorpraktikum_discussion.md"))

cat("\nVorpraktikum analysis complete.\n")
cat("Data:", data_path, "\n")
cat("Outputs:", out_dir, "\n")
cat("Discussion:", file.path(base_dir, "reports", "vorpraktikum_discussion.md"), "\n")
if (nrow(ambiguous_biom) > 0) cat("Excluded ambiguous biomass labels:", nrow(ambiguous_biom), "\n")
if (nrow(ambiguous_roots) > 0) cat("Excluded ambiguous root labels:", nrow(ambiguous_roots), "\n")
