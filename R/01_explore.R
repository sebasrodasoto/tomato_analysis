.libPaths("C:/Users/sebas/R/win-library/4.5.2")
ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

ensure_pkg("readxl")
ensure_pkg("dplyr")
ensure_pkg("ggplot2")
ensure_pkg("readr")
ensure_pkg("broom")
ensure_pkg("stringr")
ensure_pkg("dunn.test")
ensure_pkg("lme4")
ensure_pkg("lmerTest")
ensure_pkg("broom.mixed")
ensure_pkg("RColorBrewer")
# multcompView for compact letter display
multcomp_available <- requireNamespace("multcompView", quietly = TRUE) || tryCatch({
  install.packages("multcompView", repos = "https://cloud.r-project.org")
  requireNamespace("multcompView", quietly = TRUE)
}, error = function(e) FALSE)

# ---- Configuration ----
# Main experiment only — do not point this script at Vorpraktikum data (see R/02_vorpraktikum.R).
base_dir <- "C:/Users/sebas/Downloads/tomato_analysis"
data_path <- "C:/Users/sebas/Downloads/2026Main_Versuch Harvest_Rodas.xlsx"
treatment_path <- file.path(base_dir, "data/raw/treatment.csv")
sheets <- c("Fruit w. weight", "Biomasse w. weight")
# Output structure
out_dir <- file.path(base_dir, "outputs")
plot_dir <- file.path(out_dir, "plots")
diag_dir <- file.path(out_dir, "diagnostics")
table_dir <- file.path(out_dir, "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

# Column names expected from the raw sheets
clean_names <- c("sample_nr", "tag", "box", "plant_nr", "value")
# Boxes to exclude (unreliable readings)
excluded_boxes <- c(9, 11)
# Shapes reused for replicate markers (boxes within a treatment)
rep_shapes <- c(16, 17, 15, 3, 7, 8, 0, 1, 2, 4)

# Helper: mean +/- 95% CI using normal approximation (no Hmisc dependency)
mean_cl_normal_local <- function(x, mult = 1.96) {
  m <- mean(x)
  se <- sd(x) / sqrt(length(x))
  data.frame(y = m, ymin = m - mult * se, ymax = m + mult * se)
}

# Helper: readable p-value formatting (avoids printing as 0)
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

make_abbrev <- function(treatment) {
  salt <- str_extract(treatment, "\\d+\\s*mM") %>% str_extract("\\d+")
  salt <- ifelse(is.na(salt), "0", salt)
  dose <- str_extract(treatment, "\\d+\\s*g/L") %>% str_extract("\\d+")
  prod <- case_when(
    str_detect(treatment, regex("Strandaster", ignore_case = TRUE)) ~ "STR",
    str_detect(treatment, regex("Salico", ignore_case = TRUE)) ~ "SAL",
    TRUE ~ "CTRL"
  )
  dose_part <- ifelse(is.na(dose), "", paste0("d", dose))
  paste0(prod, dose_part, "_NaCl", salt)
}

letters_from_tukey <- function(tukey_tbl, alpha = 0.05) {
  if (!multcomp_available) return(NULL)
  if (is.null(tukey_tbl) || nrow(tukey_tbl) == 0) return(NULL)
  # Expect comparison column like "A-B"
  comps <- tukey_tbl$contrast %||% tukey_tbl$comparison %||% tukey_tbl$term
  if (is.null(comps)) return(NULL)
  names_vec <- comps
  pvals <- tukey_tbl$adj.p.value
  names(pvals) <- names_vec
  tryCatch({
    multcompView::multcompLetters(pvals <= alpha)$Letters
  }, error = function(e) NULL)
}

load_treatment_map <- function(data_path, treatment_path) {
  sheets_avail <- excel_sheets(data_path)
  if ("Treatment" %in% sheets_avail) {
    read_excel(data_path, sheet = "Treatment", .name_repair = "unique")
  } else if (file.exists(treatment_path)) {
    read_csv(treatment_path, show_col_types = FALSE)
  } else {
    stop("No Treatment sheet in workbook and treatment file not found: ", treatment_path)
  }
}

# Treatment lookup with replicate id per treatment (paired boxes)
treatments <- load_treatment_map(data_path, treatment_path) %>%
  rename(box = Box, treatment = Treatment) %>%
  mutate(box = factor(box), treatment = factor(treatment)) %>%
  mutate(
    abbrev = make_abbrev(treatment),
    salt_mM_raw = str_extract(treatment, "\\d+\\s*mM"),
    salt_mM = suppressWarnings(as.numeric(str_extract(salt_mM_raw, "\\d+"))),
    salt_num = dplyr::coalesce(
      salt_mM,
      suppressWarnings(as.numeric(str_extract(treatment, "\\d+"))),
      0
    ),
    is_control = str_detect(treatment, regex("Control", ignore_case = TRUE))
  ) %>%
  arrange(salt_num, desc(is_control), abbrev) %>%
  mutate(
    treatment_abbrev = factor(abbrev, levels = unique(abbrev)),
    treatment = factor(treatment, levels = treatment[match(levels(treatment_abbrev), abbrev)])
  ) %>%
  group_by(treatment) %>%
  mutate(rep = row_number()) %>%
  ungroup()

find_outliers <- function(df, group_vars = character(0)) {
  # Flag outliers using 1.5*IQR rule, optionally within groups
  if (length(group_vars) == 0) {
    q1 <- quantile(df$value, 0.25, na.rm = TRUE)
    q3 <- quantile(df$value, 0.75, na.rm = TRUE)
    iqr <- IQR(df$value, na.rm = TRUE)
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    return(df %>%
      mutate(
        Q1 = q1, Q3 = q3, IQR = iqr,
        lower = lower, upper = upper,
        is_outlier = value < lower | value > upper
      ))
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

compute_correlation_table <- function(df, subset_label) {
  if (nrow(df) == 0) {
    return(tibble(
      subset = subset_label,
      method = c("pearson", "spearman"),
      estimate = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      n = 0
    ))
  }

  x <- df$fruit_value
  y <- df$biom_value
  pear <- cor.test(x, y, method = "pearson")
  spear <- tryCatch(cor.test(x, y, method = "spearman", exact = FALSE), error = function(e) NULL)

  tibble(
    subset = subset_label,
    method = c("pearson", "spearman"),
    estimate = c(unname(pear$estimate), if (!is.null(spear)) unname(spear$estimate) else NA_real_),
    p_value = c(pear$p.value, if (!is.null(spear)) spear$p.value else NA_real_),
    conf_low = c(ifelse(!is.null(pear$conf.int), pear$conf.int[1], NA_real_), NA_real_),
    conf_high = c(ifelse(!is.null(pear$conf.int), pear$conf.int[2], NA_real_), NA_real_),
    n = c(length(x), length(x))
  )
}

analyze_sheet <- function(sheet, treatment_map) {
  # Load and harmonize the response sheet, then attach treatment/replicate info
  df_raw <- read_excel(data_path, sheet = sheet, .name_repair = "unique")
  names(df_raw) <- clean_names
  df <- df_raw %>%
    filter(!box %in% excluded_boxes) %>%
    mutate(box = factor(box), tag = factor(tag)) %>%
    left_join(treatment_map, by = "box") %>%
    mutate(
      treatment = droplevels(factor(treatment, levels = levels(treatment_map$treatment))),
      treatment_abbrev = droplevels(factor(treatment_abbrev, levels = levels(treatment_map$treatment_abbrev)))
    )

  n_missing <- sum(is.na(df$value))
  if (n_missing > 0) {
    message(sprintf("Sheet %s: dropping %s rows with missing values", sheet, n_missing))
  }
  df <- df %>% filter(!is.na(value))

  out_treatment_flag <- find_outliers(df, c("treatment")) %>%
    select(sample_nr, outlier_treatment = is_outlier, lower_treatment = lower, upper_treatment = upper) %>%
    distinct()
  out_box_flag <- find_outliers(df, c("treatment", "box")) %>%
    select(sample_nr, outlier_box = is_outlier, lower_box = lower, upper_box = upper) %>%
    distinct()

  value <- df[["value"]]
  safe_sheet <- gsub("[^A-Za-z0-9]+", "_", sheet)

  overall <- tibble(
    sheet = sheet,
    n = nrow(df),
    mean = mean(value),
    sd = sd(value),
    median = median(value),
    IQR = IQR(value),
    min = min(value),
    max = max(value)
  )

  trt_lookup <- treatment_map %>%
    distinct(treatment, treatment_abbrev, salt_num, is_control)

  by_box <- df %>%
    group_by(box, treatment, rep) %>%
    summarise(
      n = n(),
      mean = mean(value),
      sd = sd(value),
      median = median(value),
      IQR = IQR(value),
      min = min(value),
      max = max(value),
      .groups = "drop"
    )

  by_treatment <- df %>%
    group_by(treatment, treatment_abbrev) %>%
    summarise(
      n = n(),
      n_boxes = n_distinct(box),
      mean = mean(value),
      sd = sd(value),
      se = sd / sqrt(n),
      t_crit = qt(0.975, df = pmax(n - 1, 1)),
      ci_lower = mean - t_crit * se,
      ci_upper = mean + t_crit * se,
      median = median(value),
      IQR = IQR(value),
      min = min(value),
      max = max(value),
      .groups = "drop"
    ) %>%
    left_join(trt_lookup, by = c("treatment", "treatment_abbrev")) %>%
    arrange(salt_num, desc(is_control), treatment_abbrev) %>%
    mutate(
      plot_label = case_when(
        is_control & salt_num == 0 ~ "Control (no salt)",
        is_control ~ paste0("Control ", salt_num, " mM"),
        TRUE ~ as.character(treatment_abbrev)
      ),
      plot_label = factor(plot_label, levels = unique(plot_label))
    )

  df_clean_export <- df %>%
    left_join(out_treatment_flag, by = "sample_nr") %>%
    left_join(out_box_flag, by = "sample_nr") %>%
    left_join(by_treatment %>% select(treatment, trt_mean = mean, trt_sd = sd), by = "treatment") %>%
    mutate(
      outlier_any = coalesce(outlier_treatment, FALSE) | coalesce(outlier_box, FALSE),
      zscore_within_treatment = ifelse(trt_sd == 0, NA_real_, (value - trt_mean) / trt_sd)
    ) %>%
    select(-trt_mean, -trt_sd)
  write_csv(df_clean_export, file.path(out_dir, paste0("clean_", safe_sheet, ".csv")))

  df_no_outliers <- df_clean_export %>% filter(!outlier_any)

  by_treatment_no_outliers <- df_no_outliers %>%
    group_by(treatment, treatment_abbrev) %>%
    summarise(
      n = n(),
      n_boxes = n_distinct(box),
      mean = mean(value),
      sd = sd(value),
      se = sd / sqrt(n),
      t_crit = qt(0.975, df = pmax(n - 1, 1)),
      ci_lower = mean - t_crit * se,
      ci_upper = mean + t_crit * se,
      median = median(value),
      IQR = IQR(value),
      min = min(value),
      max = max(value),
      .groups = "drop"
    )
  write_csv(by_treatment_no_outliers, file.path(out_dir, paste0("summary_by_treatment_no_outliers_", safe_sheet, ".csv")))
  write_csv(by_treatment_no_outliers, file.path(table_dir, paste0("summary_by_treatment_no_outliers_", safe_sheet, ".csv")))

  by_plant <- df_no_outliers %>%
    group_by(treatment, treatment_abbrev, box, rep, plant_nr, tag) %>%
    summarise(
      n_samples = n(),
      mean_value = mean(value),
      sum_value = sum(value),
      sd_value = sd(value),
      median_value = median(value),
      min_value = min(value),
      max_value = max(value),
      .groups = "drop"
    )
  write_csv(by_plant, file.path(table_dir, paste0("summary_by_plant_", safe_sheet, ".csv")))

  by_plot <- df_no_outliers %>%
    group_by(treatment, treatment_abbrev, box, rep) %>%
    summarise(
      n_samples = n(),
      n_plants = n_distinct(plant_nr),
      total_value = sum(value),
      mean_value = mean(value),
      sd_value = sd(value),
      median_value = median(value),
      min_value = min(value),
      max_value = max(value),
      .groups = "drop"
    )
  write_csv(by_plot, file.path(table_dir, paste0("summary_plot_level_", safe_sheet, ".csv")))

  # One-way ANOVA by treatment with normality / variance checks
  fit <- aov(value ~ treatment, data = df)
  tidy_fit <- tidy(fit)
  effect_row <- tidy_fit %>% filter(term == "treatment")
  eta2 <- effect_row$sumsq / sum(tidy_fit$sumsq)

  resid_vals <- residuals(fit)
  shapiro_res <- tryCatch(shapiro.test(resid_vals), error = function(e) NULL)
  bartlett_res <- tryCatch(bartlett.test(value ~ treatment, data = df), error = function(e) NULL)

  diag_lines <- c(
    sprintf("Sheet: %s", sheet),
    sprintf("Residual Shapiro-W p=%s", fmt_p(ifelse(is.null(shapiro_res), NA, shapiro_res$p.value))),
    sprintf("Bartlett p=%s", fmt_p(ifelse(is.null(bartlett_res), NA, bartlett_res$p.value)))
  )
  diag_file <- file.path(diag_dir, paste0("diagnostics_", safe_sheet, ".txt"))
  writeLines(diag_lines, diag_file)

  anova_out <- tibble(
    sheet = sheet,
    term = effect_row$term,
    df = effect_row$df,
    sumsq = effect_row$sumsq,
    meansq = effect_row$meansq,
    statistic = effect_row$statistic,
    p_value = effect_row$p.value,
    eta2 = eta2
  )
  write_csv(anova_out, file.path(table_dir, paste0("anova_", safe_sheet, ".csv")))

  tukey <- TukeyHSD(fit)
  tukey_tbl <- tidy(tukey) %>% filter(term == "treatment")
  write_csv(tukey_tbl, file.path(table_dir, paste0("tukey_", safe_sheet, ".csv")))
  letters <- letters_from_tukey(tukey_tbl)
  if (!is.null(letters)) {
    letters_df <- tibble(treatment = names(letters), letter = unname(letters))
    write_csv(letters_df, file.path(table_dir, paste0("letters_", safe_sheet, ".csv")))
  }

  # Descriptive summaries
  write_csv(overall, file.path(out_dir, paste0("summary_overall_", safe_sheet, ".csv")))
  write_csv(by_box, file.path(out_dir, paste0("summary_by_box_", safe_sheet, ".csv")))
  write_csv(by_treatment, file.path(out_dir, paste0("summary_by_treatment_", safe_sheet, ".csv")))
  write_csv(by_treatment, file.path(table_dir, paste0("summary_by_treatment_", safe_sheet, ".csv")))

  # Replicate comparison table: box-level stats and deviation vs treatment mean
  rep_compare <- by_box %>%
    left_join(by_treatment %>% select(treatment, trt_mean = mean, trt_sd = sd), by = "treatment") %>%
    mutate(
      se = sd / sqrt(n),
      t_crit = qt(0.975, df = pmax(n - 1, 1)),
      ci_lower = mean - t_crit * se,
      ci_upper = mean + t_crit * se,
      diff_from_trt_mean = mean - trt_mean,
      zscore_within_trt = ifelse(trt_sd == 0, NA_real_, (mean - trt_mean) / trt_sd)
    ) %>%
    select(treatment, box, rep, n, mean, sd, se, ci_lower, ci_upper, diff_from_trt_mean, zscore_within_trt)
  write_csv(rep_compare, file.path(table_dir, paste0("replicate_compare_", safe_sheet, ".csv")))

  # Non-parametric: Kruskal-Wallis + Dunn (BH)
  kw <- tryCatch(kruskal.test(value ~ treatment, data = df), error = function(e) NULL)
  if (!is.null(kw)) {
    kw_out <- tibble(statistic = kw$statistic, p_value = kw$p.value, parameter = kw$parameter, method = kw$method)
    write_csv(kw_out, file.path(table_dir, paste0("kruskal_", safe_sheet, ".csv")))
  }
  dunn_res <- tryCatch(dunn.test::dunn.test(df$value, df$treatment, method = "bh"), error = function(e) NULL)
  if (!is.null(dunn_res)) {
    dunn_tbl <- tibble(
      comparison = dunn_res$comparisons,
      Z = dunn_res$Z,
      p_adj = dunn_res$P.adjusted
    )
    write_csv(dunn_tbl, file.path(table_dir, paste0("dunn_", safe_sheet, ".csv")))
  }

  # Mixed model: random intercept for box, fixed treatment
  lmer_fit <- tryCatch(lmerTest::lmer(value ~ treatment + (1 | box), data = df), error = function(e) NULL)
  if (!is.null(lmer_fit)) {
    lmer_anova <- as_tibble(anova(lmer_fit), rownames = "term")
    write_csv(lmer_anova, file.path(table_dir, paste0("lmer_anova_", safe_sheet, ".csv")))
    lmer_fixed <- broom.mixed::tidy(lmer_fit, effects = "fixed")
    write_csv(lmer_fixed, file.path(table_dir, paste0("lmer_fixed_", safe_sheet, ".csv")))
  }

  # Outlier detection (1.5*IQR): overall, by treatment, by treatment+box
  out_overall <- find_outliers(df, character(0))
  out_by_treatment <- find_outliers(df, c("treatment"))
  out_by_box <- find_outliers(df, c("treatment", "box"))
  write_csv(out_overall %>% select(sample_nr, box, treatment, value, is_outlier, lower, upper), file.path(table_dir, paste0("outliers_overall_", safe_sheet, ".csv")))
  write_csv(out_by_treatment %>% select(sample_nr, box, treatment, value, is_outlier, lower, upper), file.path(table_dir, paste0("outliers_by_treatment_", safe_sheet, ".csv")))
  write_csv(out_by_box %>% select(sample_nr, box, treatment, value, is_outlier, lower, upper), file.path(table_dir, paste0("outliers_by_box_", safe_sheet, ".csv")))

  min_tukey_p <- if (nrow(tukey_tbl) > 0) min(tukey_tbl$adj.p.value, na.rm = TRUE) else NA
  anova_p <- effect_row$p.value
  boxes_per_trt <- by_treatment %>%
    mutate(label = paste0(treatment_abbrev, " (n_boxes=", n_boxes, ")")) %>%
    pull(label) %>%
    paste(collapse = "; ")

  label_df <- by_treatment %>%
    mutate(
      letter = if (!is.null(letters)) letters[as.character(treatment)] else NA_character_,
      ymax = ci_upper
    ) %>%
    filter(!is.na(letter)) %>%
    select(plot_label, letter, ymax)

  subtitle_base <- sprintf(
    "ANOVA p=%s (%s); min Tukey p=%s (%s); boxes: %s",
    fmt_p(anova_p), sig_stars(anova_p),
    fmt_p(min_tukey_p), sig_stars(min_tukey_p),
    boxes_per_trt
  )

  p_box <- ggplot(by_treatment, aes(x = plot_label, y = mean, fill = treatment_abbrev)) +
    geom_col(alpha = 0.92) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#111111") +
    geom_text(
      data = label_df,
      aes(x = plot_label, y = ymax * 1.05, label = letter),
      inherit.aes = FALSE,
      vjust = 0,
      size = 3.5,
      fontface = "bold"
    ) +
    scale_fill_brewer(palette = "Set3", guide = "none") +
    labs(
      title = paste("Mean ±95% CI by treatment", sheet),
      subtitle = subtitle_base,
      x = "Treatment (abbrev)",
      y = sheet
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  ggsave(file.path(plot_dir, paste0("boxplot_", safe_sheet, ".png")), p_box, width = 9, height = 6, dpi = 150)

  p_box_dataonly <- ggplot(by_treatment, aes(x = treatment_abbrev, y = mean, fill = treatment_abbrev)) +
    geom_col(alpha = 0.92) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#111111") +
    geom_text(
      aes(label = sprintf("%.1f g", mean), y = ci_upper * 1.05, color = treatment_abbrev),
      hjust = 0, vjust = 0, size = 3.5, fontface = "bold",
      position = position_nudge(x = 0.22)
    ) +
    scale_fill_brewer(palette = "Set3", guide = "none") +
    scale_color_brewer(palette = "Set3", guide = "none") +
    labs(
      title = paste("Mean ±95% CI by treatment", sheet),
      x = "Treatment (abbrev)",
      y = sheet
    ) +
    expand_limits(y = max(by_treatment$ci_upper, na.rm = TRUE) * 1.15) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  ggsave(file.path(plot_dir, paste0("boxplot_dataonly_", safe_sheet, ".png")), p_box_dataonly, width = 9, height = 6, dpi = 150)

  p_mean_ci <- ggplot(by_treatment, aes(x = plot_label, y = mean, color = treatment_abbrev)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 0.7) +
    geom_text(
      data = label_df,
      aes(x = plot_label, y = ymax * 1.05, label = letter),
      inherit.aes = FALSE,
      vjust = 0,
      size = 3.5,
      fontface = "bold"
    ) +
    scale_color_brewer(palette = "Set3", name = "Treatment (abbr.)") +
    labs(
      title = paste("Mean ±95% CI by treatment", sheet),
      subtitle = subtitle_base,
      x = "Treatment (abbrev)",
      y = sheet,
      color = "Treatment (abbr.)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, paste0("mean_ci_by_treatment_", safe_sheet, ".png")), p_mean_ci, width = 9, height = 6, dpi = 150)

  p_mean_ci_dataonly <- ggplot(by_treatment, aes(x = treatment_abbrev, y = mean, color = treatment_abbrev)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 0.7) +
    geom_text(
      aes(label = sprintf("%.1f g", mean), y = ci_upper * 1.05, color = treatment_abbrev),
      hjust = 0, vjust = 0, size = 3.5, fontface = "bold",
      position = position_nudge(x = 0.22), show.legend = FALSE
    ) +
    scale_color_brewer(palette = "Set3", name = "Treatment (abbr.)") +
    labs(
      title = paste("Mean ±95% CI by treatment", sheet),
      x = "Treatment (abbrev)",
      y = sheet,
      color = "Treatment (abbr.)"
    ) +
    expand_limits(y = max(by_treatment$ci_upper, na.rm = TRUE) * 1.15) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, paste0("mean_ci_by_treatment_dataonly_", safe_sheet, ".png")), p_mean_ci_dataonly, width = 9, height = 6, dpi = 150)

  p_hist <- ggplot(df, aes(x = value)) +
    geom_histogram(bins = 20, fill = "#72B7B2", color = "white") +
    labs(title = paste("Distribution of", sheet), x = sheet, y = "Count") +
    theme_minimal(base_size = 12)
  ggsave(file.path(plot_dir, paste0("hist_", safe_sheet, ".png")), p_hist, width = 7, height = 5, dpi = 150)

  analyze_within_salt <- function(df_salt, salt_value, sheet_label) {
    if (nrow(df_salt) == 0) return(NULL)
    if (dplyr::n_distinct(df_salt$treatment) < 2) return(NULL)
    df_salt <- df_salt %>% mutate(treatment = droplevels(treatment), treatment_abbrev = droplevels(treatment_abbrev))
    suffix <- paste0("_salt", salt_value)
    by_treatment_salt <- df_salt %>%
      group_by(treatment, treatment_abbrev) %>%
      summarise(
        n = n(),
        n_boxes = n_distinct(box),
        mean = mean(value),
        sd = sd(value),
        se = sd / sqrt(n),
        t_crit = qt(0.975, df = pmax(n - 1, 1)),
        ci_lower = mean - t_crit * se,
        ci_upper = mean + t_crit * se,
        median = median(value),
        IQR = IQR(value),
        min = min(value),
        max = max(value),
        .groups = "drop"
      )
    write_csv(by_treatment_salt, file.path(table_dir, paste0("summary_by_treatment", suffix, "_", sheet_label, ".csv")))

    fit_salt <- aov(value ~ treatment, data = df_salt)
    tidy_salt <- tidy(fit_salt)
    effect_row_salt <- tidy_salt %>% filter(term == "treatment")
    eta2_salt <- effect_row_salt$sumsq / sum(tidy_salt$sumsq)

    anova_out_salt <- tibble(
      sheet = sheet_label,
      salt = salt_value,
      term = effect_row_salt$term,
      df = effect_row_salt$df,
      sumsq = effect_row_salt$sumsq,
      meansq = effect_row_salt$meansq,
      statistic = effect_row_salt$statistic,
      p_value = effect_row_salt$p.value,
      eta2 = eta2_salt
    )
    write_csv(anova_out_salt, file.path(table_dir, paste0("anova", suffix, "_", sheet_label, ".csv")))

    tukey_salt <- TukeyHSD(fit_salt)
    tukey_tbl_salt <- tidy(tukey_salt) %>% filter(term == "treatment")
    write_csv(tukey_tbl_salt, file.path(table_dir, paste0("tukey", suffix, "_", sheet_label, ".csv")))
    letters_salt <- letters_from_tukey(tukey_tbl_salt)

    label_df_salt <- by_treatment_salt %>%
      mutate(
        letter = if (!is.null(letters_salt)) letters_salt[as.character(treatment)] else NA_character_,
        ymax = ci_upper
      ) %>%
      filter(!is.na(letter)) %>%
      select(treatment_abbrev, letter, ymax)

    subtitle_salt <- sprintf(
      "Salt %s mM; ANOVA p=%s (%s); min Tukey p=%s (%s)",
      salt_value,
      fmt_p(effect_row_salt$p.value), sig_stars(effect_row_salt$p.value),
      fmt_p(if (nrow(tukey_tbl_salt) > 0) min(tukey_tbl_salt$adj.p.value, na.rm = TRUE) else NA),
      sig_stars(if (nrow(tukey_tbl_salt) > 0) min(tukey_tbl_salt$adj.p.value, na.rm = TRUE) else NA)
    )

    p_box_salt <- ggplot(by_treatment_salt, aes(x = treatment_abbrev, y = mean, fill = treatment_abbrev)) +
      geom_col(alpha = 0.92) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#111111") +
      geom_text(
        data = label_df_salt,
        aes(x = treatment_abbrev, y = ymax * 1.05, label = letter),
        inherit.aes = FALSE,
        vjust = 0,
        size = 3.5,
        fontface = "bold"
      ) +
      scale_fill_brewer(palette = "Set3", guide = "none") +
      labs(
        title = paste("Mean ±95% CI by treatment", sheet_label, paste0("(salt ", salt_value, " mM)")),
        subtitle = subtitle_salt,
        x = "Treatment (abbrev)",
        y = sheet_label
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    ggsave(file.path(plot_dir, paste0("boxplot", suffix, "_", safe_sheet, ".png")), p_box_salt, width = 9, height = 6, dpi = 150)

    p_box_salt_dataonly <- ggplot(by_treatment_salt, aes(x = treatment_abbrev, y = mean, fill = treatment_abbrev)) +
      geom_col(alpha = 0.92) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#111111") +
      geom_text(
        aes(label = sprintf("%.1f g", mean), y = ci_upper * 1.05, color = treatment_abbrev),
        hjust = 0, vjust = 0, size = 3.5, fontface = "bold",
        position = position_nudge(x = 0.22)
      ) +
      scale_fill_brewer(palette = "Set3", guide = "none") +
      scale_color_brewer(palette = "Set3", guide = "none") +
      labs(
        title = paste("Mean ±95% CI by treatment", sheet_label, paste0("(salt ", salt_value, " mM)")),
        x = "Treatment (abbrev)",
        y = sheet_label
      ) +
      expand_limits(y = max(by_treatment_salt$ci_upper, na.rm = TRUE) * 1.15) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    ggsave(file.path(plot_dir, paste0("boxplot_dataonly", suffix, "_", safe_sheet, ".png")), p_box_salt_dataonly, width = 9, height = 6, dpi = 150)

    p_mean_ci_salt <- ggplot(by_treatment_salt, aes(x = treatment_abbrev, y = mean, color = treatment_abbrev)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 0.7) +
      geom_text(
        data = label_df_salt,
        aes(x = treatment_abbrev, y = ymax * 1.05, label = letter),
        inherit.aes = FALSE,
        vjust = 0,
        size = 3.5,
        fontface = "bold"
      ) +
      scale_color_brewer(palette = "Set3", name = "Treatment (abbr.)") +
      labs(
        title = paste("Mean ±95% CI by treatment", sheet_label, paste0("(salt ", salt_value, " mM)")),
        subtitle = subtitle_salt,
        x = "Treatment (abbrev)",
        y = sheet_label,
        color = "Treatment (abbr.)"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    ggsave(file.path(plot_dir, paste0("mean_ci_by_treatment", suffix, "_", safe_sheet, ".png")), p_mean_ci_salt, width = 9, height = 6, dpi = 150)

    p_mean_ci_salt_dataonly <- ggplot(by_treatment_salt, aes(x = treatment_abbrev, y = mean, color = treatment_abbrev)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 0.7) +
      geom_text(
        aes(label = sprintf("%.1f g", mean), y = ci_upper * 1.05, color = treatment_abbrev),
        hjust = 0, vjust = 0, size = 3.5, fontface = "bold",
        position = position_nudge(x = 0.22), show.legend = FALSE
      ) +
      scale_color_brewer(palette = "Set3", name = "Treatment (abbr.)") +
      labs(
        title = paste("Mean ±95% CI by treatment", sheet_label, paste0("(salt ", salt_value, " mM)")),
        x = "Treatment (abbrev)",
        y = sheet_label,
        color = "Treatment (abbr.)"
      ) +
      expand_limits(y = max(by_treatment_salt$ci_upper, na.rm = TRUE) * 1.15) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    ggsave(file.path(plot_dir, paste0("mean_ci_by_treatment_dataonly", suffix, "_", safe_sheet, ".png")), p_mean_ci_salt_dataonly, width = 9, height = 6, dpi = 150)
  }

  df_salt_60 <- df %>% filter(salt_num == 60)
  df_salt_90 <- df %>% filter(salt_num == 90)
  analyze_within_salt(df_salt_60, 60, sheet)
  analyze_within_salt(df_salt_90, 90, sheet)

  invisible(list(
    overall = overall,
    by_box = by_box,
    by_treatment = by_treatment,
    anova = anova_out,
    tukey = tukey_tbl
  ))
}

results <- lapply(sheets, analyze_sheet, treatment_map = treatments)

fruit <- read_excel(data_path, sheet = sheets[1], .name_repair = "unique")
names(fruit) <- clean_names
biomass <- read_excel(data_path, sheet = sheets[2], .name_repair = "unique")
names(biomass) <- clean_names

fruit_clean <- fruit %>%
  filter(!box %in% excluded_boxes) %>%
  mutate(box = factor(box)) %>%
  left_join(treatments, by = "box") %>%
  transmute(sample_nr, tag, box, plant_nr, treatment, treatment_abbrev, fruit_value = value)

biomass_clean <- biomass %>%
  filter(!box %in% excluded_boxes) %>%
  mutate(box = factor(box)) %>%
  left_join(treatments, by = "box") %>%
  transmute(sample_nr, box, treatment, treatment_abbrev, biom_value = value)

fruit_outliers <- find_outliers(rename(fruit_clean, value = fruit_value), c("treatment")) %>%
  select(sample_nr, fruit_outlier = is_outlier)
biomass_outliers <- find_outliers(rename(biomass_clean, value = biom_value), c("treatment")) %>%
  select(sample_nr, biom_outlier = is_outlier)

merged <- fruit_clean %>%
  inner_join(biomass_clean %>% select(sample_nr, biom_value), by = "sample_nr") %>%
  left_join(fruit_outliers, by = "sample_nr") %>%
  left_join(biomass_outliers, by = "sample_nr") %>%
  mutate(
    fruit_outlier = coalesce(fruit_outlier, FALSE),
    biom_outlier = coalesce(biom_outlier, FALSE),
    outlier_any = fruit_outlier | biom_outlier
  ) %>%
  filter(!is.na(fruit_value), !is.na(biom_value))

merged_no_outliers <- merged %>% filter(!outlier_any)

cor_tbl <- bind_rows(
  compute_correlation_table(merged, "all (boxes excluded only)"),
  compute_correlation_table(merged_no_outliers, "no outliers (by treatment)")
)
write_csv(cor_tbl, file.path(out_dir, "correlation_fruit_vs_biomass.csv"))
write_csv(cor_tbl, file.path(table_dir, "correlation_fruit_vs_biomass.csv"))
write_csv(merged %>% select(sample_nr, treatment, treatment_abbrev, fruit_value, biom_value, fruit_outlier, biom_outlier, outlier_any),
  file.path(table_dir, "merged_fruit_biomass_with_outliers.csv")
)

lm_simple <- NULL
lm_treatment <- NULL
r2_simple <- NA_real_
if (nrow(merged_no_outliers) > 1) {
  lm_simple <- lm(biom_value ~ fruit_value, data = merged_no_outliers)
  lm_treatment <- lm(biom_value ~ fruit_value + treatment, data = merged_no_outliers)
  write_csv(tidy(lm_simple), file.path(table_dir, "lm_biomass_on_fruit.csv"))
  write_csv(glance(lm_simple), file.path(table_dir, "lm_biomass_on_fruit_glance.csv"))
  write_csv(tidy(lm_treatment), file.path(table_dir, "lm_biomass_on_fruit_treatment.csv"))
  write_csv(glance(lm_treatment), file.path(table_dir, "lm_biomass_on_fruit_treatment_glance.csv"))
  r2_simple <- summary(lm_simple)$r.squared
}

pearson_clean <- cor_tbl %>%
  filter(subset == "no outliers (by treatment)", method == "pearson") %>%
  pull(estimate)
pearson_clean <- ifelse(length(pearson_clean) == 0, NA_real_, pearson_clean[1])

subtitle_text <- if (!is.na(pearson_clean) && !is.na(r2_simple)) {
  sprintf("Pearson r=%.3f; R²=%.3f", pearson_clean, r2_simple)
} else {
  "Correlation/regression not computed (insufficient data)"
}

if (nrow(merged_no_outliers) > 0) {
  p_scatter <- ggplot(merged_no_outliers, aes(x = fruit_value, y = biom_value, color = treatment_abbrev)) +
    geom_point(alpha = 0.75) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    scale_color_brewer(palette = "Set3", name = "Treatment (abbr.)") +
    labs(
      title = "Fruit vs Biomass (outliers removed by treatment)",
      subtitle = subtitle_text,
      x = sheets[1],
      y = sheets[2],
      color = "Treatment (abbr.)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, "scatter_fruit_vs_biomass.png"), p_scatter, width = 8, height = 6, dpi = 150)
}

cat("\nCorrelation results (with and without outliers):\n")
print(cor_tbl)

# ---- Compact tables for plotting side labels ----
make_mean_table <- function(fruit_file, biom_file, out_name) {
  if (!file.exists(fruit_file) || !file.exists(biom_file)) return(invisible(NULL))
  fruit_tbl <- read_csv(fruit_file, show_col_types = FALSE) %>%
    transmute(
      treatment,
      treatment_abbrev,
      n_fruit = n,
      mean_fruit = mean,
      ci_lower_fruit = ci_lower,
      ci_upper_fruit = ci_upper
    )
  biom_tbl <- read_csv(biom_file, show_col_types = FALSE) %>%
    transmute(
      treatment,
      treatment_abbrev,
      n_biomass = n,
      mean_biomass = mean,
      ci_lower_biomass = ci_lower,
      ci_upper_biomass = ci_upper
    )
  combo <- full_join(fruit_tbl, biom_tbl, by = c("treatment", "treatment_abbrev"))
  write_csv(combo, out_name)
}

make_mean_table(
  file.path(out_dir, "summary_by_treatment_Fruit_w_weight.csv"),
  file.path(out_dir, "summary_by_treatment_Biomasse_w_weight.csv"),
  file.path(table_dir, "means_overall.csv")
)

make_mean_table(
  file.path(table_dir, "summary_by_treatment_salt60_Fruit w. weight.csv"),
  file.path(table_dir, "summary_by_treatment_salt60_Biomasse w. weight.csv"),
  file.path(table_dir, "means_salt60.csv")
)

make_mean_table(
  file.path(table_dir, "summary_by_treatment_salt90_Fruit w. weight.csv"),
  file.path(table_dir, "summary_by_treatment_salt90_Biomasse w. weight.csv"),
  file.path(table_dir, "means_salt90.csv")
)
