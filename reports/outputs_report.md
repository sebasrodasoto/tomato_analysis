# Tomato analysis exports

Dataset: `data/raw/2026Main_Versuch Harvest_Rodas.xlsx` with treatment mapping from `data/raw/treatment.csv`.

## Highlights
- Fruit weight varies strongly by treatment (ANOVA p ≈ 1.5e-18, η² ≈ 0.77); the unsalted control shows the highest mean (~1360 g), while 90 mM salt treatments cluster around 367–425 g.
- Biomass also differs by treatment (ANOVA p ≈ 5.2e-5, η² ≈ 0.38); control averages ~322 g, and 90 mM salt groups range ~172–200 g.
- Fruit vs. biomass is positively related (Pearson r ≈ 0.73 with all data; r ≈ 0.60 after removing treatment-level outliers); relationship remains significant.
- Tukey letters: unsalted control is group **b** for fruit and biomass; all salt treatments share group **a** (fruit) or overlap **a/ab** (biomass).
- Boxes 9 and 11 were excluded up front; additional outliers flagged via 1.5×IQR within treatment for clean summaries.
- Diagnostics flag non-normal residuals and unequal variances for fruit (Shapiro p ≈ 6.6e-7, Bartlett p ≈ 4.1e-8); Kruskal–Wallis and Dunn tables are included as non-parametric checks.

## Key outputs (ready to share)
- Tables: `outputs/tables/` — ANOVA, Tukey, letters, Dunn, Kruskal, mixed models, outlier flags, correlations, and plot/plant/box summaries. Compact combined means: `means_overall.csv`, `means_salt60.csv`, `means_salt90.csv`.
- Cleaned data: `outputs/clean_*` plus merged fruit/biomass with outlier flags in `outputs/tables/merged_fruit_biomass_with_outliers.csv`.
- Plots: `outputs/plots/` — `boxplot_*`, `mean_ci_by_treatment_*`, salt-stratified variants, `hist_*`, `scatter_fruit_vs_biomass.png`.
- Diagnostics: `outputs/diagnostics/diagnostics_*.txt` with Shapiro/Bartlett checks.

## Regenerate
From the project root:

```powershell
& "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" "R/01_explore.R"
```

Packages auto-install if missing. Outputs write to `outputs/`, `outputs/plots/`, `outputs/diagnostics/`, and `outputs/tables/`.
