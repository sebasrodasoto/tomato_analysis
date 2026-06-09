# Vorpraktikum miniproject — analysis exports

**Data:** `C:/Users/sebas/Documents/2Vorpraktikum_msc2025_ohneduplicate.xlsx`  
**Script:** `R/02_vorpraktikum.R`  
**Outputs:** `outputs_vorpraktikum/` (separate from main experiment `outputs/`)

## Design (6 boxes)
| Box | Treatment |
|-----|-----------|
| B1, B2 | Salicornia extract (salt stress) |
| B3, B4 | Strandaster extract (salt stress) |
| B5 | Control (no salt) |
| B6 | Salt control (no extract) |

## Highlights
- **Fruit fresh weight (box level, n=6):** highest in B5 control (~444 g) and B2 Salicornia (~661 g); lowest in B6 salt control (~181 g). ANOVA p ≈ 0.30 (not significant with only 6 boxes); Kruskal–Wallis p ≈ 0.23.
- **Salicornia vs salt control (fruit):** mean ~549 g vs ~181 g across B1/B2 vs B6 — consistent with protocol narrative, but n=2 vs n=1 limits formal inference.
- **Shoot biomass (plant level):** fresh and dry weight ANOVAs/Kruskal tables in `outputs_vorpraktikum/tables/`; mixed models include box as random intercept where applicable.
- **Root dry weight:** multiple root samples per box; box-mean table in `means_by_box.csv`.
- **Excluded labels:** `P6P3` and `P2P8` (biomass), plus `P2` and `V4` (roots) — not mapped to B1–B6; see `excluded_ambiguous_*_labels.csv`.
- **Duplicate resolved:** two `B1P2` entries averaged before analysis.

## Key tables
- `means_by_box.csv` — all response means per box (good for protocol tables)
- `anova_*`, `tukey_*`, `letters_*`, `kruskal_*`, `dunn_*` — per response and level (`sample` vs `box`)
- `correlation_box_level.csv` — Pearson correlations between box-mean responses
- `clean_all_long.csv` — harmonized long-format data

## Key plots (46 figures in `outputs_vorpraktikum/plots/`)
- `panel_all_responses_by_box.png` — overview of fruit, shoot fresh/dry, roots
- `bar_*` / `bar_dataonly_*` — treatment means with ANOVA subtitle or values only
- `bar_by_box_*` / `mean_ci_by_box_*` — per-box (B1–B6) comparisons
- `boxplot_*_sample.png` — plant-level distributions (biomass, roots)
- `hist_*` — histograms by treatment
- `scatter_*` — pairwise box-mean relationships

## Statistics & discussion
- `tables/anova_summary_all_responses.csv` — ANOVA across all four responses
- `reports/vorpraktikum_discussion.md` — interpretation and interesting patterns

## Regenerate
```powershell
& "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" "R/02_vorpraktikum.R"
```

**Caution:** With only six independent boxes (and single-box controls), treat inferential tests as exploratory; emphasize descriptive means and protocol-aligned interpretation.
