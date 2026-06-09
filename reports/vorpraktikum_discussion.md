# Vorpraktikum — results, statistics, and discussion

**Data:** C:/Users/sebas/Documents/2Vorpraktikum_msc2025_ohneduplicate.xlsx
**Generated:** 2026-06-08 11:33

## Data quality
- 6 boxes (B1–B6), 4 treatments, 36 shoot biomass plants, 16 root samples, 6 fruit records.
- All plant labels map to B1–B6; no ambiguous P2/P6 labels in the cleaned Biomasse sheet.

## Descriptive highlights (box means)

- **B1 (SAL):** fruit 438 g; shoot fresh 393 g; shoot dry 49 g; root dry 32.1 g
- **B2 (SAL):** fruit 661 g; shoot fresh 500 g; shoot dry 64 g; root dry 13.2 g
- **B3 (STR):** fruit 374 g; shoot fresh 503 g; shoot dry 60 g; root dry 11.2 g
- **B4 (STR):** fruit 322 g; shoot fresh 390 g; shoot dry 53 g; root dry 8.1 g
- **B5 (CTRL0):** fruit 444 g; shoot fresh 552 g; shoot dry 64 g; root dry 10.8 g
- **B6 (SALT_CTRL):** fruit 181 g; shoot fresh 492 g; shoot dry 63 g; root dry 18.0 g

## Inferential tests (ANOVA)

- **fruit_weight (box):** F=2.54, p=0.2954 n.s., eta²=0.79
- **biomass_fresh (sample):** F=0.40, p=0.7533 n.s., eta²=0.04
- **biomass_dry (sample):** F=0.23, p=0.8775 n.s., eta²=0.02
- **root_dry_weight (box):** F=0.72, p=0.6247 n.s., eta²=0.52

## What looks interesting?

### 1. Fruit yield — strongest treatment contrast
- **B5 (control, no salt)** and **B2 (Salicornia)** show the highest fruit weights (~444 and ~661 g).
- **B6 (salt control, no extract)** is clearly lowest (~181 g) — salt stress without biostimulant hurts fruit most.
- Salicornia (mean ~549 g across B1–B2) outperforms Strandaster (~348 g) and the salt control on fruit.
- *Caveat:* only 6 independent boxes; controls are n=1 box each.

### 2. Shoot biomass — less separation between treatments
- Fresh shoot weights are relatively similar across salt-stressed boxes (~390–552 g per box).
- **B5 (control)** has the highest mean shoot fresh weight (~552 g); salt treatments do not collapse vegetative biomass as strongly as fruit yield.
- High within-box variability (e.g. B1: one very large plant B1P1 at 940 g vs B1P7 at 70 g) drives large SDs.

### 3. Root dry weight — allocation signal
- **B1** has a high root mean (~32 g) but only one root sample recorded — interpret cautiously.
- **B6** shows the highest root dry weight among boxes with 3 samples (~18 g), possibly indicating more biomass allocated below ground under salt stress without extract.
- Fruit vs root scatter: boxes with low fruit (B6) do not always have the lowest roots — allocation patterns differ.

### 4. Halophyte extracts under salt
- Under salt stress, **Salicornia-treated boxes (B1, B2)** tend to produce more fruit than **Strandaster (B3, B4)** and especially the salt control (B6).
- This supports the protocol narrative that Salicornia extract may partially mitigate salt impact on yield, but replication is too low for definitive claims.

## Recommended interpretation
- Treat ANOVA/Tukey as **exploratory** (6 boxes, unequal replication). Prefer Kruskal–Wallis and visual patterns.
- Emphasize **fruit weight** and **B6 vs B2/B5 contrasts** as the clearest biological story.
- Note high plant-to-plant variation within boxes when discussing shoot biomass.

## Outputs
- Tables: `outputs_vorpraktikum/tables/`
- Plots: `outputs_vorpraktikum/plots/`
- Overview: `outputs_vorpraktikum/tables/overview/`
