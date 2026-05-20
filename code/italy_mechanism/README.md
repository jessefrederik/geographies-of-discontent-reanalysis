# `code/italy_mechanism/` — replication folder for the ITT/ATT compliance framework

Self-contained analysis testing whether Cremaschi et al.'s (2024) headline
sub-threshold far-right effect reflects actual compliance with the 2010
mandatory associated-delivery mandate, or pre-existing composition.

The substantive writeup is in `report_itt_att.md` at the project root.
This README documents only the code structure and how to run it.

## Run

From the **project root** (paths are relative):

```bash
Rscript code/italy_mechanism/run_all.R
```

All scripts read from `data_processed/italy/...` and `data_raw/italy/...` and write
to `output/...` — same convention as the rest of the repo.

## Required inputs

| File | Contents |
|---|---|
| `data_processed/italy/electoral_panel_dataset.dta` | Cremaschi's panel (2001-2018) with `farright_sh`, `treated`, `mont_group`, baseline covariates |
| `data_processed/italy/electoral_panel_extended.csv` | Extended panel with the 2022 election (toggle `INCLUDE_2022` in scripts) |
| `data_raw/italy/ministero_interno/unioni_comuni_ministero_2020.csv` | Raw Ministry of Interior registry of *unioni di comuni* (2020 vintage). Builds `data_processed/italy/post2010_union_indicator.csv` via `build_post2010_union_indicator.R`. |
| `data_raw/italy/opencivitas/Metadati_Enti_2022.xlsx` | OpenCivitas metadata (USERNAME crosswalk) |
| `data_raw/italy/opencivitas/Ind_FC20*.csv` | 2015 OpenCivitas indicator files (one per service category) |

## Scripts

Run sequentially. `build_*` and `00_` are preprocessing; `01_`–`06_` are
the analyses in narrative order; `07_` is a robustness check.

| Script | Produces | Headline |
|---|---|---|
| `build_post2010_union_indicator.R` | `data_processed/italy/post2010_union_indicator.csv` | Builds the post-2010 unione flag from the raw 2020 ministry registry (mid-year cut: D.L. 78/2010 published 31-May-2010, law 30-Jul-2010 → unions founded from June 2010 onward count as mandate-induced) |
| `00_build_crosswalk.R` | `data_processed/italy/opencivitas_panel_crosswalk.csv` | Augmented panel↔OpenCivitas USERNAME crosswalk (4 passes: direct + normalize + suffix-strip + cross-prov) |
| `01_itt_att_compliance.R` | `output/itt_att_compliance.csv` | ITT replication (Cremaschi 0.0154) + ATT-A/B/C variants per service |
| `02_att_sub5k_strat.R` | `output/att_sub5k_strat.csv` | Within-sub-5k stratified ATT (compliers vs non-compliers, mountain vs non-mountain) |
| `03_att_mtwfe_strat.R` | `output/att_mtwfe_strat.csv` | MTWFE stratum-only — primary matched ATT |
| `04_mtwfe_balance_check.R` | `output/mtwfe_balance_check.csv` | Pre/post-match SMD balance diagnostic |
| `05_att_count_fundamental.R` | `output/att_count_fundamental.csv` | Cross-function count (8 of 8 / 6 of 6 Art. 14 fundamental functions) |
| `06_att_post2010_union.R` | `output/att_post2010_union.csv` | Ministry post-2010 union flag — cleanest mandate test (POOLED MTWFE ≈ 0, t ≈ 0) |
| `07_att_mtwfe_strat_fullpool.R` | `output/att_mtwfe_strat_fullpool.csv` | MTWFE with full-panel match pool (sensitivity, optional) |

## Toggles

Each script has `INCLUDE_2022 <- FALSE` near the top. Set to `TRUE` to
re-run on the 2022-extended panel. Headline numbers in
`report_itt_att.md` use the original Cremaschi years (2001-2018) for
comparability with their published estimates.

## Random seed

`set.seed(20241201)` — matches the seed in `code/italy/01_analysis_italy.R`.
MTWFE results are deterministic given this seed.

## Dependencies (R packages)

`data.table`, `haven`, `readxl`, `fixest`, `MatchIt`, `stringi`.
