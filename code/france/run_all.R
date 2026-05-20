#!/usr/bin/env Rscript
################################################################################
# France Placebo DID Analysis Pipeline
#
# Builds a France placebo test showing that arbitrary population thresholds
# produce treatment-like DID estimates in the absence of any reform.
#
# Run from the repository root:
#   Rscript code/france/run_all.R
#
# Required R packages:
#   data.table, fixest, ggplot2, scales, arrow, readxl
#   Optional: MatchIt (for matching robustness)
#
# Runtime: ~5 minutes (mostly the threshold sweep in 04_analysis.R)
#
# Outputs:
#   data_processed/france/final/panel_commune.csv         Balanced commune x election panel
#   output/tables/france/*.csv                  Regression results
#   output/figures/france/*.pdf                 Publication figures
################################################################################

cat("============================================================\n")
cat("FRANCE PLACEBO DID PIPELINE\n")
cat("============================================================\n\n")

t0 <- Sys.time()

# Ensure all working directories exist. They are gitignored, so on a fresh
# clone the subscripts would otherwise fail with "No such file or directory"
# when they try to write outputs.
for (d in c("data_raw/france/elections",
            "data_raw/france/cog",
            "data_raw/france/population",
            "data_processed/france/intermediate",
            "data_processed/france/final",
            "output/tables/france",
            "output/figures/france")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

source("code/france/00_download.R")
source("code/france/01_parse_elections.R")
source("code/france/02_population.R")
source("code/france/03_harmonize_panel.R")
source("code/france/04_analysis.R")
source("code/france/05_figures.R")

elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
cat(sprintf("\nDONE — all France placebo results produced in %s minutes.\n", elapsed))
