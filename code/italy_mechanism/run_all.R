################################################################################
# Master script for the ITT/ATT framework testing whether Cremaschi et al.'s
# (2024) sub-threshold far-right effect reflects actual mandate compliance.
#
# Run from the project root:
#   Rscript code/italy_mechanism/run_all.R
#
# Produces all CSV outputs in output/. The 07_*_fullpool.R sensitivity is
# included by default; comment out the last source() to skip it.
################################################################################

stopifnot(file.exists("data_processed/italy/electoral_panel_extended.csv"))
stopifnot(file.exists(
  "data_raw/italy/ministero_interno/unioni_comuni_ministero_2020.csv"
))
stopifnot(dir.exists("data_raw/italy/opencivitas"))

# Ensure output directories exist (they are gitignored).
for (d in c("output", "output/tables")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# code/italy_mechanism/fig_compliance_gradient_no_rd.R and fig_union_formation_did.R and
# fig_service_diff_delta_no_rd.R are referenced by the paper but not in the
# scripts list below; run_all currently runs the with-RD figure variant only.
# Add the no-RD variants explicitly:
scripts <- c(
  "code/italy_mechanism/build_post2010_union_indicator.R",
  "code/italy_mechanism/00_build_crosswalk.R",
  "code/italy_mechanism/01_itt_att_compliance.R",
  "code/italy_mechanism/02_att_sub5k_strat.R",
  "code/italy_mechanism/03_att_mtwfe_strat.R",
  "code/italy_mechanism/04_mtwfe_balance_check.R",
  "code/italy_mechanism/05_att_count_fundamental.R",
  "code/italy_mechanism/06_att_post2010_union.R",
  "code/italy_mechanism/07_att_mtwfe_strat_fullpool.R",
  "code/italy_mechanism/08_att_single_function.R",
  "code/italy_mechanism/09_sdid_compliance.R",
  "code/italy_mechanism/10_gradient_compliance.R",
  "code/italy_mechanism/fig_compliance_gradient.R",
  "code/italy_mechanism/fig_compliance_gradient_no_rd.R",
  "code/italy_mechanism/fig_union_formation_did.R",
  "code/italy_mechanism/fig_service_diff_delta_no_rd.R"
)

for (s in scripts) {
  cat(sprintf("\n################################################################\n"))
  cat(sprintf("### %s\n", s))
  cat(sprintf("################################################################\n"))
  source(s, echo = FALSE)
}

cat("\nAll done. Outputs in output/.\n")
