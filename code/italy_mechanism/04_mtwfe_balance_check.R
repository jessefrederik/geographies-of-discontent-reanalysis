################################################################################
# Matched-balance diagnostic for the MTWFE within-stratum ATTs.
# Reports standardized mean differences (SMD) before vs after matching,
# and the variance ratio. Conventional thresholds:
#   |SMD| < 0.1  -> well balanced
#   |SMD| < 0.25 -> acceptable
#   |SMD| > 0.25 -> imbalanced (matching failed)
################################################################################

library(data.table)
library(MatchIt)

INCLUDE_2022 <- FALSE
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008", "share_university2001",
                "max_altitude")

d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")
xw    <- fread("data_processed/italy/opencivitas_panel_crosswalk.csv")
d_ext <- merge(d_ext, xw[, .(id08, USERNAME)], by = "id08", all.x = TRUE)
if (!INCLUDE_2022) d_ext <- d_ext[year != 2022]

parse_val <- function(x) as.numeric(gsub(",", ".", x))
services_2015 <- list(
  RIFIUTI   = list(file = "Ind_FC20RIFIUTI_3.csv", var = "DUMMY_RIFIUTI_ASSOC", blank_as_zero = TRUE),
  SOCIALE   = list(file = "Ind_FC20SOCNID_2.csv",  var = "DUMMY_SOCIALE_ASSOCIATA", blank_as_zero = FALSE),
  POLIZIA   = list(file = "Ind_FC20POLIZIA_3.csv", var = "DUMMY_POLIZIA_ASSOC", blank_as_zero = FALSE),
  AMM_ALTRI = list(file = "Ind_FC20AMMIN_2.csv",   var = "DUMMY_ALT_SER_ASSOCIATA", blank_as_zero = FALSE)
)
get_complier <- function(spec) {
  ind <- fread(file.path("data_raw/italy/opencivitas", spec$file), sep = ";")
  a <- ind[`Indicatore/Determinante` == spec$var]
  a[, val := if (spec$blank_as_zero) ifelse(Valore == "" | is.na(Valore), 0, parse_val(Valore))
             else parse_val(Valore)]
  unique(a[!is.na(val) & val %in% c(0,1), .(USERNAME, complier = as.integer(val == 1))])
}

strata <- list(
  "non-mont sub-5k"       = function(x) x$mont_group == 0 & x$pop_tot_2008 < 5000,
  "mont sub-3k (treated)" = function(x) x$mont_group == 1 & x$pop_tot_2008 < 3000
)

set.seed(20241201)
all_balance <- list()

for (svc in names(services_2015)) {
  comp  <- get_complier(services_2015[[svc]])
  d_svc <- merge(d_ext, comp, by = "USERNAME", all.x = TRUE)

  for (stratum_name in names(strata)) {
    d_s <- d_svc[strata[[stratum_name]](d_svc) & !is.na(complier)]
    d08 <- unique(d_s[year == min(year), c("id08", "complier", match_vars), with = FALSE])
    fml <- as.formula(paste("complier ~", paste(match_vars, collapse = " + ")))
    m_out <- matchit(fml, data = d08, method = "nearest",
                     distance = "mahalanobis", replace = TRUE)
    s <- summary(m_out, standardize = TRUE)

    cat(sprintf("\n========== %s | %s ==========\n", svc, stratum_name))
    cat("Standardized mean differences (and variance ratios), pre vs post:\n\n")

    pre  <- s$sum.all
    post <- s$sum.matched
    bal <- data.table(
      covariate = rownames(pre),
      mean_treated = pre[, "Means Treated"],
      mean_control_pre  = pre[, "Means Control"],
      smd_pre  = pre[, "Std. Mean Diff."],
      mean_control_post = post[, "Means Control"],
      smd_post = post[, "Std. Mean Diff."],
      var_ratio_pre  = pre[, "Var. Ratio"],
      var_ratio_post = post[, "Var. Ratio"]
    )
    print(bal, digits = 3)

    bal[, service := svc][, stratum := stratum_name]
    all_balance[[paste(svc, stratum_name)]] <- bal
  }
}

cat("\n========================================================\n")
cat("WORST-BALANCED COVARIATES (post-match |SMD| > 0.1)\n")
cat("========================================================\n")
out <- rbindlist(all_balance)
worst <- out[abs(smd_post) > 0.10, .(service, stratum, covariate, smd_pre, smd_post)][order(-abs(smd_post))]
if (nrow(worst) > 0) {
  print(worst, digits = 3)
} else {
  cat("None -- all matched covariates have |SMD| <= 0.1.\n")
}

fwrite(out, "output/csvs/italy/mtwfe_balance_check.csv")
cat("\nSaved to output/csvs/italy/mtwfe_balance_check.csv\n")
