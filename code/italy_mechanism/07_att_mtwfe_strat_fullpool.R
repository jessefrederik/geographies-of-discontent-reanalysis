################################################################################
# Matched TWFE (MTWFE) with FULL-PANEL match pool.
#
# Treated arm: 2015 compliers, stratified by Cremaschi's treated rule
#   - non-mountain sub-5,000 compliers
#   - mountain     sub-3,000 compliers
#
# Match pool: ALL non-compliers in the panel (any pop, any mountain status).
# Each treated complier is matched to its nearest non-complier neighbour on
# Mahalanobis distance over the standard covariates.
#
# This relaxes the prior stratum-only matching, which restricted matches to
# non-compliers within the same stratum. Allowing the full pool lets matchit
# find better balance on income/age/altitude even if it has to reach above
# the Cremaschi threshold to do so.
################################################################################

library(data.table)
library(fixest)
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
results <- list()

for (svc in names(services_2015)) {
  comp  <- get_complier(services_2015[[svc]])
  d_svc <- merge(d_ext, comp, by = "USERNAME", all.x = TRUE)

  cat(sprintf("\n=== Service: %s ===\n", svc))
  cat(sprintf("  %-25s  %14s  %14s  %s\n",
              "Stratum", "TWFE est (t)", "MTWFE est (t)",
              "n_tr / n_ct (full-pool match)"))

  for (stratum_name in names(strata)) {
    # Treated arm: stratum compliers
    d_tr <- d_svc[strata[[stratum_name]](d_svc) & complier == 1]
    # Control pool: ALL non-compliers in the panel (any pop, any mountain)
    d_ct <- d_svc[!is.na(complier) & complier == 0]
    d_s  <- rbind(d_tr, d_ct)
    d_s[, t_att := as.integer(complier == 1 & post == 1)]

    n_tr <- uniqueN(d_tr$id08)
    n_ct_pool <- uniqueN(d_ct$id08)
    if (n_tr < 5 || n_ct_pool < 5) next

    # --- TWFE on the wide-pool sample (no matching) ----------------------
    # This is what TWFE looks like when sub-stratum compliers are
    # compared to all non-compliers (still a contaminated comparison
    # without matching, but useful as a baseline).
    m_twfe <- feols(farright_sh ~ t_att | id08 + year, data = d_s, cluster = "id08")
    twfe_est <- coef(m_twfe)["t_att"]
    twfe_t   <- twfe_est / se(m_twfe)["t_att"]

    # --- MTWFE: Mahalanobis NN matching, FULL pool of non-compliers -----
    d08 <- unique(d_s[year == min(year), c("id08", "complier", match_vars), with = FALSE])
    fml <- as.formula(paste("complier ~", paste(match_vars, collapse = " + ")))
    m_out <- tryCatch(
      matchit(fml, data = d08, method = "nearest",
              distance = "mahalanobis", replace = TRUE),
      error = function(e) NULL
    )
    if (is.null(m_out)) next

    md  <- match.data(m_out)
    d_m <- merge(d_s, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
    m_mt <- feols(farright_sh ~ t_att | id08 + year, data = d_m,
                  weights = d_m$weights, cluster = "id08")
    mtwfe_est <- coef(m_mt)["t_att"]
    mtwfe_t   <- mtwfe_est / se(m_mt)["t_att"]
    n_tr_m    <- uniqueN(d_m[complier == 1]$id08)
    n_ct_m    <- uniqueN(d_m[complier == 0]$id08)

    # Diagnostic: of matched controls, how many came from outside the stratum?
    matched_controls <- unique(d_m[complier == 0, .(id08, mont_group, pop_tot_2008)])
    in_stratum <- strata[[stratum_name]](matched_controls)
    n_ct_in    <- sum(in_stratum)
    n_ct_out   <- n_ct_m - n_ct_in

    cat(sprintf("  %-25s  %+8.4f (%5.1f)   %+8.4f (%5.1f)   %d / %d  (%d in stratum, %d outside)\n",
                stratum_name, twfe_est, twfe_t, mtwfe_est, mtwfe_t,
                n_tr_m, n_ct_m, n_ct_in, n_ct_out))

    results[[paste(svc, stratum_name)]] <- data.table(
      service = svc, stratum = stratum_name,
      twfe_est = twfe_est, twfe_t = twfe_t,
      mtwfe_est = mtwfe_est, mtwfe_t = mtwfe_t,
      n_tr = n_tr_m, n_ct = n_ct_m,
      n_ct_in_stratum = n_ct_in, n_ct_out_stratum = n_ct_out
    )
  }
}

out <- rbindlist(results)
cat("\n========================================================\n")
cat("SUMMARY: TWFE vs MTWFE with FULL-POOL match\n")
cat("========================================================\n")
print(out)

fwrite(out, "output/csvs/italy/att_mtwfe_strat_fullpool.csv")
cat("\nSaved to output/att_mtwfe_strat_fullpool.csv\n")
