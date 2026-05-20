################################################################################
# Within-sub-5000 ATT, stratified by mountain status.
#
# Spec: feols(farright_sh ~ t | id08 + year, cluster = id08)
# t = (2015 complier) x post-2010
#
# Sub-samples per service:
#   1. Non-mountain sub-5000 (all of these are Cremaschi-treated)
#   2. Mountain        sub-5000 (mix: sub-3000 are Cremaschi-treated;
#                                3000-5000 are Cremaschi-untreated)
#   3. Mountain        sub-3000 only (Cremaschi-treated mountain stratum)
#   4. Mountain      3000-5000  only (Cremaschi-untreated mountain stratum)
#
# All specs are within-sub-5000 (no above-threshold control). Compares
# 2015 compliers to 2015 non-compliers.
################################################################################

library(data.table)
library(fixest)

# ---- Toggle ---------------------------------------------------------------
INCLUDE_2022 <- FALSE

# ---- Panel + crosswalk ---------------------------------------------------
d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")
xw    <- fread("data_processed/italy/opencivitas_panel_crosswalk.csv")
d_ext <- merge(d_ext, xw[, .(id08, USERNAME)], by = "id08", all.x = TRUE)
if (!INCLUDE_2022) d_ext <- d_ext[year != 2022]

cat(sprintf("Panel: %d obs, %d munis, years %s%s\n",
            nrow(d_ext), uniqueN(d_ext$id08),
            paste(sort(unique(d_ext$year)), collapse = ","),
            if (INCLUDE_2022) "" else "  (2022 excluded)"))

# ---- 2015 compliance flags ------------------------------------------------
parse_val <- function(x) as.numeric(gsub(",", ".", x))

services_2015 <- list(
  RIFIUTI   = list(file = "Ind_FC20RIFIUTI_3.csv", var = "DUMMY_RIFIUTI_ASSOC",
                   blank_as_zero = TRUE),
  SOCIALE   = list(file = "Ind_FC20SOCNID_2.csv",  var = "DUMMY_SOCIALE_ASSOCIATA",
                   blank_as_zero = FALSE),
  POLIZIA   = list(file = "Ind_FC20POLIZIA_3.csv", var = "DUMMY_POLIZIA_ASSOC",
                   blank_as_zero = FALSE),
  AMM_ALTRI = list(file = "Ind_FC20AMMIN_2.csv",   var = "DUMMY_ALT_SER_ASSOCIATA",
                   blank_as_zero = FALSE)
)

get_complier <- function(spec) {
  ind <- fread(file.path("data_raw/italy/opencivitas", spec$file), sep = ";")
  a <- ind[`Indicatore/Determinante` == spec$var]
  if (spec$blank_as_zero) {
    a[, val := ifelse(Valore == "" | is.na(Valore), 0, parse_val(Valore))]
  } else {
    a[, val := parse_val(Valore)]
  }
  a <- a[!is.na(val) & val %in% c(0, 1)]
  unique(a[, .(USERNAME, complier = as.integer(val == 1))])
}

# ---- Strata definitions ---------------------------------------------------
# Each stratum is a function returning a logical vector on d_svc.
strata <- list(
  "non-mont sub-5k"          = function(x) x$mont_group == 0 & x$pop_tot_2008 < 5000,
  "mont sub-5k (any)"        = function(x) x$mont_group == 1 & x$pop_tot_2008 < 5000,
  "mont sub-3k (treated)"    = function(x) x$mont_group == 1 & x$pop_tot_2008 < 3000,
  "mont 3k-5k (untreated)"   = function(x) x$mont_group == 1 & x$pop_tot_2008 >= 3000 & x$pop_tot_2008 < 5000
)

# ---- Run per service x stratum -------------------------------------------
results <- list()

for (svc in names(services_2015)) {
  comp  <- get_complier(services_2015[[svc]])
  d_svc <- merge(d_ext, comp, by = "USERNAME", all.x = TRUE)
  cat(sprintf("\n=== Service: %s ===\n", svc))

  for (stratum_name in names(strata)) {
    stratum_filter <- strata[[stratum_name]]
    d_s <- d_svc[stratum_filter(d_svc) & !is.na(complier)]
    d_s[, t_att := as.integer(complier == 1 & post == 1)]

    n_tr <- uniqueN(d_s[complier == 1]$id08)
    n_ct <- uniqueN(d_s[complier == 0]$id08)

    if (n_tr < 5 || n_ct < 5) {
      cat(sprintf("  %-25s  SKIPPED (n_tr=%d, n_ct=%d too small)\n",
                  stratum_name, n_tr, n_ct))
      next
    }

    m <- tryCatch(
      feols(farright_sh ~ t_att | id08 + year, data = d_s, cluster = "id08"),
      error = function(e) NULL
    )
    if (is.null(m)) {
      cat(sprintf("  %-25s  ERROR fitting model\n", stratum_name))
      next
    }
    est <- coef(m)["t_att"]
    tstat <- est / se(m)["t_att"]
    share <- mean(d_s[year == min(d_s$year), complier], na.rm = TRUE)

    cat(sprintf("  %-25s  est = %+.4f   t = %+5.2f   n_tr = %4d  n_ct = %4d  comply%% = %4.1f\n",
                stratum_name, est, tstat, n_tr, n_ct, 100*share))

    results[[paste(svc, stratum_name)]] <- data.table(
      service = svc,
      stratum = stratum_name,
      est = est, t = tstat,
      n_obs = nobs(m),
      n_treated = n_tr, n_control = n_ct,
      share_compliers = share
    )
  }
}

out <- rbindlist(results)
cat("\n========================================================\n")
cat("SUMMARY (compliers vs non-compliers, within each stratum)\n")
cat("========================================================\n")
print(dcast(out, service ~ stratum, value.var = "est"))
cat("\nt-statistics:\n")
print(dcast(out, service ~ stratum, value.var = "t"))
cat("\nSample sizes (n_treated + n_control):\n")
print(dcast(out, service ~ stratum, value.var = "n_treated"))

fwrite(out, "output/csvs/italy/att_sub5k_strat.csv")
cat("\nSaved to output/att_sub5k_strat.csv\n")
