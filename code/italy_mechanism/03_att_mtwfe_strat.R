################################################################################
# Matched TWFE (MTWFE) version of the within-stratum compliers-vs-non-compliers
# ATT. Same spec as Cremaschi/replication's MTWFE: nearest-neighbour Mahalanobis
# matching on baseline covariates (with replacement), TWFE with matching weights.
#
# Strata (Cremaschi's treated-rule cuts):
#   1. Non-mountain sub-5,000  (Cremaschi-treated non-mountain stratum)
#   2. Mountain     sub-3,000  (Cremaschi-treated mountain stratum)
#
# For each (service, stratum):
#   - Restrict to munis with a valid 2015 OpenCivitas flag for the service
#   - Match compliers to non-compliers on Mahalanobis distance over match_vars
#   - Run feols(farright_sh ~ t_att | id08 + year, weights, cluster = id08)
################################################################################

library(data.table)
library(fixest)
library(MatchIt)

INCLUDE_2022 <- FALSE

# Same covariate set as the existing analysis.R MTWFE
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008", "share_university2001",
                "max_altitude")

# ---- Panel + crosswalk ----------------------------------------------------
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

# ---- Strata ---------------------------------------------------------------
strata <- list(
  "non-mont sub-5k"       = function(x) x$mont_group == 0 & x$pop_tot_2008 < 5000,
  "mont sub-3k (treated)" = function(x) x$mont_group == 1 & x$pop_tot_2008 < 3000
)

# ---- Run TWFE + MTWFE per (service, stratum) ------------------------------
set.seed(20241201)
results <- list()

for (svc in names(services_2015)) {
  comp  <- get_complier(services_2015[[svc]])
  d_svc <- merge(d_ext, comp, by = "USERNAME", all.x = TRUE)

  cat(sprintf("\n=== Service: %s ===\n", svc))
  cat(sprintf("  %-25s  %12s  %12s  %s\n",
              "Stratum", "TWFE est (t)", "MTWFE est (t)", "n_tr / n_ct (post-match)"))

  for (stratum_name in names(strata)) {
    d_s <- d_svc[strata[[stratum_name]](d_svc) & !is.na(complier)]
    d_s[, t_att := as.integer(complier == 1 & post == 1)]

    n_tr <- uniqueN(d_s[complier == 1]$id08)
    n_ct <- uniqueN(d_s[complier == 0]$id08)
    if (n_tr < 5 || n_ct < 5) next

    # --- TWFE (reuse for completeness alongside MTWFE) -------------------
    m_twfe <- feols(farright_sh ~ t_att | id08 + year, data = d_s, cluster = "id08")
    twfe_est <- coef(m_twfe)["t_att"]
    twfe_t   <- twfe_est / se(m_twfe)["t_att"]

    # --- MTWFE: Mahalanobis nearest-neighbour matching, weighted TWFE ---
    d08 <- unique(d_s[year == min(year), c("id08", "complier", match_vars), with = FALSE])
    fml <- as.formula(paste("complier ~", paste(match_vars, collapse = " + ")))
    m_out <- tryCatch(
      matchit(fml, data = d08, method = "nearest",
              distance = "mahalanobis", replace = TRUE),
      error = function(e) NULL
    )
    if (is.null(m_out)) {
      cat(sprintf("  %-25s  %12s  %12s\n",
                  stratum_name, sprintf("%+.4f (%.1f)", twfe_est, twfe_t), "MATCH FAILED"))
      next
    }
    md   <- match.data(m_out)
    d_m  <- merge(d_s, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
    m_mt <- feols(farright_sh ~ t_att | id08 + year, data = d_m,
                  weights = d_m$weights, cluster = "id08")
    mtwfe_est <- coef(m_mt)["t_att"]
    mtwfe_t   <- mtwfe_est / se(m_mt)["t_att"]
    n_tr_m    <- uniqueN(d_m[complier == 1]$id08)
    n_ct_m    <- uniqueN(d_m[complier == 0]$id08)

    cat(sprintf("  %-25s  %+8.4f (%5.1f)   %+8.4f (%5.1f)   %d / %d (was %d / %d)\n",
                stratum_name, twfe_est, twfe_t, mtwfe_est, mtwfe_t,
                n_tr_m, n_ct_m, n_tr, n_ct))

    results[[paste(svc, stratum_name)]] <- data.table(
      service = svc,
      stratum = stratum_name,
      twfe_est = twfe_est, twfe_t = twfe_t,
      mtwfe_est = mtwfe_est, mtwfe_t = mtwfe_t,
      n_tr_pre = n_tr, n_ct_pre = n_ct,
      n_tr_post = n_tr_m, n_ct_post = n_ct_m
    )
  }
}

out <- rbindlist(results)
cat("\n========================================================\n")
cat("SUMMARY: TWFE vs MTWFE (matched TWFE) within stratum\n")
cat("========================================================\n")
print(out)

fwrite(out, "output/csvs/italy/att_mtwfe_strat.csv")
cat("\nSaved to output/att_mtwfe_strat.csv\n")
