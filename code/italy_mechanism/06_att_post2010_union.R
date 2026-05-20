################################################################################
# ATT using `in_post2010_union` as the treatment indicator.
#
# This is a CLEANER mandate-mechanism test than the OpenCivitas 2015 snapshot:
# the treatment captures munis that joined a unione di comuni FORMED post-2010,
# i.e., the mandate-induced compliers Cremaschi's mechanism would predict to
# be politically affected.
#
# Caveat on the control arm: in_post2010_union == 0 includes both never-in-union
# and pre-2010-union (now dissolved) munis. The Ministry registry only lists
# currently-active unions, so dissolved pre-2010 unions disappear from the data.
# About 27% of unions in the SIOPE registry are marked dissolved. The control
# arm therefore mixes always-non-compliers and ex-always-compliers; the bias
# this introduces should be small and conservative.
#
# Specs:
#   TWFE   : farright_sh ~ I(in_post2010_union * post) | id08 + year
#   MTWFE  : same, weighted by Mahalanobis NN matching on baseline covariates
#
# Strata: pooled Cremaschi-treated; non-mont sub-5k; mont sub-3k
################################################################################

library(data.table)
library(haven)
library(fixest)
library(MatchIt)

INCLUDE_2022 <- FALSE
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008", "share_university2001",
                "max_altitude")

# ---- Panel + union flag ---------------------------------------------------
d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")
u     <- fread("data_processed/italy/post2010_union_indicator.csv")
u[, muni_clean := toupper(trimws(municipality))]
d_ext[, muni_clean := toupper(trimws(municipality))]
d_ext <- merge(d_ext, u[, .(muni_clean, in_post2010_union)], by = "muni_clean", all.x = TRUE)
if (!INCLUDE_2022) d_ext <- d_ext[year != 2022]

cat(sprintf("Panel: %d obs, %d munis, years %s%s\n",
            nrow(d_ext), uniqueN(d_ext$id08),
            paste(sort(unique(d_ext$year)), collapse = ","),
            if (INCLUDE_2022) "" else "  (2022 excluded)"))
cat(sprintf("Munis with post-2010 union flag = 1: %d (%.1f%%)\n",
            uniqueN(d_ext[in_post2010_union == 1]$id08),
            100 * mean(d_ext[year == min(year)]$in_post2010_union, na.rm = TRUE)))

# ---- Strata ---------------------------------------------------------------
strata <- list(
  "POOLED (Cremaschi-treated)" = function(x) (x$mont_group == 0 & x$pop_tot_2008 < 5000) |
                                              (x$mont_group == 1 & x$pop_tot_2008 < 3000),
  "non-mont sub-5k"            = function(x) x$mont_group == 0 & x$pop_tot_2008 < 5000,
  "mont sub-3k (treated)"      = function(x) x$mont_group == 1 & x$pop_tot_2008 < 3000
)

# ---- Run TWFE + MTWFE per stratum -----------------------------------------
set.seed(20241201)
results <- list()

cat("\n========================================================\n")
cat("ATT: in_post2010_union as treatment indicator\n")
cat("========================================================\n")

for (stratum_name in names(strata)) {
  d_s <- d_ext[strata[[stratum_name]](d_ext) & !is.na(in_post2010_union)]
  d_s[, t_post2010 := as.integer(in_post2010_union == 1 & post == 1)]

  n_tr <- uniqueN(d_s[in_post2010_union == 1]$id08)
  n_ct <- uniqueN(d_s[in_post2010_union == 0]$id08)

  # TWFE
  m_twfe <- feols(farright_sh ~ t_post2010 | id08 + year, data = d_s, cluster = "id08")
  twfe_est <- coef(m_twfe)["t_post2010"]
  twfe_t   <- twfe_est / se(m_twfe)["t_post2010"]

  # MTWFE
  d08 <- unique(d_s[year == min(year), c("id08", "in_post2010_union", match_vars), with = FALSE])
  d08 <- d08[complete.cases(d08)]
  fml <- as.formula(paste("in_post2010_union ~", paste(match_vars, collapse = " + ")))
  m_out <- tryCatch(matchit(fml, data = d08, method = "nearest",
                            distance = "mahalanobis", replace = TRUE),
                    error = function(e) NULL)
  if (!is.null(m_out)) {
    md  <- match.data(m_out)
    d_m <- merge(d_s, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
    m_mt <- feols(farright_sh ~ t_post2010 | id08 + year, data = d_m,
                  weights = d_m$weights, cluster = "id08")
    mtwfe_est <- coef(m_mt)["t_post2010"]
    mtwfe_t   <- mtwfe_est / se(m_mt)["t_post2010"]
    n_tr_m    <- uniqueN(d_m[in_post2010_union == 1]$id08)
    n_ct_m    <- uniqueN(d_m[in_post2010_union == 0]$id08)

    # Quick balance summary: mean SMD across covariates pre vs post match
    s <- summary(m_out, standardize = TRUE)
    smd_pre  <- mean(abs(s$sum.all[, "Std. Mean Diff."]),     na.rm = TRUE)
    smd_post <- mean(abs(s$sum.matched[, "Std. Mean Diff."]), na.rm = TRUE)
  } else {
    mtwfe_est <- mtwfe_t <- NA_real_; n_tr_m <- n_ct_m <- NA_integer_
    smd_pre <- smd_post <- NA_real_
  }

  cat(sprintf("\n[%s]  N munis: %d treated, %d control\n",
              stratum_name, n_tr, n_ct))
  cat(sprintf("  TWFE : %+8.4f (t=%5.2f)\n", twfe_est, twfe_t))
  cat(sprintf("  MTWFE: %+8.4f (t=%5.2f)   matched n_tr/n_ct = %d/%d   mean |SMD|: %.2f -> %.3f\n",
              mtwfe_est, mtwfe_t, n_tr_m, n_ct_m, smd_pre, smd_post))

  results[[stratum_name]] <- data.table(
    stratum = stratum_name,
    n_treated = n_tr, n_control = n_ct,
    twfe_est = twfe_est, twfe_t = twfe_t,
    mtwfe_est = mtwfe_est, mtwfe_t = mtwfe_t,
    n_tr_matched = n_tr_m, n_ct_matched = n_ct_m,
    mean_abs_smd_pre = smd_pre, mean_abs_smd_post = smd_post
  )
}

out <- rbindlist(results)
cat("\n========================================================\n")
cat("SUMMARY\n")
cat("========================================================\n")
print(out)

fwrite(out, "output/csvs/italy/att_post2010_union.csv")
cat("\nSaved to output/att_post2010_union.csv\n")
