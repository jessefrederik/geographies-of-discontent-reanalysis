################################################################################
# Cross-function compliance ATT: count of Art. 14 D.L. 78/2010 fundamental
# functions shared in 2015. Two treatments tried:
#   - count_fund   : continuous count (0-8), as `count * post` in TWFE
#   - high_comply  : 1 if shares >= 4 of 8 fundamental functions, used for
#                    matching-amenable binary ATT/MTWFE
#
# 8 fundamental functions per Art. 14 mapping:
#   POLIZIA, RIFIUTI, SOCIALE, TERRITORIO, VIABILITA, ANAGRAFE, TRIBUTI,
#   ISTRUZ_any (= max of 6 ISTR_* sub-functions)
#
# Sensitivity: 6-function version excluding TRIBUTI and ANAGRAFE.
#
# Stratified by mountain (Cremaschi treated rule):
#   - non-mont sub-5k
#   - mont sub-3k
################################################################################

library(data.table)
library(fixest)
library(MatchIt)

INCLUDE_2022 <- FALSE
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008", "share_university2001",
                "max_altitude")

# ---- Panel + crosswalk ----------------------------------------------------
d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")
xw    <- fread("data_processed/italy/opencivitas_panel_crosswalk.csv")
d_ext <- merge(d_ext, xw[, .(id08, USERNAME)], by = "id08", all.x = TRUE)
if (!INCLUDE_2022) d_ext <- d_ext[year != 2022]

# ---- Indicator extraction (2015 only) -------------------------------------
parse_val <- function(x) as.numeric(gsub(",", ".", x))

# Each entry: file, indicator name, blank-as-zero policy
ind_specs <- list(
  POLIZIA   = list(file = "Ind_FC20POLIZIA_3.csv",  var = "DUMMY_POLIZIA_ASSOC",         blank_as_zero = FALSE),
  RIFIUTI   = list(file = "Ind_FC20RIFIUTI_3.csv",  var = "DUMMY_RIFIUTI_ASSOC",         blank_as_zero = TRUE),
  SOCIALE   = list(file = "Ind_FC20SOCNID_2.csv",   var = "DUMMY_SOCIALE_ASSOCIATA",     blank_as_zero = FALSE),
  TERR      = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_TERR_ASSOCIATA",        blank_as_zero = FALSE),
  VIAB      = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_VIAB_ASSOCIATA",        blank_as_zero = FALSE),
  ANAG      = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_ANAGR_ASSOCIATA",       blank_as_zero = FALSE),
  TRIB      = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_TRIBUTI_ASSOCIATA",     blank_as_zero = FALSE),
  IST_INFA  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_INFANZIA_GP_ASSOC",     blank_as_zero = FALSE),
  IST_PRSE  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_PRIMSEC_GP_ASSOC",      blank_as_zero = FALSE),
  IST_REFE  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_REFEZIONE_GP_ASSOC",    blank_as_zero = FALSE),
  IST_TRAS  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_TRASPORTO_GP_ASSOC",    blank_as_zero = FALSE),
  IST_DISA  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_DISABILI_GP_ASSOC",     blank_as_zero = FALSE),
  IST_ALTR  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_ALTRIISTR_GP_ASSOC",    blank_as_zero = FALSE)
)

get_one <- function(spec) {
  ind <- fread(file.path("data_raw/italy/opencivitas", spec$file), sep = ";")
  a <- ind[`Indicatore/Determinante` == spec$var]
  a[, val := if (spec$blank_as_zero) ifelse(Valore == "" | is.na(Valore), 0, parse_val(Valore))
             else parse_val(Valore)]
  unique(a[!is.na(val) & val %in% c(0,1), .(USERNAME, val)])
}

# Build USERNAME-level wide table of all indicator dummies
flags <- NULL
for (nm in names(ind_specs)) {
  d_nm <- get_one(ind_specs[[nm]])
  setnames(d_nm, "val", nm)
  if (is.null(flags)) flags <- d_nm
  else flags <- merge(flags, d_nm, by = "USERNAME", all = TRUE)
}

# Pool the 6 ISTRUZ sub-functions into a single "any education" flag
istr_cols <- c("IST_INFA","IST_PRSE","IST_REFE","IST_TRAS","IST_DISA","IST_ALTR")
flags[, ISTRUZ_any := as.integer(rowSums(.SD == 1, na.rm = TRUE) >= 1), .SDcols = istr_cols]
flags[, c(istr_cols) := NULL]

# Compose count of Art. 14 fundamental functions shared (8-fn version)
fn_cols_8 <- c("POLIZIA", "RIFIUTI", "SOCIALE", "TERR", "VIAB", "ANAG", "TRIB", "ISTRUZ_any")
flags[, count_fund_8 := rowSums(.SD == 1, na.rm = TRUE), .SDcols = fn_cols_8]
flags[, n_valid_8 := rowSums(!is.na(.SD)), .SDcols = fn_cols_8]
flags[, high_comply_8 := as.integer(count_fund_8 >= 4)]

# 6-function sensitivity (drops TRIB, ANAG)
fn_cols_6 <- c("POLIZIA", "RIFIUTI", "SOCIALE", "TERR", "VIAB", "ISTRUZ_any")
flags[, count_fund_6 := rowSums(.SD == 1, na.rm = TRUE), .SDcols = fn_cols_6]
flags[, n_valid_6 := rowSums(!is.na(.SD)), .SDcols = fn_cols_6]
flags[, high_comply_6 := as.integer(count_fund_6 >= 3)]

# Restrict to munis with ALL 8 indicators valid (so the count is comparable)
flags_complete_8 <- flags[n_valid_8 == 8]
flags_complete_6 <- flags[n_valid_6 == 6]

cat(sprintf("Munis with valid 2015 data on ALL 8 fundamental functions: %d\n", nrow(flags_complete_8)))
cat(sprintf("Munis with valid 2015 data on ALL 6 (excl TRIB/ANAG):       %d\n", nrow(flags_complete_6)))
cat("\nDistribution of count_fund_8 across all munis:\n")
print(flags_complete_8[, .N, by = count_fund_8][order(count_fund_8)])
cat(sprintf("\n%%%% high-comply (>=4 of 8): %.1f%%\n", 100*mean(flags_complete_8$high_comply_8)))

# ---- Strata ---------------------------------------------------------------
strata <- list(
  "POOLED (Cremaschi-treated)" = function(x) (x$mont_group == 0 & x$pop_tot_2008 < 5000) |
                                              (x$mont_group == 1 & x$pop_tot_2008 < 3000),
  "non-mont sub-5k"       = function(x) x$mont_group == 0 & x$pop_tot_2008 < 5000,
  "mont sub-3k (treated)" = function(x) x$mont_group == 1 & x$pop_tot_2008 < 3000
)

# ---- Run analyses ---------------------------------------------------------
set.seed(20241201)
results <- list()

run_specs <- function(d_svc, count_var, binary_var, label_suffix) {
  for (stratum_name in names(strata)) {
    d_s <- d_svc[strata[[stratum_name]](d_svc) & !is.na(get(count_var))]

    # Continuous: count * post
    d_s[, t_count := get(count_var) * post]
    m_count <- feols(farright_sh ~ t_count | id08 + year, data = d_s, cluster = "id08")
    est_c <- coef(m_count)["t_count"]; t_c <- est_c / se(m_count)["t_count"]

    # Binary: high_comply * post (TWFE)
    d_s[, t_high := get(binary_var) * post]
    m_bin <- feols(farright_sh ~ t_high | id08 + year, data = d_s, cluster = "id08")
    est_b <- coef(m_bin)["t_high"]; t_b <- est_b / se(m_bin)["t_high"]

    # Binary: MTWFE on high_comply
    d08 <- unique(d_s[year == min(year), c("id08", binary_var, match_vars), with = FALSE])
    setnames(d08, binary_var, "high_comply")
    fml <- as.formula(paste("high_comply ~", paste(match_vars, collapse = " + ")))
    m_out <- tryCatch(matchit(fml, data = d08, method = "nearest",
                              distance = "mahalanobis", replace = TRUE),
                      error = function(e) NULL)
    if (!is.null(m_out)) {
      md  <- match.data(m_out)
      d_m <- merge(d_s, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
      m_mt <- feols(farright_sh ~ t_high | id08 + year, data = d_m,
                    weights = d_m$weights, cluster = "id08")
      est_mt <- coef(m_mt)["t_high"]; t_mt <- est_mt / se(m_mt)["t_high"]
      n_tr_mt <- uniqueN(d_m[get(binary_var) == 1]$id08)
      n_ct_mt <- uniqueN(d_m[get(binary_var) == 0]$id08)
    } else {
      est_mt <- t_mt <- NA_real_; n_tr_mt <- n_ct_mt <- NA_integer_
    }

    cat(sprintf("\n[%s | %s]  count_fund mean = %.2f  high_comply%% = %.1f\n",
                label_suffix, stratum_name,
                mean(d08[[match_vars[1]]] >= 0) * 0 + mean(d_s[year == min(year)][[count_var]], na.rm = TRUE),
                100 * mean(d_s[year == min(year)][[binary_var]], na.rm = TRUE)))
    cat(sprintf("  TWFE continuous   : per-function effect = %+.4f (t=%5.2f)\n", est_c, t_c))
    cat(sprintf("  TWFE  binary high : %+.4f (t=%5.2f)\n", est_b, t_b))
    cat(sprintf("  MTWFE binary high : %+.4f (t=%5.2f)   n_tr/n_ct (post-match) = %d / %d\n",
                est_mt, t_mt, n_tr_mt, n_ct_mt))

    results[[paste(label_suffix, stratum_name)]] <<- data.table(
      version = label_suffix, stratum = stratum_name,
      twfe_count_est = est_c, twfe_count_t = t_c,
      twfe_bin_est = est_b, twfe_bin_t = t_b,
      mtwfe_bin_est = est_mt, mtwfe_bin_t = t_mt,
      n_tr_mt = n_tr_mt, n_ct_mt = n_ct_mt
    )
  }
}

cat("\n========================================================\n")
cat("8-FUNCTION VERSION (POLIZIA+RIFIUTI+SOCIALE+TERR+VIAB+ANAG+TRIB+ISTRUZ_any)\n")
cat("========================================================\n")
d_svc8 <- merge(d_ext, flags_complete_8[, .(USERNAME, count_fund_8, high_comply_8)],
                by = "USERNAME", all.x = TRUE)
run_specs(d_svc8, "count_fund_8", "high_comply_8", "8-fn")

cat("\n========================================================\n")
cat("6-FUNCTION SENSITIVITY (drops TRIB and ANAG)\n")
cat("========================================================\n")
d_svc6 <- merge(d_ext, flags_complete_6[, .(USERNAME, count_fund_6, high_comply_6)],
                by = "USERNAME", all.x = TRUE)
run_specs(d_svc6, "count_fund_6", "high_comply_6", "6-fn")

out <- rbindlist(results)
cat("\n========================================================\n")
cat("SUMMARY\n")
cat("========================================================\n")
print(out)
fwrite(out, "output/csvs/italy/att_count_fundamental.csv")
cat("\nSaved to output/csvs/italy/att_count_fundamental.csv\n")
