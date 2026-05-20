################################################################################
# SDID estimates for the 11 compliance measures in Table 9:
#   - 3 aggregate measures: post-2010 unione (Ministry), 8-fn >=4 OpenCivitas,
#     6-fn >=4 OpenCivitas
#   - 8 single Article-14 functions (OpenCivitas 2015 dummies)
#
# All on the pooled Cremaschi-treated stratum, 2001-2018 panel. SEs are
# unit-level jackknife per Arkhangelsky et al. (2021).
################################################################################

library(data.table)
library(synthdid)

INCLUDE_2022 <- FALSE
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008", "share_university2001",
                "max_altitude")

in_treated <- function(x) (x$mont_group == 0 & x$pop_tot_2008 < 5000) |
                          (x$mont_group == 1 & x$pop_tot_2008 < 3000)

# ---- Panel + crosswalk + post-2010 unione flag ----------------------------
d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")
xw    <- fread("data_processed/italy/opencivitas_panel_crosswalk.csv")
d_ext <- merge(d_ext, xw[, .(id08, USERNAME)], by = "id08", all.x = TRUE)

u <- fread("data_processed/italy/post2010_union_indicator.csv")
u[, muni_clean := toupper(trimws(municipality))]
d_ext[, muni_clean := toupper(trimws(municipality))]
d_ext <- merge(d_ext, u[, .(muni_clean, in_post2010_union)],
               by = "muni_clean", all.x = TRUE)

if (!INCLUDE_2022) d_ext <- d_ext[year != 2022]

# ---- OpenCivitas single-function flags ------------------------------------
parse_val <- function(x) as.numeric(gsub(",", ".", x))

ind_specs <- list(
  POLIZIA   = list(file = "Ind_FC20POLIZIA_3.csv",  var = "DUMMY_POLIZIA_ASSOC",     blank_as_zero = FALSE),
  RIFIUTI   = list(file = "Ind_FC20RIFIUTI_3.csv",  var = "DUMMY_RIFIUTI_ASSOC",     blank_as_zero = TRUE),
  SOCIALE   = list(file = "Ind_FC20SOCNID_2.csv",   var = "DUMMY_SOCIALE_ASSOCIATA", blank_as_zero = FALSE),
  TERR      = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_TERR_ASSOCIATA",    blank_as_zero = FALSE),
  VIAB      = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_VIAB_ASSOCIATA",    blank_as_zero = FALSE),
  ANAG      = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_ANAGR_ASSOCIATA",   blank_as_zero = FALSE),
  TRIB      = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_TRIBUTI_ASSOCIATA", blank_as_zero = FALSE),
  IST_INFA  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_INFANZIA_GP_ASSOC",  blank_as_zero = FALSE),
  IST_PRSE  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_PRIMSEC_GP_ASSOC",   blank_as_zero = FALSE),
  IST_REFE  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_REFEZIONE_GP_ASSOC", blank_as_zero = FALSE),
  IST_TRAS  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_TRASPORTO_GP_ASSOC", blank_as_zero = FALSE),
  IST_DISA  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_DISABILI_GP_ASSOC",  blank_as_zero = FALSE),
  IST_ALTR  = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_ALTRIISTR_GP_ASSOC", blank_as_zero = FALSE)
)

get_one <- function(spec) {
  ind <- fread(file.path("data_raw/italy/opencivitas", spec$file), sep = ";")
  a <- ind[`Indicatore/Determinante` == spec$var]
  a[, val := if (spec$blank_as_zero) ifelse(Valore == "" | is.na(Valore), 0, parse_val(Valore))
             else parse_val(Valore)]
  unique(a[!is.na(val) & val %in% c(0,1), .(USERNAME, val)])
}

flags <- NULL
for (nm in names(ind_specs)) {
  d_nm <- get_one(ind_specs[[nm]])
  setnames(d_nm, "val", nm)
  if (is.null(flags)) flags <- d_nm
  else flags <- merge(flags, d_nm, by = "USERNAME", all = TRUE)
}

istr_cols <- c("IST_INFA","IST_PRSE","IST_REFE","IST_TRAS","IST_DISA","IST_ALTR")
flags[, ISTRUZ_any := {
  n_valid <- rowSums(!is.na(.SD))
  ifelse(n_valid == 0, NA_integer_, as.integer(rowSums(.SD == 1, na.rm = TRUE) >= 1))
}, .SDcols = istr_cols]
flags[, c(istr_cols) := NULL]

# Aggregate 8-fn and 6-fn binary indicators
fn_cols_8 <- c("POLIZIA", "RIFIUTI", "SOCIALE", "TERR", "VIAB", "ANAG", "TRIB", "ISTRUZ_any")
flags[, count_fund_8 := rowSums(.SD == 1, na.rm = TRUE), .SDcols = fn_cols_8]
flags[, n_valid_8 := rowSums(!is.na(.SD)), .SDcols = fn_cols_8]
flags[, high_comply_8 := as.integer(count_fund_8 >= 4)]
flags[n_valid_8 < 8, high_comply_8 := NA_integer_]

fn_cols_6 <- c("POLIZIA", "RIFIUTI", "SOCIALE", "TERR", "VIAB", "ISTRUZ_any")
flags[, count_fund_6 := rowSums(.SD == 1, na.rm = TRUE), .SDcols = fn_cols_6]
flags[, n_valid_6 := rowSums(!is.na(.SD)), .SDcols = fn_cols_6]
flags[, high_comply_6 := as.integer(count_fund_6 >= 3)]
flags[n_valid_6 < 6, high_comply_6 := NA_integer_]

d_full <- merge(d_ext, flags, by = "USERNAME", all.x = TRUE)

# ---- SDID runner ----------------------------------------------------------
run_sdid <- function(d_in, treatment_col, label) {
  d <- copy(d_in[in_treated(d_in) & !is.na(get(treatment_col))])
  d[, W := as.integer(get(treatment_col) == 1 & post == 1)]

  # Drop munis without a full balanced panel of years
  yrs <- sort(unique(d$year))
  n_yrs <- length(yrs)
  obs_per_unit <- d[, .N, by = id08]
  keep_ids <- obs_per_unit[N == n_yrs, id08]
  d <- d[id08 %in% keep_ids]

  n_tr <- uniqueN(d[get(treatment_col) == 1]$id08)
  n_ct <- uniqueN(d[get(treatment_col) == 0]$id08)
  if (n_tr == 0 || n_ct == 0) {
    return(data.table(label = label, n_treated = n_tr, n_control = n_ct,
                      sdid_est = NA_real_, sdid_se = NA_real_, sdid_t = NA_real_))
  }

  d_sdid <- d[, .(unit = id08, time = year, Y = farright_sh, W = W)]
  setup <- panel.matrices(as.data.frame(d_sdid))
  sd_est_obj <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  sd_se      <- sqrt(vcov(sd_est_obj, method = "jackknife"))
  sd_est     <- c(sd_est_obj)

  data.table(label = label, n_treated = n_tr, n_control = n_ct,
             sdid_est = sd_est, sdid_se = sd_se, sdid_t = sd_est / sd_se)
}

set.seed(20241201)
results <- list()

cat("\n========================================================\n")
cat("SDID: Aggregate compliance measures\n")
cat("========================================================\n")

results$post2010 <- run_sdid(d_full, "in_post2010_union",
                             "Post-2010 unione (Ministry)")
results$agg8     <- run_sdid(d_full, "high_comply_8",
                             "8-of-8 functions >= 4 (OpenCivitas)")
results$agg6     <- run_sdid(d_full, "high_comply_6",
                             "6-of-8 functions >= 4 (OpenCivitas)")

cat("\n========================================================\n")
cat("SDID: Single-function compliance measures\n")
cat("========================================================\n")

single_specs <- list(
  POLIZIA    = "Local police",
  RIFIUTI    = "Waste collection",
  SOCIALE    = "Social services",
  TERR       = "Territorial planning",
  VIAB       = "Road maintenance",
  ANAG       = "Civil registry",
  TRIB       = "Tax collection",
  ISTRUZ_any = "Education (any)"
)

for (fn in names(single_specs)) {
  results[[fn]] <- run_sdid(d_full, fn, single_specs[[fn]])
}

out <- rbindlist(results, idcol = "key")
print(out)
fwrite(out, "output/csvs/italy/sdid_compliance.csv")
cat("\nSaved to output/csvs/italy/sdid_compliance.csv\n")
