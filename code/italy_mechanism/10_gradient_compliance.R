################################################################################
# Gradient-controlled TWFE for the 11 compliance measures in Table 9.
#
# Specification mirrors Section 4 / Table 6: TWFE plus log(pop)*year interactions
# (year-specific slopes on log baseline population). If the size gradient drives
# the SDID positives, adding these interactions should collapse the compliance
# TWFE estimates the same way it collapses the placebo-threshold estimates.
#
# All on the pooled Cremaschi-treated stratum, 2001-2018 panel.
################################################################################

library(data.table)
library(fixest)

INCLUDE_2022 <- FALSE
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
d_ext[, log_pop := log(pop_tot_2008)]

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

# ---- Runner ----------------------------------------------------------------
run_grad <- function(d_in, treatment_col, label) {
  d <- d_in[in_treated(d_in) & !is.na(get(treatment_col))]
  d[, t_flag := as.integer(get(treatment_col) == 1 & post == 1)]

  # Plain TWFE (no size control)
  m_twfe <- feols(farright_sh ~ t_flag | id08 + year, data = d, cluster = "id08")
  twfe_est <- coef(m_twfe)["t_flag"]
  twfe_se  <- se(m_twfe)["t_flag"]

  # TWFE + log(pop) * year (gradient control, same as Section 4 / Table 6)
  ref_year <- min(d$year)
  m_grad <- feols(farright_sh ~ t_flag + i(year, log_pop, ref = ref_year) | id08 + year,
                  data = d, cluster = "id08")
  grad_est <- coef(m_grad)["t_flag"]
  grad_se  <- se(m_grad)["t_flag"]

  n_tr <- uniqueN(d[get(treatment_col) == 1]$id08)
  cat(sprintf("  %-40s n_tr=%-5d  TWFE=%+.4f (%.4f)   TWFE+log(pop)*year=%+.4f (%.4f)\n",
              label, n_tr, twfe_est, twfe_se, grad_est, grad_se))

  data.table(label = label, n_treated = n_tr,
             twfe_est = twfe_est, twfe_se = twfe_se,
             twfe_t = twfe_est / twfe_se,
             grad_est = grad_est, grad_se = grad_se,
             grad_t = grad_est / grad_se)
}

set.seed(20241201)
results <- list()

cat("\n========================================================\n")
cat("Gradient-controlled compliance TWFE\n")
cat("(pooled Cremaschi-treated stratum, 2001-2018)\n")
cat("========================================================\n")

results$post2010 <- run_grad(d_full, "in_post2010_union",
                             "Post-2010 unione (Ministry)")
results$agg8     <- run_grad(d_full, "high_comply_8",
                             "8-of-8 functions >= 4 (OpenCivitas)")
results$agg6     <- run_grad(d_full, "high_comply_6",
                             "6-of-8 functions >= 4 (OpenCivitas)")

cat("\n-- Single functions --\n")
single_specs <- list(
  SOCIALE    = "Social services",
  POLIZIA    = "Local police",
  RIFIUTI    = "Waste collection",
  ISTRUZ_any = "Education (any)",
  TERR       = "Territorial planning",
  TRIB       = "Tax collection",
  ANAG       = "Civil registry",
  VIAB       = "Road maintenance"
)

for (fn in names(single_specs)) {
  results[[fn]] <- run_grad(d_full, fn, single_specs[[fn]])
}

out <- rbindlist(results, idcol = "key")
fwrite(out, "output/csvs/italy/gradient_compliance.csv")
cat("\nSaved to output/csvs/italy/gradient_compliance.csv\n")
