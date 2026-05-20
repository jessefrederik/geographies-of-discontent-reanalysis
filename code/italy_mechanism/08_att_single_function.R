################################################################################
# Single-function compliance ATT: for each of the 8 Article 14 fundamental
# functions individually, treat as compliers the munis that delivered THAT
# function in associated form in 2015 and as controls the matched munis that
# did not.
#
# Same TWFE + MTWFE spec as 05_att_count_fundamental.R; only the treatment
# definition changes. Sample is the per-function valid universe (all RSO munis
# with a non-NA flag for that function), restricted to the pooled
# Cremaschi-treated stratum.
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

ind_specs <- list(
  POLIZIA   = list(file = "Ind_FC20POLIZIA_3.csv",  var = "DUMMY_POLIZIA_ASSOC",     blank_as_zero = FALSE,
                   label = "Local police"),
  RIFIUTI   = list(file = "Ind_FC20RIFIUTI_3.csv",  var = "DUMMY_RIFIUTI_ASSOC",     blank_as_zero = TRUE,
                   label = "Waste collection"),
  SOCIALE   = list(file = "Ind_FC20SOCNID_2.csv",   var = "DUMMY_SOCIALE_ASSOCIATA", blank_as_zero = FALSE,
                   label = "Social services"),
  TERR      = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_TERR_ASSOCIATA",    blank_as_zero = FALSE,
                   label = "Territorial planning"),
  VIAB      = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_VIAB_ASSOCIATA",    blank_as_zero = FALSE,
                   label = "Road maintenance"),
  ANAG      = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_ANAGR_ASSOCIATA",   blank_as_zero = FALSE,
                   label = "Civil registry"),
  TRIB      = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_TRIBUTI_ASSOCIATA", blank_as_zero = FALSE,
                   label = "Tax collection"),
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

# Pool the 6 ISTRUZ sub-functions into a single "any education" flag
istr_cols <- c("IST_INFA","IST_PRSE","IST_REFE","IST_TRAS","IST_DISA","IST_ALTR")
flags[, ISTRUZ_any := {
  n_valid <- rowSums(!is.na(.SD))
  ifelse(n_valid == 0, NA_integer_, as.integer(rowSums(.SD == 1, na.rm = TRUE) >= 1))
}, .SDcols = istr_cols]
flags[, c(istr_cols) := NULL]

# Single-function treatment list: 8 Article-14 functions
fn_specs <- list(
  POLIZIA    = "Local police",
  RIFIUTI    = "Waste collection",
  SOCIALE    = "Social services",
  TERR       = "Territorial planning",
  VIAB       = "Road maintenance",
  ANAG       = "Civil registry",
  TRIB       = "Tax collection",
  ISTRUZ_any = "Education (any)"
)

# ---- Stratum: pooled Cremaschi-treated only ---------------------------------
in_treated <- function(x) (x$mont_group == 0 & x$pop_tot_2008 < 5000) |
                          (x$mont_group == 1 & x$pop_tot_2008 < 3000)

# ---- Run analyses ----------------------------------------------------------
set.seed(20241201)
results <- list()
matched_panels <- list()

cat("\n========================================================\n")
cat("SINGLE-FUNCTION ATT  (pooled Cremaschi-treated stratum)\n")
cat("========================================================\n")

for (fn in names(fn_specs)) {
  d_fn <- merge(d_ext, flags[, c("USERNAME", fn), with = FALSE],
                by = "USERNAME", all.x = TRUE)
  setnames(d_fn, fn, "flag")
  d_s <- d_fn[in_treated(d_fn) & !is.na(flag)]

  n_tr_pre <- uniqueN(d_s[flag == 1]$id08)
  n_ct_pre <- uniqueN(d_s[flag == 0]$id08)
  if (n_tr_pre == 0 || n_ct_pre == 0) {
    cat(sprintf("\n[%s]  skipped (treated=%d, control=%d)\n",
                fn, n_tr_pre, n_ct_pre))
    next
  }

  d_s[, t_flag := as.integer(flag == 1 & post == 1)]

  # TWFE (unmatched)
  m_twfe <- feols(farright_sh ~ t_flag | id08 + year, data = d_s, cluster = "id08")
  twfe_est <- coef(m_twfe)["t_flag"]
  twfe_se  <- se(m_twfe)["t_flag"]
  twfe_t   <- twfe_est / twfe_se

  # MTWFE: match each treated muni to a non-treated muni on baseline covariates
  d08 <- unique(d_s[year == min(year), c("id08", "flag", match_vars), with = FALSE])
  d08 <- d08[complete.cases(d08)]
  fml <- as.formula(paste("flag ~", paste(match_vars, collapse = " + ")))
  m_out <- tryCatch(matchit(fml, data = d08, method = "nearest",
                            distance = "mahalanobis", replace = TRUE),
                    error = function(e) NULL)
  if (!is.null(m_out)) {
    md  <- match.data(m_out)
    d_m <- merge(d_s, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
    m_mt <- feols(farright_sh ~ t_flag | id08 + year, data = d_m,
                  weights = d_m$weights, cluster = "id08")
    d_m[, fn_code := fn]
    matched_panels[[fn]] <- d_m[, .(id08, year, farright_sh, t_flag, weights, fn_code)]
    mtwfe_est <- coef(m_mt)["t_flag"]
    mtwfe_se  <- se(m_mt)["t_flag"]
    mtwfe_t   <- mtwfe_est / mtwfe_se
    n_tr_m    <- uniqueN(d_m[flag == 1]$id08)
    n_ct_m    <- uniqueN(d_m[flag == 0]$id08)

    s <- summary(m_out, standardize = TRUE)
    smd_pre  <- mean(abs(s$sum.all[, "Std. Mean Diff."]),     na.rm = TRUE)
    smd_post <- mean(abs(s$sum.matched[, "Std. Mean Diff."]), na.rm = TRUE)
  } else {
    mtwfe_est <- mtwfe_se <- mtwfe_t <- NA_real_
    n_tr_m <- n_ct_m <- NA_integer_
    smd_pre <- smd_post <- NA_real_
  }

  cat(sprintf("\n[%-10s %s]  n_tr=%d  n_ct=%d  share_treated=%.2f\n",
              fn, fn_specs[[fn]], n_tr_pre, n_ct_pre,
              n_tr_pre / (n_tr_pre + n_ct_pre)))
  cat(sprintf("  TWFE : %+8.4f (SE=%.4f, t=%5.2f)\n", twfe_est, twfe_se, twfe_t))
  cat(sprintf("  MTWFE: %+8.4f (SE=%.4f, t=%5.2f)   matched n_tr/n_ct=%d/%d   |SMD|: %.2f -> %.3f\n",
              mtwfe_est, mtwfe_se, mtwfe_t, n_tr_m, n_ct_m, smd_pre, smd_post))

  results[[fn]] <- data.table(
    function_code = fn,
    function_label = fn_specs[[fn]],
    n_treated = n_tr_pre,
    n_control = n_ct_pre,
    share_treated = n_tr_pre / (n_tr_pre + n_ct_pre),
    twfe_est = twfe_est, twfe_se = twfe_se, twfe_t = twfe_t,
    mtwfe_est = mtwfe_est, mtwfe_se = mtwfe_se, mtwfe_t = mtwfe_t,
    n_tr_matched = n_tr_m, n_ct_matched = n_ct_m,
    mean_abs_smd_pre = smd_pre, mean_abs_smd_post = smd_post
  )
}

out <- rbindlist(results)
cat("\n========================================================\n")
cat("SUMMARY\n")
cat("========================================================\n")
print(out)

fwrite(out, "output/csvs/italy/att_single_function.csv")
cat("\nSaved to output/csvs/italy/att_single_function.csv\n")

# ---- Joint test: are all 8 single-function MTWFE ATTs simultaneously zero? ----
# Stack the per-function MATCHED panels (each function has its own matched
# treated-control pairs with weights from MatchIt), then run a single weighted
# regression with function-specific treatment dummies and Wald-test that all
# eight equal zero. Standard errors clustered by municipality so the same
# id08 appearing in multiple function panels is accounted for.

d_stack_m <- rbindlist(matched_panels)
for (fn in names(fn_specs)) {
  d_stack_m[, (paste0("t_", fn)) := as.integer(t_flag == 1 & fn_code == fn)]
}
t_vars <- paste0("t_", names(fn_specs))
fml_joint <- as.formula(paste0("farright_sh ~ ", paste(t_vars, collapse = " + "),
                               " | id08^fn_code + year^fn_code"))
m_joint <- feols(fml_joint, data = d_stack_m, weights = d_stack_m$weights,
                 cluster = "id08")

joint_w <- wald(m_joint, keep = paste0("^t_", names(fn_specs), "$"), print = FALSE)
cat("\n========================================================\n")
cat("JOINT TEST: all 8 single-function MTWFE ATTs = 0 simultaneously\n")
cat("========================================================\n")
cat(sprintf("  Wald F(%d, %d) = %.3f, p = %.4f  (matched, clustered by id08)\n",
            joint_w$df1, joint_w$df2, joint_w$stat, joint_w$p))

# Bonferroni count on the individual MTWFE t-stats
bonf_alpha <- 0.05 / length(names(fn_specs))
bonf_thr   <- qnorm(1 - bonf_alpha / 2)
n_signif_bonf <- sum(abs(out$mtwfe_t) > bonf_thr, na.rm = TRUE)
cat(sprintf("  Bonferroni at alpha=0.05/%d => |t| threshold = %.2f; significant: %d / %d\n",
            length(names(fn_specs)), bonf_thr,
            n_signif_bonf, length(names(fn_specs))))
cat(sprintf("  Of the %d, signs are: %s\n",
            n_signif_bonf,
            paste(sign(out[abs(mtwfe_t) > bonf_thr]$mtwfe_est), collapse = ", ")))
