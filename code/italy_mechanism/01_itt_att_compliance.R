################################################################################
# ITT vs ATT TWFE: does the Cremaschi result reflect the legal mandate (ITT)
# or actual compliance with shared service delivery (ATT)?
#
# Spec: feols(farright_sh ~ t | id08 + year, cluster = "id08")
# Panel: extended (2001, 2006, 2008, 2013, 2018, 2022).
# t = treated x post-2010, where 'treated' is redefined per spec.
#
# Compliance flag: time-invariant 2015 status from OpenCivitas DUMMY_*_ASSOC.
#
# Specs run:
#   ITT      : treated = (mont=0 & pop<5000) | (mont=1 & pop<3000)  -- Cremaschi
#   ATT-A_*  : treated = sub-threshold AND 2015 complier; control = above-threshold
#              (drops sub-threshold non-compliers)
#   ATT-B_*  : treated = sub-threshold AND 2015 complier; control = sub-threshold
#              non-compliers (drops above-threshold)
# where * in {RIFIUTI, SOCIALE, POLIZIA, AMM_ALTRI}.
################################################################################

library(data.table)
library(fixest)

# ---- Extended panel + crosswalk -------------------------------------------
d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")
xw    <- fread("data_processed/italy/opencivitas_panel_crosswalk.csv")
d_ext <- merge(d_ext, xw[, .(id08, USERNAME)], by = "id08", all.x = TRUE)

# Toggle: include 2022 election or not
INCLUDE_2022 <- FALSE
if (!INCLUDE_2022) {
  d_ext <- d_ext[year != 2022]
  cat("** 2022 election EXCLUDED -- panel matches original Cremaschi years **\n")
}

cat(sprintf("Panel: %d obs, %d munis, years %s\n",
            nrow(d_ext), uniqueN(d_ext$id08),
            paste(sort(unique(d_ext$year)), collapse = ",")))

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

compliers <- lapply(services_2015, get_complier)
for (svc in names(compliers)) {
  cat(sprintf("  %s 2015: %d munis with valid flag, %.1f%% compliers\n",
              svc, nrow(compliers[[svc]]),
              100 * mean(compliers[[svc]]$complier)))
}

# ---- ITT (standard Cremaschi spec on extended panel) -----------------------
cat("\n========================================================\n")
cat("ITT  (treated = sub-threshold; standard Cremaschi spec)\n")
cat("========================================================\n")

m_itt <- feols(farright_sh ~ t | id08 + year, data = d_ext, cluster = "id08")
n_treated_itt <- uniqueN(d_ext[treated == 1]$id08)
n_control_itt <- uniqueN(d_ext[treated == 0]$id08)
cat(sprintf("  Estimate: %.4f   t = %.2f   N obs = %d\n",
            coef(m_itt)["t"], coef(m_itt)["t"]/se(m_itt)["t"], nobs(m_itt)))
cat(sprintf("  Munis treated: %d  control: %d\n",
            n_treated_itt, n_control_itt))

# ---- ATT runs --------------------------------------------------------------
results <- list()
results[["ITT"]] <- data.table(
  spec = "ITT", service = "(all)", variant = "(all sub-threshold)",
  est = coef(m_itt)["t"], t = coef(m_itt)["t"]/se(m_itt)["t"],
  n_obs = nobs(m_itt), n_treated = n_treated_itt, n_control = n_control_itt,
  share_compliers = NA_real_
)

for (svc in names(services_2015)) {
  comp <- compliers[[svc]]
  d_svc <- merge(d_ext, comp, by = "USERNAME", all.x = TRUE)
  # complier is NA for: munis not in OpenCivitas at all, or in OC but blank
  # for this indicator.

  # Compliance share: among sub-threshold munis with a valid flag, % compliers
  sub_with_flag <- unique(d_svc[treated == 1 & !is.na(complier), .(id08, complier)])
  share_comp <- mean(sub_with_flag$complier)

  # ---- ITT-RSO: restrict ITT to the same universe as ATT-C --------------
  # Same Cremaschi spec, but only on munis with a valid 2015 flag for this
  # service. Lets us read off how much the sample restriction alone moves
  # the ITT relative to the full-panel ITT (0.0154/0.0203).
  d_rso <- d_svc[!is.na(complier)]
  m_itt_rso <- feols(farright_sh ~ t | id08 + year, data = d_rso, cluster = "id08")
  n_tr_rso <- uniqueN(d_rso[treated == 1]$id08)
  n_ct_rso <- uniqueN(d_rso[treated == 0]$id08)

  cat(sprintf("\nITT-RSO (%s)  restrict to munis with valid 2015 flag for this service\n", svc))
  cat(sprintf("  Estimate: %.4f   t = %.2f   N obs = %d\n",
              coef(m_itt_rso)["t"], coef(m_itt_rso)["t"]/se(m_itt_rso)["t"], nobs(m_itt_rso)))
  cat(sprintf("  Munis treated: %d  control: %d\n", n_tr_rso, n_ct_rso))

  results[[paste0("ITT-RSO_", svc)]] <- data.table(
    spec = "ITT-RSO", service = svc, variant = "panel restricted to OC-flagged munis",
    est = coef(m_itt_rso)["t"], t = coef(m_itt_rso)["t"]/se(m_itt_rso)["t"],
    n_obs = nobs(m_itt_rso), n_treated = n_tr_rso, n_control = n_ct_rso,
    share_compliers = share_comp
  )

  # ---- ATT-A: above-threshold as control (ASYMMETRIC universe) ----------
  # Keep: above-threshold (any) OR sub-threshold + complier == 1.
  # Note: control arm is NOT restricted to OpenCivitas — asymmetric sample.
  d_A <- d_svc[treated == 0 | (treated == 1 & complier == 1)]
  d_A[, t_att := as.integer(treated == 1 & complier == 1 & post == 1)]
  m_A <- feols(farright_sh ~ t_att | id08 + year, data = d_A, cluster = "id08")
  n_tr_A <- uniqueN(d_A[treated == 1 & complier == 1]$id08)
  n_ct_A <- uniqueN(d_A[treated == 0]$id08)

  cat(sprintf("ATT-A (%s)  drop sub-thr non-compliers; control = ALL above-thr (incl RSS)\n", svc))
  cat(sprintf("  Estimate: %.4f   t = %.2f   N obs = %d\n",
              coef(m_A)["t_att"], coef(m_A)["t_att"]/se(m_A)["t_att"], nobs(m_A)))
  cat(sprintf("  Munis treated (compliers): %d  control (above-thr): %d  | sub-thr complier share: %.1f%%\n",
              n_tr_A, n_ct_A, 100*share_comp))

  results[[paste0("ATT-A_", svc)]] <- data.table(
    spec = "ATT-A", service = svc, variant = "above-threshold control (asymmetric)",
    est = coef(m_A)["t_att"], t = coef(m_A)["t_att"]/se(m_A)["t_att"],
    n_obs = nobs(m_A), n_treated = n_tr_A, n_control = n_ct_A,
    share_compliers = share_comp
  )

  # ---- ATT-B: sub-threshold non-compliers as control (RSO sub-thr only) -
  d_B <- d_svc[treated == 1 & !is.na(complier)]
  d_B[, t_att := as.integer(complier == 1 & post == 1)]
  m_B <- feols(farright_sh ~ t_att | id08 + year, data = d_B, cluster = "id08")
  n_tr_B <- uniqueN(d_B[complier == 1]$id08)
  n_ct_B <- uniqueN(d_B[complier == 0]$id08)

  cat(sprintf("ATT-B (%s)  drop above-thr; control = sub-thr non-compliers\n", svc))
  cat(sprintf("  Estimate: %.4f   t = %.2f   N obs = %d\n",
              coef(m_B)["t_att"], coef(m_B)["t_att"]/se(m_B)["t_att"], nobs(m_B)))
  cat(sprintf("  Munis treated (compliers): %d  control (non-compliers): %d\n", n_tr_B, n_ct_B))

  results[[paste0("ATT-B_", svc)]] <- data.table(
    spec = "ATT-B", service = svc, variant = "sub-threshold non-complier control",
    est = coef(m_B)["t_att"], t = coef(m_B)["t_att"]/se(m_B)["t_att"],
    n_obs = nobs(m_B), n_treated = n_tr_B, n_control = n_ct_B,
    share_compliers = share_comp
  )

  # ---- ATT-C: clean above-thr control restricted to OC universe ---------
  # Same as ATT-A but ALSO restrict above-threshold control to munis with
  # a valid 2015 flag for this service. This is the apples-to-apples ATT
  # that shares its universe with ITT-RSO and ATT-B.
  d_C <- d_svc[!is.na(complier) & (treated == 0 | (treated == 1 & complier == 1))]
  d_C[, t_att := as.integer(treated == 1 & complier == 1 & post == 1)]
  m_C <- feols(farright_sh ~ t_att | id08 + year, data = d_C, cluster = "id08")
  n_tr_C <- uniqueN(d_C[treated == 1 & complier == 1]$id08)
  n_ct_C <- uniqueN(d_C[treated == 0]$id08)

  cat(sprintf("ATT-C (%s)  symmetric: BOTH arms restricted to OC universe\n", svc))
  cat(sprintf("  Estimate: %.4f   t = %.2f   N obs = %d\n",
              coef(m_C)["t_att"], coef(m_C)["t_att"]/se(m_C)["t_att"], nobs(m_C)))
  cat(sprintf("  Munis treated (compliers): %d  control (above-thr in OC): %d\n", n_tr_C, n_ct_C))

  results[[paste0("ATT-C_", svc)]] <- data.table(
    spec = "ATT-C", service = svc, variant = "above-threshold control (symmetric, OC-only)",
    est = coef(m_C)["t_att"], t = coef(m_C)["t_att"]/se(m_C)["t_att"],
    n_obs = nobs(m_C), n_treated = n_tr_C, n_control = n_ct_C,
    share_compliers = share_comp
  )
}

# ---- Summary table --------------------------------------------------------
out <- rbindlist(results)
cat("\n========================================================\n")
cat("SUMMARY\n")
cat("========================================================\n")
print(out)

fwrite(out, "output/csvs/italy/itt_att_compliance.csv")
cat("\nSaved to output/csvs/italy/itt_att_compliance.csv\n")
