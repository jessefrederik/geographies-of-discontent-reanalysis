################################################################################
# Geographies of Discontent: A Reanalysis
# Jesse Frederik, De Correspondent
#
# This script reproduces all tables and figures in the correspondence.
# All analyses use the electoral panel dataset from the Harvard Dataverse
# replication package of Cremaschi, Rettl, Cappelluti & De Vries (AJPS 2024).
#
# Software: R (tested on 4.4.x)
# Required packages: haven, fixest, MatchIt, data.table, rdrobust, synthdid
#
# Output mapping (code label -> paper exhibit):
#   TABLE 1  - Narrow-band TWFE estimates (Paper Table 1, tab:rd)
#   TABLE 2  - Temporal placebo + extended-sample estimates (Paper Table 2, tab:temporal)
#   TABLE 3  - Placebo threshold estimates: TWFE, MTWFE, SDID (Paper Table 3, tab:placebo)
#   TABLE 4  - Size-trend controls: TWFE and MTWFE (Paper Table 4, tab:gradient)
#   TABLE 5  - Mountain municipality narrow-band (Appendix D, tab:mountain_nb)
#   TABLE 5b - Mountain gradient controls + placebo sweep (Appendix D)
#   TABLE 6  - Above-5k placebo thresholds (Appendix E, tab:above5k)
#   TABLE 7  - Event study (Paper Table 5, tab:eventstudy)
#   APP A    - 2022 extension: TWFE, event study, placebos (tab:twfe_2022, tab:eventstudy_2022, tab:placebo_2022)
#   FIGURE 1 - Placebo scatter: DID vs log(pop) gap (Paper Figure 3, fig:scatter)
#   INLINE   - Diff-in-Disc and cross-sectional RD estimates + McCrary test (Section 1.1)
################################################################################

set.seed(20241201)

library(haven)
library(fixest)
library(MatchIt)
library(data.table)
library(rdrobust)
library(rddensity)
library(synthdid)

dir.create("output", showWarnings = FALSE)

cat("====================================================================\n")
cat("LOAD DATA\n")
cat("====================================================================\n\n")

d <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
d[, log_pop := log(pop_tot_2008)]

# Population rank controls
d_base <- d[year == 2008]
d_base[, log_rank := log(.N + 1 - rank(pop_tot_2008))]  # rank 1 = largest
d_base[, pop_pctile := rank(pop_tot_2008) / .N]          # continuous percentile (0-1)
d <- merge(d, d_base[, .(id08, log_rank, pop_pctile)], by = "id08")

cat(sprintf("Dataset: %d obs x %d vars (%d municipalities x %d elections)\n",
            nrow(d), ncol(d), uniqueN(d$id08), uniqueN(d$year)))
cat(sprintf("Treated: %d  Control: %d\n",
            uniqueN(d$id08[d$treated == 1]),
            uniqueN(d$id08[d$treated == 0])))

# Verify treatment reconstruction
d[, verify_treated := as.integer((mont_group == 0 & pop_tot_2008 < 5000) |
                                  (mont_group == 1 & pop_tot_2008 < 3000))]
stopifnot(all(d$treated == d$verify_treated, na.rm = TRUE))
cat("Treatment variable reconstruction: VERIFIED\n")

# Replication check: paper's Table 1 Col 1
m_paper <- feols(farright_sh ~ t | id08 + year, data = d, cluster = "id08")
cat(sprintf("Paper Table 1 Col 1 replication: %.4f (paper: 0.015) — %s\n\n",
            coef(m_paper)["t"],
            ifelse(abs(coef(m_paper)["t"] - 0.015) < 0.001, "MATCH", "CHECK")))

################################################################################
cat("====================================================================\n")
cat("TABLE 1: Narrow-band TWFE estimates (Paper Table 1, tab:rd)\n")
cat("====================================================================\n\n")
# TWFE on progressively narrower population bands around the 5,000 threshold.

bandwidths <- c(Inf, 5000, 3000, 2000, 1000, 500, 250, 100)

cat(sprintf("%-15s %10s %10s %8s\n", "Bandwidth", "Estimate", "t-stat", "N"))

tab1_rows <- list()
for (bw in bandwidths) {
  if (is.infinite(bw)) {
    dsub <- d
  } else {
    # Dual center: 5,000 for plain municipalities, 3,000 for mountain
    dsub <- d[ifelse(mont_group == 1,
                     abs(pop_tot_2008 - 3000),
                     abs(pop_tot_2008 - 5000)) <= bw]
  }
  m <- feols(farright_sh ~ t | id08 + year, data = dsub, cluster = "id08")
  label <- if (is.infinite(bw)) "Full sample" else sprintf("+/-%s", format(bw, big.mark = ","))
  cat(sprintf("%-15s %10.4f %10.2f %8d\n",
              label, coef(m)["t"], coef(m)["t"] / se(m)["t"], nobs(m)))
  label_tex <- if (is.infinite(bw)) "Full sample" else sprintf("$\\pm$%s", format(bw, big.mark = ","))
  tab1_rows[[length(tab1_rows) + 1]] <- list(
    label_tex = label_tex, est = coef(m)["t"], se = se(m)["t"],
    tstat = coef(m)["t"] / se(m)["t"], n = nobs(m),
    n_mun = uniqueN(dsub$id08))
}

################################################################################
cat("\n====================================================================\n")
cat("TABLE 3: Placebo threshold estimates (Paper Table 3, tab:placebo)\n")
cat("====================================================================\n\n")
# Applies the paper's three estimators at ten population cutoffs.

thresholds <- c(2000, 5000, 7500, 10000, 20000, 35000, 50000)
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008", "share_university2001",
                "max_altitude")

cat(sprintf("%-10s %8s %8s %8s %8s %8s %8s\n",
            "Threshold", "TWFE", "t", "MTWFE", "t", "SDID", "t"))

placebo_results <- data.frame(threshold = integer(), estimate = numeric(),
                               log_pop_gap = numeric(), stringsAsFactors = FALSE)
tab3_rows <- list()

for (thr in thresholds) {
  # Define treatment
  if (thr == 5000) {
    d[, pl_tr := as.integer((mont_group == 0 & pop_tot_2008 < 5000) |
                             (mont_group == 1 & pop_tot_2008 < 3000))]
  } else {
    d[, pl_tr := as.integer(pop_tot_2008 < thr)]
  }
  d[, pl_t := as.integer(pl_tr == 1 & year > 2010)]

  # TWFE
  m_twfe <- feols(farright_sh ~ pl_t | id08 + year, data = d, cluster = "id08")
  twfe_est <- coef(m_twfe)["pl_t"]
  twfe_t <- twfe_est / se(m_twfe)["pl_t"]

  # Log(pop) gap for Figure 1
  gap <- mean(d[pl_tr == 0]$log_pop) - mean(d[pl_tr == 1]$log_pop)
  placebo_results <- rbind(placebo_results, data.frame(
    threshold = thr, estimate = twfe_est, log_pop_gap = gap))

  # MTWFE
  d08 <- d[year == 2008]
  fml <- as.formula(paste("pl_tr ~", paste(match_vars, collapse = " + ")))
  m_out <- matchit(fml, data = d08, method = "nearest",
                   distance = "mahalanobis", replace = TRUE)
  md <- match.data(m_out)
  d_m <- merge(d, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
  m_mtwfe <- feols(farright_sh ~ pl_t | id08 + year, data = d_m,
                   weights = d_m$weights, cluster = "id08")
  mtwfe_est <- coef(m_mtwfe)["pl_t"]
  mtwfe_t <- mtwfe_est / se(m_mtwfe)["pl_t"]

  # SDID
  d_sdid <- d[, .(unit = id08, time = year, Y = farright_sh, W = pl_t)]
  setup <- panel.matrices(as.data.frame(d_sdid))
  sdid <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  sdid_se <- sqrt(vcov(sdid, method = "jackknife"))
  sdid_est <- c(sdid)
  sdid_t <- sdid_est / sdid_se

  cat(sprintf("%-10d %8.3f %8.1f %8.3f %8.1f %8.3f %8.1f\n",
              thr, twfe_est, twfe_t, mtwfe_est, mtwfe_t, sdid_est, sdid_t))
  tab3_rows[[length(tab3_rows) + 1]] <- list(
    thr = thr, twfe = twfe_est, twfe_se = se(m_twfe)["pl_t"], twfe_t = twfe_t,
    mtwfe = mtwfe_est, mtwfe_se = se(m_mtwfe)["pl_t"], mtwfe_t = mtwfe_t,
    sdid = sdid_est, sdid_se = sdid_se, sdid_t = sdid_t)
}

################################################################################
cat("\n====================================================================\n")
cat("TABLE 2: Temporal placebo + extended-sample estimates (Paper Table 2, tab:temporal)\n")
cat("====================================================================\n\n")
# Pre-reform placebos, actual reform, 2022 extension, and post-reform placebo.

cat(sprintf("%-45s %10s %10s %8s\n", "Specification", "Estimate", "t-stat", "N"))

# Fake reform in 2001
d_pre <- d[year %in% c(2001, 2006, 2008)]
d_pre[, t_fake := as.integer(treated == 1 & year > 2001)]
m_2001 <- feols(farright_sh ~ t_fake | id08 + year, data = d_pre, cluster = "id08")
cat(sprintf("%-45s %10.4f %10.2f %8d\n", "2001 (placebo): 2001, 2006, 2008",
            coef(m_2001)["t_fake"], coef(m_2001)["t_fake"]/se(m_2001)["t_fake"],
            nobs(m_2001)))

# Fake reform in 2006
d_pre[, t_fake := as.integer(treated == 1 & year > 2006)]
m_2006 <- feols(farright_sh ~ t_fake | id08 + year, data = d_pre, cluster = "id08")
cat(sprintf("%-45s %10.4f %10.2f %8d\n", "2006 (placebo): 2001, 2006, 2008",
            coef(m_2006)["t_fake"], coef(m_2006)["t_fake"]/se(m_2006)["t_fake"],
            nobs(m_2006)))

# Actual reform (original sample)
cat(sprintf("%-45s %10.4f %10.2f %8d\n", "2010 (actual): 2001-2018",
            coef(m_paper)["t"], coef(m_paper)["t"]/se(m_paper)["t"],
            nobs(m_paper)))

# 2022 extended sample + post-reform placebo
if (file.exists("data_processed/italy/electoral_panel_extended.csv")) {
  d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")

  # 2010 actual with extended sample
  m_ext <- feols(farright_sh ~ t | id08 + year, data = d_ext, cluster = "id08")
  cat(sprintf("%-45s %10.4f %10.2f %8d\n", "2010 (actual): 2001-2022",
              coef(m_ext)["t"], coef(m_ext)["t"]/se(m_ext)["t"], nobs(m_ext)))

  # Post-reform placebo at 2014
  d_post <- d_ext[year %in% c(2013, 2018, 2022)]
  d_post[, t_fake := as.integer(treated == 1 & year > 2014)]
  m_2014 <- feols(farright_sh ~ t_fake | id08 + year, data = d_post, cluster = "id08")
  cat(sprintf("%-45s %10.4f %10.2f %8d\n", "2014 (placebo): 2013, 2018, 2022",
              coef(m_2014)["t_fake"], coef(m_2014)["t_fake"]/se(m_2014)["t_fake"],
              nobs(m_2014)))
} else {
  cat("  (skipping 2022 rows — run 02_extend_panel.R first)\n")
}

################################################################################
cat("\n====================================================================\n")
cat("TABLE 4: Size-trend controls (Paper Table 4, tab:gradient)\n")
cat("====================================================================\n\n")
# Adds log(pop) x year interactions to TWFE and MTWFE.

# TWFE
m_twfe_base <- feols(farright_sh ~ t | id08 + year, data = d, cluster = "id08")
m_twfe_logpop <- feols(farright_sh ~ t + log_pop:i(year) | id08 + year,
                       data = d, cluster = "id08")
m_twfe_pctile <- feols(farright_sh ~ t + pop_pctile:i(year) | id08 + year,
                       data = d, cluster = "id08")

# MTWFE (matching at real threshold)
d08 <- d[year == 2008]
fml <- as.formula(paste("treated ~", paste(match_vars, collapse = " + ")))
m_out <- matchit(fml, data = d08, method = "nearest",
                 distance = "mahalanobis", replace = TRUE)
md <- match.data(m_out)
d_m <- merge(d, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
d_m[, log_pop := log(pop_tot_2008)]

gap_before <- mean(d08[treated == 0]$pop_tot_2008) - mean(d08[treated == 1]$pop_tot_2008)
ctrl_m <- md[md$treated == 0, ]
gap_after <- weighted.mean(ctrl_m$pop_tot_2008, ctrl_m$weights) -
             mean(md[md$treated == 1, ]$pop_tot_2008)
cat(sprintf("Population gap: %.0f (before matching) -> %.0f (after)\n\n", gap_before, gap_after))

m_mtwfe_base <- feols(farright_sh ~ t | id08 + year, data = d_m,
                      weights = d_m$weights, cluster = "id08")
m_mtwfe_logpop <- feols(farright_sh ~ t + log_pop:i(year) | id08 + year,
                        data = d_m, weights = d_m$weights, cluster = "id08")
m_mtwfe_pctile <- feols(farright_sh ~ t + pop_pctile:i(year) | id08 + year,
                        data = d_m, weights = d_m$weights, cluster = "id08")

# Triple-interaction test for the bad-control objection (response to reviewer #2):
# log(pop) x year x treated. If the gradient steepened DIFFERENTLY for
# Cremaschi-treated communes relative to control communes post-2010, this
# coefficient picks it up. A null here means the gradient evolves identically
# in treated and control groups, closing the 'log(pop) x year absorbs a true
# treatment effect' loophole.
m_triple <- feols(farright_sh ~ t + t:log_pop + log_pop:i(year) | id08 + year,
                  data = d, cluster = "id08")
tri_est <- coef(m_triple)["t:log_pop"]
tri_se  <- se(m_triple)["t:log_pop"]
cat(sprintf("\nTriple interaction (t:log_pop): %+.4f (SE %.4f, t = %+.2f, p = %.3f)\n",
            tri_est, tri_se, tri_est / tri_se, 2 * pnorm(-abs(tri_est / tri_se))))

cat(sprintf("%-45s %10s %10s\n", "Specification", "Estimate", "t-stat"))
for (s in list(
  list("TWFE (paper)", m_twfe_base),
  list("TWFE + log(pop) x year", m_twfe_logpop),
  list("TWFE + pctile(pop) x year", m_twfe_pctile),
  list("MTWFE (paper)", m_mtwfe_base),
  list("MTWFE + log(pop) x year", m_mtwfe_logpop),
  list("MTWFE + pctile(pop) x year", m_mtwfe_pctile)
)) {
  cat(sprintf("%-45s %10.4f %10.2f\n",
              s[[1]], coef(s[[2]])["t"], coef(s[[2]])["t"]/se(s[[2]])["t"]))
}

################################################################################
cat("\n====================================================================\n")
cat("TABLE 4b: Covariate vs size controls (tab:covariate_mediation)\n")
cat("====================================================================\n\n")
# Tests whether the size gradient persists beyond matched covariates.

cov_vars <- c("foreign_share_2008", "over65_share_2008", "mean_income2008",
              "share_university2001", "female_share_2008", "max_altitude")
cov_interaction <- paste0(cov_vars, ":i(year)", collapse = " + ")

# TWFE + covariate x year only
fml_cov <- as.formula(paste("farright_sh ~ t +", cov_interaction, "| id08 + year"))
m_twfe_cov <- feols(fml_cov, data = d, cluster = "id08")

# TWFE + covariate x year + log(pop) x year
fml_both <- as.formula(paste("farright_sh ~ t + log_pop:i(year) +",
                              cov_interaction, "| id08 + year"))
m_twfe_both <- feols(fml_both, data = d, cluster = "id08")

cat(sprintf("%-50s %10s %10s\n", "Specification", "Estimate", "t-stat"))
for (s in list(
  list("TWFE (baseline)", m_twfe_base),
  list("TWFE + covariate x year", m_twfe_cov),
  list("TWFE + log(pop) x year", m_twfe_logpop),
  list("TWFE + covariate x year + log(pop) x year", m_twfe_both)
)) {
  cat(sprintf("%-50s %10.4f %10.2f\n",
              s[[1]], coef(s[[2]])["t"], coef(s[[2]])["t"]/se(s[[2]])["t"]))
}

################################################################################
cat("\n====================================================================\n")
cat("TABLE 4c: Gradient controls at all thresholds (tab:gradient_thresholds)\n")
cat("====================================================================\n\n")
# Shows that log(pop) x year controls eliminate estimates at every threshold.

grad_thresholds <- c(2000, 5000, 10000, 20000, 50000)
cat(sprintf("%-10s %10s %10s %10s %10s\n",
            "Threshold", "TWFE", "SE", "TWFE+grad", "SE"))
tab_grad_thr <- list()
for (thr in grad_thresholds) {
  if (thr == 5000) {
    d[, pl_tr := as.integer((mont_group == 0 & pop_tot_2008 < 5000) |
                             (mont_group == 1 & pop_tot_2008 < 3000))]
  } else {
    d[, pl_tr := as.integer(pop_tot_2008 < thr)]
  }
  d[, pl_t := as.integer(pl_tr == 1 & year > 2010)]

  m1 <- feols(farright_sh ~ pl_t | id08 + year, data = d, cluster = "id08")
  m2 <- feols(farright_sh ~ pl_t + log_pop:i(year) | id08 + year,
              data = d, cluster = "id08")
  cat(sprintf("%-10d %10.4f %10.4f %10.4f %10.4f\n",
              thr, coef(m1)["pl_t"], se(m1)["pl_t"],
              coef(m2)["pl_t"], se(m2)["pl_t"]))
  tab_grad_thr[[length(tab_grad_thr) + 1]] <- list(
    thr = thr,
    twfe = coef(m1)["pl_t"], twfe_se = se(m1)["pl_t"],
    twfe_t = coef(m1)["pl_t"] / se(m1)["pl_t"],
    grad = coef(m2)["pl_t"], grad_se = se(m2)["pl_t"],
    grad_t = coef(m2)["pl_t"] / se(m2)["pl_t"])
}

################################################################################
cat("\n====================================================================\n")
cat("FIGURE 1: Placebo scatter (Paper Figure 3, fig:scatter)\n")
cat("====================================================================\n\n")
# Runs TWFE at every threshold from 1,000 to 50,000 in steps of 250.

scatter_thresholds <- seq(1000, 50000, by = 250)
scatter_results <- data.frame(threshold = integer(), estimate = numeric(),
                               log_pop_gap = numeric())

for (thr in scatter_thresholds) {
  # Use dual threshold at 5,000 to match paper's treatment definition
  if (thr == 5000) {
    d[, pl_tr := as.integer((mont_group == 0 & pop_tot_2008 < 5000) |
                             (mont_group == 1 & pop_tot_2008 < 3000))]
  } else {
    d[, pl_tr := as.integer(pop_tot_2008 < thr)]
  }
  n_tr <- sum(d[year == 2008]$pl_tr == 1)
  n_ct <- sum(d[year == 2008]$pl_tr == 0)
  if (n_tr < 30 || n_ct < 30) next

  d[, pl_t := as.integer(pl_tr == 1 & year > 2010)]
  m <- feols(farright_sh ~ pl_t | id08 + year, data = d, cluster = "id08")
  gap <- mean(d[pl_tr == 0]$log_pop) - mean(d[pl_tr == 1]$log_pop)
  scatter_results <- rbind(scatter_results, data.frame(
    threshold = thr, estimate = coef(m)["pl_t"], log_pop_gap = gap))
}

fit <- lm(estimate ~ log_pop_gap, data = scatter_results)
fit_coef <- coef(fit)
fit_se   <- coef(summary(fit))[, "Std. Error"]
fit_r2   <- summary(fit)$r.squared
cat(sprintf("Regression: DID = %.4f (SE %.4f) + %.4f (SE %.4f) * log(pop) gap, R^2 = %.3f (%d thresholds)\n",
            fit_coef[1], fit_se[1], fit_coef[2], fit_se[2], fit_r2, nrow(scatter_results)))
# Predicted spurious bias at the 5,000-inhabitant cutoff (Appendix B formula)
real_idx_fit <- which.min(abs(scatter_results$threshold - 5000))
real_gap     <- scatter_results$log_pop_gap[real_idx_fit]
real_pred    <- as.numeric(fit_coef[1] + fit_coef[2] * real_gap)
real_obs     <- scatter_results$estimate[real_idx_fit]
cat(sprintf("At the 5,000 cutoff: log-pop gap = %.3f (ratio %.1fx); predicted DID = %.4f, observed = %.4f\n",
            real_gap, exp(real_gap), real_pred, real_obs))

# Generate Figure 2
# Plot on ratio scale (exp of log-pop gap) for interpretability
scatter_results$pop_ratio <- exp(scatter_results$log_pop_gap)
ratio_ticks <- c(8, 10, 15, 20, 30)
log_ticks <- log(ratio_ticks)

pdf("output/figures/italy/placebo_scatter.pdf", width = 5.5, height = 4)
par(mar = c(4.5, 4.5, 1.5, 1), family = "serif")
plot(scatter_results$log_pop_gap, scatter_results$estimate,
     pch = 16, cex = 0.5, col = "grey50",
     xlab = expression("Mean population ratio (control / treated)"),
     ylab = "DID estimate",
     xlim = range(scatter_results$log_pop_gap) * c(0.95, 1.05),
     ylim = range(scatter_results$estimate) * c(0.9, 1.1),
     las = 1, cex.lab = 1.0, cex.axis = 0.85, xaxt = "n")
axis(1, at = log_ticks, labels = paste0(ratio_ticks, "\u00d7"), cex.axis = 0.85)
abline(fit, col = "firebrick", lwd = 2, lty = 2)
real_idx <- which.min(abs(scatter_results$threshold - 5000))
real <- scatter_results[real_idx, ]
points(real$log_pop_gap, real$estimate, pch = 16, cex = 1.4, col = "firebrick")
text(real$log_pop_gap + 0.08, real$estimate - 0.0010,
     "5,000 (reform)", cex = 0.75, col = "firebrick", adj = 0)
text(min(scatter_results$log_pop_gap) + 0.05,
     max(scatter_results$estimate) * 0.97,
     bquote(R^2 == .(sprintf("%.2f", summary(fit)$r.squared))),
     cex = 0.9, adj = 0)
dev.off()
cat("Saved output/placebo_scatter.pdf\n")

################################################################################
cat("\n====================================================================\n")
cat("TABLE 5: Mountain municipality narrow-band (Appendix D, tab:mountain_nb)\n")
cat("====================================================================\n\n")
# Narrow-band TWFE within the mountain subsample at 3,000 threshold.

d_mtn <- d[mont_group == 1]
d_mtn[, tr_mtn := as.integer(pop_tot_2008 < 3000)]
d_mtn[, t_mtn := as.integer(tr_mtn == 1 & year > 2010)]

cat(sprintf("%-15s %10s %10s %8s\n", "Bandwidth", "Estimate", "t-stat", "N"))

tab_mtn_rows <- list()
for (bw in c(Inf, 5000, 3000, 2000, 1000, 500)) {
  if (is.infinite(bw)) {
    dsub <- d_mtn
  } else {
    dsub <- d_mtn[abs(pop_tot_2008 - 3000) <= bw]
  }
  m <- feols(farright_sh ~ t_mtn | id08 + year, data = dsub, cluster = "id08")
  label <- if (is.infinite(bw)) "Full sample" else sprintf("+/-%s", format(bw, big.mark = ","))
  cat(sprintf("%-15s %10.4f %10.2f %8d\n",
              label, coef(m)["t_mtn"], coef(m)["t_mtn"] / se(m)["t_mtn"], nobs(m)))
  label_tex <- if (is.infinite(bw)) "Full sample" else sprintf("$\\pm$%s", format(bw, big.mark = ","))
  tab_mtn_rows[[length(tab_mtn_rows) + 1]] <- list(
    label_tex = label_tex, est = coef(m)["t_mtn"], se = se(m)["t_mtn"],
    tstat = coef(m)["t_mtn"] / se(m)["t_mtn"], n = nobs(m),
    n_mun = uniqueN(dsub$id08))
}

################################################################################
cat("\n====================================================================\n")
cat("TABLE 5b: Mountain gradient controls + placebo sweep (Appendix D)\n")
cat("====================================================================\n\n")
# Gradient controls and 79-threshold placebo sweep within mountain subsample.

# Reuse mountain subsample from TABLE 5
m_mtn_base <- feols(farright_sh ~ t_mtn | id08 + year, data = d_mtn, cluster = "id08")
m_mtn_logpop <- feols(farright_sh ~ t_mtn + log_pop:i(year) | id08 + year,
                       data = d_mtn, cluster = "id08")

cat(sprintf("%-40s %10s %10s\n", "Specification", "Estimate", "t-stat"))
cat(sprintf("%-40s %10.4f %10.2f\n", "Mountain TWFE (base)",
            coef(m_mtn_base)["t_mtn"], coef(m_mtn_base)["t_mtn"] / se(m_mtn_base)["t_mtn"]))
cat(sprintf("%-40s %10.4f %10.2f\n", "Mountain TWFE + log(pop) x year",
            coef(m_mtn_logpop)["t_mtn"], coef(m_mtn_logpop)["t_mtn"] / se(m_mtn_logpop)["t_mtn"]))

# 79-threshold placebo sweep (500 to 20,000 by 250)
mtn_thresholds <- seq(500, 20000, by = 250)
mtn_scatter <- data.frame(threshold = integer(), estimate = numeric(),
                           se = numeric(), log_pop_gap = numeric())

for (thr in mtn_thresholds) {
  d_mtn[, pl_tr := as.integer(pop_tot_2008 < thr)]
  n_tr <- sum(d_mtn[year == 2008]$pl_tr == 1)
  n_ct <- sum(d_mtn[year == 2008]$pl_tr == 0)
  if (n_tr < 30 || n_ct < 30) next

  d_mtn[, pl_t := as.integer(pl_tr == 1 & year > 2010)]
  m <- feols(farright_sh ~ pl_t | id08 + year, data = d_mtn, cluster = "id08")
  gap <- mean(d_mtn[pl_tr == 0]$log_pop) - mean(d_mtn[pl_tr == 1]$log_pop)
  mtn_scatter <- rbind(mtn_scatter, data.frame(
    threshold = thr, estimate = coef(m)["pl_t"],
    se = se(m)["pl_t"], log_pop_gap = gap))
}

mtn_fit <- lm(estimate ~ log_pop_gap, data = mtn_scatter)
n_sig <- sum(abs(mtn_scatter$estimate / mtn_scatter$se) > 1.96)
cat(sprintf("\nMountain placebo sweep: %d testable thresholds (of %d)\n",
            nrow(mtn_scatter), length(mtn_thresholds)))
cat(sprintf("Meta-regression: DID = %.4f + %.4f * log(pop) gap, R^2 = %.3f\n",
            coef(mtn_fit)[1], coef(mtn_fit)[2], summary(mtn_fit)$r.squared))
cat(sprintf("Significant at 5%%: %d / %d thresholds\n", n_sig, nrow(mtn_scatter)))

################################################################################
cat("\n====================================================================\n")
cat("TABLE 6: Above-5k placebo thresholds (Appendix E, tab:above5k)\n")
cat("====================================================================\n\n")
# Placebo thresholds using only untreated municipalities (pop >= 5,000).

d_above <- d[pop_tot_2008 >= 5000]
above_thresholds <- c(7000, 8000, 10000, 12000, 15000, 20000, 30000)

cat(sprintf("%-10s %10s %10s %8s\n", "Threshold", "Estimate", "t-stat", "N"))

tab6_rows <- list()
for (thr in above_thresholds) {
  d_above[, pl_tr := as.integer(pop_tot_2008 < thr)]
  d_above[, pl_t := as.integer(pl_tr == 1 & year > 2010)]
  m <- feols(farright_sh ~ pl_t | id08 + year, data = d_above, cluster = "id08")
  cat(sprintf("%-10d %10.4f %10.2f %8d\n",
              thr, coef(m)["pl_t"], coef(m)["pl_t"] / se(m)["pl_t"], nobs(m)))
  tab6_rows[[length(tab6_rows) + 1]] <- list(
    thr = thr, est = coef(m)["pl_t"], se = se(m)["pl_t"],
    tstat = coef(m)["pl_t"] / se(m)["pl_t"], n = nobs(m),
    n_mun = uniqueN(d_above$id08))
}

################################################################################
cat("\n====================================================================\n")
cat("INLINE: Diff-in-Disc and cross-sectional RD (Section 1.1)\n")
cat("====================================================================\n\n")
# These estimates appear inline in the text, not in a numbered table.
#
# SIGN CONVENTION: rdrobust reports tau = lim(x -> 0+) - lim(x -> 0-).
# With running variable r = pop - 5000, x > 0 is ABOVE the threshold
# (untreated) and x < 0 is BELOW (treated by the reform). The raw output
# is therefore (above - below) = (control - treated). The paper inverts
# the sign in prose so positive means "sub-threshold communes swung more
# far-right" -- matching the TWFE/headline convention. If you reproduce
# from this script, multiply the printed coefficient by -1 to compare.

# Running variable
d[, r := ifelse(mont_group == 1, pop_tot_2008 - 3000, pop_tot_2008 - 5000)]

# Municipality-level change scores
d_pre  <- d[year <= 2008, .(fr_pre = mean(farright_sh, na.rm = TRUE)),
            by = .(id08, r, treated, mont_group, pop_tot_2008)]
d_post <- d[year > 2010,  .(fr_post = mean(farright_sh, na.rm = TRUE)), by = id08]
d_x <- merge(d_pre, d_post, by = "id08")
d_x[, delta_fr := fr_post - fr_pre]

# Pooled Diff-in-Disc (rdrobust)
cat("Diff-in-Disc (rdrobust, optimal bandwidth):\n")
rd_all <- rdrobust(d_x$delta_fr, d_x$r)
cat(sprintf("  Pooled:       est = %.3f, p = %.2f\n", rd_all$coef[1], rd_all$pv[1]))

# Cross-sectional RD by year
cat("\nCross-sectional RD by election year:\n")
rd_years <- c(2001, 2006, 2008, 2013, 2018)
rd_data <- d
if (exists("d_ext")) {
  rd_years <- c(rd_years, 2022)
  rd_data <- d_ext
}
for (yr in rd_years) {
  d_yr <- rd_data[year == yr]
  d_yr[, r_yr := ifelse(mont_group == 1, pop_tot_2008 - 3000, pop_tot_2008 - 5000)]
  rd_yr <- rdrobust(d_yr$farright_sh, d_yr$r_yr)
  cat(sprintf("  %d: est = %.3f, SE = %.3f, p = %.2f\n",
              yr, rd_yr$coef[1], rd_yr$se[1], rd_yr$pv[1]))
}

# McCrary density test (formal)
cat("\nMcCrary density test (rddensity):\n")
mcc <- rddensity(d_x$r, c = 0)
cat(sprintf("  T-statistic = %.3f, p-value = %.3f\n", mcc$test$t_jk, mcc$test$p_jk))

# Simple density check
below <- nrow(d_x[r >= -500 & r < 0])
above <- nrow(d_x[r >= 0 & r < 500])
cat(sprintf("  Bin count: %d below, %d above threshold (ratio %.2f)\n", below, above, below/above))

################################################################################
cat("\n====================================================================\n")
cat("TABLE 7: Event study (Paper Table 5, tab:eventstudy)\n")
cat("====================================================================\n\n")
# Year-specific treatment interactions with 2008 as base year.

cat(sprintf("%-8s %10s %10s\n", "Year", "Estimate", "t-stat"))

m_es <- feols(farright_sh ~ t01 + t06 + t13 + t18 | id08 + year, data = d, cluster = "id08")
for (v in c("t01", "t06", "t13", "t18")) {
  yr <- as.integer(gsub("t0?", "20", v))
  if (v == "t01") yr <- 2001
  if (v == "t06") yr <- 2006
  if (v == "t13") yr <- 2013
  if (v == "t18") yr <- 2018
  cat(sprintf("%-8d %10.4f %10.1f\n", yr, coef(m_es)[v], coef(m_es)[v] / se(m_es)[v]))
}
cat(sprintf("%-8d %10s %10s\n", 2008, "(base)", ""))

################################################################################
cat("\n====================================================================\n")
cat("APPENDIX A: Extended sample 2022 (tab:twfe_2022, tab:eventstudy_2022, tab:placebo_2022)\n")
cat("====================================================================\n\n")

if (file.exists("data_processed/italy/electoral_panel_extended.csv")) {
  d_ext <- fread("data_processed/italy/electoral_panel_extended.csv")

  # Table C1: TWFE original vs extended
  cat("Table C1: TWFE original vs extended\n")
  m_orig <- feols(farright_sh ~ t | id08 + year, data = d, cluster = "id08")
  m_ext_twfe <- feols(farright_sh ~ t | id08 + year, data = d_ext, cluster = "id08")
  d_ext[, log_pop := log(pop_tot_2008)]
  m_ext_lp <- feols(farright_sh ~ t + log_pop:i(year) | id08 + year, data = d_ext, cluster = "id08")
  cat(sprintf("  2001-2018 (original): %.3f  t=%.1f  N=%d\n",
      coef(m_orig)["t"], coef(m_orig)["t"]/se(m_orig)["t"], nobs(m_orig)))
  cat(sprintf("  2001-2022 (extended): %.3f  t=%.1f  N=%d\n",
      coef(m_ext_twfe)["t"], coef(m_ext_twfe)["t"]/se(m_ext_twfe)["t"], nobs(m_ext_twfe)))
  cat(sprintf("  2001-2022 + log(pop)xyear: %.3f  t=%.1f  N=%d\n",
      coef(m_ext_lp)["t"], coef(m_ext_lp)["t"]/se(m_ext_lp)["t"], nobs(m_ext_lp)))

  # Table C2: Event study with 2022
  cat("\nTable C2: Event study with 2022\n")
  m_es22 <- feols(farright_sh ~ t01 + t06 + t13 + t18 + t22 | id08 + year,
                  data = d_ext, cluster = "id08")
  for (v in c("t01", "t06", "t13", "t18", "t22")) {
    yr <- switch(v, t01=2001, t06=2006, t13=2013, t18=2018, t22=2022)
    cat(sprintf("  %d: %.4f  t=%.1f\n", yr, coef(m_es22)[v], coef(m_es22)[v]/se(m_es22)[v]))
  }

  # Table C3: Placebo thresholds 2001-2022
  cat("\nTable C3: Placebo thresholds 2001-2022\n")
  tab_a3_rows <- list()
  for (thr in c(2000, 5000, 7500, 10000, 20000, 35000, 50000)) {
    if (thr == 5000) {
      d_ext[, pl_tr := as.integer((mont_group == 0 & pop_tot_2008 < 5000) |
                                   (mont_group == 1 & pop_tot_2008 < 3000))]
    } else {
      d_ext[, pl_tr := as.integer(pop_tot_2008 < thr)]
    }
    d_ext[, pl_t := as.integer(pl_tr == 1 & year > 2010)]
    m <- feols(farright_sh ~ pl_t | id08 + year, data = d_ext, cluster = "id08")
    cat(sprintf("  %6d: %.3f  t=%.1f\n", thr, coef(m)["pl_t"], coef(m)["pl_t"]/se(m)["pl_t"]))
    tab_a3_rows[[length(tab_a3_rows) + 1]] <- list(
      thr = thr, est = coef(m)["pl_t"], se = se(m)["pl_t"],
      tstat = coef(m)["pl_t"] / se(m)["pl_t"], n = nobs(m))
  }
} else {
  cat("  (skipping — run 02_extend_panel.R first)\n")
}

################################################################################
cat("\n====================================================================\n")
cat("GENERATE LATEX TABLES\n")
cat("====================================================================\n\n")

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# Format helpers
fmt3 <- function(x) sprintf("%.3f", x)
fmt3z <- fmt3
fmt_se <- function(x) sprintf("{(%.3f)}", x)

fmt_t1 <- function(x) sprintf("%.1f", x)
fmt_t2 <- function(x) sprintf("%.2f", x)
pstars <- function(tstat) ""
star_note <- ""

### Table 1 (tab:rd) -----------------------------------------------------------
rows <- vapply(tab1_rows, function(r) {
  paste0(
    sprintf("      %s & %s & %s \\\\",
      r$label_tex, paste0(fmt3(r$est), pstars(r$tstat)),
      format(r$n_mun, big.mark = ",")),
    sprintf("\n      & %s & \\\\", fmt_se(r$se)))
}, character(1))
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Narrow-band TWFE estimates around the reform threshold}",
  "    \\label{tab:rd}",
  "    \\begin{tabular*}{0.75\\linewidth}{@{\\extracolsep{\\fill}}lSc@{}}",
  "      \\toprule",
  "      Bandwidth & {Estimate} & Municipalities \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} TWFE with municipality and year FE, clustered SEs. Each row restricts to municipalities within the indicated bandwidth of the dual threshold (5,000 non-mountain / 3,000 mountain). Post is $\\text{year} > 2010$.", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_rd.tex")
cat("  output/tables/italy/tab_rd.tex\n")

### Table 2 (tab:temporal) -----------------------------------------------------
# Build rows dynamically based on available models
# Helper: row with SE for temporal table
temporal_row <- function(label, elections, model, var, bold = FALSE) {
  e <- coef(model)[var]; s <- se(model)[var]; t <- e / s
  est_str <- paste0(fmt3z(e), pstars(t))
  se_str <- fmt_se(s)
  n_str <- format(nobs(model), big.mark = ",")
  if (bold) {
    c(sprintf("      \\textbf{%s} & \\textbf{%s} & {\\textbf{%s}} & \\textbf{%s} \\\\",
        label, elections, est_str, n_str),
      sprintf("      & & {\\textbf{%s}} & \\\\", gsub("^\\{|\\}$", "", se_str)))
  } else {
    c(sprintf("      %s & %s & %s & %s \\\\", label, elections, est_str, n_str),
      sprintf("      & & %s & \\\\", se_str))
  }
}
tab2_rows <- c(
  temporal_row("2001 (placebo)", "2001, 2006, 2008", m_2001, "t_fake"),
  temporal_row("2006 (placebo)", "2001, 2006, 2008", m_2006, "t_fake"),
  "      \\addlinespace",
  temporal_row("2010 (actual)", "2001--2018", m_paper, "t", bold = TRUE)
)
if (exists("m_ext") && exists("m_2014")) {
  tab2_rows <- c(tab2_rows,
    "      \\addlinespace",
    temporal_row("2010 (actual)", "2001--2022", m_ext, "t"),
    "      \\addlinespace",
    temporal_row("2014 (placebo)", "2013, 2018, 2022", m_2014, "t_fake")
  )
}
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Temporal placebo and extended-sample estimates}",
  "    \\label{tab:temporal}",
  "    \\begin{tabular*}{0.85\\linewidth}{@{\\extracolsep{\\fill}}llSc@{}}",
  "      \\toprule",
  "      Reform date & Elections used & {Estimate} & $N$ \\\\",
  "      \\midrule",
  tab2_rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} Each row uses the paper's specification (municipality and year FE, clustered SEs) and dual-threshold treatment. The first two rows use only pre-reform elections, with ``post'' redefined as elections after the alternative break year. The fourth extends the panel through 2022. The last uses only post-reform elections with break at 2014.", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_temporal.tex")
cat("  output/tables/italy/tab_temporal.tex\n")

### Table 3 (tab:placebo) ------------------------------------------------------
rows <- vapply(tab3_rows, function(r) {
  twfe_str <- paste0(fmt3(r$twfe), pstars(r$twfe_t))
  mtwfe_str <- paste0(fmt3(r$mtwfe), pstars(r$mtwfe_t))
  sdid_str <- paste0(fmt3(r$sdid), pstars(r$sdid_t))
  twfe_se <- fmt_se(r$twfe_se); mtwfe_se <- fmt_se(r$mtwfe_se); sdid_se <- fmt_se(r$sdid_se)
  if (r$thr == 5000) {
    paste0(sprintf("      \\textbf{%s} & {\\textbf{%s}} & {\\textbf{%s}} & {\\textbf{%s}} \\\\",
      format(r$thr, big.mark = ","), twfe_str, mtwfe_str, sdid_str),
      sprintf("\n      & {\\textbf{%s}} & {\\textbf{%s}} & {\\textbf{%s}} \\\\",
        gsub("^\\{|\\}$", "", twfe_se), gsub("^\\{|\\}$", "", mtwfe_se), gsub("^\\{|\\}$", "", sdid_se)))
  } else {
    paste0(sprintf("      %s & %s & %s & %s \\\\",
      format(r$thr, big.mark = ","), twfe_str, mtwfe_str, sdid_str),
      sprintf("\n      & %s & %s & %s \\\\", twfe_se, mtwfe_se, sdid_se))
  }
}, character(1))
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Placebo threshold estimates across all three estimators}",
  "    \\label{tab:placebo}",
  "    \\begin{tabular*}{0.85\\linewidth}{@{\\extracolsep{\\fill}}rSSS@{}}",
  "      \\toprule",
  "      Threshold & {TWFE} & {MTWFE} & {SDID} \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} Each row is a separate estimation at the indicated cutoff. TWFE: municipality and year FE, clustered SEs. MTWFE: Mahalanobis matching with replacement on pre-treatment covariates. SDID: \\citet{arkhangelsky2021}, jackknife SEs. At 5,000, treatment uses the paper's dual threshold (5,000/3,000); all other rows use a uniform cutoff.", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_placebo.tex")
cat("  output/tables/italy/tab_placebo.tex\n")

### Table 4 (tab:gradient) -----------------------------------------------------
tab4_specs <- list(
  list("TWFE (paper)", m_twfe_base),
  list("TWFE $+$ $\\log(\\text{pop}) \\times$ year", m_twfe_logpop),
  list("TWFE $+$ pctile(pop) $\\times$ year", m_twfe_pctile),
  list("MTWFE (paper)", m_mtwfe_base),
  list("MTWFE $+$ $\\log(\\text{pop}) \\times$ year", m_mtwfe_logpop),
  list("MTWFE $+$ pctile(pop) $\\times$ year", m_mtwfe_pctile))
rows <- character()
for (i in seq_along(tab4_specs)) {
  s <- tab4_specs[[i]]
  e <- coef(s[[2]])["t"]; sv <- se(s[[2]])["t"]; t_val <- e / sv
  rows <- c(rows,
    sprintf("      %s & %s \\\\", s[[1]], paste0(fmt3z(e), pstars(t_val))),
    sprintf("      & %s \\\\", fmt_se(sv)))
  if (i == 3) rows <- c(rows, "      \\addlinespace")
}
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Electoral estimates with and without size-specific time trends}",
  "    \\label{tab:gradient}",
  "    \\begin{tabular*}{0.85\\linewidth}{@{\\extracolsep{\\fill}}lS@{}}",
  "      \\toprule",
  "      Specification & {Estimate} \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0(sprintf("      \\item \\textit{Notes:} All specifications include municipality and year FE with SEs clustered by municipality. pctile(pop) is each municipality's percentile rank in the national population distribution (continuous, 0--1 scale). MTWFE uses Mahalanobis matching on pre-treatment covariates including population (with replacement), which reduces the mean population gap from %s to %s. ",
    format(round(gap_before), big.mark = ","), format(round(gap_after), big.mark = ",")), star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_gradient.tex")
cat("  output/tables/italy/tab_gradient.tex\n")

### Table 4b (tab:covariate_mediation) -----------------------------------------
tab4b_specs <- list(
  list("TWFE (baseline)", m_twfe_base),
  list("TWFE $+$ covariates $\\times$ year", m_twfe_cov),
  list("TWFE $+$ $\\log(\\text{pop}) \\times$ year", m_twfe_logpop),
  list("TWFE $+$ covariates $\\times$ year $+$ $\\log(\\text{pop}) \\times$ year", m_twfe_both))
rows <- character()
for (i in seq_along(tab4b_specs)) {
  s <- tab4b_specs[[i]]
  e <- coef(s[[2]])["t"]; sv <- se(s[[2]])["t"]; t_val <- e / sv
  rows <- c(rows,
    sprintf("      %s & %s \\\\", s[[1]], paste0(fmt3z(e), pstars(t_val))),
    sprintf("      & %s \\\\", fmt_se(sv)))
  if (i == 1) rows <- c(rows, "      \\addlinespace")
}
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Covariate controls versus population controls}",
  "    \\label{tab:covariate_mediation}",
  "    \\begin{tabular*}{0.85\\linewidth}{@{\\extracolsep{\\fill}}lS@{}}",
  "      \\toprule",
  "      Specification & {Estimate} \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} All specifications include municipality and year FE with SEs clustered by municipality. ``Covariates $\\times$ year'' adds year-specific slopes for foreign-born share, over-65 share, mean income, university share, female share, and maximum altitude---the same variables used for Mahalanobis matching. ", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_covariate_mediation.tex")
cat("  output/tables/italy/tab_covariate_mediation.tex\n")

### Table 4c (tab:gradient_thresholds) ------------------------------------------
rows <- vapply(tab_grad_thr, function(r) {
  twfe_str <- paste0(fmt3(r$twfe), pstars(r$twfe_t))
  grad_str <- paste0(fmt3(r$grad), pstars(r$grad_t))
  twfe_se_str <- fmt_se(r$twfe_se)
  grad_se_str <- fmt_se(r$grad_se)
  if (r$thr == 5000) {
    paste0(sprintf("      \\textbf{%s} & {\\textbf{%s}} & {\\textbf{%s}} \\\\",
      format(r$thr, big.mark = ","), twfe_str, grad_str),
      sprintf("\n      & {\\textbf{%s}} & {\\textbf{%s}} \\\\",
        gsub("^\\{|\\}$", "", twfe_se_str), gsub("^\\{|\\}$", "", grad_se_str)))
  } else {
    paste0(sprintf("      %s & %s & %s \\\\",
      format(r$thr, big.mark = ","), twfe_str, grad_str),
      sprintf("\n      & %s & %s \\\\", twfe_se_str, grad_se_str))
  }
}, character(1))
# Add spacing between rows
rows <- paste(rows, collapse = "\n      \\addlinespace\n")
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Gradient controls eliminate estimates at every threshold}",
  "    \\label{tab:gradient_thresholds}",
  "    \\begin{tabular*}{0.75\\linewidth}{@{\\extracolsep{\\fill}}rSS@{}}",
  "      \\toprule",
  "      Threshold & {TWFE} & {TWFE $+$ $\\log(\\text{pop}) \\times$ year} \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} Each row is a separate estimation at the indicated cutoff. TWFE with municipality and year FE, SEs clustered by municipality (in parentheses). At 5,000, treatment uses the paper's dual threshold (5,000/3,000). All other rows use a uniform cutoff. ", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_gradient_thresholds.tex")
cat("  output/tables/italy/tab_gradient_thresholds.tex\n")

### Table 5 (tab:eventstudy) ---------------------------------------------------
es_vars <- c("t01", "t06", "t13", "t18")
es_years <- c(2001, 2006, 2013, 2018)
rows <- character()
for (i in seq_along(es_vars)) {
  v <- es_vars[i]
  e <- coef(m_es)[v]; sv <- se(m_es)[v]; t_val <- e / sv
  rows <- c(rows,
    sprintf("      %d & %s \\\\", es_years[i], paste0(fmt3(e), pstars(t_val))),
    sprintf("      & %s \\\\", fmt_se(sv)))
}
rows <- c(rows[1:4],  # 2001 + SE, 2006 + SE
  "      2008 & {(base year)} \\\\",
  rows[5:8])  # 2013 + SE, 2018 + SE
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Event study (base year: 2008)}",
  "    \\label{tab:eventstudy}",
  "    \\begin{tabular*}{0.65\\linewidth}{@{\\extracolsep{\\fill}}lS@{}}",
  "      \\toprule",
  "      Year & {Estimate} \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  sprintf("      \\item \\textit{Notes:} TWFE with municipality and year FE, SEs clustered by municipality. Coefficients are year-specific treatment interactions (treated $\\times$ year), with 2008 as the omitted category. Sample: 2001--2018, $N = %s$.",
    format(nobs(m_es), big.mark = ",")),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_eventstudy.tex")
cat("  output/tables/italy/tab_eventstudy.tex\n")

### Table 11 (tab:mountain_nb) -------------------------------------------------
rows <- vapply(tab_mtn_rows, function(r) {
  paste0(
    sprintf("      %s & %s & %s \\\\",
      r$label_tex, paste0(fmt3(r$est), pstars(r$tstat)),
      format(r$n_mun, big.mark = ",")),
    sprintf("\n      & %s & \\\\", fmt_se(r$se)))
}, character(1))
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Narrow-band TWFE: mountain municipalities at 3,000 threshold}",
  "    \\label{tab:mountain_nb}",
  "    \\begin{tabular*}{0.75\\linewidth}{@{\\extracolsep{\\fill}}lSc@{}}",
  "      \\toprule",
  "      Bandwidth & {Estimate} & Municipalities \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  sprintf("      \\item \\textit{Notes:} Mountain municipalities only ($N = %s$). TWFE with municipality and year FE, SEs clustered by municipality. Treatment is $\\mathbf{1}(\\text{pop} < 3{,}000)$; post is $\\text{year} > 2010$.",
    format(uniqueN(d_mtn[year == 2008]$id08), big.mark = ",")),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_mountain_nb.tex")
cat("  output/tables/italy/tab_mountain_nb.tex\n")

### Appendix D: Mountain gradient + sweep (tab:mountain_gradient) --------------
e1 <- coef(m_mtn_base)["t_mtn"]; s1 <- se(m_mtn_base)["t_mtn"]; t1 <- e1 / s1
e2 <- coef(m_mtn_logpop)["t_mtn"]; s2 <- se(m_mtn_logpop)["t_mtn"]; t2 <- e2 / s2
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Mountain subsample: gradient controls and placebo sweep}",
  "    \\label{tab:mountain_gradient}",
  "    \\begin{tabular*}{0.65\\linewidth}{@{\\extracolsep{\\fill}}lS@{}}",
  "      \\toprule",
  "      Specification & {Estimate} \\\\",
  "      \\midrule",
  sprintf("      Mountain TWFE (baseline) & %s \\\\", paste0(fmt3z(e1), pstars(t1))),
  sprintf("      & %s \\\\", fmt_se(s1)),
  sprintf("      Mountain TWFE $+$ $\\log(\\text{pop}) \\times$ year & %s \\\\", paste0(fmt3z(e2), pstars(t2))),
  sprintf("      & %s \\\\", fmt_se(s2)),
  "      \\midrule",
  sprintf("      \\multicolumn{2}{l}{Placebo sweep: %d testable thresholds (500--20,000 by 250)} \\\\",
    nrow(mtn_scatter)),
  sprintf("      Meta-regression $R^2$ & {%.3f} \\\\", summary(mtn_fit)$r.squared),
  sprintf("      Significant at 5\\%% & {%d / %d} \\\\", n_sig, nrow(mtn_scatter)),
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  sprintf("      \\item \\textit{Notes:} Meta-regression: DID $= %.4f + %.4f \\times$ log-pop gap. Municipality and year FE; SEs clustered by municipality.",
    coef(mtn_fit)[1], coef(mtn_fit)[2]),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_mountain_gradient.tex")
cat("  output/tables/italy/tab_mountain_gradient.tex\n")

### Table 12 (tab:above5k) -----------------------------------------------------
rows <- vapply(tab6_rows, function(r) {
  paste0(
    sprintf("      %s & %s & %s \\\\",
      format(r$thr, big.mark = ","), paste0(fmt3(r$est), pstars(r$tstat)),
      format(r$n_mun, big.mark = ",")),
    sprintf("\n      & %s & \\\\", fmt_se(r$se)))
}, character(1))
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Placebo thresholds using only untreated municipalities (pop $\\geq$ 5,000)}",
  "    \\label{tab:above5k}",
  "    \\begin{tabular*}{0.75\\linewidth}{@{\\extracolsep{\\fill}}rSc@{}}",
  "      \\toprule",
  "      Threshold & {Estimate} & Municipalities \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  "      \\item \\textit{Notes:} Sample restricted to municipalities with 2008 census population $\\geq$ 5,000. None of these municipalities were subject to the reform. TWFE with municipality and year FE, SEs clustered by municipality. Post is $\\text{year} > 2010$.",
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_above5k.tex")
cat("  output/tables/italy/tab_above5k.tex\n")

### Appendix A tables (conditional on 2022 extension) --------------------------
if (exists("m_ext_twfe")) {
  # Table 5 (tab:twfe_2022)
  a1_specs <- list(
    list("2001--2018 (original)", m_orig),
    list("2001--2022 (extended)", m_ext_twfe),
    list("2001--2022 + $\\log(\\text{pop}) \\times$ year", m_ext_lp))
  rows <- vapply(a1_specs, function(s) {
    e <- coef(s[[2]])["t"]; sv <- se(s[[2]])["t"]; t_val <- e / sv
    paste0(
      sprintf("      %s & %s & %s \\\\",
        s[[1]], paste0(fmt3(e), pstars(t_val)), format(nobs(s[[2]]), big.mark = ",")),
      sprintf("\n      & %s & \\\\", fmt_se(sv)))
  }, character(1))
  writeLines(c(
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\begin{threeparttable}",
    "    \\caption{TWFE estimates: original vs.\\ extended sample}",
    "    \\label{tab:twfe_2022}",
    "    \\begin{tabular*}{0.75\\linewidth}{@{\\extracolsep{\\fill}}lSc@{}}",
    "      \\toprule",
    "      Sample & {Estimate} & $N$ \\\\",
    "      \\midrule",
    rows,
    "      \\bottomrule",
    "    \\end{tabular*}",
    "    \\begin{tablenotes}[flushleft]\\small",
    "      \\item \\textit{Notes:} TWFE with municipality and year FE, SEs clustered by municipality. The extended sample adds the September 2022 general election (7,506 matched municipalities). The third row adds $\\log(\\text{pop}) \\times$ year interactions.",
    "    \\end{tablenotes}",
    "  \\end{threeparttable}",
    "\\end{table}"
  ), "output/tables/italy/tab_twfe_2022.tex")
  cat("  output/tables/italy/tab_twfe_2022.tex\n")

  # Table 6 (tab:eventstudy_2022)
  es22_vars <- c("t01", "t06", "t13", "t18", "t22")
  es22_years <- c(2001, 2006, 2013, 2018, 2022)
  rows <- character()
  for (i in seq_along(es22_vars)) {
    v <- es22_vars[i]
    e <- coef(m_es22)[v]; sv <- se(m_es22)[v]; t_val <- e / sv
    rows <- c(rows,
      sprintf("      %d & %s \\\\", es22_years[i], paste0(fmt3(e), pstars(t_val))),
      sprintf("      & %s \\\\", fmt_se(sv)))
  }
  rows <- c(rows[1:4],  # 2001 + SE, 2006 + SE
    "      2008 & {(base year)} \\\\",
    rows[5:10])  # 2013 + SE, 2018 + SE, 2022 + SE
  writeLines(c(
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\begin{threeparttable}",
    "    \\caption{Event study with 2022 (base year: 2008)}",
    "    \\label{tab:eventstudy_2022}",
    "    \\begin{tabular*}{0.65\\linewidth}{@{\\extracolsep{\\fill}}lS@{}}",
    "      \\toprule",
    "      Year & {Estimate} \\\\",
    "      \\midrule",
    rows,
    "      \\bottomrule",
    "    \\end{tabular*}",
    "    \\begin{tablenotes}[flushleft]\\small",
    sprintf("      \\item \\textit{Notes:} TWFE with municipality and year FE, SEs clustered by municipality. Coefficients are year-specific treatment interactions (treated $\\times$ year), with 2008 as the omitted category. Sample: 2001--2022, $N = %s$.",
      format(nobs(m_es22), big.mark = ",")),
    "    \\end{tablenotes}",
    "  \\end{threeparttable}",
    "\\end{table}"
  ), "output/tables/italy/tab_eventstudy_2022.tex")
  cat("  output/tables/italy/tab_eventstudy_2022.tex\n")

  # Table 7 (tab:placebo_2022)
  rows <- vapply(tab_a3_rows, function(r) {
    if (r$thr == 5000) {
      paste0(sprintf("      \\textbf{%s} & {\\textbf{%s}} \\\\",
        format(r$thr, big.mark = ","), paste0(fmt3(r$est), pstars(r$tstat))),
        sprintf("\n      & {\\textbf{%s}} \\\\", gsub("^\\{|\\}$", "", fmt_se(r$se))))
    } else {
      paste0(sprintf("      %s & %s \\\\",
        format(r$thr, big.mark = ","), paste0(fmt3(r$est), pstars(r$tstat))),
        sprintf("\n      & %s \\\\", fmt_se(r$se)))
    }
  }, character(1))
  writeLines(c(
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\begin{threeparttable}",
    "    \\caption{Placebo threshold estimates, 2001--2022}",
    "    \\label{tab:placebo_2022}",
    "    \\begin{tabular*}{0.65\\linewidth}{@{\\extracolsep{\\fill}}rS@{}}",
    "      \\toprule",
    "      Threshold & {Estimate} \\\\",
    "      \\midrule",
    rows,
    "      \\bottomrule",
    "    \\end{tabular*}",
    "    \\begin{tablenotes}[flushleft]\\small",
    "      \\item \\textit{Notes:} TWFE with municipality and year FE, SEs clustered by municipality. Each row is a separate estimation at the indicated cutoff. At 5,000, treatment uses the paper's dual threshold. Sample: 2001--2022.",
    "    \\end{tablenotes}",
    "  \\end{threeparttable}",
    "\\end{table}"
  ), "output/tables/italy/tab_placebo_2022.tex")
  cat("  output/tables/italy/tab_placebo_2022.tex\n")
}

cat("\nDone generating LaTeX tables.\n")

################################################################################
cat("\n====================================================================\n")
cat("EXPORT: municipality_data.csv for interactive simulation\n")
cat("====================================================================\n\n")

d_pre_sim  <- d[year <= 2008, .(fr_pre = mean(farright_sh, na.rm = TRUE)),
                by = .(id08, pop_tot_2008)]
d_post_sim <- d[year > 2010, .(fr_post = mean(farright_sh, na.rm = TRUE)), by = id08]
d_sim <- merge(d_pre_sim, d_post_sim, by = "id08")
d_sim[, delta_fr := fr_post - fr_pre]
d_sim[, log_pop := log(pop_tot_2008)]
out <- d_sim[, .(pop = pop_tot_2008, log_pop, delta_fr)]
write.csv(out, "output/csvs/italy/municipality_data.csv", row.names = FALSE, quote = FALSE)
cat(sprintf("Exported %d municipalities to output/municipality_data.csv\n", nrow(out)))

cat("\n====================================================================\n")
cat("DONE — all tables, figures, and inline estimates reproduced.\n")
cat("====================================================================\n")

# Log session info for reproducibility
cat("\n"); print(sessionInfo())
