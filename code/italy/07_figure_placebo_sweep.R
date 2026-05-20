################################################################################
# 07_figure_placebo_sweep.R
#
# Placebo threshold sweep across all three estimators (TWFE, MTWFE, SDID),
# with a denser threshold grid than Table 2 and 95% confidence intervals.
#
# x-axis = population threshold (log scale, 1,000 to 50,000)
# y-axis = DID estimate with 95% CI
# Three facets = TWFE / MTWFE / SDID
#
# Output:
#   output/figures/italy/fig_placebo_sweep.pdf
#   output/csvs/italy/placebo_sweep.csv
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(fixest)
  library(MatchIt)
  library(synthdid)
  library(ggplot2)
  library(scales)
})

d <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
d[, log_pop := log(pop_tot_2008)]

match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008", "share_university2001",
                "max_altitude")

# Log-spaced threshold grid: 20 points from 1,000 to 50,000
thresholds <- unique(round(exp(seq(log(1000), log(50000), length.out = 20))))

results <- data.table()

for (thr in thresholds) {
  cat(sprintf("threshold = %d\n", thr))
  # Define treatment (dual at the actual reform threshold, single elsewhere)
  if (thr == 5000) {
    d[, pl_tr := as.integer((mont_group == 0 & pop_tot_2008 < 5000) |
                             (mont_group == 1 & pop_tot_2008 < 3000))]
  } else {
    d[, pl_tr := as.integer(pop_tot_2008 < thr)]
  }
  d[, pl_t := as.integer(pl_tr == 1 & year > 2010)]

  # --- TWFE ---
  m_twfe <- feols(farright_sh ~ pl_t | id08 + year, data = d, cluster = "id08")
  twfe_est <- unname(coef(m_twfe)["pl_t"])
  twfe_se  <- unname(se(m_twfe)["pl_t"])

  # --- MTWFE ---
  d08 <- d[year == 2008]
  fml <- as.formula(paste("pl_tr ~", paste(match_vars, collapse = " + ")))
  m_out <- matchit(fml, data = d08, method = "nearest",
                   distance = "mahalanobis", replace = TRUE)
  md <- match.data(m_out)
  d_m <- merge(d, md[, c("id08", "weights")], by = "id08", all.x = FALSE)
  m_mtwfe <- feols(farright_sh ~ pl_t | id08 + year, data = d_m,
                   weights = d_m$weights, cluster = "id08")
  mtwfe_est <- unname(coef(m_mtwfe)["pl_t"])
  mtwfe_se  <- unname(se(m_mtwfe)["pl_t"])

  # --- SDID ---
  d_sdid <- d[, .(unit = id08, time = year, Y = farright_sh, W = pl_t)]
  setup <- panel.matrices(as.data.frame(d_sdid))
  sdid <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  sdid_se  <- sqrt(vcov(sdid, method = "jackknife"))
  sdid_est <- as.numeric(sdid)

  # Compute SDID's implied weighted population gap (response to reviewer #1).
  # SDID returns unit weights (omega) on the control units. The implied
  # treated-control population comparison is: mean log-pop(treated) minus
  # weighted-mean log-pop(controls under SDID weights). For TWFE the gap is
  # the unweighted (treated minus control) log-pop diff.
  unit_pop <- d[year == 2008, .(unit = id08, log_pop)]
  unit_order <- rownames(setup$Y)
  unit_pop_ordered <- unit_pop[match(unit_order, as.character(unit))]$log_pop
  N0 <- setup$N0
  N  <- length(unit_pop_ordered)
  pop_ctl <- unit_pop_ordered[1:N0]
  pop_trt <- unit_pop_ordered[(N0 + 1):N]
  omega <- attr(sdid, "weights")$omega  # control unit weights, sum to 1
  twfe_gap <- mean(pop_trt) - mean(pop_ctl)
  sdid_gap <- mean(pop_trt) - sum(omega * pop_ctl)

  results <- rbind(results, data.table(
    threshold = thr,
    estimator = c("TWFE", "MTWFE", "SDID"),
    est = c(twfe_est, mtwfe_est, sdid_est),
    se  = c(twfe_se,  mtwfe_se,  sdid_se),
    pop_gap = c(twfe_gap, NA_real_, sdid_gap)
  ))
}

results[, ci_lo := est - 1.96 * se]
results[, ci_hi := est + 1.96 * se]
results[, estimator := factor(estimator, levels = c("TWFE", "MTWFE", "SDID"))]

dir.create("output/csvs/italy", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/italy", showWarnings = FALSE, recursive = TRUE)
fwrite(results, "output/csvs/italy/placebo_sweep.csv")

brks <- c(1000, 5000, 20000, 50000)

p <- ggplot(results, aes(x = threshold, y = est)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "grey75", alpha = 0.55) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_vline(xintercept = 5000, linetype = "dotted", color = "grey55", linewidth = 0.35) +
  facet_wrap(~ estimator, nrow = 1) +
  scale_x_log10(
    breaks = brks,
    labels = label_comma(),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(
    labels = function(x) sprintf("%+.2f", x)
  ) +
  labs(
    x = "Population threshold (log scale)",
    y = "DID estimate (95% CI)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.25),
    strip.text = element_text(face = "bold", size = 11),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

ggsave("output/figures/italy/fig_placebo_sweep.pdf", p,
       width = 9.5, height = 3.6)
ggsave("output/figures/italy/fig_placebo_sweep.png", p,
       width = 9.5, height = 3.6, dpi = 300)

cat(sprintf("\nWritten: output/figures/italy/fig_placebo_sweep.{pdf,png}\n"))
cat(sprintf("         output/csvs/italy/placebo_sweep.csv (%d rows)\n",
            nrow(results)))

# ---- Population-gap summary across cutoffs (reviewer #1 SDID question) ----
cat("\n---- Implied log-population gap (treated minus weighted control) ----\n")
gap_tbl <- dcast(results[!is.na(pop_gap),
                          .(threshold, estimator, pop_gap)],
                 threshold ~ estimator, value.var = "pop_gap")
gap_tbl[, share_closed := 1 - SDID / TWFE]
print(gap_tbl[, .(threshold,
                  TWFE_gap = round(TWFE, 3),
                  SDID_gap = round(SDID, 3),
                  ratio_SDID_to_TWFE = round(SDID / TWFE, 3),
                  share_closed = round(share_closed, 3))])
cat("\n  Note: SDID weights chosen to match pre-period treated outcomes.\n")
cat("  If SDID closed the population gap, SDID_gap would be near zero.\n")
