################################################################################
# 06_figure_bandwidth_sweep.R
#
# TWFE estimate as a function of the population bandwidth around each muni's
# own threshold (5,000 non-mountain / 3,000 mountain), on a log scale from
# 50 to 200,000. Shows the attenuation pattern continuously.
#
# Specification: farright_sh ~ treated:post | id08 + year, SE clustered by id08
#
# Output:
#   output/figures/italy/fig_bandwidth_sweep.pdf  (Figure for §2.1)
#   output/csvs/italy/bandwidth_sweep.csv         (underlying estimates)
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(fixest)
  library(ggplot2)
  library(scales)
})

d <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
d[, t := as.integer(treated == 1 & year > 2010)]

# Log-spaced grid of bandwidths
bandwidths <- unique(round(exp(seq(log(50), log(200000), length.out = 60))))

# Minimum sample for inclusion (skip degenerate cases)
MIN_MUN <- 30

results <- lapply(bandwidths, function(bw) {
  dsub <- d[ifelse(mont_group == 1,
                   abs(pop_tot_2008 - 3000),
                   abs(pop_tot_2008 - 5000)) <= bw]
  n_mun <- uniqueN(dsub$id08)
  if (n_mun < MIN_MUN) return(NULL)
  m <- feols(farright_sh ~ t | id08 + year, data = dsub, cluster = "id08")
  data.table(bw = bw, est = unname(coef(m)["t"]), se = unname(se(m)["t"]),
             n_mun = n_mun)
})
results <- rbindlist(results)
results[, ci_lo := est - 1.96 * se]
results[, ci_hi := est + 1.96 * se]

dir.create("output/csvs/italy", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/italy", showWarnings = FALSE, recursive = TRUE)
fwrite(results, "output/csvs/italy/bandwidth_sweep.csv")

# Reform threshold reference: ±5,000 bandwidth (the implicit cutoff for
# "everyone in the analysis sample").
brks <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 200000)

# Headline TWFE estimate from the full sample (for reference line) and
# MSE-optimal RD bandwidth (from rdrobust in 01_analysis_italy.R: 1,420 inhabitants).
HEADLINE <- 0.015
MSE_OPT_BW <- 1420

# Whether the upper 95% CI at each bandwidth excludes the headline -- for the
# textual claim that narrow-band intervals reject the full-sample estimate.
results[, ci_excludes_headline := ci_hi < HEADLINE]
narrow_excl <- results[bw <= 2000, sum(ci_excludes_headline)]
narrow_total <- results[bw <= 2000, .N]
cat(sprintf("\nNarrow-band (bw <= 2,000) results: %d/%d intervals have upper 95%% CI below the headline 0.015\n",
            narrow_excl, narrow_total))
cat(sprintf("MSE-optimal RD bandwidth (rdrobust, full sample): %d inhabitants\n", MSE_OPT_BW))

p <- ggplot(results, aes(x = bw, y = est)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "grey75", alpha = 0.55) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_hline(yintercept = HEADLINE, linetype = "dotted", color = "firebrick", linewidth = 0.4) +
  geom_vline(xintercept = MSE_OPT_BW, linetype = "dotted", color = "firebrick", linewidth = 0.4) +
  annotate("text", x = MSE_OPT_BW * 1.15, y = -0.035,
           label = sprintf("MSE-optimal\nbandwidth\n(±1,420)"),
           hjust = 0, size = 2.7, color = "firebrick", lineheight = 0.9) +
  annotate("text", x = 200000, y = HEADLINE + 0.0025,
           label = "Headline 0.015", hjust = 1, size = 2.7, color = "firebrick") +
  scale_x_log10(
    breaks = brks,
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(-0.04, 0.04, by = 0.01),
    labels = function(x) sprintf("%+.2f", x)
  ) +
  labs(
    x = "Bandwidth around each municipality's own threshold (log scale)",
    y = "TWFE estimate (95% CI)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.25),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

ggsave("output/figures/italy/fig_bandwidth_sweep.pdf", p,
       width = 8, height = 4.2)
ggsave("output/figures/italy/fig_bandwidth_sweep.png", p,
       width = 8, height = 4.2, dpi = 300)

cat(sprintf("\nWritten: output/figures/italy/fig_bandwidth_sweep.{pdf,png}\n"))
cat(sprintf("         output/csvs/italy/bandwidth_sweep.csv  (%d rows)\n",
            nrow(results)))
cat(sprintf("Range of estimates: [%+.4f, %+.4f]\n",
            min(results$est), max(results$est)))

# ---- Robustness: run the sweep separately for non-mountain (5,000 cutoff)
#       and mountain (3,000 cutoff) communes (response to reviewer #19) ----
sweep_subset <- function(d_sub, cutoff, label) {
  res <- lapply(bandwidths, function(bw) {
    dsub <- d_sub[abs(pop_tot_2008 - cutoff) <= bw]
    n_mun <- uniqueN(dsub$id08)
    if (n_mun < MIN_MUN) return(NULL)
    m <- feols(farright_sh ~ t | id08 + year, data = dsub, cluster = "id08")
    data.table(bw = bw, est = unname(coef(m)["t"]),
               se = unname(se(m)["t"]), n_mun = n_mun, group = label)
  })
  rbindlist(res)
}
res_nonmtn <- sweep_subset(d[mont_group == 0], 5000, "non-mountain (cutoff 5,000)")
res_mtn    <- sweep_subset(d[mont_group == 1], 3000, "mountain (cutoff 3,000)")
cat("\nSeparate-threshold bandwidth sweeps (peak estimate near full sample, attenuation toward cutoff):\n")
for (rs in list(res_nonmtn, res_mtn)) {
  peak <- rs[bw >= 100000][1]
  narrow <- rs[bw <= 2000 & bw >= 200]
  cat(sprintf("  %s: peak (full) est=%+.4f; narrow-band (bw 200-2000) mean est=%+.4f, max upper-CI=%+.4f\n",
              rs$group[1], peak$est, mean(narrow$est),
              max(narrow$est + 1.96 * narrow$se)))
}
fwrite(rbind(res_nonmtn, res_mtn), "output/csvs/italy/bandwidth_sweep_by_group.csv")
