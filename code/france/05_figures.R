################################################################################
# 05_figures.R — Generate all France placebo figures
#
# Produces:
#   1. Placebo scatter: DID estimate vs log-pop gap (main figure)
#   2. DID estimate vs cutoff
#   3. Far-right share vs log(pop) by election year (faceted)
#   4. Narrow-band coefficient path
#
# Figure styling matches analysis.R (Italy) and analysis_nl.R (NL).
################################################################################

cat("====================================================================\n")
cat("05_figures.R — Generating figures\n")
cat("====================================================================\n\n")

library(data.table)
library(ggplot2)
library(scales)

# Load data
results <- fread("output/csvs/france/placebo_threshold_results.csv")
d <- fread("data_processed/france/final/panel_commune.csv")
d[, log_pop := log(pop_2006)]
nb <- fread("output/csvs/france/narrowband_results.csv")

################################################################################
cat("Figure 1: Placebo scatter (DID vs log-pop gap)\n")
################################################################################
# Exact styling from analysis.R lines 258-278.

results$pop_ratio <- exp(results$log_pop_gap)
fit <- lm(estimate ~ log_pop_gap, data = results)
r2 <- summary(fit)$r.squared

ratio_ticks <- c(3, 5, 8, 10, 15, 20, 30, 50, 100, 200)
log_ticks <- log(ratio_ticks)
# Keep only ticks within data range
in_range <- log_ticks >= min(results$log_pop_gap) * 0.95 &
            log_ticks <= max(results$log_pop_gap) * 1.05
ratio_ticks <- ratio_ticks[in_range]
log_ticks <- log_ticks[in_range]

pdf("output/figures/france/placebo_scatter_fr.pdf", width = 5.5, height = 4)
par(mar = c(4.5, 4.5, 1.5, 1), family = "serif")
plot(results$log_pop_gap, results$estimate,
     pch = 16, cex = 0.5, col = "grey50",
     xlab = expression("Mean population ratio (control / treated)"),
     ylab = "DID estimate",
     xlim = range(results$log_pop_gap) * c(0.95, 1.05),
     ylim = range(results$estimate) * c(0.9, 1.1),
     las = 1, cex.lab = 1.0, cex.axis = 0.85, xaxt = "n")
axis(1, at = log_ticks, labels = paste0(ratio_ticks, "\u00d7"), cex.axis = 0.85)
abline(fit, col = "firebrick", lwd = 2, lty = 2)
# R-squared annotation
text(min(results$log_pop_gap) + 0.05,
     max(results$estimate) * 0.97,
     bquote(R^2 == .(sprintf("%.2f", r2))),
     cex = 0.9, adj = 0)
# Subtitle
mtext("France \u2014 no reform", side = 3, line = 0.3, cex = 0.85,
      font = 3, col = "grey40")
dev.off()
cat("  Saved output/figures/france/placebo_scatter_fr.pdf\n")


################################################################################
cat("Figure 2: DID estimate vs cutoff\n")
################################################################################

pdf("output/figures/france/fig_did_vs_cutoff.pdf", width = 6, height = 4)
par(mar = c(4.5, 4.5, 1.5, 1), family = "serif")

# CI bands
upper <- results$estimate + 1.96 * results$se
lower <- results$estimate - 1.96 * results$se

plot(results$threshold / 1000, results$estimate, type = "n",
     xlab = "Population threshold (thousands)",
     ylab = "DID estimate",
     ylim = c(min(lower) * 1.1, max(upper) * 1.1),
     las = 1, cex.lab = 1.0, cex.axis = 0.85)
polygon(c(results$threshold / 1000, rev(results$threshold / 1000)),
        c(upper, rev(lower)),
        col = adjustcolor("steelblue", alpha.f = 0.15), border = NA)
lines(results$threshold / 1000, results$estimate, col = "steelblue", lwd = 1.5)
abline(h = 0, lty = 3, col = "grey50")
mtext("France \u2014 no reform", side = 3, line = 0.3, cex = 0.85,
      font = 3, col = "grey40")
dev.off()
cat("  Saved output/figures/france/fig_did_vs_cutoff.pdf\n")


################################################################################
cat("Figure 3: Far-right share vs log(pop) by election year\n")
################################################################################
# Styling from fig_logpop_facet.R.

# Binscatter: percentile bins on log(population) within each year
n_bins <- 100
d[, bin := cut(log_pop,
               breaks = quantile(log_pop, probs = seq(0, 1, length.out = n_bins + 1)),
               include.lowest = TRUE, labels = FALSE),
  by = year]
bins <- d[, .(farright_sh = mean(farright_sh, na.rm = TRUE),
              pop_2006 = exp(mean(log_pop))),
          by = .(year, bin)]

p <- ggplot(bins, aes(x = pop_2006, y = farright_sh)) +
  geom_point(size = 1.2, color = "grey30") +
  scale_x_log10(
    breaks = c(100, 1000, 5000, 50000, 1000000),
    labels = c("100", "1k", "5k", "50k", "1M")
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 0.8, by = 0.2)) +
  facet_wrap(~ year, nrow = 1) +
  labs(x = "Population (2006 census)",
       y = "Far-right vote share") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey90", linewidth = 0.3),
    strip.background   = element_rect(fill = "white", color = "grey70"),
    strip.text         = element_text(face = "bold", size = 10),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 9),
    axis.title         = element_text(size = 11),
    plot.margin        = margin(5, 10, 5, 5)
  )

ggsave("output/figures/france/fig_logpop_facet_fr.pdf", p, width = 10, height = 4)
ggsave("output/figures/france/fig_logpop_facet_fr.png", p, width = 10, height = 4, dpi = 300)
cat("  Saved output/figures/france/fig_logpop_facet_fr.pdf and .png\n")


################################################################################
cat("Figure 4: Narrow-band coefficient path\n")
################################################################################

nb[, bandwidth_label := ifelse(bandwidth == Inf, "Full", as.character(bandwidth))]
nb[, bandwidth_num := ifelse(bandwidth == Inf, max(bandwidth[bandwidth != Inf]) * 1.5, bandwidth)]

pdf("output/figures/france/fig_narrowband.pdf", width = 7, height = 3.5)
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 2, 1), family = "serif")

for (ctr in c(5000, 10000, 20000)) {
  nb_sub <- nb[center == ctr]
  nb_sub <- nb_sub[order(-bandwidth_num)]

  upper <- nb_sub$estimate + 1.96 * nb_sub$se
  lower <- nb_sub$estimate - 1.96 * nb_sub$se
  idx <- seq_len(nrow(nb_sub))

  plot(idx, nb_sub$estimate, type = "b", pch = 16, cex = 0.8,
       col = "steelblue", lwd = 1.5,
       xlab = "Population bandwidth",
       ylab = ifelse(ctr == 5000, "DID estimate", ""),
       ylim = c(min(lower, 0) - 0.005, max(upper) + 0.005),
       las = 1, cex.lab = 0.9, cex.axis = 0.75, xaxt = "n",
       main = sprintf("Cutoff = %s", format(ctr, big.mark = ",")))
  axis(1, at = idx, labels = nb_sub$bandwidth_label, cex.axis = 0.65, las = 2)
  arrows(idx, lower, idx, upper, angle = 90, code = 3, length = 0.04, col = "steelblue")
  abline(h = 0, lty = 3, col = "grey50")
}
dev.off()
cat("  Saved output/figures/france/fig_narrowband.pdf\n")


cat("\n====================================================================\n")
cat("All figures saved to output/figures/france/\n")
cat("====================================================================\n\n")
