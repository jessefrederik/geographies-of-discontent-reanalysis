################################################################################
# 06_figure_bandwidth_sweep_fr.R
#
# France narrow-band TWFE estimate as a function of bandwidth around 5,000.
# Log scale from ±50 to ±200,000. Mirrors code/italy/06_figure_bandwidth_sweep.R.
#
# Output:
#   output/figures/france/fig_bandwidth_sweep_fr.pdf
#   output/csvs/france/bandwidth_sweep_fr.csv
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(scales)
})

d <- fread("data_processed/france/final/panel_commune.csv")
d[, post := as.integer(year > 2010)]

# Reform-threshold placebo: 5,000 (France has no real reform; pick a center)
CENTER <- 5000

bandwidths <- unique(round(exp(seq(log(50), log(200000), length.out = 60))))
MIN_COMM <- 30

results <- lapply(bandwidths, function(bw) {
  dsub <- d[abs(pop_2006 - CENTER) <= bw]
  n_comm <- uniqueN(dsub$commune_id)
  if (n_comm < MIN_COMM) return(NULL)
  dsub[, treated := as.integer(pop_2006 < CENTER)]
  dsub[, t_post := as.integer(treated == 1 & post == 1)]
  m <- feols(farright_sh ~ t_post | commune_id + year,
             data = dsub, cluster = "commune_id")
  data.table(bw = bw, est = unname(coef(m)["t_post"]),
             se = unname(se(m)["t_post"]), n_comm = n_comm)
})
results <- rbindlist(results)
results[, ci_lo := est - 1.96 * se]
results[, ci_hi := est + 1.96 * se]

dir.create("output/csvs/france", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/france", showWarnings = FALSE, recursive = TRUE)
fwrite(results, "output/csvs/france/bandwidth_sweep_fr.csv")

brks <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 200000)

p <- ggplot(results, aes(x = bw, y = est)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "grey75", alpha = 0.55) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  scale_x_log10(
    breaks = brks,
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(labels = function(x) sprintf("%+.2f", x)) +
  labs(
    x = "Bandwidth around 5,000 (log scale)",
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

ggsave("output/figures/france/fig_bandwidth_sweep_fr.pdf", p,
       width = 8, height = 4.2)
ggsave("output/figures/france/fig_bandwidth_sweep_fr.png", p,
       width = 8, height = 4.2, dpi = 300)

cat(sprintf("\nWritten: output/figures/france/fig_bandwidth_sweep_fr.{pdf,png}\n"))
cat(sprintf("         output/csvs/france/bandwidth_sweep_fr.csv (%d rows)\n",
            nrow(results)))
