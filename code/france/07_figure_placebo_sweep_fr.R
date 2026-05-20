################################################################################
# 07_figure_placebo_sweep_fr.R
#
# France TWFE estimate at log-spaced placebo population thresholds from 1,000
# to 50,000, with 95% confidence intervals. Mirrors the Italy threshold sweep
# but France uses only TWFE (the placebo demonstration doesn't need matching
# or SDID).
#
# Output:
#   output/figures/france/fig_placebo_sweep_fr.pdf
#   output/csvs/france/placebo_sweep_fr.csv
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(scales)
})

d <- fread("data_processed/france/final/panel_commune.csv")
d[, post := as.integer(year > 2010)]

thresholds <- unique(round(exp(seq(log(1000), log(50000), length.out = 30))))

results <- lapply(thresholds, function(thr) {
  d[, treated := as.integer(pop_2006 < thr)]
  d[, t_post := as.integer(treated == 1 & post == 1)]
  n_tr <- sum(d[year == 2002]$treated)
  n_ct <- sum(d[year == 2002]$treated == 0)
  if (n_tr < 30 || n_ct < 30) return(NULL)
  m <- feols(farright_sh ~ t_post | commune_id + year,
             data = d, cluster = "commune_id")
  data.table(threshold = thr, est = unname(coef(m)["t_post"]),
             se = unname(se(m)["t_post"]), n_tr = n_tr, n_ct = n_ct)
})
results <- rbindlist(results)
results[, ci_lo := est - 1.96 * se]
results[, ci_hi := est + 1.96 * se]

dir.create("output/csvs/france", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/france", showWarnings = FALSE, recursive = TRUE)
fwrite(results, "output/csvs/france/placebo_sweep_fr.csv")

brks <- c(1000, 2000, 5000, 10000, 20000, 50000)

p <- ggplot(results, aes(x = threshold, y = est)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "grey75", alpha = 0.55) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  scale_x_log10(
    breaks = brks,
    labels = label_comma(),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(labels = function(x) sprintf("%+.2f", x)) +
  labs(
    x = "Population threshold (log scale)",
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

ggsave("output/figures/france/fig_placebo_sweep_fr.pdf", p,
       width = 8, height = 4.2)
ggsave("output/figures/france/fig_placebo_sweep_fr.png", p,
       width = 8, height = 4.2, dpi = 300)

cat(sprintf("\nWritten: output/figures/france/fig_placebo_sweep_fr.{pdf,png}\n"))
cat(sprintf("         output/csvs/france/placebo_sweep_fr.csv (%d rows)\n",
            nrow(results)))
