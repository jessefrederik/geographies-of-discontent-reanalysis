## Figure 1: Far-right vote share vs. municipality size, by election year
## Style: AJPS-appropriate (minimal, black/white, no embedded title)

library(haven)
library(data.table)
library(ggplot2)
library(scales)

# Use extended panel if available (includes 2022), otherwise original
if (file.exists("data_processed/italy/electoral_panel_extended.csv")) {
  d <- fread("data_processed/italy/electoral_panel_extended.csv")
} else {
  d <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
}

# Binscatter: percentile bins on log(population) within each year
n_bins <- 100
d[, log_pop := log(pop_tot_2008)]
d[, bin := cut(log_pop,
               breaks = quantile(log_pop, probs = seq(0, 1, length.out = n_bins + 1)),
               include.lowest = TRUE, labels = FALSE),
  by = year]
bins <- d[, .(farright_sh = mean(farright_sh, na.rm = TRUE),
              pop_tot_2008 = exp(mean(log_pop))),
          by = .(year, bin)]

ggplot(bins, aes(x = pop_tot_2008, y = farright_sh)) +
  geom_point(size = 1.2, color = "grey30") +
  geom_vline(xintercept = 5000, linetype = "dashed", color = "grey50",
             linewidth = 0.4) +
  scale_x_log10(
    breaks = c(100, 1000, 5000, 50000, 1000000),
    labels = c("100", "1k", "5k", "50k", "1M")
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 0.6, by = 0.2)) +
  facet_wrap(~ year, nrow = ifelse(uniqueN(d$year) > 5, 2, 1)) +
  labs(x = "Population (2008 census)",
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

fig_h <- ifelse(uniqueN(d$year) > 5, 6, 4)
ggsave("output/figures/italy/fig_logpop_facet.pdf", width = 10, height = fig_h)
ggsave("output/figures/italy/fig_logpop_facet.png", width = 10, height = fig_h, dpi = 300)
cat("Savedoutput/figures/italy/fig_logpop_facet.pdf and .png\n")
