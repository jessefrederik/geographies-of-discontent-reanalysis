################################################################################
# Muni-level CHANGE in Cremaschi's "Services against Standard Demand" outcome
# (their main-paper Table 3 outcome), plotted against distance to legal
# threshold.
#
# This is the muni-level analogue of Cremaschi's main-paper MTWFE estimates:
#   Police   -0.196 (0.047) **
#   Registry -0.157 (0.080) *
#   Garbage  -0.076 (0.036) *
# The dependent variable is the z-score (standardized across the full sample)
# of the percentage deviation in service output relative to the population-
# segment average. Cremaschi's exact transformation is reproduced here:
#   egen z_var_diff = std(var_diff)   ->   y = z_var_diff
#
# As with the SCI delta figure, the question is whether the differential
# trend Cremaschi captures with TWFE/MTWFE reflects a sharp policy
# discontinuity at the threshold or a continuous size gradient.
#
# Source: data_raw/italy/cremaschi_replication/service_dataset.dta.
################################################################################

library(data.table)
library(haven)
library(ggplot2)
library(scales)
library(rdrobust)

svc <- as.data.table(read_dta("data_raw/italy/cremaschi_replication/service_dataset.dta"))

# Drop munis missing in one wave
svc[, n := .N, by = id08]
svc <- svc[n == 2]
svc[, n := NULL]

svc[, threshold := fifelse(mont_group == 1, 3000, 5000)]
svc[, x := log(pop_tot_2008 / threshold)]

service_cols   <- c("pol_diff", "reg_diff", "garb_diff")
service_labels <- c(pol_diff  = "Local police (z-score)",
                    reg_diff  = "Civil registry (z-score)",
                    garb_diff = "Garbage collection (z-score)")
mis_cols       <- c(pol_diff  = "mis_pol",
                    reg_diff  = "mis_reg",
                    garb_diff = "mis_garb")

# ---- Standardize each diff variable across the full sample ---------------
# Mirrors Cremaschi's main-paper transform:
#   egen z_var_diff = std(var_diff); drop var_diff; rename z_var_diff var_diff
for (v in service_cols) {
  mc <- mis_cols[v]
  use <- svc[get(mc) == 0 & !is.na(get(v)), get(v)]
  mu  <- mean(use); sd_v <- sd(use)
  svc[get(mc) == 0 & !is.na(get(v)),
      paste0("z_", v) := (get(v) - mu) / sd_v]
}

# ---- Compute Delta z per muni per service --------------------------------
make_delta <- function(v) {
  zv <- paste0("z_", v); mc <- mis_cols[v]
  long <- svc[, .(id08, x, year, z = get(zv), m = get(mc))]
  d_wide <- dcast(long, id08 + x ~ year, value.var = c("z","m"))
  d_wide <- d_wide[m_2009 == 0 & m_2013 == 0 &
                   !is.na(z_2009) & !is.na(z_2013) & !is.na(x)]
  d_wide[, .(id08, x, dy = z_2013 - z_2009, service = v)]
}
delta <- rbindlist(lapply(service_cols, make_delta))
delta[, service_lab := factor(service_labels[service], levels = service_labels)]

cat("\n---- Muni-pair counts after both-waves-non-missing filter ----\n")
print(delta[, .(.N), by = service])

# ---- Per-service: bin scatter + loess fit + RD estimate -----------------
n_bins_per_side <- 25

make_panel <- function(v) {
  d <- delta[service == v]
  qx <- quantile(d$x, c(0.02, 0.98), na.rm = TRUE)
  d  <- d[x >= qx[1] & x <= qx[2]]

  bin_side <- function(side_d) {
    if (nrow(side_d) < 30) return(data.table())
    breaks <- unique(quantile(side_d$x,
                              probs = seq(0, 1, length.out = n_bins_per_side + 1),
                              na.rm = TRUE))
    side_d[, bin := cut(x, breaks = breaks, include.lowest = TRUE,
                         labels = FALSE)]
    side_d[!is.na(bin), .(x = mean(x), y = mean(dy), n = .N), by = bin]
  }
  bins <- rbind(bin_side(d[x <  0])[, side := "L"],
                bin_side(d[x >= 0])[, side := "R"])
  bins[, service := v]

  rd <- suppressWarnings(rdrobust(y = d$dy, x = d$x, c = 0,
                                   p = 1, kernel = "tri", bwselect = "mserd"))
  # rdrobust returns the conventional jump (right - left). Flip sign to
  # report as (treated - control) = (below - above), matching the sign
  # convention of Cremaschi et al.'s Table 3: negative = sub-threshold
  # municipalities declined more than above-threshold ones.
  est <- -as.numeric(rd$Estimate[, "tau.us"])
  se  <-  as.numeric(rd$se["Conventional", 1])
  p_r <-  as.numeric(rd$pv["Robust", 1])
  h_l <-  as.numeric(rd$bws["h", 1])
  h_r <-  as.numeric(rd$bws["h", 2])
  rd_row <- data.table(service = v, est = est, se = se, p = p_r, bw = h_l,
                       label = sprintf("RD: %+0.3f (%0.3f)", est, se))

  fit_side <- function(side_d, h, x_grid) {
    side_d <- side_d[abs(x) <= h]
    side_d[, w := pmax(0, 1 - abs(x) / h)]
    fit  <- lm(dy ~ x, data = side_d, weights = w)
    pred <- predict(fit, newdata = data.frame(x = x_grid), se.fit = TRUE)
    data.table(x = x_grid, y = pred$fit,
               ymin = pred$fit - 1.96 * pred$se.fit,
               ymax = pred$fit + 1.96 * pred$se.fit)
  }
  grid_left  <- seq(-h_l,  -0.001, length.out = 60)
  grid_right <- seq( 0.001,  h_r, length.out = 60)
  fits <- rbind(fit_side(d[x <  0], h_l, grid_left)[,  side := "L"],
                fit_side(d[x >= 0], h_r, grid_right)[, side := "R"])
  fits[, service := v]
  list(bins = bins, fits = fits, rd = rd_row)
}

panels <- lapply(service_cols, make_panel)
bins   <- rbindlist(lapply(panels, `[[`, "bins"),  fill = TRUE)
fits   <- rbindlist(lapply(panels, `[[`, "fits"),  fill = TRUE)
rd_tbl <- rbindlist(lapply(panels, `[[`, "rd"),    fill = TRUE)

bins[,   service_lab := factor(service_labels[service], levels = service_labels)]
fits[,   service_lab := factor(service_labels[service], levels = service_labels)]
rd_tbl[, service_lab := factor(service_labels[service], levels = service_labels)]

cat("\n---- RD estimates: Delta z (Services against Standard Demand) ----\n")
# MDE at 80% power, two-sided alpha=0.05 (z_{0.975} + z_{0.80} = 1.96 + 0.842).
mde_mult <- qnorm(0.975) + qnorm(0.80)
rd_tbl[, mde80 := mde_mult * se]
# Published Cremaschi et al. (2024) Table 3 MTWFE point estimates, in
# standard-deviation units of the Services Against Standard Demand outcome:
mtwfe_published <- c(pol_diff = -0.196, reg_diff = -0.157, garb_diff = -0.076)
rd_tbl[, mtwfe_pub := mtwfe_published[service]]
rd_tbl[, can_detect := abs(mtwfe_pub) >= mde80]
print(rd_tbl[, .(service, est = round(est, 3), se = round(se, 3),
                 p = round(p, 3), bw = round(bw, 2),
                 mde80 = round(mde80, 3),
                 mtwfe_pub = round(mtwfe_pub, 3),
                 can_detect)])
cat(sprintf("  MDE multiplier (z_{0.975} + z_{0.80}) = %.3f\n", mde_mult))

# ---- Plot ----------------------------------------------------------------
x_breaks <- log(c(1/10, 1/3, 1, 3, 10))
x_labels <- c("÷10", "÷3", "1", "×3", "×10")

p <- ggplot() +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50",
             linewidth = 0.4) +
  geom_ribbon(data = fits, aes(x = x, ymin = ymin, ymax = ymax, group = side),
              fill = "grey70", alpha = 0.40) +
  geom_point(data = bins, aes(x = x, y = y),
             size = 0.9, color = "grey25", alpha = 0.7) +
  geom_line(data = fits, aes(x = x, y = y, group = side),
            color = "grey15", linewidth = 0.65) +
  geom_text(data = rd_tbl,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.08, vjust = 1.4, size = 2.7,
            family = "mono", color = "grey15") +
  facet_wrap(~ service_lab, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  scale_y_continuous() +
  labs(x = "Population relative to legal threshold (log scale)",
       y = "Change in Services-against-Standard-Demand z-score, 2013 - 2009") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.25),
    strip.background   = element_rect(fill = "white", color = "grey70"),
    strip.text         = element_text(face = "bold", size = 9),
    axis.text          = element_text(size = 8),
    axis.title         = element_text(size = 9.5),
    plot.margin        = margin(5, 8, 5, 5)
  )

ggsave("output/figures/italy/fig_service_diff_delta.pdf", p, width = 9, height = 3.4)
ggsave("output/figures/italy/fig_service_diff_delta.png", p, width = 9, height = 3.4,
       dpi = 300)
cat("\nSavedoutput/figures/italy/fig_service_diff_delta.pdf and .png\n")
