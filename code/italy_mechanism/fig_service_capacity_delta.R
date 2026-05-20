################################################################################
# Muni-level CHANGE in Cremaschi's Service Capacity Index (2013 - 2009),
# plotted against distance to legal threshold.
#
# This is the muni-level analogue of Cremaschi's MTWFE estimate. If the reform
# caused service decline in sub-threshold munis, we should see Delta_y < 0 on
# the left of x=0 and a clean upward jump at x=0. If the change is a smooth
# function of size with no jump, Cremaschi's negative MTWFE estimates reflect
# a size-gradient confound rather than a policy effect.
#
# Source: data_raw/italy/cremaschi_replication/service_dataset.dta.
# Outcome: Delta y = y_2013 - y_2009 for pol_cap, reg_cap, garb_cap.
################################################################################

library(data.table)
library(haven)
library(ggplot2)
library(scales)
library(rdrobust)

svc <- as.data.table(read_dta("data_raw/italy/cremaschi_replication/service_dataset.dta"))

# Drop munis missing in one of the two waves (matches do-file)
svc[, n := .N, by = id08]
svc <- svc[n == 2]
svc[, n := NULL]

svc[, threshold := fifelse(mont_group == 1, 3000, 5000)]
svc[, x := log(pop_tot_2008 / threshold)]

service_cols   <- c("pol_cap", "reg_cap", "garb_cap")
service_labels <- c(pol_cap = "Local police",
                    reg_cap = "Civil registry",
                    garb_cap = "Garbage collection")
mis_cols       <- c(pol_cap = "mis_pol", reg_cap = "mis_reg", garb_cap = "mis_garb")

# Compute muni-level Delta y for each service
make_delta <- function(svc_var) {
  mc <- mis_cols[svc_var]
  d_wide <- dcast(svc, id08 + x ~ year, value.var = c(svc_var, mc))
  setnames(d_wide,
           c(paste0(svc_var, "_2009"), paste0(svc_var, "_2013"),
             paste0(mc, "_2009"),     paste0(mc, "_2013")),
           c("y_2009", "y_2013", "mis_2009", "mis_2013"))
  d_wide <- d_wide[mis_2009 == 0 & mis_2013 == 0 &
                   !is.na(y_2009) & !is.na(y_2013) & !is.na(x)]
  d_wide[, dy := y_2013 - y_2009]
  d_wide[, .(id08, x, dy, service = svc_var)]
}

delta <- rbindlist(lapply(service_cols, make_delta))
delta[, service_lab := factor(service_labels[service], levels = service_labels)]

cat("\n---- Muni-pair counts after both-waves-non-missing filter ----\n")
print(delta[, .(.N), by = service])

# ---- Per-service: bin scatter + loess fit + RD estimate -----------------
n_bins_per_side <- 25

make_panel <- function(svc_var) {
  d <- delta[service == svc_var]
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
  bins[, service := svc_var]

  rd <- suppressWarnings(rdrobust(y = d$dy, x = d$x, c = 0,
                                   p = 1, kernel = "tri", bwselect = "mserd"))
  est <- as.numeric(rd$Estimate[, "tau.us"])
  se  <- as.numeric(rd$se["Conventional", 1])
  p_r <- as.numeric(rd$pv["Robust", 1])
  h_l <- as.numeric(rd$bws["h", 1])
  h_r <- as.numeric(rd$bws["h", 2])
  rd_row <- data.table(service = svc_var, est = est, se = se, p = p_r,
                       bw = h_l,
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
  fits[, service := svc_var]

  list(bins = bins, fits = fits, rd = rd_row)
}

panels <- lapply(service_cols, make_panel)
bins   <- rbindlist(lapply(panels, `[[`, "bins"),  fill = TRUE)
fits   <- rbindlist(lapply(panels, `[[`, "fits"),  fill = TRUE)
rd_tbl <- rbindlist(lapply(panels, `[[`, "rd"),    fill = TRUE)

bins[,   service_lab := factor(service_labels[service], levels = service_labels)]
fits[,   service_lab := factor(service_labels[service], levels = service_labels)]
rd_tbl[, service_lab := factor(service_labels[service], levels = service_labels)]

cat("\n---- RD estimates: Delta service capacity at the threshold ----\n")
print(rd_tbl[, .(service, est = round(est, 3), se = round(se, 3),
                 p = round(p, 3), bw = round(bw, 2))])

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
       y = "Change in Service Capacity Index, 2013 - 2009") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.25),
    strip.background   = element_rect(fill = "white", color = "grey70"),
    strip.text         = element_text(face = "bold", size = 9),
    axis.text          = element_text(size = 8),
    axis.title         = element_text(size = 10),
    plot.margin        = margin(5, 8, 5, 5)
  )

ggsave("output/figures/italy/fig_service_capacity_delta.pdf", p, width = 9, height = 3.4)
ggsave("output/figures/italy/fig_service_capacity_delta.png", p, width = 9, height = 3.4,
       dpi = 300)
cat("\nSavedoutput/figures/italy/fig_service_capacity_delta.pdf and .png\n")
