################################################################################
# Service-delivery change figure -- no-RD variant of fig_service_diff_delta.R.
#
# Same data preparation and panel layout as the original (3 panels: police,
# registry, garbage; muni-level change 2013 - 2009 in standardized "Services
# against Standard Demand" outcome, pooled across mountain and non-mountain
# on x = log(pop / threshold)).
#
# Difference: drops the local-linear fit lines, confidence ribbons, and "RD"
# label annotations. Each panel now shows ONLY the percentile-bin scatter and
# the dashed line at the legal threshold (x = 0). The rdrobust computation is
# retained so the threshold-discontinuity numbers can still be cited in the
# prose, but they are not drawn on the figure.
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

# ---- Per-service: bin scatter + RD estimate (computed but not plotted) ---
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
  est <- -as.numeric(rd$Estimate[, "tau.us"])
  se  <-  as.numeric(rd$se["Conventional", 1])
  p_r <-  as.numeric(rd$pv["Robust", 1])
  h_l <-  as.numeric(rd$bws["h", 1])
  rd_row <- data.table(service = v, est = est, se = se, p = p_r, bw = h_l)

  list(bins = bins, rd = rd_row)
}

panels <- lapply(service_cols, make_panel)
bins   <- rbindlist(lapply(panels, `[[`, "bins"),  fill = TRUE)
rd_tbl <- rbindlist(lapply(panels, `[[`, "rd"),    fill = TRUE)

bins[,   service_lab := factor(service_labels[service], levels = service_labels)]
rd_tbl[, service_lab := factor(service_labels[service], levels = service_labels)]

cat("\n---- RD estimates: Delta z (Services against Standard Demand) ----\n")
print(rd_tbl[, .(service, est = round(est, 3), se = round(se, 3),
                 p = round(p, 3), bw = round(bw, 2))])

# ---- Plot ----------------------------------------------------------------
x_breaks <- log(c(1/10, 1/3, 1, 3, 10))
x_labels <- c("÷10", "÷3", "1", "×3", "×10")

p <- ggplot() +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50",
             linewidth = 0.4) +
  geom_point(data = bins, aes(x = x, y = y),
             size = 1.1, color = "grey25", alpha = 0.75) +
  facet_wrap(~ service_lab, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  scale_y_continuous() +
  labs(x = "Population relative to legal threshold (log scale)",
       y = expression(Delta * " z-score, 2013 - 2009")) +
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

ggsave("output/figures/italy/fig_service_diff_delta_no_rd.pdf", p, width = 9, height = 3.4)
ggsave("output/figures/italy/fig_service_diff_delta_no_rd.png", p, width = 9, height = 3.4,
       dpi = 300)
cat("\nSavedoutput/figures/italy/fig_service_diff_delta_no_rd.pdf and .png\n")
