################################################################################
# Service-capacity-index gradient by distance to legal threshold (Cremaschi
# Table M.2 outcome). 2 rows (year) x 3 cols (service) = 6 panels.
#
# Question: does Cremaschi's service-capacity-index outcome show a size
# gradient through the legal threshold, and if so, did anything change at
# the threshold between 2009 and 2013 (the pre- and post-reform waves)?
#
# Outcome: pol_cap, reg_cap, garb_cap (Service Capacity Index, 1-10 scale).
# Source: data_raw/italy/cremaschi_replication/service_dataset.dta.
#
# Methodology mirrors fig_compliance_gradient.R: pooled across mountain
# status via x = log(pop / threshold); triangular-kernel local linear
# regression within MSE-optimal bandwidth (rdrobust default p=1, kernel="tri",
# bwselect="mserd"); bin scatter for context; RD estimate annotated per panel.
################################################################################

library(data.table)
library(haven)
library(ggplot2)
library(scales)
library(rdrobust)

# ---- Load Cremaschi service data ----------------------------------------
# mont_group is already in service_dataset.dta (no merge needed).
svc <- as.data.table(read_dta("data_raw/italy/cremaschi_replication/service_dataset.dta"))

# Drop munis missing in one year (mirrors do-file lines 152-154)
svc[, n := .N, by = id08]
svc <- svc[n == 2]
svc[, n := NULL]

# Distance from threshold: 5,000 (non-mountain) or 3,000 (mountain).
svc[, threshold := fifelse(mont_group == 1, 3000, 5000)]
svc[, x := log(pop_tot_2008 / threshold)]

service_cols   <- c("pol_cap", "reg_cap", "garb_cap")
service_labels <- c(pol_cap = "Local police",
                    reg_cap = "Civil registry",
                    garb_cap = "Garbage collection")
mis_cols       <- c(pol_cap = "mis_pol",
                    reg_cap = "mis_reg",
                    garb_cap = "mis_garb")

n_bins_per_side <- 25

make_panel <- function(svc_var, yr) {
  d <- svc[year == yr & get(mis_cols[svc_var]) == 0,
           .(x, y = get(svc_var))]
  d <- d[!is.na(y) & !is.na(x)]
  qx <- quantile(d$x, c(0.02, 0.98), na.rm = TRUE)
  d  <- d[x >= qx[1] & x <= qx[2]]

  bin_side <- function(side_d) {
    if (nrow(side_d) < 30) return(data.table())
    breaks <- unique(quantile(side_d$x,
                              probs = seq(0, 1, length.out = n_bins_per_side + 1),
                              na.rm = TRUE))
    side_d[, bin := cut(x, breaks = breaks, include.lowest = TRUE,
                         labels = FALSE)]
    side_d[!is.na(bin), .(x = mean(x), y = mean(y), n = .N), by = bin]
  }
  bins <- rbind(bin_side(d[x <  0])[, side := "L"],
                bin_side(d[x >= 0])[, side := "R"])
  bins[, `:=`(service = svc_var, year = yr)]

  rd <- tryCatch(
    suppressWarnings(rdrobust(y = d$y, x = d$x, c = 0,
                              p = 1, kernel = "tri", bwselect = "mserd")),
    error = function(e) NULL
  )
  if (is.null(rd)) {
    return(list(bins = bins, fits = data.table(),
                rd = data.table(service = svc_var, year = yr,
                                est = NA_real_, se = NA_real_, p = NA_real_,
                                bw = NA_real_, label = "RD: n/a")))
  }
  est <- as.numeric(rd$Estimate[, "tau.us"])
  se  <- as.numeric(rd$se["Conventional", 1])
  p_r <- as.numeric(rd$pv["Robust", 1])
  h_l <- as.numeric(rd$bws["h", 1])
  h_r <- as.numeric(rd$bws["h", 2])
  rd_row <- data.table(service = svc_var, year = yr, est = est, se = se,
                       p = p_r, bw = h_l,
                       label = sprintf("RD: %+0.3f (%0.3f)", est, se))

  fit_side <- function(side_d, h, x_grid) {
    side_d <- side_d[abs(x) <= h]
    if (nrow(side_d) < 5) return(data.table(x = x_grid, y = NA_real_,
                                              ymin = NA_real_, ymax = NA_real_))
    side_d[, w := pmax(0, 1 - abs(x) / h)]
    fit  <- lm(y ~ x, data = side_d, weights = w)
    pred <- predict(fit, newdata = data.frame(x = x_grid), se.fit = TRUE)
    data.table(x = x_grid, y = pred$fit,
               ymin = pred$fit - 1.96 * pred$se.fit,
               ymax = pred$fit + 1.96 * pred$se.fit)
  }
  grid_left  <- seq(-h_l,  -0.001, length.out = 60)
  grid_right <- seq( 0.001,  h_r, length.out = 60)
  fits <- rbind(
    fit_side(d[x <  0], h_l, grid_left)[,  side := "L"],
    fit_side(d[x >= 0], h_r, grid_right)[, side := "R"]
  )
  fits[, `:=`(service = svc_var, year = yr)]

  list(bins = bins, fits = fits, rd = rd_row)
}

panels <- list()
for (yr in c(2009, 2013))
  for (svc_var in service_cols)
    panels[[paste(svc_var, yr)]] <- make_panel(svc_var, yr)

bins   <- rbindlist(lapply(panels, `[[`, "bins"),  fill = TRUE)
fits   <- rbindlist(lapply(panels, `[[`, "fits"),  fill = TRUE)
rd_tbl <- rbindlist(lapply(panels, `[[`, "rd"),    fill = TRUE)

bins[,   service_lab := factor(service_labels[service], levels = service_labels)]
fits[,   service_lab := factor(service_labels[service], levels = service_labels)]
rd_tbl[, service_lab := factor(service_labels[service], levels = service_labels)]
bins[,   year_lab := factor(paste0(year), levels = c("2009","2013"))]
fits[,   year_lab := factor(paste0(year), levels = c("2009","2013"))]
rd_tbl[, year_lab := factor(paste0(year), levels = c("2009","2013"))]

cat("\n---- RD estimates: service capacity at the threshold (2009 vs 2013) ----\n")
print(rd_tbl[, .(year, service, est = round(est, 3), se = round(se, 3),
                 p = round(p, 3), bw = round(bw, 2))])

# ---- Plot ----------------------------------------------------------------
x_breaks <- log(c(1/10, 1/3, 1, 3, 10))
x_labels <- c("÷10", "÷3", "1", "×3", "×10")

p <- ggplot() +
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
  facet_grid(year_lab ~ service_lab, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  scale_y_continuous() +
  labs(x = "Population relative to legal threshold (log scale)",
       y = "Service Capacity Index (1–10 scale)") +
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

ggsave("output/figures/italy/fig_service_capacity_gradient.pdf", p, width = 9, height = 5)
ggsave("output/figures/italy/fig_service_capacity_gradient.png", p, width = 9, height = 5,
       dpi = 300)
cat("\nSavedoutput/figures/italy/fig_service_capacity_gradient.pdf and .png\n")
