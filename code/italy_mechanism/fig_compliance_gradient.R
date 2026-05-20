################################################################################
# Compliance gradient figure for the paper's Section 4.2.
#
# 3 rows x 3 cols = 9 panels: post-2010 unione (Ministry registry) plus the
# eight Article 14 fundamental functions delivered in associated form
# (OpenCivitas 2015). Mountain and non-mountain municipalities are pooled;
# the x-axis is log(pop / threshold), where threshold = 5,000 for non-mountain
# and 3,000 for mountain. The legal cutoff is at x = 0 in every panel; a
# discontinuity in compliance behaviour at the legal cutoff would appear as
# a jump there.
#
# Each panel shows:
#   - Bin scatter: ~25 percentile bins per side of the cutoff (~100-150
#     municipalities per bin), plotted as grey points.
#   - Local linear regression (loess, span = 0.5, degree = 1) fitted to the
#     raw 0/1 observations separately on each side, with shaded 95% CI.
################################################################################

library(data.table)
library(haven)
library(ggplot2)
library(scales)
library(rdrobust)

# ---- Panel + post-2010 unione flag ---------------------------------------
panel <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
panel08 <- unique(panel[year == 2008,
                        .(id08, municipality, pop_tot_2008, mont_group)])

u <- fread("data_processed/italy/post2010_union_indicator.csv")
panel08 <- merge(panel08, u[, .(id08, in_post2010_union)],
                 by = "id08", all.x = TRUE)
panel08[is.na(in_post2010_union), in_post2010_union := 0L]

# ---- OpenCivitas associated-delivery flags (per service) -----------------
xw <- fread("data_processed/italy/opencivitas_panel_crosswalk.csv")
panel08 <- merge(panel08, xw[, .(id08, USERNAME)], by = "id08", all.x = TRUE)

parse_val <- function(x) as.numeric(gsub(",", ".", x))
oc_specs <- list(
  POLIZIA  = list(file = "Ind_FC20POLIZIA_3.csv",  var = "DUMMY_POLIZIA_ASSOC",     blank_as_zero = FALSE),
  RIFIUTI  = list(file = "Ind_FC20RIFIUTI_3.csv",  var = "DUMMY_RIFIUTI_ASSOC",     blank_as_zero = TRUE),
  SOCIALE  = list(file = "Ind_FC20SOCNID_2.csv",   var = "DUMMY_SOCIALE_ASSOCIATA", blank_as_zero = FALSE),
  TERR     = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_TERR_ASSOCIATA",    blank_as_zero = FALSE),
  VIAB     = list(file = "Ind_FC20TERRVIAB_2.csv", var = "DUMMY_VIAB_ASSOCIATA",    blank_as_zero = FALSE),
  ANAG     = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_ANAGR_ASSOCIATA",   blank_as_zero = FALSE),
  TRIB     = list(file = "Ind_FC20AMMIN_2.csv",    var = "DUMMY_TRIBUTI_ASSOCIATA", blank_as_zero = FALSE),
  IST_INFA = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_INFANZIA_GP_ASSOC", blank_as_zero = FALSE),
  IST_PRSE = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_PRIMSEC_GP_ASSOC",  blank_as_zero = FALSE),
  IST_REFE = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_REFEZIONE_GP_ASSOC",blank_as_zero = FALSE),
  IST_TRAS = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_TRASPORTO_GP_ASSOC",blank_as_zero = FALSE),
  IST_DISA = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_DISABILI_GP_ASSOC", blank_as_zero = FALSE),
  IST_ALTR = list(file = "Ind_FC20ISTRUZ_2.csv",   var = "DUMMY_ALTRIISTR_GP_ASSOC",blank_as_zero = FALSE)
)
get_one <- function(spec) {
  ind <- fread(file.path("data_raw/italy/opencivitas", spec$file), sep = ";")
  a   <- ind[`Indicatore/Determinante` == spec$var]
  a[, val := if (spec$blank_as_zero)
               ifelse(Valore == "" | is.na(Valore), 0, parse_val(Valore))
             else parse_val(Valore)]
  unique(a[!is.na(val) & val %in% c(0, 1), .(USERNAME, val)])
}
for (nm in names(oc_specs)) {
  d_nm <- get_one(oc_specs[[nm]])
  setnames(d_nm, "val", paste0("oc_", nm))
  panel08 <- merge(panel08, d_nm, by = "USERNAME", all.x = TRUE)
}

# Pool the 6 ISTRUZ sub-functions into a single "any education function" flag
istr_cols <- paste0("oc_", c("IST_INFA","IST_PRSE","IST_REFE",
                             "IST_TRAS","IST_DISA","IST_ALTR"))
panel08[, oc_ISTRUZ := {
  m <- as.matrix(.SD)
  ifelse(rowSums(!is.na(m)) == 0, NA_integer_,
         as.integer(rowSums(m == 1, na.rm = TRUE) >= 1))
}, .SDcols = istr_cols]

# ---- Distance from threshold on log scale --------------------------------
panel08[, threshold := fifelse(mont_group == 1, 3000, 5000)]
panel08[, x := log(pop_tot_2008 / threshold)]

# Panel order: unione first, then the 8 fundamental functions in a sensible
# substantive grouping (clearly-mandated services first).
service_cols <- c("in_post2010_union",
                  "oc_POLIZIA", "oc_RIFIUTI",
                  "oc_SOCIALE", "oc_ANAG",   "oc_ISTRUZ",
                  "oc_TERR",    "oc_VIAB",   "oc_TRIB")
service_labels <- c(
  in_post2010_union = "Post-2010 unione (Ministry)",
  oc_POLIZIA   = "Local police",
  oc_RIFIUTI   = "Garbage",
  oc_SOCIALE   = "Social services",
  oc_ANAG      = "Civil registry",
  oc_ISTRUZ    = "Education (any of 6)",
  oc_TERR      = "Territorial planning",
  oc_VIAB      = "Road maintenance",
  oc_TRIB      = "Tax collection"
)

# ---- For each panel: bin scatter + loess fit on each side ----------------
n_bins_per_side <- 25  # ~100-150 obs per bin for OC panels; ~150 for unione

make_panel <- function(svc) {
  d <- panel08[!is.na(get(svc)), .(x, y = as.numeric(get(svc)))]
  qx <- quantile(d$x, c(0.02, 0.98), na.rm = TRUE)
  d  <- d[x >= qx[1] & x <= qx[2]]

  # ---- Percentile bins, separately on each side (visual context only) ----
  bin_side <- function(side_d) {
    if (nrow(side_d) < 30) return(data.table())
    breaks <- quantile(side_d$x,
                       probs = seq(0, 1, length.out = n_bins_per_side + 1),
                       na.rm = TRUE)
    breaks <- unique(breaks)
    side_d[, bin := cut(x, breaks = breaks, include.lowest = TRUE,
                         labels = FALSE)]
    side_d[!is.na(bin), .(x = mean(x), y = mean(y), n = .N), by = bin]
  }
  bins_left  <- bin_side(d[x <  0])
  bins_right <- bin_side(d[x >= 0])
  bins <- rbind(bins_left[, side := "L"], bins_right[, side := "R"])
  bins[, service := svc]

  # ---- rdrobust: discontinuity estimate at the cutoff (c = 0) ----
  # Local polynomial of degree 1, triangular kernel, MSE-optimal bandwidth.
  # We display the CONVENTIONAL estimate so it matches the visual line
  # exactly (the line's value at x=0 is, by construction, the conventional
  # point estimate). The robust bias-corrected p-value is also reported.
  rd <- tryCatch(
    suppressWarnings(rdrobust(y = d$y, x = d$x, c = 0,
                              p = 1, kernel = "tri", bwselect = "mserd")),
    error = function(e) { message("rdrobust failed for ", svc, ": ", e$message); NULL }
  )
  if (is.null(rd)) {
    return(list(bins = bins,
                fits = data.table(),
                rd = data.table(service = svc, est = NA_real_, se = NA_real_,
                                p = NA_real_, bw = NA_real_, label = "RD: n/a"),
                n = nrow(d)))
  }
  rd_est_us <- as.numeric(rd$Estimate[, "tau.us"])     # Conventional point est.
  rd_se_us  <- as.numeric(rd$se["Conventional", 1])    # Conventional SE
  rd_p_rb   <- as.numeric(rd$pv["Robust", 1])          # Robust bias-corrected p
  h_left    <- as.numeric(rd$bws["h", 1])
  h_right   <- as.numeric(rd$bws["h", 2])
  rd_label  <- sprintf("RD: %+0.3f (%0.3f)", rd_est_us, rd_se_us)
  rd_row <- data.table(service = svc, est = rd_est_us, se = rd_se_us,
                       p = rd_p_rb, bw = h_left, label = rd_label)

  # ---- Triangular-kernel-weighted local linear regression on each side ----
  # SAME kernel and SAME bandwidth as rdrobust (this is the "Conventional"
  # local linear fit that rdrobust's tau.us is based on). The fit's value at
  # x = 0+ minus its value at x = 0- equals rd_est_us by construction.
  fit_side <- function(side_d, h, x_grid) {
    side_d <- side_d[abs(x) <= h]
    if (nrow(side_d) < 5)
      return(data.table(x = x_grid, y = NA_real_,
                        ymin = NA_real_, ymax = NA_real_))
    side_d[, w := pmax(0, 1 - abs(x) / h)]
    fit  <- lm(y ~ x, data = side_d, weights = w)
    pred <- predict(fit, newdata = data.frame(x = x_grid), se.fit = TRUE)
    data.table(x = x_grid,
               y    = pmax(0, pmin(1, pred$fit)),
               ymin = pmax(0, pmin(1, pred$fit - 1.96 * pred$se.fit)),
               ymax = pmax(0, pmin(1, pred$fit + 1.96 * pred$se.fit)))
  }
  grid_left  <- seq(-h_left,  -0.001, length.out = 60)
  grid_right <- seq( 0.001,    h_right, length.out = 60)
  fit_left  <- fit_side(d[x <  0], h_left,  grid_left)[,  side := "L"]
  fit_right <- fit_side(d[x >= 0], h_right, grid_right)[, side := "R"]
  fits <- rbind(fit_left, fit_right)
  fits[, service := svc]

  list(bins = bins, fits = fits, rd = rd_row, n = nrow(d))
}

panel_list <- lapply(service_cols, make_panel)
names(panel_list) <- service_cols

bins <- rbindlist(lapply(panel_list, `[[`, "bins"), fill = TRUE)
fits <- rbindlist(lapply(panel_list, `[[`, "fits"), fill = TRUE)
rd_tbl <- rbindlist(lapply(panel_list, `[[`, "rd"), fill = TRUE)

bins[,   service_lab := factor(service_labels[service], levels = service_labels)]
fits[,   service_lab := factor(service_labels[service], levels = service_labels)]
rd_tbl[, service_lab := factor(service_labels[service], levels = service_labels)]

ns <- sapply(panel_list, `[[`, "n")
cat("\n---- Sample sizes per panel (after 2nd-98th pctile trim) ----\n")
for (s in names(ns)) cat(sprintf("  %-22s n = %d\n", s, ns[s]))

cat("\n---- rdrobust estimates (degree 1, triangular, MSE-optimal bw) ----\n")
print(rd_tbl[, .(service, est = round(est, 4), se = round(se, 4),
                 p = round(p, 3), bw = round(bw, 2))])

# ---- Save CSV and a small LaTeX table for the paper appendix (response to
#       reviewer #7: formal RD estimates for each of the 9 compliance
#       measures at the 5,000 cutoff) -----------------------------------------
rd_tbl[, ci_lo := est - 1.96 * se]
rd_tbl[, ci_hi := est + 1.96 * se]
dir.create("output/csvs/italy",   showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables/italy", showWarnings = FALSE, recursive = TRUE)
fwrite(rd_tbl[, .(service, label = as.character(service_lab),
                  est, se, ci_lo, ci_hi, p, bw)],
       "output/csvs/italy/compliance_rd.csv")

esc <- function(s) gsub("&", "\\&", s, fixed = TRUE)
rows <- character()
for (i in seq_len(nrow(rd_tbl))) {
  r <- rd_tbl[i]
  rows <- c(rows, sprintf("      %s & %+0.3f & [%+0.3f, %+0.3f] & %0.3f & %0.2f \\\\",
                           esc(as.character(r$service_lab)),
                           r$est, r$ci_lo, r$ci_hi, r$p, r$bw))
}
writeLines(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Local-linear RD estimates of the discontinuity in compliance at the 5,000-inhabitant cutoff}",
  "    \\label{tab:compliance_rd}",
  "    \\begin{tabular*}{0.90\\linewidth}{@{\\extracolsep{\\fill}}lcccc@{}}",
  "      \\toprule",
  "      Compliance measure & Estimate & 95\\% CI & $p$ (robust) & MSE-bw (log-pop) \\\\",
  "      \\midrule",
  rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  "      \\item \\textit{Notes:} Each row is a separate \\texttt{rdrobust} regression of a 0/1 compliance indicator on the running variable $\\log(\\text{pop}/\\text{threshold})$, with degree-1 local polynomial, triangular kernel, and MSE-optimal bandwidth. The Ministry registry indicator (post-2010 \\textit{unione} membership) and the eight OpenCivitas associated-delivery indicators are reported. The conventional point estimate is shown with its 95\\% confidence interval; the $p$-value is the robust bias-corrected $p$ from \\citet{calonico2014}.",
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "output/tables/italy/tab_compliance_rd.tex")
cat("Saved output/tables/italy/tab_compliance_rd.tex\n")
cat("Saved output/csvs/italy/compliance_rd.csv\n")

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
  facet_wrap(~ service_lab, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(x = "Population relative to legal threshold (log scale)",
       y = "Share of municipalities with associated delivery") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.25),
    strip.background   = element_rect(fill = "white", color = "grey70"),
    strip.text         = element_text(face = "bold", size = 8.5),
    axis.text.x        = element_text(size = 8),
    axis.text.y        = element_text(size = 8),
    axis.title         = element_text(size = 10),
    plot.margin        = margin(5, 8, 5, 5)
  )

ggsave("output/figures/italy/fig_compliance_gradient.pdf", p, width = 9, height = 7)
ggsave("output/figures/italy/fig_compliance_gradient.png", p, width = 9, height = 7,
       dpi = 300)
cat("\nSavedoutput/figures/italy/fig_compliance_gradient.pdf and .png\n")
