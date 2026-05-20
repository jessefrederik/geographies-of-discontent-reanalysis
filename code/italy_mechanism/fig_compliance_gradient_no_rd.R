################################################################################
# Compliance gradient figure -- no-RD variant of fig_compliance_gradient.R.
#
# Same data preparation and panel layout as the original (9 panels: post-2010
# unione plus the 8 Article 14 fundamental functions, pooled across mountain
# and non-mountain on x = log(pop / threshold)).
#
# Difference: drops the rdrobust estimate, the side-specific local linear fits
# and the RD label annotation. Each panel now shows ONLY the percentile-bin
# scatter and the dashed line at the legal threshold (x = 0).
################################################################################

library(data.table)
library(haven)
library(ggplot2)
library(scales)

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

# ---- For each panel: bin scatter only ------------------------------------
# Bins are computed separately on each side of x = 0 so no bin straddles the
# legal threshold (purely cosmetic -- there's no fit being estimated here).
n_bins_per_side <- 25

make_panel <- function(svc) {
  d <- panel08[!is.na(get(svc)), .(x, y = as.numeric(get(svc)))]
  qx <- quantile(d$x, c(0.02, 0.98), na.rm = TRUE)
  d  <- d[x >= qx[1] & x <= qx[2]]

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
  list(bins = bins, n = nrow(d))
}

panel_list <- lapply(service_cols, make_panel)
names(panel_list) <- service_cols

bins <- rbindlist(lapply(panel_list, `[[`, "bins"), fill = TRUE)
bins[, service_lab := factor(service_labels[service], levels = service_labels)]

ns <- sapply(panel_list, `[[`, "n")
cat("\n---- Sample sizes per panel (after 2nd-98th pctile trim) ----\n")
for (s in names(ns)) cat(sprintf("  %-22s n = %d\n", s, ns[s]))

# ---- Plot ----------------------------------------------------------------
x_breaks <- log(c(1/10, 1/3, 1, 3, 10))
x_labels <- c("÷10", "÷3", "1", "×3", "×10")

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50",
             linewidth = 0.4) +
  geom_point(data = bins, aes(x = x, y = y),
             size = 1.1, color = "grey20", alpha = 0.8) +
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

ggsave("output/figures/italy/fig_compliance_gradient_no_rd.pdf", p, width = 9, height = 7)
ggsave("output/figures/italy/fig_compliance_gradient_no_rd.png", p, width = 9, height = 7,
       dpi = 300)
cat("\nSavedoutput/figures/italy/fig_compliance_gradient_no_rd.pdf and .png\n")
