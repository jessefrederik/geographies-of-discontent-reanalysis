################################################################################
# DID-style plot of union-formation rates over time, sub-threshold vs
# above-threshold municipalities.
#
# Question: did the 2010 reform produce a visible divergence in unione
# formation between Cremaschi-treated and Cremaschi-control municipalities?
#
# Outcome: share of municipalities in any active unione di comuni as of year y
# (formed on or before y, observed active in the 2020 registry).
# Caveat: the 2020 registry contains only surviving unioni. Pre-2010 unioni
# that dissolved before 2020 are missing — this biases the cumulative-share
# series toward more recent formations.
#
# Source: data_raw/italy/ministero_interno/unioni_comuni_ministero_2020.csv.
#
# Outputs:
#  output/figures/italy/fig_union_formation_did.{pdf,png}
#   output/csvs/italy/union_formation_by_year.csv  (the underlying data)
################################################################################

library(data.table)
library(haven)
library(ggplot2)
library(scales)
library(stringi)

# ---- Panel + Cremaschi-treated indicator --------------------------------
panel <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
panel08 <- unique(panel[year == 2008,
                        .(id08, municipality, pop_tot_2008, mont_group)])
panel08[, treated := as.integer(
  (mont_group == 1 & pop_tot_2008 < 3000) |
  (mont_group == 0 & pop_tot_2008 < 5000)
)]

# ---- Raw union registry: forward-fill formation dates onto member rows --
norm <- function(x) {
  x <- toupper(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("'", "", x, fixed = TRUE)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\bS\\b\\.?",  "SAN",   x)
  x <- gsub("\\bSS\\b\\.?", "SANTI", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
raw <- fread("data_raw/italy/ministero_interno/unioni_comuni_ministero_2020.csv",
             sep = ";", encoding = "UTF-8", colClasses = "character",
             na.strings = "")
setnames(raw, c("nr","codice","regione","provincia","descrizione",
                "data_cost","nr_comuni","comune","pop_m","pop_f","pop_tot"))

# Forward-fill the union founding date from header rows down to member rows
header_idx <- cummax(ifelse(raw$nr == "0", seq_len(nrow(raw)), 0L))
raw[, data_cost_filled := raw$data_cost[header_idx]]

# Parse Italian-month dates (e.g., "12-Feb-2013")
it_months <- c(Gen=1, Feb=2, Mar=3, Apr=4, Mag=5, Giu=6,
               Lug=7, Ago=8, Set=9, Ott=10, Nov=11, Dic=12)
parse_year <- function(s) {
  parts <- tstrsplit(s, "-", fixed = TRUE)
  as.integer(parts[[3]])
}

members <- raw[nr != "0" & !is.na(comune)]
members[, year_cost := parse_year(data_cost_filled)]
members[, comune_norm := norm(comune)]

# Earliest year each muni joined an active unione (in case a muni appears
# in multiple unioni — rare, but pick the earliest)
muni_join <- members[!is.na(year_cost),
                     .(union_year = min(year_cost)), by = comune_norm]

panel08[, comune_norm := norm(municipality)]
panel08 <- merge(panel08, muni_join, by = "comune_norm", all.x = TRUE)

cat(sprintf("Panel: %d munis (%d treated, %d control); %d ever in a 2020-active unione (%.1f%%)\n",
            nrow(panel08),
            sum(panel08$treated == 1), sum(panel08$treated == 0),
            sum(!is.na(panel08$union_year)),
            100 * mean(!is.na(panel08$union_year))))

# ---- Cumulative share in any active unione, by year and group -----------
years <- 1990:2020
n_treat <- sum(panel08$treated == 1)
n_ctrl  <- sum(panel08$treated == 0)

stock <- rbindlist(lapply(years, function(y) {
  data.table(
    year = y,
    treated   = sum(panel08$treated == 1 & !is.na(panel08$union_year) & panel08$union_year <= y) / n_treat,
    control   = sum(panel08$treated == 0 & !is.na(panel08$union_year) & panel08$union_year <= y) / n_ctrl
  )
}))
stock_long <- melt(stock, id.vars = "year",
                    variable.name = "group", value.name = "share")
stock_long[, group_lab := factor(
  fifelse(group == "treated",
          "Sub-threshold (Cremaschi-treated)",
          "Above-threshold (control)"),
  levels = c("Sub-threshold (Cremaschi-treated)",
             "Above-threshold (control)")
)]

# ---- Yearly NEW-formation rate (flow), by group -------------------------
flow <- rbindlist(lapply(years, function(y) {
  data.table(
    year = y,
    treated = sum(panel08$treated == 1 & panel08$union_year == y, na.rm = TRUE) / n_treat,
    control = sum(panel08$treated == 0 & panel08$union_year == y, na.rm = TRUE) / n_ctrl
  )
}))
flow_long <- melt(flow, id.vars = "year",
                   variable.name = "group", value.name = "share")
flow_long[, group_lab := factor(
  fifelse(group == "treated",
          "Sub-threshold (Cremaschi-treated)",
          "Above-threshold (control)"),
  levels = c("Sub-threshold (Cremaschi-treated)",
             "Above-threshold (control)")
)]

# ---- DID arithmetic: stock change 2009 -> 2018 -------------------------
s_2009 <- stock[year == 2009]
s_2018 <- stock[year == 2018]
delta_treated <- s_2018$treated - s_2009$treated
delta_control <- s_2018$control - s_2009$control
did_estimate  <- delta_treated - delta_control
cat("\n---- DID arithmetic: stock change 2009 -> 2018 ----\n")
cat(sprintf("  Sub-threshold:    %.3f -> %.3f  (Delta = %+.3f)\n",
            s_2009$treated, s_2018$treated, delta_treated))
cat(sprintf("  Above-threshold:  %.3f -> %.3f  (Delta = %+.3f)\n",
            s_2009$control, s_2018$control, delta_control))
cat(sprintf("  DID = (Sub Delta) - (Above Delta) = %+.3f\n", did_estimate))

# ---- Plot ---------------------------------------------------------------
p_stock <- ggplot(stock_long, aes(x = year, y = share,
                                   color = group_lab,
                                   linetype = group_lab)) +
  annotate("rect", xmin = 2010, xmax = 2020, ymin = -Inf, ymax = Inf,
           fill = "grey92", alpha = 0.5) +
  geom_vline(xintercept = 2010, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.3) +
  scale_color_manual(values = c("grey15", "grey55"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 0.5, by = 0.1)) +
  annotate("text", x = 2010.3, y = 0.45,
           label = "2010 reform", hjust = 0, size = 3,
           color = "grey30") +
  labs(x = "Year", y = "Share in any active unione",
       title = "Cumulative union membership, by Cremaschi-treated status") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.25),
    legend.position    = "bottom",
    legend.box.margin  = margin(-5, 0, 0, 0),
    plot.title         = element_text(size = 10, face = "bold"),
    axis.title         = element_text(size = 10),
    axis.text          = element_text(size = 9)
  )

p_flow <- ggplot(flow_long, aes(x = year, y = share,
                                 color = group_lab,
                                 linetype = group_lab)) +
  annotate("rect", xmin = 2010, xmax = 2020, ymin = -Inf, ymax = Inf,
           fill = "grey92", alpha = 0.5) +
  geom_vline(xintercept = 2010, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.0) +
  scale_color_manual(values = c("grey15", "grey55"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(x = "Year",
       y = "Share forming a new unione in year y",
       title = "Annual new-formation rate") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.25),
    legend.position    = "none",
    plot.title         = element_text(size = 10, face = "bold"),
    axis.title         = element_text(size = 10),
    axis.text          = element_text(size = 9)
  )

# Stack vertically: cumulative on top, annual flow below
library(patchwork)
p_combined <- p_stock / p_flow +
  plot_layout(heights = c(1.2, 1))

ggsave("output/figures/italy/fig_union_formation_did.pdf", p_combined,
       width = 7, height = 6)
ggsave("output/figures/italy/fig_union_formation_did.png", p_combined,
       width = 7, height = 6, dpi = 300)
fwrite(stock, "output/csvs/italy/union_formation_by_year.csv")
cat("\nSavedoutput/figures/italy/fig_union_formation_did.{pdf,png}\n")
cat("Saved output/csvs/italy/union_formation_by_year.csv\n")
