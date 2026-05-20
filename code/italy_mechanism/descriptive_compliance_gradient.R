################################################################################
# Descriptive evidence: do sub-threshold municipalities use more shared service
# delivery than above-threshold municipalities? Compares unione membership and
# OpenCivitas fundamental-function counts across population strata.
#
# Scope: descriptive only — no causal estimates.
#
# Outputs:
#   output/csvs/italy/desc_compliance_by_stratum.csv  — share of munis in any unione,
#                                            in post-2010 unione, and OC mean
#                                            fundamental-function count, by
#                                            (mountain status × pop bin).
################################################################################

library(data.table)
library(haven)
library(stringi)

# ---- Load panel + union flag ---------------------------------------------
panel <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
panel08 <- unique(panel[year == 2008,
                        .(id08, municipality, pop_tot_2008, mont_group)])

u <- fread("data_processed/italy/post2010_union_indicator.csv")
panel08 <- merge(panel08, u[, .(id08, in_post2010_union)], by = "id08",
                 all.x = TRUE)

# ---- Compute "in any active unione" from the raw 2020 registry ----------
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
members <- raw[nr != "0" & !is.na(comune)]
members[, comune_norm := norm(comune)]
in_any_union <- unique(members$comune_norm)

panel08[, comune_norm := norm(municipality)]
panel08[, in_any_union := as.integer(comune_norm %in% in_any_union)]

# Note: the per-function OpenCivitas count (count_fund 0-8) is constructed
# in 05_att_count_fundamental.R from twelve sub-flags across five OC files.
# Reproducing that here would duplicate ~100 lines; this descriptive script
# uses the unione registry only. The OpenCivitas evidence on differential
# compliance is summarised in Appendix C from output/csvs/italy/att_count_fundamental.csv.

# ---- Stratify by mountain x sub/above threshold -------------------------
panel08[, stratum := fifelse(
  mont_group == 1 & pop_tot_2008 < 3000, "Mountain, pop < 3,000 (treated)",
  fifelse(mont_group == 0 & pop_tot_2008 < 5000, "Non-mountain, pop < 5,000 (treated)",
  fifelse(mont_group == 1 & pop_tot_2008 >= 3000, "Mountain, pop >= 3,000 (control)",
                                                  "Non-mountain, pop >= 5,000 (control)"))
)]

summary_tab <- panel08[, .(
  n_munis           = .N,
  share_in_union    = mean(in_any_union, na.rm = TRUE),
  share_post2010    = mean(in_post2010_union, na.rm = TRUE)
), by = stratum][order(stratum)]

cat("\n---- Service-sharing by population stratum (2008 census, 2020 registry) ----\n")
print(summary_tab)

# Also: pooled treated vs pooled control
pooled <- panel08[, .(
  group = fifelse((mont_group == 1 & pop_tot_2008 < 3000) |
                  (mont_group == 0 & pop_tot_2008 < 5000),
                  "Treated (Cremaschi)", "Control (above threshold)"),
  in_any_union, in_post2010_union
)][, .(
  n_munis           = .N,
  share_in_union    = mean(in_any_union, na.rm = TRUE),
  share_post2010    = mean(in_post2010_union, na.rm = TRUE)
), by = group][order(group)]

cat("\n---- Pooled comparison ----\n")
print(pooled)

# Population bins for a finer cut
panel08[, pop_bin := cut(pop_tot_2008,
  breaks = c(0, 1000, 2000, 3000, 5000, 10000, 50000, Inf),
  labels = c("<1k","1-2k","2-3k","3-5k","5-10k","10-50k","50k+"))]

bin_tab <- panel08[, .(
  n_munis        = .N,
  share_in_union = mean(in_any_union, na.rm = TRUE),
  share_post2010 = mean(in_post2010_union, na.rm = TRUE)
), by = pop_bin][order(pop_bin)]

cat("\n---- By population bin (pooling mountain status) ----\n")
print(bin_tab)

# ---- Write CSV output ---------------------------------------------------
fwrite(summary_tab, "output/csvs/italy/desc_compliance_by_stratum.csv")
fwrite(bin_tab, "output/csvs/italy/desc_compliance_by_popbin.csv")
cat("\nWritten: output/csvs/italy/desc_compliance_by_stratum.csv,\n",
    "         output/csvs/italy/desc_compliance_by_popbin.csv\n")
