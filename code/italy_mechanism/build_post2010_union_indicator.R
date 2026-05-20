################################################################################
# Build data/post2010_union_indicator.csv from the Ministry of Interior
# unioni_comuni registry (2020 vintage).
#
# Source: data_raw/italy/ministero_interno/unioni_comuni_ministero_2020.csv
#   (downloaded from finanzalocale.interno.gov.it)
#
# Raw structure: hierarchical, semicolon-separated. One header row per
# unione (NR. == 0) carrying CODICE / DESCRIZIONE / DATA COSTITUZIONE,
# followed by one row per member muni (NR. > 0) with only the muni name
# in COMUNI APPARTENENTI ALL'UNIONE. Dates: Italian abbreviated months,
# e.g. "12-Feb-2013", "29-Dic-2011".
#
# Output schema matches the previous static artifact so downstream
# scripts (06_att_post2010_union.R) keep working unchanged:
#   id08, municipality, in_post2010_union
#
# CAVEAT (already documented in 06_att_post2010_union.R): the Ministry
# registry only lists CURRENTLY-ACTIVE unions. Pre-2010 unions that
# dissolved before 2020 disappear from the data; post-2010 unions that
# formed and dissolved within 2011-2019 also disappear. The 2020 vintage
# captures fewer post-2010 formations than later vintages.
################################################################################

library(data.table)
library(haven)
library(stringi)

RAW_PATH <- "data_raw/italy/ministero_interno/unioni_comuni_ministero_2020.csv"
PANEL_PATH <- "data_raw/italy/electoral_panel_dataset.dta"
OUT_PATH   <- "data_processed/italy/post2010_union_indicator.csv"

# ---- Load raw registry -----------------------------------------------------
raw <- fread(RAW_PATH, sep = ";", encoding = "UTF-8",
             colClasses = "character", na.strings = "")
setnames(raw, c("nr","codice","regione","provincia","descrizione",
                "data_cost","nr_comuni","comune","pop_m","pop_f","pop_tot"))

cat(sprintf("Raw rows: %d  (header rows: %d, member rows: %d)\n",
            nrow(raw),
            sum(raw$nr == "0"),
            sum(raw$nr != "0")))

# ---- Forward-fill the union founding date down to member rows --------------
# Header rows (nr == "0") have data_cost populated; member rows inherit it.
# Carry the most-recent header-row value down by indexing on cummax of
# header-row positions.
header_idx <- cummax(ifelse(raw$nr == "0", seq_len(nrow(raw)), 0L))
raw[, data_cost_filled := raw$data_cost[header_idx]]
raw[, codice_filled    := raw$codice[header_idx]]

members <- raw[nr != "0" & !is.na(comune)]

# ---- Parse Italian dates ---------------------------------------------------
it_months <- c(Gen=1, Feb=2, Mar=3, Apr=4, Mag=5, Giu=6,
               Lug=7, Ago=8, Set=9, Ott=10, Nov=11, Dic=12)
parse_year <- function(s) {
  parts <- tstrsplit(s, "-", fixed = TRUE)
  as.integer(parts[[3]])
}
parse_month <- function(s) {
  parts <- tstrsplit(s, "-", fixed = TRUE)
  unname(it_months[parts[[2]]])
}
members[, year_cost  := parse_year(data_cost_filled)]
members[, month_cost := parse_month(data_cost_filled)]
stopifnot(all(!is.na(members$year_cost)))
stopifnot(all(!is.na(members$month_cost)))

cat(sprintf("Member munis: %d  | unique unions: %d  | year range: %d-%d\n",
            nrow(members),
            uniqueN(members$codice_filled),
            min(members$year_cost), max(members$year_cost)))

# ----------------------------------------------------------------------------
# DECISION POINT — define what counts as a "post-2010" union.
#
# Art. 14 D.L. 78/2010 was published 31-May-2010 and converted into law
# 30-Jul-2010. The mandate took political force in mid-2010. Two reasonable
# operational definitions of "mandate-induced unione":
#
#   (a) STRICT       year_cost >  2010   — only 2011+ formations.
#                    Cleanest "after the mandate" cut. Misses second-half-2010
#                    formations that may already be mandate responses.
#
#   (b) INCLUSIVE    year_cost >= 2010   — includes 2010 itself.
#                    Captures late-2010 mandate responses but also any
#                    early-2010 formations that pre-dated the law.
#
#   (b') MID-YEAR    (year_cost == 2010 & month_cost >= 6) | year_cost > 2010
#                    Hybrid: include 2010 only from June onward.
#
# Cremaschi et al. use the 2010 mandate as the treatment shock, so the
# control group should be "munis whose union membership is NOT explained
# by the mandate." Anything that would assign 1 to a pre-mandate union
# muni biases the test against finding a mandate effect.
#
# TODO: implement is_post2010_union(year_cost, month_cost) below.
# Returns a logical vector. ~5 lines.
# ----------------------------------------------------------------------------
is_post2010_union <- function(year_cost, month_cost) {
  # Mid-year cut: D.L. 78/2010 published 31-May-2010, law 30-Jul-2010.
  # Treat unions founded from June 2010 onward as mandate-induced.
  (year_cost == 2010 & month_cost >= 6) | year_cost > 2010
}

members[, post2010 := is_post2010_union(year_cost, month_cost)]
cat(sprintf("Post-2010 union memberships: %d / %d (%.1f%%)\n",
            sum(members$post2010), nrow(members),
            100 * mean(members$post2010)))

# ---- Normalize muni names (mirrors itt_att/00_build_crosswalk.R) -----------
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
members[, comune_norm := norm(comune)]

# Collapse to muni level: a muni is in_post2010_union if ANY of its current
# union memberships is post-2010 (in practice each muni belongs to <=1 unione
# at a time; this guards against duplicate listings).
muni_flag <- members[, .(in_post2010_union = as.integer(any(post2010))),
                     by = comune_norm]

# ---- Join to panel ---------------------------------------------------------
panel <- as.data.table(read_dta(PANEL_PATH))
panel08 <- unique(panel[year == 2008, .(id08, municipality)])
panel08[, comune_norm := norm(municipality)]

out <- merge(panel08, muni_flag, by = "comune_norm", all.x = TRUE)
out[is.na(in_post2010_union), in_post2010_union := 0L]

# Provenance / sanity checks --------------------------------------------------
matched_munis <- sum(out$in_post2010_union == 1)
ministry_post2010_munis <- uniqueN(members[post2010 == TRUE]$comune_norm)
cat(sprintf("\nPanel munis (2008 cross-section): %d\n", nrow(out)))
cat(sprintf("  flagged in_post2010_union = 1: %d (%.1f%%)\n",
            matched_munis, 100 * matched_munis / nrow(out)))
cat(sprintf("Ministry post-2010 munis (unique by normalized name): %d\n",
            ministry_post2010_munis))
cat(sprintf("Unmatched ministry post-2010 munis: %d\n",
            ministry_post2010_munis - matched_munis))

setcolorder(out, c("id08", "municipality", "in_post2010_union"))
fwrite(out[, .(id08, municipality, in_post2010_union)], OUT_PATH)
cat(sprintf("\nWritten: %s\n", OUT_PATH))
