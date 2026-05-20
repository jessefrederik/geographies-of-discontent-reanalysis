################################################################################
# Build augmented panel <-> OpenCivitas USERNAME crosswalk.
#
# Recovery passes (conservative — same legal entity over time, no fusioni):
#   1. Direct match on (muni_clean, cod_prov)
#   2. Name normalization (accents, apostrophes, hyphens, S./SAN)
#   3. Strip 2-letter province suffix glued onto homonym disambiguation
#      (e.g., LIVOCO -> LIVO in prov 13, PEGLIOPU -> PEGLIO in prov 41)
#   4. Cross-province match for unique-nationally names (handles 2009/2021
#      Marche -> Rimini referendum transfers)
#
# Output: data/opencivitas_panel_crosswalk.csv with columns:
#   id08, USERNAME, match_method
################################################################################

library(data.table)
library(haven)
library(readxl)
library(stringi)

# ---- Load panel and meta ---------------------------------------------------
d <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
d08 <- unique(d[year == 2008,
                .(id08, municipality, cod_prov, region, pop_tot_2008, mont_group)])
d08[, muni_clean := toupper(trimws(municipality))]

meta22 <- as.data.table(read_excel("data_raw/italy/opencivitas/Metadati_Enti_2022.xlsx"))
meta22[, muni_clean := toupper(trimws(ENTE))]
meta22[, prov_code := as.integer(PROVINCIA_ISTAT_COD)]

cat(sprintf("Panel munis (2008): %d | OpenCivitas entities (2022): %d\n",
            nrow(d08), nrow(meta22)))

# ---- Pass 1: direct match --------------------------------------------------
m1 <- merge(
  d08[, .(id08, muni_clean, cod_prov, pop_tot_2008, mont_group, region)],
  meta22[, .(muni_clean, prov_code, USERNAME)],
  by.x = c("muni_clean", "cod_prov"),
  by.y = c("muni_clean", "prov_code")
)
m1[, match_method := "direct"]
cat(sprintf("Pass 1 (direct):       %d\n", nrow(m1)))

unmatched <- d08[!(id08 %in% m1$id08)]

# ---- Pass 2: normalization -------------------------------------------------
norm <- function(x) {
  x <- toupper(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("'", "", x, fixed = TRUE)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\bS\\b\\.?", "SAN", x)
  x <- gsub("\\bSS\\b\\.?", "SANTI", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
d08[, muni_norm := norm(municipality)]
meta22[, muni_norm := norm(ENTE)]

m2 <- merge(
  unmatched[, .(id08, muni_clean, cod_prov, pop_tot_2008, mont_group, region,
                muni_norm = norm(municipality))],
  meta22[, .(muni_norm, prov_code, USERNAME)],
  by.x = c("muni_norm", "cod_prov"),
  by.y = c("muni_norm", "prov_code")
)
m2 <- m2[, .(muni_clean, cod_prov, id08, pop_tot_2008, mont_group, region, USERNAME)]
m2[, match_method := "norm"]
cat(sprintf("Pass 2 (normalize):    +%d\n", nrow(m2)))

unmatched <- unmatched[!(id08 %in% c(m1$id08, m2$id08))]

# ---- Pass 3: suffix-stripping ----------------------------------------------
# Panel sometimes appends 2-letter province abbrev to homonym names.
# Strategy: try the last-2-chars-stripped form against meta22 with same cod_prov.
unmatched[, muni_strip := substr(muni_clean, 1, nchar(muni_clean) - 2)]
m3 <- merge(
  unmatched[, .(id08, muni_clean, cod_prov, pop_tot_2008, mont_group, region, muni_strip)],
  meta22[, .(muni_strip = muni_clean, prov_code, USERNAME)],
  by.x = c("muni_strip", "cod_prov"),
  by.y = c("muni_strip", "prov_code")
)
# Sanity: also try normalized stripped form
unmatched[, muni_strip_norm := norm(muni_strip)]
m3b <- merge(
  unmatched[!(id08 %in% m3$id08), .(id08, muni_clean, cod_prov, pop_tot_2008, mont_group, region, muni_strip_norm)],
  meta22[, .(muni_strip_norm = muni_norm, prov_code, USERNAME)],
  by.x = c("muni_strip_norm", "cod_prov"),
  by.y = c("muni_strip_norm", "prov_code")
)
m3 <- rbind(
  m3[,  .(muni_clean, cod_prov, id08, pop_tot_2008, mont_group, region, USERNAME)],
  m3b[, .(muni_clean, cod_prov, id08, pop_tot_2008, mont_group, region, USERNAME)]
)
m3[, match_method := "suffix_strip"]
cat(sprintf("Pass 3 (suffix strip): +%d\n", nrow(m3)))

unmatched <- unmatched[!(id08 %in% c(m1$id08, m2$id08, m3$id08))]

# ---- Pass 4: cross-province for nationally-unique names --------------------
# Some munis were transferred between provinces (e.g., Milano->Monza 2009,
# Marche->Rimini 2021, Lecco->Bergamo 2018). Match cross-prov ONLY if the
# (normalized) name is unique nationally in meta22.
#
# CRITICAL: restrict to RSO panel munis. RSS munis (Trento, Bolzano, FVG,
# VdA, Sardinia) are not in OpenCivitas at all -- any cross-prov "match" of
# an RSS muni to an RSO USERNAME is a name coincidence (e.g., LIVO-Trento
# spuriously matched to LIVO-Como; SORAGA-Trento matched to a Frosinone code).
rss <- c("SARDEGNA","TRENTINO-ALTO ADIGE","FRIULI-VENEZIA GIULIA","VALLE D'AOSTA")
meta22_unique_names <- meta22[, .N, by = muni_norm][N == 1]$muni_norm
unmatched_norm <- unmatched[!(region %in% rss),
                            .(id08, muni_clean, cod_prov, pop_tot_2008, mont_group, region,
                              muni_norm = norm(muni_clean))]
# Allow strip-then-cross for the rare case of a panel-suffix muni that was
# also transferred provinces (none observed so far, but harmless)
unmatched_norm[, muni_norm_strip := norm(substr(muni_clean, 1, nchar(muni_clean) - 2))]

m4_a <- merge(
  unmatched_norm[muni_norm %in% meta22_unique_names, .(id08, muni_clean, cod_prov, pop_tot_2008, mont_group, region, muni_norm)],
  meta22[muni_norm %in% meta22_unique_names, .(muni_norm, USERNAME, meta_prov = prov_code)],
  by = "muni_norm"
)
m4_a[, match_method := "cross_prov_unique"]

m4_b <- merge(
  unmatched_norm[!(id08 %in% m4_a$id08) & muni_norm_strip %in% meta22_unique_names,
                 .(id08, muni_clean, cod_prov, pop_tot_2008, mont_group, region, muni_norm_strip)],
  meta22[muni_norm %in% meta22_unique_names, .(muni_norm_strip = muni_norm, USERNAME, meta_prov = prov_code)],
  by = "muni_norm_strip"
)
m4_b[, match_method := "cross_prov_unique_strip"]

m4 <- rbind(
  m4_a[, .(muni_clean, cod_prov, id08, pop_tot_2008, mont_group, region, USERNAME, meta_prov, match_method)],
  m4_b[, .(muni_clean, cod_prov, id08, pop_tot_2008, mont_group, region, USERNAME, meta_prov, match_method)]
)

cat(sprintf("Pass 4 (cross-prov):   +%d\n", nrow(m4)))
if (nrow(m4) > 0) {
  cat("\nCross-province recoveries (panel prov vs OpenCivitas prov):\n")
  print(m4[, .(muni_clean, panel_prov = cod_prov, oc_prov = meta_prov, USERNAME, match_method)])
}

# ---- Combine ---------------------------------------------------------------
xw <- rbind(
  m1[, .(id08, USERNAME, match_method)],
  m2[, .(id08, USERNAME, match_method)],
  m3[, .(id08, USERNAME, match_method)],
  m4[, .(id08, USERNAME, match_method)]
)
stopifnot(uniqueN(xw$id08) == nrow(xw))
fwrite(xw, "data_processed/italy/opencivitas_panel_crosswalk.csv")

cat(sprintf("\n========================================\n"))
cat(sprintf("Total panel munis: %d\n", nrow(d08)))
cat(sprintf("  matched (any method): %d (%.1f%%)\n", nrow(xw), 100*nrow(xw)/nrow(d08)))
cat(sprintf("Sub-threshold (3000/5000) panel: %d\n",
    nrow(d08[(mont_group==1 & pop_tot_2008<3000)|(mont_group!=1 & pop_tot_2008<5000)])))
sub_id <- d08[(mont_group==1 & pop_tot_2008<3000)|(mont_group!=1 & pop_tot_2008<5000)]$id08
cat(sprintf("  matched: %d (%.1f%%)\n",
    sum(sub_id %in% xw$id08), 100*sum(sub_id %in% xw$id08)/length(sub_id)))
cat(sprintf("\nCrosswalk written to data/opencivitas_panel_crosswalk.csv\n"))
