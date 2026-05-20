## Build 2022 municipality-level far-right vote shares
## Input:  data_raw/italy/elections_2022/camera_2022_eligendo.csv  (Eligendo, via ondata)
##         data_raw/italy/cremaschi_replication/map_2008.tab       (Cremaschi et al. crosswalk)
## Output: data_processed/italy/election_2022.csv                  (one row per id08 municipality)
##
## Far-right party classification follows Cremaschi et al. (AJPS 2024),
## who use Chapel Hill Expert Survey scores. For 2022:
##   - FRATELLI D'ITALIA CON GIORGIA MELONI
##   - LEGA PER SALVINI PREMIER
##
## Italian Rosatellum note: each ballot has an uninominal-collegio vote and
## (optionally) a list vote in the plurinominal collegio. Large cities span
## multiple uninominal collegi, so the source CSV reports VOTI LISTE separately
## per (commune, uninominal collegio, list). We therefore dedupe to that grain
## before summing list votes to the commune level.

library(data.table)

dir.create("data_processed/italy", showWarnings = FALSE, recursive = TRUE)

# ---- Load 2022 election results ----
e <- fread("data_raw/italy/elections_2022/camera_2022_eligendo.csv")
cat(sprintf("Raw 2022 data: %d rows, %d municipalities\n",
            nrow(e), uniqueN(e$`CODICE ISTAT`)))

# ---- Aggregate list votes to commune level ----
# Far-right lists
farright_lists <- c("FRATELLI D'ITALIA CON GIORGIA MELONI",
                    "LEGA PER SALVINI PREMIER")

# Each row is (commune, uninominal collegio, list, candidate). VOTI LISTE is
# repeated across candidate rows but DIFFERS across uninominal collegi within a
# split commune (Roma, Milano, Torino, Napoli, Genova, Palermo, etc.).
# Dedupe to the (commune, uninominal, list) grain, then sum to commune level.
list_votes <- unique(e[, .(`CODICE ISTAT`, `COLLEGIO UNINOMINALE`,
                           LISTA, `VOTI LISTE`)])
list_votes <- list_votes[, .(voti_lista = sum(`VOTI LISTE`)),
                         by = .(`CODICE ISTAT`, LISTA)]

# Commune-level totals: list votes act as the denominator for list shares.
totals <- list_votes[, .(voti_validi = sum(voti_lista)), by = `CODICE ISTAT`]
fr_votes <- list_votes[LISTA %in% farright_lists,
                       .(farright_votes = sum(voti_lista)), by = `CODICE ISTAT`]
lega_votes <- list_votes[LISTA == "LEGA PER SALVINI PREMIER",
                         .(lega_votes = sum(voti_lista)), by = `CODICE ISTAT`]
fdi_votes <- list_votes[LISTA == "FRATELLI D'ITALIA CON GIORGIA MELONI",
                        .(fdi_votes = sum(voti_lista)), by = `CODICE ISTAT`]

# Municipality totals (votanti, elettori, schede): repeated per row, but DIFFER
# across uninominal collegi within a split commune. Dedupe at (commune,
# uninominal) before summing.
muni_totals <- unique(e[, .(`CODICE ISTAT`, COMUNE, `COLLEGIO UNINOMINALE`,
                            `VOTANTI TOTALI`, `ELETTORI TOTALI`,
                            `SCHEDE BIANCHE`, `SCHEDE NULLE`,
                            `SCHEDE CONTESTATE`)])
muni <- muni_totals[, .(
  votanti_totali    = sum(`VOTANTI TOTALI`),
  elettori_totali   = sum(`ELETTORI TOTALI`),
  schede_bianche    = sum(`SCHEDE BIANCHE`),
  schede_nulle      = sum(`SCHEDE NULLE`),
  schede_contestate = sum(`SCHEDE CONTESTATE`)
), by = .(`CODICE ISTAT`, COMUNE)]

# Merge commune-level totals
result <- merge(muni, totals, by = "CODICE ISTAT")
result <- merge(result, fr_votes,   by = "CODICE ISTAT", all.x = TRUE)
result <- merge(result, lega_votes, by = "CODICE ISTAT", all.x = TRUE)
result <- merge(result, fdi_votes,  by = "CODICE ISTAT", all.x = TRUE)

# Fill NAs with 0 (a commune where no far-right list ran is treated as zero
# votes, not missing).
result[is.na(farright_votes), farright_votes := 0]
result[is.na(lega_votes),     lega_votes     := 0]
result[is.na(fdi_votes),      fdi_votes      := 0]

result[, farright_sh := farright_votes / voti_validi]
result[, lega_sh     := lega_votes     / voti_validi]
result[, fdi_sh      := fdi_votes      / voti_validi]

cat(sprintf("2022 municipality results: %d municipalities\n", nrow(result)))
cat(sprintf("National far-right share: %.2f%% (FdI: %.2f%%, Lega: %.2f%%)\n",
            100 * sum(result$farright_votes) / sum(result$voti_validi),
            100 * sum(result$fdi_votes)      / sum(result$voti_validi),
            100 * sum(result$lega_votes)     / sum(result$voti_validi)))

# ---- Load id08 crosswalk and merge ----
# map_2008.tab has PRO_COM (numeric ISTAT) and id08. CODICE ISTAT is parsed as
# integer by fread (leading zero lost), so zero-pad both sides for the merge.
xwalk <- fread("data_raw/italy/cremaschi_replication/map_2008.tab",
               select = c("id08", "municipality", "PRO_COM"))
xwalk[, municipality := gsub('"', '', municipality)]
xwalk[, istat_code := sprintf("%06d", PRO_COM)]

result[, istat_code := sprintf("%06d", `CODICE ISTAT`)]

matched <- merge(xwalk,
                 result[, .(istat_code, farright_sh, lega_sh, fdi_sh,
                            farright_votes, voti_validi,
                            votanti_totali, elettori_totali)],
                 by = "istat_code", all.x = TRUE)

n_matched <- sum(!is.na(matched$farright_sh))
n_missing <- sum(is.na(matched$farright_sh))
cat(sprintf("Crosswalk: %d id08 entries; ISTAT-matched %d (missing %d, e.g. merged communes)\n",
            nrow(xwalk), n_matched, n_missing))

# ---- Save ----
out <- matched[!is.na(farright_sh),
               .(id08, municipality, farright_sh, lega_sh, fdi_sh,
                 farright_votes, voti_validi,
                 votanti_totali, elettori_totali)]
fwrite(out, "data_processed/italy/election_2022.csv")
cat(sprintf("Saved data_processed/italy/election_2022.csv: %d municipalities\n", nrow(out)))

# ---- Validation ----
cat(sprintf("\nValidation:\n"))
cat(sprintf("  Mean farright_sh:   %.3f\n", mean(out$farright_sh)))
cat(sprintf("  Median farright_sh: %.3f\n", median(out$farright_sh)))
cat(sprintf("  Range:              [%.3f, %.3f]\n",
            min(out$farright_sh), max(out$farright_sh)))
