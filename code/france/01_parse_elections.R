################################################################################
# 01_parse_elections.R — Parse presidential first-round election data
#
# Reads the data.gouv.fr aggregated Parquet files, filters to presidential
# first rounds (2002, 2007, 2012, 2017, 2022), aggregates bureau-de-vote
# results to commune level, and computes far-right vote share.
#
# Output: data_processed/france/intermediate/elections_commune.csv
################################################################################

cat("====================================================================\n")
cat("01_parse_elections.R — Parsing election data\n")
cat("====================================================================\n\n")

library(data.table)
library(arrow)

outfile <- "data_processed/france/intermediate/elections_commune.csv"

if (file.exists(outfile)) {
  cat(sprintf("[skip] %s already exists. Delete to regenerate.\n\n", outfile))
} else {

# --------------------------------------------------------------------------
# Far-right candidate classification (narrow definition)
# --------------------------------------------------------------------------
farright <- data.table(
  id_election = c("2002_pres_t1", "2002_pres_t1",
                   "2007_pres_t1",
                   "2012_pres_t1",
                   "2017_pres_t1",
                   "2022_pres_t1", "2022_pres_t1"),
  nom_match   = c("LE PEN", "MEGRET",
                   "LE PEN",
                   "LE PEN",
                   "LE PEN",
                   "LE PEN", "ZEMMOUR"),
  party       = c("FN", "MNR", "FN", "FN", "FN", "RN", "Reconquête")
)
cat("Far-right candidates:\n")
print(farright[, .(id_election, nom_match, party)])

# --------------------------------------------------------------------------
# Read candidate-level results
# --------------------------------------------------------------------------
cat("\nReading candidats_results.parquet...\n")
cand <- as.data.table(read_parquet(
  "data_raw/france/elections/candidats_results.parquet",
  col_select = c("id_election", "code_departement", "code_commune",
                 "code_bv", "nom", "prenom", "voix")
))

# Filter to presidential first rounds
pres_ids <- c("2002_pres_t1", "2007_pres_t1", "2012_pres_t1",
              "2017_pres_t1", "2022_pres_t1")
cand <- cand[id_election %in% pres_ids]
cat(sprintf("Presidential T1 rows: %s\n", format(nrow(cand), big.mark = ",")))

# --------------------------------------------------------------------------
# Filter to metropolitan France
# Exclude overseas: Z* prefix codes (ZA, ZB, ZC, ZD, ZM, ZN, ZP, ZS, ZW, ZX, ZZ)
# --------------------------------------------------------------------------
cand <- cand[!grepl("^Z", code_departement)]
cat(sprintf("After metropolitan filter: %s rows\n", format(nrow(cand), big.mark = ",")))

# --------------------------------------------------------------------------
# Read general results (inscrits, votants, exprimes) at bureau level
# --------------------------------------------------------------------------
cat("\nReading general_results.parquet...\n")
gen <- as.data.table(read_parquet(
  "data_raw/france/elections/general_results.parquet",
  col_select = c("id_election", "code_departement", "code_commune",
                 "code_bv", "inscrits", "votants", "exprimes")
))
gen <- gen[id_election %in% pres_ids & !grepl("^Z", code_departement)]

# --------------------------------------------------------------------------
# Mark far-right candidates
# --------------------------------------------------------------------------
cand[, nom_upper := toupper(trimws(nom))]

# Handle accent variations: MEGRET vs MÉGRET
cand[, nom_ascii := gsub("\u00e9|\u00e8|\u00ea|\u00eb", "E",
                    gsub("\u00c9|\u00c8|\u00ca|\u00cb", "E", nom_upper))]

cand[, is_farright := 0L]
for (i in seq_len(nrow(farright))) {
  cand[id_election == farright$id_election[i] &
       nom_ascii == farright$nom_match[i],
       is_farright := 1L]
}

# Validate: total far-right votes per year
cat("\nFar-right candidate votes (national totals):\n")
fr_check <- cand[is_farright == 1, .(votes = sum(voix, na.rm = TRUE)),
                 by = .(id_election, nom, prenom)]
print(fr_check[order(id_election, -votes)])

# --------------------------------------------------------------------------
# Aggregate to commune level
# --------------------------------------------------------------------------
cat("\nAggregating bureau -> commune ...\n")

# Candidate votes at commune level
comm_votes <- cand[, .(farright_votes = sum(voix[is_farright == 1], na.rm = TRUE),
                       total_votes_cand = sum(voix, na.rm = TRUE)),
                   by = .(id_election, code_commune)]

# General results at commune level
comm_gen <- gen[, .(inscrits = sum(inscrits, na.rm = TRUE),
                    votants = sum(votants, na.rm = TRUE),
                    exprimes = sum(exprimes, na.rm = TRUE)),
                by = .(id_election, code_commune)]

# Merge
comm <- merge(comm_votes, comm_gen, by = c("id_election", "code_commune"), all = TRUE)

# Compute far-right share
comm[, farright_sh := farright_votes / exprimes]

# Extract year
comm[, year := as.integer(substr(id_election, 1, 4))]

# --------------------------------------------------------------------------
# Handle Paris / Lyon / Marseille arrondissements
# Paris: 75101-75120, city code 75056
# Lyon: 69381-69389, city code 69123
# Marseille: 13201-13216, city code 13055
# --------------------------------------------------------------------------
cat("Aggregating Paris/Lyon/Marseille arrondissements to city level...\n")

plm_map <- function(code) {
  ifelse(grepl("^751[0-2][0-9]$", code), "75056",
  ifelse(grepl("^6938[1-9]$", code), "69123",
  ifelse(grepl("^132[0-1][0-9]$", code), "13055",
         code)))
}
comm[, code_commune := plm_map(code_commune)]

# Re-aggregate after PLM mapping
comm <- comm[, .(inscrits = sum(inscrits, na.rm = TRUE),
                 votants = sum(votants, na.rm = TRUE),
                 exprimes = sum(exprimes, na.rm = TRUE),
                 farright_votes = sum(farright_votes, na.rm = TRUE)),
             by = .(year, code_commune)]
comm[, farright_sh := farright_votes / exprimes]

# --------------------------------------------------------------------------
# Validation
# --------------------------------------------------------------------------
cat("\nValidation — commune-level far-right share (weighted national mean):\n")
nat <- comm[, .(fr_share = sum(farright_votes) / sum(exprimes),
                n_communes = .N),
            by = year]
for (i in seq_len(nrow(nat))) {
  cat(sprintf("  %d: %.2f%% (%d communes)\n",
              nat$year[i], nat$fr_share[i] * 100, nat$n_communes[i]))
}

# Expected approximate values:
# 2002: ~19.2% (Le Pen + Mégret)
# 2007: ~10.4%
# 2012: ~17.9%
# 2017: ~21.3%
# 2022: ~30.2% (Le Pen + Zemmour)

# --------------------------------------------------------------------------
# Save
# --------------------------------------------------------------------------
comm <- comm[order(code_commune, year)]
fwrite(comm, outfile)
cat(sprintf("\nSaved %s (%s rows)\n", outfile,
            format(nrow(comm), big.mark = ",")))

} # end if file exists check

cat("====================================================================\n\n")
