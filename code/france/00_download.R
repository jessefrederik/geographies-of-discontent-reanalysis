################################################################################
# 00_download.R — Download raw data for France placebo analysis
#
# Sources:
#   1. data.gouv.fr aggregated elections (Parquet) — presidential 1st rounds
#   2. INSEE COG historique — commune events/movements since 1943
#   3. INSEE populations légales — commune population 1968–2023
#
# All files are saved to data_raw/france/. Downloads are skipped if files
# already exist, so re-runs are cheap.
################################################################################

cat("====================================================================\n")
cat("00_download.R — Downloading raw data\n")
cat("====================================================================\n\n")

raw_dir <- "data_raw/france"

# Helper: download only if file does not already exist
safe_download <- function(url, destfile, description = "") {
  if (file.exists(destfile)) {
    cat(sprintf("  [skip] %s already exists\n", basename(destfile)))
    return(invisible(NULL))
  }
  # Ensure the destination directory exists. download.file does NOT create
  # parent directories and silently fails (with a warning) if they're missing.
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("  [download] %s ...\n", description))
  tryCatch(
    download.file(url, destfile, mode = "wb", quiet = TRUE),
    error = function(e) {
      cat(sprintf("  [ERROR] Failed to download %s: %s\n", description, e$message))
      cat(sprintf("          URL: %s\n", url))
      cat("          Please download manually and place in:", destfile, "\n")
    }
  )
}

# --------------------------------------------------------------------------
# 1. Election data: aggregated Parquet files from data.gouv.fr
#    Contains ALL French elections 1999–2026 at bureau-de-vote level.
#    We filter for presidential first rounds in 01_parse_elections.R.
# --------------------------------------------------------------------------
cat("Election data (data.gouv.fr aggregated Parquet):\n")

safe_download(
  url = "https://object.files.data.gouv.fr/data-pipeline-open/elections/candidats_results.parquet",
  destfile = file.path(raw_dir, "elections", "candidats_results.parquet"),
  description = "candidats_results.parquet (~154 MB)"
)

safe_download(
  url = "https://object.files.data.gouv.fr/data-pipeline-open/elections/general_results.parquet",
  destfile = file.path(raw_dir, "elections", "general_results.parquet"),
  description = "general_results.parquet (~68 MB)"
)

# --------------------------------------------------------------------------
# 2. INSEE COG (Code Officiel Géographique)
#    Commune events table: tracks mergers, splits, code changes since 1943.
#    Current commune list: reference geography (2026 vintage).
# --------------------------------------------------------------------------
cat("\nINSEE COG (commune geography):\n")

safe_download(
  url = "https://www.insee.fr/fr/statistiques/fichier/8740222/v_mvt_commune_2026.csv",
  destfile = file.path(raw_dir, "cog", "v_mvt_commune_2026.csv"),
  description = "COG commune movements (mergers/splits since 1943)"
)

safe_download(
  url = "https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_2026.csv",
  destfile = file.path(raw_dir, "cog", "v_commune_2026.csv"),
  description = "COG current commune list (2026 vintage)"
)

# --------------------------------------------------------------------------
# 3. INSEE population data
#    Reference populations by commune from 1968 to 2023, in a single Excel
#    workbook. We use the 2006 census population as the fixed baseline.
# --------------------------------------------------------------------------
cat("\nINSEE population data:\n")

safe_download(
  url = "https://www.insee.fr/fr/statistiques/fichier/2522602/fichier_pop_reference_6823.xlsx",
  destfile = file.path(raw_dir, "population", "pop_reference_1968_2023.xlsx"),
  description = "Populations légales 1968–2023 (~16 MB)"
)

# --------------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------------
cat("\n--------------------------------------------------------------------\n")
cat("Download summary:\n")

files <- list(
  c("elections/candidats_results.parquet", "Candidate votes by bureau de vote"),
  c("elections/general_results.parquet",   "Participation / blank / null votes"),
  c("cog/v_mvt_commune_2026.csv",         "Commune events since 1943"),
  c("cog/v_commune_2026.csv",             "Current commune list"),
  c("population/pop_reference_1968_2023.xlsx", "Commune populations 1968–2023")
)

for (f in files) {
  path <- file.path(raw_dir, f[1])
  status <- if (file.exists(path)) "OK" else "MISSING"
  size <- if (file.exists(path)) {
    sprintf("%.1f MB", file.info(path)$size / 1e6)
  } else {
    "—"
  }
  cat(sprintf("  [%s] %-45s %8s  %s\n", status, f[1], size, f[2]))
}

cat("\nRequired R packages for the pipeline:\n")
cat("  data.table, fixest, ggplot2, scales, arrow, readxl, MatchIt\n")
cat("====================================================================\n\n")
