################################################################################
# 02_population.R — Parse INSEE population data
#
# Reads the 2006 census population (municipal population) from the INSEE
# reference populations Excel file. This serves as the FIXED baseline
# population for treatment assignment — it does not change over time.
#
# Output: data_processed/france/intermediate/commune_population.csv
################################################################################

cat("====================================================================\n")
cat("02_population.R — Building population variables\n")
cat("====================================================================\n\n")

library(data.table)
library(readxl)

outfile <- "data_processed/france/intermediate/commune_population.csv"

if (file.exists(outfile)) {
  cat(sprintf("[skip] %s already exists. Delete to regenerate.\n\n", outfile))
} else {

# --------------------------------------------------------------------------
# Read 2006 population sheet
# The Excel sheet has 7 header/metadata rows before data starts.
# Row 7 contains column name codes: COM, NCC, PMUN06
# --------------------------------------------------------------------------
cat("Reading 2006 population from INSEE Excel file...\n")

pop_raw <- read_excel(
  "data_raw/france/population/pop_reference_1968_2023.xlsx",
  sheet = "2006",
  skip = 7,           # skip metadata rows; row 8 is first data row
  col_names = FALSE
)

pop <- as.data.table(pop_raw)
setnames(pop, c("code_commune", "libelle", "pop_2006"))

# Clean types
pop[, pop_2006 := as.integer(pop_2006)]
pop <- pop[!is.na(pop_2006) & !is.na(code_commune)]

# --------------------------------------------------------------------------
# Filter to metropolitan France
# Commune codes: 01xxx–95xxx plus 2Axxx, 2Bxxx
# Exclude overseas (97xxx) and special codes
# --------------------------------------------------------------------------
pop <- pop[grepl("^(0[1-9]|[1-8][0-9]|9[0-5]|2[AB])", code_commune)]

cat(sprintf("Metropolitan communes with 2006 population: %d\n", nrow(pop)))
cat(sprintf("Population range: %s to %s (median %s)\n",
            format(min(pop$pop_2006), big.mark = ","),
            format(max(pop$pop_2006), big.mark = ","),
            format(median(pop$pop_2006), big.mark = ",")))

# --------------------------------------------------------------------------
# Handle Paris / Lyon / Marseille arrondissements
# Sum arrondissement populations to city level
# --------------------------------------------------------------------------
plm_map <- function(code) {
  ifelse(grepl("^751[0-2][0-9]$", code), "75056",
  ifelse(grepl("^6938[1-9]$", code), "69123",
  ifelse(grepl("^132[0-1][0-9]$", code), "13055",
         code)))
}
pop[, code_commune := plm_map(code_commune)]
pop <- pop[, .(pop_2006 = sum(pop_2006, na.rm = TRUE)),
           by = code_commune]

cat(sprintf("After PLM aggregation: %d communes\n", nrow(pop)))

# --------------------------------------------------------------------------
# Compute log(population)
# --------------------------------------------------------------------------
pop[, log_pop := log(pop_2006)]

# --------------------------------------------------------------------------
# Save
# --------------------------------------------------------------------------
pop <- pop[order(code_commune)]
fwrite(pop, outfile)
cat(sprintf("\nSaved %s (%d rows)\n", outfile, nrow(pop)))

} # end if file exists check

cat("====================================================================\n\n")
