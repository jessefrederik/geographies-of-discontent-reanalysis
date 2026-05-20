################################################################################
# 03_harmonize_panel.R — Harmonize commune codes and build balanced panel
#
# Uses the INSEE COG historique to map historical commune codes to 2026
# geography. When communes merged, their votes and populations are summed.
# The final panel contains one row per commune × election year for communes
# present in all 5 elections.
#
# Output: data_processed/france/final/panel_commune.csv
################################################################################

cat("====================================================================\n")
cat("03_harmonize_panel.R — Harmonizing commune panel\n")
cat("====================================================================\n\n")

library(data.table)

outfile <- "data_processed/france/final/panel_commune.csv"

if (file.exists(outfile)) {
  cat(sprintf("[skip] %s already exists. Delete to regenerate.\n\n", outfile))
} else {

# --------------------------------------------------------------------------
# 1. Build crosswalk from COG commune movements
#    Map every historical commune code to its 2026 successor.
# --------------------------------------------------------------------------
cat("Building commune code crosswalk from COG...\n")

mvt <- fread("data_raw/france/cog/v_mvt_commune_2026.csv")

# Keep events where a real commune (COM) was replaced by another commune (COM)
# and the code actually changed. These are mergers + code changes.
# MOD: 30 (suppression), 31 (fusion simple), 32 (commune nouvelle),
#      33 (fusion-association), 41 (code change)
xwalk <- mvt[MOD %in% c(30, 31, 32, 33, 41) &
             TYPECOM_AV == "COM" & TYPECOM_AP == "COM" &
             COM_AV != COM_AP,
             .(old_code = COM_AV, new_code = COM_AP, date = DATE_EFF)]

# De-duplicate: keep the LATEST event for each old_code
xwalk <- xwalk[order(old_code, -date)]
xwalk <- xwalk[!duplicated(old_code)]

# Break circular chains using the current commune list.
# If old_code exists in the 2026 COG as an active commune, drop the mapping.
cog_current <- fread("data_raw/france/cog/v_commune_2026.csv")
active_codes <- cog_current[TYPECOM == "COM", unique(COM)]
circular_drop <- xwalk[old_code %in% active_codes & !(new_code %in% active_codes)]
if (nrow(circular_drop) > 0) {
  cat(sprintf("  Dropping %d stale mappings (old_code is still active in 2026 COG)\n",
              nrow(circular_drop)))
}
xwalk <- xwalk[!(old_code %in% active_codes & !(new_code %in% active_codes))]
# Also drop any remaining where old_code IS active and new_code IS also active
# but old_code→new_code creates a cycle
xwalk <- xwalk[!(old_code %in% active_codes)]

cat(sprintf("  Direct mappings: %d old codes -> successor codes\n", nrow(xwalk)))

# --------------------------------------------------------------------------
# Resolve transitive chains: if A -> B and B -> C, then A -> C
# --------------------------------------------------------------------------
resolve_chains <- function(dt) {
  map <- setNames(dt$new_code, dt$old_code)
  changed <- TRUE
  iter <- 0
  while (changed && iter < 100) {
    iter <- iter + 1
    changed <- FALSE
    for (i in seq_along(map)) {
      if (map[i] %in% names(map)) {
        map[i] <- map[map[i]]
        changed <- TRUE
      }
    }
  }
  remaining <- sum(map %in% names(map))
  if (remaining > 0) {
    cat(sprintf("  WARNING: %d unresolved chains after %d iterations\n", remaining, iter))
  } else {
    cat(sprintf("  Resolved all chains in %d iterations\n", iter))
  }
  data.table(old_code = names(map), new_code = unname(map))
}

xwalk <- resolve_chains(xwalk)

# --------------------------------------------------------------------------
# 2. Load election and population data
# --------------------------------------------------------------------------
cat("\nLoading intermediate data...\n")
elec <- fread("data_processed/france/intermediate/elections_commune.csv")
pop  <- fread("data_processed/france/intermediate/commune_population.csv")

cat(sprintf("  Elections: %s rows, %d unique communes\n",
            format(nrow(elec), big.mark = ","), uniqueN(elec$code_commune)))
cat(sprintf("  Population: %d communes\n", nrow(pop)))

# --------------------------------------------------------------------------
# 3. Apply crosswalk to election data
# --------------------------------------------------------------------------
cat("\nApplying commune code crosswalk...\n")

# Map old codes to 2026 codes
map_code <- function(codes, xwalk_dt) {
  lookup <- setNames(xwalk_dt$new_code, xwalk_dt$old_code)
  mapped <- lookup[codes]
  ifelse(is.na(mapped), codes, mapped)  # unmapped = identity
}

elec[, commune_id := map_code(code_commune, xwalk)]

# Count how many codes changed
n_mapped <- sum(elec$code_commune != elec$commune_id)
cat(sprintf("  Remapped %s election rows (%.1f%%)\n",
            format(n_mapped, big.mark = ","),
            100 * n_mapped / nrow(elec)))

# Re-aggregate after mapping (merged communes need their votes summed)
elec <- elec[, .(inscrits = sum(inscrits, na.rm = TRUE),
                 votants = sum(votants, na.rm = TRUE),
                 exprimes = sum(exprimes, na.rm = TRUE),
                 farright_votes = sum(farright_votes, na.rm = TRUE)),
             by = .(commune_id, year)]
elec[, farright_sh := farright_votes / exprimes]

cat(sprintf("  After re-aggregation: %s rows, %d unique communes\n",
            format(nrow(elec), big.mark = ","), uniqueN(elec$commune_id)))

# --------------------------------------------------------------------------
# 4. Apply crosswalk to population data
# --------------------------------------------------------------------------
pop[, commune_id := map_code(code_commune, xwalk)]

n_mapped_pop <- sum(pop$code_commune != pop$commune_id)
cat(sprintf("  Remapped %d population rows\n", n_mapped_pop))

# Re-aggregate (sum populations of merged communes)
pop <- pop[, .(pop_2006 = sum(pop_2006, na.rm = TRUE)),
           by = commune_id]
pop[, log_pop := log(pop_2006)]

# --------------------------------------------------------------------------
# 5. Merge election + population
# --------------------------------------------------------------------------
cat("\nMerging election + population...\n")
panel <- merge(elec, pop, by = "commune_id", all.x = TRUE)

n_no_pop <- sum(is.na(panel$pop_2006))
cat(sprintf("  Election rows without population match: %d (%.1f%%)\n",
            n_no_pop, 100 * n_no_pop / nrow(panel)))

# Drop communes without population (can't assign treatment)
panel <- panel[!is.na(pop_2006)]

# --------------------------------------------------------------------------
# 6. Build balanced panel (communes present in all 5 election years)
# --------------------------------------------------------------------------
cat("\nBuilding balanced panel...\n")
years <- c(2002, 2007, 2012, 2017, 2022)

year_count <- panel[, .(n_years = uniqueN(year[year %in% years])),
                    by = commune_id]
balanced_ids <- year_count[n_years == length(years)]$commune_id

cat(sprintf("  Communes in all %d years: %d / %d (%.1f%%)\n",
            length(years), length(balanced_ids), uniqueN(panel$commune_id),
            100 * length(balanced_ids) / uniqueN(panel$commune_id)))

panel <- panel[commune_id %in% balanced_ids & year %in% years]
panel <- panel[order(commune_id, year)]

# --------------------------------------------------------------------------
# 7. Diagnostics
# --------------------------------------------------------------------------
cat("\n--- Panel diagnostics ---\n")
cat(sprintf("Dimensions: %s rows = %d communes x %d elections\n",
            format(nrow(panel), big.mark = ","),
            uniqueN(panel$commune_id), uniqueN(panel$year)))
cat(sprintf("Population range: %s to %s (median %s)\n",
            format(min(panel$pop_2006), big.mark = ","),
            format(max(panel$pop_2006), big.mark = ","),
            format(median(panel[year == 2002]$pop_2006), big.mark = ",")))

cat("\nFar-right share by year (weighted mean):\n")
for (yr in years) {
  d_yr <- panel[year == yr]
  wt_mean <- sum(d_yr$farright_votes) / sum(d_yr$exprimes)
  cat(sprintf("  %d: %.2f%%\n", yr, wt_mean * 100))
}

# Population distribution (for threshold sweep context)
cat("\nPopulation distribution (2006 census, unique communes):\n")
pop_base <- panel[year == 2002]
for (q in c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)) {
  cat(sprintf("  P%02d: %s\n", q * 100,
              format(round(quantile(pop_base$pop_2006, q)), big.mark = ",")))
}
cat(sprintf("  Communes < 5,000: %d (%.1f%%)\n",
            sum(pop_base$pop_2006 < 5000),
            100 * mean(pop_base$pop_2006 < 5000)))
cat(sprintf("  Communes < 10,000: %d (%.1f%%)\n",
            sum(pop_base$pop_2006 < 10000),
            100 * mean(pop_base$pop_2006 < 10000)))

# --------------------------------------------------------------------------
# 8. Save
# --------------------------------------------------------------------------
keep_cols <- c("commune_id", "year", "farright_sh", "farright_votes",
               "exprimes", "inscrits", "votants", "pop_2006", "log_pop")
fwrite(panel[, ..keep_cols], outfile)
cat(sprintf("\nSaved %s (%s rows)\n", outfile,
            format(nrow(panel), big.mark = ",")))

} # end if file exists check

cat("====================================================================\n\n")
