## Extend the electoral panel to include 2022
## Input:  data/electoral_panel_dataset.dta  (Cremaschi et al., 2001-2018)
##         data_processed/italy/election_2022.csv (from 04_build_2022.R)
## Output: data_processed/italy/electoral_panel_extended.csv (2001-2022, id08 panel)

library(haven)
library(data.table)

# ---- Load original panel ----
d <- as.data.table(read_dta("data_raw/italy/electoral_panel_dataset.dta"))
cat(sprintf("Original panel: %d obs, %d municipalities, years: %s\n",
            nrow(d), uniqueN(d$id08), paste(sort(unique(d$year)), collapse=", ")))

# ---- Load 2022 data ----
e22 <- fread("data_processed/italy/election_2022.csv")
cat(sprintf("2022 data: %d municipalities\n", nrow(e22)))

# ---- Create 2022 panel rows ----
# Take one cross-section from the existing panel (2008) as template for
# time-invariant variables
template <- d[year == 2008]
cols_invariant <- c("id08", "municipality", "cod_prov", "province", "region",
                    "treated", "mont_group", "pop_tot_2008",
                    "foreign_share_2008", "female_share_2008", "male_share_2008",
                    "over65_share_2008", "mean_income2008", "mean_income_bottom_bracket",
                    "mean_income_top_bracket", "income_inequality2008",
                    "max_altitude", "mean_altitude",
                    "share_university2001", "share_university2011", "north")
# Keep only columns that exist
cols_invariant <- intersect(cols_invariant, names(template))
template <- template[, ..cols_invariant]

# Merge with 2022 election results
new_rows <- merge(template, e22[, .(id08, farright_sh, lega_sh)],
                  by = "id08", all.x = FALSE)

# Set year and construct treatment/time variables
new_rows[, year := 2022]
new_rows[, post := 1L]
new_rows[, t := as.integer(treated == 1)]
new_rows[, date_election := as.Date("2022-09-25")]

# Year-specific treatment dummies
new_rows[, t22 := as.integer(treated == 1)]
new_rows[, t18 := 0L]
new_rows[, t13 := 0L]
new_rows[, t08 := 0L]
new_rows[, t06 := 0L]
new_rows[, t01 := 0L]

cat(sprintf("2022 rows created: %d municipalities\n", nrow(new_rows)))

# ---- Append to existing panel ----
# Add t22 column to existing data (all zeros)
d[, t22 := 0L]

# Align columns: keep only columns present in both
common_cols <- intersect(names(d), names(new_rows))
cat(sprintf("Common columns: %d\n", length(common_cols)))

panel <- rbind(d[, ..common_cols], new_rows[, ..common_cols])

# Sort
setorder(panel, id08, year)

cat(sprintf("\nExtended panel: %d obs, %d municipalities, years: %s\n",
            nrow(panel), uniqueN(panel$id08),
            paste(sort(unique(panel$year)), collapse=", ")))

# ---- Validate ----
# Check no duplicates
dupes <- panel[, .N, by = .(id08, year)][N > 1]
stopifnot(nrow(dupes) == 0)

# Check farright_sh distribution by year
cat("\nFar-right share by year:\n")
print(panel[, .(mean = mean(farright_sh, na.rm=TRUE),
                median = median(farright_sh, na.rm=TRUE),
                n = .N), by = year])

# How many municipalities have all 6 years?
obs_per_muni <- panel[, .N, by = id08]
cat(sprintf("\nMunicipalities with 6 years: %d\n", sum(obs_per_muni$N == 6)))
cat(sprintf("Municipalities with 5 years: %d (no 2022 data)\n", sum(obs_per_muni$N == 5)))

# ---- Save ----
fwrite(panel, "data_processed/italy/electoral_panel_extended.csv")
cat(sprintf("\nSaved data_processed/italy/electoral_panel_extended.csv (%d rows)\n", nrow(panel)))
