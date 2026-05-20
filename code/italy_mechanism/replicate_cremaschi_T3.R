################################################################################
# Reproduce Cremaschi et al. (2024) Table 3 (main text, p.~24): TWFE and MTWFE
# estimates of the 2010 reform's effect on local police, public registry, and
# garbage-collection "Services Against Standard Demand" (the percentage
# deviation in service output relative to population-band averages),
# standardized as z-scores.
#
# Source data:  data_raw/italy/cremaschi_replication/service_dataset.dta
#               (Harvard Dataverse: doi:10.7910/DVN/I3VHZK)
# Source code:  data_raw/italy/cremaschi_replication/01_replication_maintext.do
#               (lines 173-189: the Table 3 block)
#
# Cremaschi's specification (from the .do file):
#   psmatch2 treated if year == 2013, outcome(pop_seg) mahal($match_cov)
#   foreach var in pol reg garb {
#     egen z_`var'_diff = std(`var'_diff)
#     drop `var'_diff
#     rename z_`var'_diff `var'_diff
#     xtreg `var'_diff t i.year             if mis_`var' == 0 , fe cl(id08)
#     xtreg `var'_diff t i.year [pw = _w]   if mis_`var' == 0 & _s != ., fe cl(id08)
#   }
################################################################################

library(haven)
library(data.table)
library(MatchIt)
library(fixest)

d <- as.data.table(read_dta("data_raw/italy/cremaschi_replication/service_dataset.dta"))

# Treatment-post indicator (mirrors line 145-146 of 01_replication_maintext.do)
d[, t := as.integer(year > 2010 & treated == 1)]

# Drop munis present in only one wave (lines 152-154)
d[, n := .N, by = id08]; d <- d[n == 2]; d[, n := NULL]

# Mahalanobis 1:1 NN matching with replacement on 2013 cross-section
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008",
                "share_university2001", "max_altitude")
set.seed(20241201)
m <- matchit(treated ~ pop_tot_2008 + foreign_share_2008 + female_share_2008 +
                       over65_share_2008 + mean_income2008 +
                       share_university2001 + max_altitude,
             data = d[year == 2013], method = "nearest",
             distance = "mahalanobis", replace = TRUE)
md <- as.data.table(match.data(m))

d <- merge(d, md[, .(id08, w_match = weights)], by = "id08", all.x = TRUE)
d <- merge(d, md[, .(id08, in_support = 1L)],   by = "id08", all.x = TRUE)
d[is.na(in_support), in_support := 0L]
d[is.na(w_match),    w_match    := 0]

# Standardize each diff variable across the full sample (mirrors `egen std`).
# Stata's egen std() uses all non-missing values of the variable.
for (v in c("pol_diff", "reg_diff", "garb_diff")) {
  use <- d[!is.na(get(v)), get(v)]
  mu  <- mean(use); sd_v <- sd(use)
  d[, paste0("z_", v) := (get(v) - mu) / sd_v]
}

# TWFE and MTWFE for each service
results <- data.table()
for (svc in c("pol", "reg", "garb")) {
  z_var   <- paste0("z_", svc, "_diff")
  mis_var <- paste0("mis_", svc)
  d_svc   <- d[get(mis_var) == 0]

  m_twfe <- feols(as.formula(sprintf("%s ~ t | id08 + year", z_var)),
                  data = d_svc, cluster = ~id08)
  d_match <- d_svc[in_support == 1L]
  m_mtwfe <- feols(as.formula(sprintf("%s ~ t | id08 + year", z_var)),
                   data = d_match, weights = ~w_match, cluster = ~id08)

  results <- rbind(results, data.table(
    service = svc,
    method  = c("TWFE", "MTWFE"),
    est     = c(coef(m_twfe)["t"],  coef(m_mtwfe)["t"]),
    se      = c(se(m_twfe)["t"],    se(m_mtwfe)["t"]),
    p       = c(pvalue(m_twfe)["t"],pvalue(m_mtwfe)["t"]),
    nobs    = c(nobs(m_twfe),       nobs(m_mtwfe))
  ))
}

# Cremaschi's reported numbers (main paper, Table 3, p.~24)
cremaschi <- data.table(
  service = rep(c("pol","reg","garb"), each = 2),
  method  = rep(c("TWFE","MTWFE"), 3),
  cm_est  = c(-0.290, -0.196, -0.149, -0.157, -0.062, -0.076),
  cm_se   = c( 0.033,  0.047,  0.033,  0.080,  0.019,  0.036),
  cm_n    = c( 9282,   7238,  11132,   8998,  11794,   9686)
)

out <- merge(results, cremaschi, by = c("service","method"))
setcolorder(out, c("service","method","est","se","cm_est","cm_se",
                   "nobs","cm_n","p"))

cat("\n==== Reproduction of Cremaschi et al. (2024) Table 3 ====\n")
cat("Outcome: standardized Services Against Standard Demand (z-scores).\n\n")
print(out[, .(service, method,
              ours      = sprintf("%+.3f (%.3f)", est, se),
              cremaschi = sprintf("%+.3f (%.3f)", cm_est, cm_se),
              n_ours    = nobs,
              n_cm      = cm_n,
              p_ours    = sprintf("%.3f", p))])

fwrite(out, "output/csvs/italy/replicate_cremaschi_T3.csv")
cat("\nWritten: output/csvs/italy/replicate_cremaschi_T3.csv\n")
