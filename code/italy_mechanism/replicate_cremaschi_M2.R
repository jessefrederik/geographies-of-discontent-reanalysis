################################################################################
# Reproduce Cremaschi et al. (2024) Table M.2: TWFE and MTWFE estimates of the
# 2010 reform's effect on local police, public registry, and garbage collection
# service-capacity indices (2009 vs 2013).
#
# Source data:  data_raw/italy/cremaschi_replication/service_dataset.dta
#               (Harvard Dataverse: doi:10.7910/DVN/I3VHZK)
# Source code:  data_raw/italy/cremaschi_replication/06_replication_appendix.do
#               (lines 561-581, the Table M2 block)
#
# Cremaschi's specification:
#   psmatch2 treated if year == 2013, outcome(pop_seg) mahal($match_cov)
#   foreach var in pol reg garb {
#     xtreg `var'_cap t i.year             if mis_`var' == 0 , fe cl(id08)
#     xtreg `var'_cap t i.year [pw = _w]   if mis_`var' == 0 & _s != ., fe cl(id08)
#   }
#
# t = 1 if year > 2010 & treated == 1; xtset id08 year (muni FE).
################################################################################

library(haven)
library(data.table)
library(MatchIt)
library(fixest)

d <- as.data.table(read_dta("data_raw/italy/cremaschi_replication/service_dataset.dta"))

# Construct treatment-post indicator: t = treated * post.
d[, t := as.integer(year > 2010 & treated == 1)]

# Drop munis present in only one of the two waves (mirrors lines 152-154 of
# 01_replication_maintext.do).
d[, n := .N, by = id08]
d <- d[n == 2]
d[, n := NULL]

# Mahalanobis 1:1 NN matching with replacement on the 2013 cross-section
# (replicates psmatch2 default; Cremaschi's do file does not pass `noreplacement`).
match_vars <- c("pop_tot_2008", "foreign_share_2008", "female_share_2008",
                "over65_share_2008", "mean_income2008",
                "share_university2001", "max_altitude")
d2013 <- d[year == 2013]
set.seed(20241201)
m <- matchit(treated ~ pop_tot_2008 + foreign_share_2008 + female_share_2008 +
                       over65_share_2008 + mean_income2008 +
                       share_university2001 + max_altitude,
             data = d2013, method = "nearest", distance = "mahalanobis",
             replace = TRUE)
md <- as.data.table(match.data(m))

# Build muni-level weights (mirrors `bys id: egen _w = max(_weight)` etc.)
weights <- md[, .(id08, w_match = weights)]
support <- md[, .(id08, in_support = 1L)]
d <- merge(d, weights, by = "id08", all.x = TRUE)
d <- merge(d, support, by = "id08", all.x = TRUE)
d[is.na(in_support), in_support := 0L]
d[is.na(w_match),    w_match    := 0]

# Run TWFE and MTWFE for each service
results <- data.table()
for (svc in c("pol", "reg", "garb")) {
  cap_var <- paste0(svc, "_cap")
  mis_var <- paste0("mis_", svc)
  d_svc <- d[get(mis_var) == 0]

  # TWFE: muni FE + year FE, clustered SEs at id08
  m_twfe <- feols(as.formula(sprintf("%s ~ t | id08 + year", cap_var)),
                  data = d_svc, cluster = ~id08)

  # MTWFE: same with matching weights, restricted to in-support munis
  d_match <- d_svc[in_support == 1L]
  m_mtwfe <- feols(as.formula(sprintf("%s ~ t | id08 + year", cap_var)),
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

# Cremaschi's reported numbers (from appendix Table M.2)
cremaschi <- data.table(
  service = rep(c("pol","reg","garb"), each = 2),
  method  = rep(c("TWFE", "MTWFE"), 3),
  cm_est  = c(-0.178, 0.012, -0.595, -0.568, -0.158, -0.159),
  cm_se   = c( 0.054, 0.105,  0.061,  0.177,  0.039,  0.074),
  cm_n    = c( 9282,  7238, 11132,  8998,  11794,  9686)
)

out <- merge(results, cremaschi, by = c("service","method"))
setcolorder(out, c("service","method","est","se","cm_est","cm_se",
                   "nobs","cm_n","p"))
cat("\n==== Reproduction of Cremaschi et al. (2024) Table M.2 ====\n")
cat("Columns: est/se = our reproduction; cm_est/cm_se = Cremaschi's reported.\n\n")
print(out[, .(service, method,
              ours      = sprintf("%+.3f (%.3f)", est, se),
              cremaschi = sprintf("%+.3f (%.3f)", cm_est, cm_se),
              n_ours    = nobs,
              n_cm      = cm_n,
              p_ours    = sprintf("%.3f", p))])

fwrite(out, "output/csvs/italy/replicate_cremaschi_M2.csv")
cat("\nWritten: output/csvs/italy/replicate_cremaschi_M2.csv\n")
