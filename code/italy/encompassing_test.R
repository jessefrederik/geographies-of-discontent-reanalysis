library(data.table)
library(fixest)

d <- fread("data_processed/italy/electoral_panel_extended.csv")
d <- d[year != 2022]  # match the paper's main panel
d[, log_pop := log(pop_tot_2008)]
d[, t_post := as.integer(treated == 1 & post == 1)]

cat("Panel:", nrow(d), "obs,", uniqueN(d$id08), "munis, years",
    paste(sort(unique(d$year)), collapse=","), "\n\n")

# Model 0: threshold only (no gradient)
m0 <- feols(farright_sh ~ t_post | id08 + year, data = d, cluster = "id08")
cat("M0 (threshold only):  beta =", round(coef(m0)["t_post"], 4),
    "  R2 =", round(r2(m0, "wr2"), 4), "\n")

# Model G: gradient only (year-interacted log(pop)), no threshold
mG <- feols(farright_sh ~ i(year, log_pop, ref = 2001) | id08 + year,
            data = d, cluster = "id08")
cat("MG (gradient only):   R2 =", round(r2(mG, "wr2"), 4), "\n")

# Model F: encompassing (both)
mF <- feols(farright_sh ~ t_post + i(year, log_pop, ref = 2001) | id08 + year,
            data = d, cluster = "id08")
cat("MF (encompassing):    beta =", round(coef(mF)["t_post"], 4),
    "  R2 =", round(r2(mF, "wr2"), 4), "\n\n")

# Test 1: threshold given gradient.  H0: beta = 0 in encompassing model.
cat("=== TEST 1: Does threshold add beyond gradient? ===\n")
cat("  H0: beta_threshold = 0 in encompassing model\n")
w1 <- wald(mF, "t_post")
print(w1)

# Test 2: gradient given threshold.  H0: all log_pop:year interactions = 0.
# Compare M0 vs MF via F-test (Wald on the joint exclusion of gradient terms).
cat("\n=== TEST 2: Does gradient add beyond threshold? ===\n")
cat("  H0: all log(pop) x year interactions = 0 in encompassing model\n")
w2 <- wald(mF, "year::.*:log_pop")
print(w2)

# AIC/BIC comparison (lower = better)
cat("\n=== AIC / BIC comparison ===\n")
cat(sprintf("  M0  (threshold only):  AIC = %.0f   BIC = %.0f\n",
            AIC(m0), BIC(m0)))
cat(sprintf("  MG  (gradient only):   AIC = %.0f   BIC = %.0f\n",
            AIC(mG), BIC(mG)))
cat(sprintf("  MF  (both):            AIC = %.0f   BIC = %.0f\n",
            AIC(mF), BIC(mF)))
