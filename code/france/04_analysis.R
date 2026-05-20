################################################################################
# 04_analysis.R — France placebo DID analysis
#
# Tasks 6, 8, 9, 10: placebo threshold sweep, narrow-band tests,
# fake reform dates, and size-trend controls.
#
# Follows the same specification as analysis.R (Italy) and analysis_nl.R (NL):
#   TWFE: farright_sh ~ treated_post | commune + year, cluster = commune
#
# Outputs:
#   output/csvs/france/placebo_threshold_results.csv
#   output/csvs/france/narrowband_results.csv
#   output/csvs/france/temporal_placebo_results.csv
#   output/csvs/france/size_controls_table.csv
################################################################################

cat("====================================================================\n")
cat("04_analysis.R — France placebo analysis\n")
cat("====================================================================\n\n")

library(data.table)
library(fixest)

d <- fread("data_processed/france/final/panel_commune.csv")
d[, log_pop := log(pop_2006)]

# Population rank controls (fixed cross-section, computed once)
d_base <- d[year == 2002]
d_base[, pop_pctile := rank(pop_2006) / .N]  # continuous percentile (0-1), matches Italy
d_base[, log_rank := log(.N + 1 - rank(pop_2006))]  # rank 1 = largest
d <- merge(d, d_base[, .(commune_id, pop_pctile, log_rank)], by = "commune_id")

cat(sprintf("Panel: %s rows = %d communes x %d elections\n",
            format(nrow(d), big.mark = ","),
            uniqueN(d$commune_id), uniqueN(d$year)))

# Main breakpoint: Pre = {2002, 2007}, Post = {2012, 2017, 2022}
fake_reform <- 2007

################################################################################
cat("\n====================================================================\n")
cat("TASK 6: PLACEBO THRESHOLD SWEEP\n")
cat("====================================================================\n\n")
# Run TWFE at every threshold from 1,000 to 50,000 in steps of 250.
# Mirrors analysis.R lines 231-246 and analysis_nl.R lines 98-122.

thresholds <- seq(1000, 50000, by = 250)
results <- data.frame(
  threshold = integer(), estimate = numeric(), se = numeric(),
  t_stat = numeric(), log_pop_gap = numeric(),
  n_treated = integer(), n_control = integer(),
  mean_logpop_treated = numeric(), mean_logpop_control = numeric()
)

for (thr in thresholds) {
  d[, treated := as.integer(pop_2006 < thr)]
  n_tr <- sum(d[year == 2002]$treated)
  n_ct <- sum(d[year == 2002]$treated == 0)
  if (n_tr < 30 || n_ct < 30) next

  d[, treated_post := as.integer(treated == 1 & year > fake_reform)]
  m <- feols(farright_sh ~ treated_post | commune_id + year,
             data = d, cluster = "commune_id")

  mlp_tr <- mean(d[treated == 1]$log_pop)
  mlp_ct <- mean(d[treated == 0]$log_pop)
  gap <- mlp_ct - mlp_tr
  est <- coef(m)["treated_post"]
  se_val <- se(m)["treated_post"]

  results <- rbind(results, data.frame(
    threshold = thr, estimate = est, se = se_val,
    t_stat = est / se_val, log_pop_gap = gap,
    n_treated = n_tr, n_control = n_ct,
    mean_logpop_treated = mlp_tr, mean_logpop_control = mlp_ct
  ))
}

# Meta-regression: DID estimate ~ log(pop) gap
fit <- lm(estimate ~ log_pop_gap, data = results)
r2 <- summary(fit)$r.squared

cat(sprintf("Thresholds tested: %d (from %s to %s)\n",
            nrow(results),
            format(min(results$threshold), big.mark = ","),
            format(max(results$threshold), big.mark = ",")))
cat(sprintf("Meta-regression: DID = %.4f + %.4f * log(pop) gap, R^2 = %.3f\n",
            coef(fit)[1], coef(fit)[2], r2))
cat(sprintf("Significant at 5%%: %d / %d thresholds (%.0f%%)\n",
            sum(abs(results$t_stat) > 1.96), nrow(results),
            100 * mean(abs(results$t_stat) > 1.96)))

# Save
fwrite(results, "output/csvs/france/placebo_threshold_results.csv")
cat(sprintf("Saved output/csvs/france/placebo_threshold_results.csv (%d rows)\n",
            nrow(results)))


################################################################################
cat("\n====================================================================\n")
cat("TASK 8: NARROW-BAND TESTS\n")
cat("====================================================================\n\n")
# For selected cutoffs, restrict sample to symmetric population windows.
# Mirrors analysis.R Table 1 (lines 61-88).

centers <- c(5000, 10000, 20000)
bandwidths <- c(Inf, 5000, 4000, 3000, 2000, 1000)

nb_results <- data.frame(
  center = integer(), bandwidth = numeric(),
  estimate = numeric(), se = numeric(), t_stat = numeric(),
  n_treated = integer(), n_control = integer()
)

cat(sprintf("%-10s %10s %10s %10s %8s %8s\n",
            "Center", "Bandwidth", "Estimate", "t-stat", "Treated", "Control"))

for (center in centers) {
  for (bw in bandwidths) {
    if (bw == Inf) {
      dsub <- copy(d)
    } else {
      dsub <- d[pop_2006 >= (center - bw) & pop_2006 <= (center + bw)]
    }
    dsub[, treated := as.integer(pop_2006 < center)]
    n_tr <- sum(dsub[year == 2002]$treated)
    n_ct <- sum(dsub[year == 2002]$treated == 0)
    if (n_tr < 30 || n_ct < 30) next

    dsub[, treated_post := as.integer(treated == 1 & year > fake_reform)]
    m <- feols(farright_sh ~ treated_post | commune_id + year,
               data = dsub, cluster = "commune_id")
    est <- coef(m)["treated_post"]
    se_val <- se(m)["treated_post"]

    bw_label <- if (bw == Inf) "Full" else format(bw, big.mark = ",")
    cat(sprintf("%-10s %10s %10.4f %10.2f %8d %8d\n",
                format(center, big.mark = ","), bw_label,
                est, est / se_val, n_tr, n_ct))

    nb_results <- rbind(nb_results, data.frame(
      center = center, bandwidth = bw,
      estimate = est, se = se_val, t_stat = est / se_val,
      n_treated = n_tr, n_control = n_ct
    ))
  }
  cat("\n")
}

fwrite(nb_results, "output/csvs/france/narrowband_results.csv")
cat("Saved output/csvs/france/narrowband_results.csv\n")


################################################################################
cat("\n====================================================================\n")
cat("TASK 9: FAKE REFORM DATE PLACEBOS\n")
cat("====================================================================\n\n")
# Test whether the pattern holds at different breakpoints.

fake_dates <- list(
  list(label = "Pre-period only (2002 vs 2007)",
       years = c(2002, 2007), break_after = 2002),
  list(label = "Late break (pre=2002-2012, post=2017-2022)",
       years = c(2002, 2007, 2012, 2017, 2022), break_after = 2012)
)

temporal_results <- data.frame()

for (fd in fake_dates) {
  cat(sprintf("\n--- %s ---\n", fd$label))
  dsub <- d[year %in% fd$years]

  sub_results <- data.frame(
    threshold = integer(), estimate = numeric(), se = numeric(),
    t_stat = numeric(), log_pop_gap = numeric()
  )

  for (thr in thresholds) {
    dsub[, treated := as.integer(pop_2006 < thr)]
    n_tr <- sum(dsub[year == min(fd$years)]$treated)
    n_ct <- sum(dsub[year == min(fd$years)]$treated == 0)
    if (n_tr < 30 || n_ct < 30) next

    dsub[, treated_post := as.integer(treated == 1 & year > fd$break_after)]
    m <- feols(farright_sh ~ treated_post | commune_id + year,
               data = dsub, cluster = "commune_id")

    gap <- mean(dsub[treated == 0]$log_pop) - mean(dsub[treated == 1]$log_pop)
    est <- coef(m)["treated_post"]
    se_val <- se(m)["treated_post"]

    sub_results <- rbind(sub_results, data.frame(
      threshold = thr, estimate = est, se = se_val,
      t_stat = est / se_val, log_pop_gap = gap
    ))
  }

  sub_fit <- lm(estimate ~ log_pop_gap, data = sub_results)
  cat(sprintf("Thresholds: %d, R^2 = %.3f, significant: %d/%d (%.0f%%)\n",
              nrow(sub_results), summary(sub_fit)$r.squared,
              sum(abs(sub_results$t_stat) > 1.96), nrow(sub_results),
              100 * mean(abs(sub_results$t_stat) > 1.96)))

  sub_results$fake_date_label <- fd$label
  sub_results$break_after <- fd$break_after
  temporal_results <- rbind(temporal_results, sub_results)
}

fwrite(temporal_results, "output/csvs/france/temporal_placebo_results.csv")
cat("\nSaved output/csvs/france/temporal_placebo_results.csv\n")


################################################################################
cat("\n====================================================================\n")
cat("TASK 10: SIZE-TREND CONTROLS\n")
cat("====================================================================\n\n")
# Test whether size-trend controls eliminate the estimate.
# log(rank) x year is the theoretically motivated control: under Zipf's law,
# log(rank) is the dual of log(pop) and captures a commune's position in the
# urban hierarchy. It provides better absorption than log(pop) x year when
# the size distribution is highly skewed.

test_thresholds <- c(5000, 10000, 15000, 20000)

sc_results <- data.frame(
  threshold = integer(), spec = character(),
  estimate = numeric(), se = numeric(), t_stat = numeric()
)

cat(sprintf("%-10s %-30s %10s %10s\n",
            "Threshold", "Specification", "Estimate", "t-stat"))

for (thr in test_thresholds) {
  d[, treated := as.integer(pop_2006 < thr)]
  d[, treated_post := as.integer(treated == 1 & year > fake_reform)]

  # Base TWFE
  m_base <- feols(farright_sh ~ treated_post | commune_id + year,
                  data = d, cluster = "commune_id")
  est_base <- coef(m_base)["treated_post"]
  se_base <- se(m_base)["treated_post"]

  cat(sprintf("%-10s %-30s %10.4f %10.2f\n",
              format(thr, big.mark = ","), "TWFE",
              est_base, est_base / se_base))

  sc_results <- rbind(sc_results, data.frame(
    threshold = thr, spec = "TWFE",
    estimate = est_base, se = se_base, t_stat = est_base / se_base))

  # TWFE + log(pop) x year
  m_logpop <- feols(farright_sh ~ treated_post + log_pop:i(year) | commune_id + year,
                    data = d, cluster = "commune_id")
  est_lp <- coef(m_logpop)["treated_post"]
  se_lp <- se(m_logpop)["treated_post"]

  cat(sprintf("%-10s %-30s %10.4f %10.2f\n",
              format(thr, big.mark = ","), "TWFE + log(pop) x year",
              est_lp, est_lp / se_lp))

  sc_results <- rbind(sc_results, data.frame(
    threshold = thr, spec = "TWFE + log(pop) x year",
    estimate = est_lp, se = se_lp, t_stat = est_lp / se_lp))

  # TWFE + log(rank) x year  [theoretically motivated: Zipf dual]
  m_logrank <- feols(farright_sh ~ treated_post + log_rank:i(year) | commune_id + year,
                     data = d, cluster = "commune_id")
  est_lr <- coef(m_logrank)["treated_post"]
  se_lr <- se(m_logrank)["treated_post"]

  cat(sprintf("%-10s %-30s %10.4f %10.2f\n",
              format(thr, big.mark = ","), "TWFE + log(rank) x year",
              est_lr, est_lr / se_lr))

  sc_results <- rbind(sc_results, data.frame(
    threshold = thr, spec = "TWFE + log(rank) x year",
    estimate = est_lr, se = se_lr, t_stat = est_lr / se_lr))

  # TWFE + pop-percentile x year (continuous, matches Italy specification)
  m_pctile <- feols(farright_sh ~ treated_post + pop_pctile:i(year) | commune_id + year,
                    data = d, cluster = "commune_id")
  est_pc <- coef(m_pctile)["treated_post"]
  se_pc <- se(m_pctile)["treated_post"]

  cat(sprintf("%-10s %-30s %10.4f %10.2f\n\n",
              format(thr, big.mark = ","), "TWFE + pop-pctile x year",
              est_pc, est_pc / se_pc))

  sc_results <- rbind(sc_results, data.frame(
    threshold = thr, spec = "TWFE + pop-pctile x year",
    estimate = est_pc, se = se_pc, t_stat = est_pc / se_pc))
}

fwrite(sc_results, "output/csvs/france/size_controls_table.csv")
cat("Saved output/csvs/france/size_controls_table.csv\n")


################################################################################
cat("\n====================================================================\n")
cat("SUMMARY\n")
cat("====================================================================\n\n")

cat(sprintf("Panel: %d communes x %d elections\n",
            uniqueN(d$commune_id), uniqueN(d$year)))
cat(sprintf("Main threshold sweep: %d thresholds, R^2 = %.3f\n",
            nrow(results), r2))
cat(sprintf("Significant at 5%%: %d / %d (%.0f%%)\n",
            sum(abs(results$t_stat) > 1.96), nrow(results),
            100 * mean(abs(results$t_stat) > 1.96)))
cat("\nInterpretation:\n")
cat("The same class of population-threshold estimator produces treatment-like\n")
cat("coefficients in France, where no analogous treatment is present, consistent\n")
cat("with the estimator loading on municipality-size trends.\n")
cat("====================================================================\n\n")


################################################################################
cat("\n====================================================================\n")
cat("LaTeX TABLE GENERATION\n")
cat("====================================================================\n\n")

dir.create("output/tables/france", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------
fmt3 <- function(x) sprintf("%.3f", x)
fmt_se <- function(x) sprintf("{(%.3f)}", x)
fmt_t <- function(x) sprintf("%.1f", x)
pstars <- function(tstat) ""
star_note <- ""

# ---------------------------------------------------------------------------
# Table 8: Narrow-band TWFE at 5,000 (tab_narrowband_fr.tex)
# ---------------------------------------------------------------------------
nb5 <- nb_results[nb_results$center == 5000 &
                   nb_results$bandwidth %in% c(Inf, 5000, 3000, 2000, 1000), ]
nb5 <- nb5[order(-nb5$bandwidth), ]  # Inf first, then descending

nb_rows <- character(nrow(nb5))
for (i in seq_len(nrow(nb5))) {
  bw   <- nb5$bandwidth[i]
  est  <- paste0(fmt3(nb5$estimate[i]), pstars(nb5$t_stat[i]))
  ncom <- format(nb5$n_treated[i] + nb5$n_control[i], big.mark = ",")
  if (is.infinite(bw)) {
    lab <- "Full sample"
  } else {
    lab <- paste0("$\\pm$", format(bw, big.mark = ","))
  }
  se_str <- sprintf("{(%.3f)}", nb5$se[i])
  nb_rows[i] <- paste0(
    sprintf("      %s & %s & %s \\\\", lab, est, ncom),
    sprintf("\n      & %s & \\\\", se_str))
}

tex8 <- paste(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Narrow-band TWFE estimates around 5,000 (France)}",
  "    \\label{tab:narrowband_fr}",
  "    \\begin{tabular*}{0.75\\linewidth}{@{\\extracolsep{\\fill}}lSc@{}}",
  "      \\toprule",
  "      Bandwidth & {Estimate} & Communes \\\\",
  "      \\midrule",
  nb_rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} TWFE with commune and year FE, SEs clustered by commune. Each row restricts to communes within the indicated bandwidth of 5,000 inhabitants. Pre $= \\{2002, 2007\\}$, post $= \\{2012, 2017, 2022\\}$. ", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), collapse = "\n")

writeLines(tex8, "output/tables/france/tab_narrowband_fr.tex")
cat("Saved output/tables/france/tab_narrowband_fr.tex\n")

# ---------------------------------------------------------------------------
# Table 9: Placebo threshold TWFE (tab_placebo_fr.tex)
# ---------------------------------------------------------------------------
placebo_thrs <- c(2000, 5000, 7500, 10000, 20000, 35000, 50000)
pl <- results[results$threshold %in% placebo_thrs, ]
pl <- pl[order(pl$threshold), ]

pl_rows <- character(nrow(pl))
for (i in seq_len(nrow(pl))) {
  lab  <- format(pl$threshold[i], big.mark = ",")
  est  <- paste0(fmt3(pl$estimate[i]), pstars(pl$t_stat[i]))
  se_str <- sprintf("{(%.3f)}", pl$se[i])
  pl_rows[i] <- paste0(
    sprintf("      %s & %s \\\\", lab, est),
    sprintf("\n      & %s \\\\", se_str))
}

tex9 <- paste(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Placebo threshold TWFE estimates (France)}",
  "    \\label{tab:placebo_fr}",
  "    \\begin{tabular*}{0.65\\linewidth}{@{\\extracolsep{\\fill}}rS@{}}",
  "      \\toprule",
  "      Threshold & {Estimate} \\\\",
  "      \\midrule",
  pl_rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} TWFE with commune and year FE, SEs clustered by commune. Each row is a separate estimation at the indicated cutoff. Pre $= \\{2002, 2007\\}$, post $= \\{2012, 2017, 2022\\}$. No French reform exists at any threshold. ", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), collapse = "\n")

writeLines(tex9, "output/tables/france/tab_placebo_fr.tex")
cat("Saved output/tables/france/tab_placebo_fr.tex\n")

# ---------------------------------------------------------------------------
# Table 10: Temporal placebo at 5,000 (tab_temporal_fr.tex)
# ---------------------------------------------------------------------------
# Row 1: Pre-period only (2002 vs 2007) from temporal_results
tr_pre <- temporal_results[temporal_results$threshold == 5000 &
                           temporal_results$break_after == 2002, ]
# Row 2: Full sample — main result at 5,000 from `results`
r_main <- results[results$threshold == 5000, ]
# Row 3: Late break (pre=2002-2012, post=2017-2022) from temporal_results
tr_late <- temporal_results[temporal_results$threshold == 5000 &
                            temporal_results$break_after == 2012, ]

temp_specs <- c(
  "Pre-period only (2002 vs.\\ 2007)",
  "Full sample (pre = 2002--2007, post = 2012--2022)",
  "Late break (pre = 2002--2012, post = 2017--2022)"
)
temp_est <- c(tr_pre$estimate[1], r_main$estimate[1], tr_late$estimate[1])
temp_se  <- c(tr_pre$se[1],       r_main$se[1],       tr_late$se[1])
temp_t   <- c(tr_pre$t_stat[1],  r_main$t_stat[1],  tr_late$t_stat[1])

temp_rows <- character(3)
for (i in 1:3) {
  temp_rows[i] <- paste0(
    sprintf("      %s & %s \\\\", temp_specs[i], paste0(fmt3(temp_est[i]), pstars(temp_t[i]))),
    sprintf("\n      & {(%.3f)} \\\\", temp_se[i]))
}

tex10 <- paste(c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "    \\caption{Temporal placebo estimates at 5,000 (France)}",
  "    \\label{tab:temporal_fr}",
  "    \\begin{tabular*}{0.65\\linewidth}{@{\\extracolsep{\\fill}}lS@{}}",
  "      \\toprule",
  "      Specification & {Estimate} \\\\",
  "      \\midrule",
  temp_rows,
  "      \\bottomrule",
  "    \\end{tabular*}",
  "    \\begin{tablenotes}[flushleft]\\small",
  paste0("      \\item \\textit{Notes:} TWFE with commune and year FE, SEs clustered by commune. Treatment is $\\mathbf{1}(\\text{pop} < 5{,}000)$. The first row uses only pre-period elections. The last row shifts the break point forward. ", star_note),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), collapse = "\n")

writeLines(tex10, "output/tables/france/tab_temporal_fr.tex")
cat("Saved output/tables/france/tab_temporal_fr.tex\n")

cat("\nAll LaTeX tables written to output/tables/france/\n")
