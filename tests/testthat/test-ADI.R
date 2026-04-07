# =============================================================================
# EVENTS MATRIX TESTS
# =============================================================================

test_that("adi_quintile column exists in events matrix", {
  results <- simulate(jurisdiction = "us", n_agents = 5000, return_events = TRUE)
  expect_true("adi_quintile" %in% colnames(results$events))
})

test_that("All ADI quintile values are between the values of 1-5", {
  results <- simulate(jurisdiction = "us", n_agents = 100000, return_events = TRUE)
  error <- results$events[!results$events$adi_quintile %in% 1:5, ]
  expect_equal(
    nrow(error), 0,
    label = paste0(
      "Simulated individuals with invalid adi_quintile: ", nrow(error),
      " (values seen: ", paste(unique(error$adi_quintile), collapse = ", "), ")"
    )
  )
})

test_that("ADI quintile remains constant per simulated individual over a 50-year horizon", {
  results <- simulate(jurisdiction = "us", n_agents = 10000, time_horizon = 50, return_events = TRUE)

  quintiles_per_agent <- results$events |>
  dplyr::group_by(id) |>
  dplyr::summarise(n_unique_quintiles = dplyr::n_distinct(adi_quintile))

  expect_true(all(quintiles_per_agent$n_unique_quintiles == 1))
})


test_that("COPD relative prevalence ratios are similar to that reported in Hayes et al. 2024 (Q5/Q1=2.4, Q4/Q1=1.6, Q3/Q1=1.2) +/-1%", {
  results <- simulate(jurisdiction = "us", n_agents = 100000, time_horizon = 1, return_events = TRUE)

  all_individuals <- results$events[results$events$event == 0, ]

  # For each ADI quintile, compute the proportion of simulated individuals who have COPD
  all_copd            <- all_individuals$gold > 0
  copd_prevalence_adi <- tapply(all_copd, all_individuals$adi_quintile, mean)

  # Q1 is the reference (least deprived); compute prevalence ratios relative to Q1
  prevalence_q1 <- copd_prevalence_adi["1"]
  ratio_q3_q1   <- copd_prevalence_adi["3"] / prevalence_q1  # expected ~1.2
  ratio_q4_q1   <- copd_prevalence_adi["4"] / prevalence_q1  # expected ~1.6
  ratio_q5_q1   <- copd_prevalence_adi["5"] / prevalence_q1  # expected ~2.4

  expect_gt(ratio_q3_q1, 1.2 * 0.99); expect_lt(ratio_q3_q1, 1.2 * 1.01)  # within 1% of 1.2
  expect_gt(ratio_q4_q1, 1.6 * 0.99); expect_lt(ratio_q4_q1, 1.6 * 1.01)  # within 1% of 1.6
  expect_gt(ratio_q5_q1, 2.4 * 0.99); expect_lt(ratio_q5_q1, 2.4 * 1.01)  # within 1% of 2.4
})

test_that("ADI distribution matches that observed in the general population +/-1%", {
  # Target proportions per quintile from config_us.json (Neighborhood Atlas)
  expected_proportions <- c(0.21392596, 0.23087204, 0.21450997, 0.18572808, 0.15496396)

  results <- simulate(jurisdiction = "us", n_agents = 100000, time_horizon = 1, return_events = TRUE)

  all_individuals <- results$events[results$events$event == 0, ]
  non_copd        <- all_individuals[all_individuals$gold == 0, ]
  quintile_counts <- as.vector(table(factor(non_copd$adi_quintile, levels = 1:5)))
  observed_prop   <- quintile_counts / sum(quintile_counts)

  for (q in 1:5) {
    expect_gt(observed_prop[q], expected_proportions[q] * 0.99,
              label = paste("Q", q, "proportion below expected -1%"))
    expect_lt(observed_prop[q], expected_proportions[q] * 1.01,
              label = paste("Q", q, "proportion above expected +1%"))
  }
})

# =============================================================================
# EXTENDED OUTPUT TESTS
# =============================================================================

test_that("results$extended contains the four ADI-stratified matrices", {
  results <- simulate(jurisdiction = "us", n_agents = 10000, extended_results = TRUE)

  expect_true(!is.null(results$extended$n_alive_by_ctime_adi))
  expect_true(!is.null(results$extended$n_COPD_by_ctime_adi))
  expect_true(!is.null(results$extended$cumul_cost_by_ctime_adi))
  expect_true(!is.null(results$extended$cumul_qaly_by_ctime_adi))
})

test_that("ADI matrices have 5 columns (one per quintile)", {
  results <- simulate(jurisdiction = "us", n_agents = 10000, extended_results = TRUE)

  expect_equal(ncol(results$extended$n_alive_by_ctime_adi),    5)
  expect_equal(ncol(results$extended$n_COPD_by_ctime_adi),     5)
  expect_equal(ncol(results$extended$cumul_cost_by_ctime_adi), 5)
  expect_equal(ncol(results$extended$cumul_qaly_by_ctime_adi), 5)
})

test_that("n_COPD_by_ctime_adi never exceeds n_alive_by_ctime_adi", {
  results <- simulate(jurisdiction = "us", n_agents = 100000, extended_results = TRUE)

  alive <- results$extended$n_alive_by_ctime_adi
  copd  <- results$extended$n_COPD_by_ctime_adi
  expect_true(all(copd <= alive))
})

test_that("Row sums of n_alive_by_ctime_adi equal total alive agents each year", {
  results <- simulate(jurisdiction = "us", n_agents = 100000, extended_results = TRUE)

  total_from_adi <- rowSums(results$extended$n_alive_by_ctime_adi)
  total_marginal <- rowSums(results$extended$n_alive_by_ctime_sex)
  expect_equal(total_from_adi, total_marginal)
})

test_that("COPD prevalence ratios by ADI quintile hold over time +/-1%", {
  results <- simulate(jurisdiction = "us", n_agents = 100000, time_horizon = 50, extended_results = TRUE)

  alive <- results$extended$n_alive_by_ctime_adi
  copd  <- results$extended$n_COPD_by_ctime_adi

  # Compute COPD rate per quintile per year
  rates <- copd / pmax(alive, 1)

  # Compute year-by-year ratios relative to Q1 (column 1)
  ratio_q3_q1 <- rates[, 3] / rates[, 1]  # expected ~1.2
  ratio_q4_q1 <- rates[, 4] / rates[, 1]  # expected ~1.6
  ratio_q5_q1 <- rates[, 5] / rates[, 1]  # expected ~2.4

  expect_true(all(ratio_q3_q1 >= 1.2 * 0.99) && all(ratio_q3_q1 <= 1.2 * 1.01))
  expect_true(all(ratio_q4_q1 >= 1.6 * 0.99) && all(ratio_q4_q1 <= 1.6 * 1.01))
  expect_true(all(ratio_q5_q1 >= 2.4 * 0.99) && all(ratio_q5_q1 <= 2.4 * 1.01))
})
