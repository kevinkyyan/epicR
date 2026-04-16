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

test_that("ADI quintile changes at most once per agent (at COPD onset)", {
  results <- simulate(
    jurisdiction = "us", n_agents = 1000000,
    time_horizon = 50, return_events = TRUE
  )

  events <- results$events

  ever_copd <- events |>
    dplyr::group_by(id) |>
    dplyr::summarise(ever_copd = any(gold > 0))

  quintiles_per_agent <- events |>
    dplyr::group_by(id) |>
    dplyr::summarise(n_unique_quintiles = dplyr::n_distinct(adi_quintile)) |>
    dplyr::left_join(ever_copd, by = "id")

  non_copd <- quintiles_per_agent[!quintiles_per_agent$ever_copd, ]
  copd     <- quintiles_per_agent[quintiles_per_agent$ever_copd, ]

  # Agents who never develop COPD must have a fixed quintile
  expect_true(all(non_copd$n_unique_quintiles == 1))

  # Agents who develop COPD may switch quintile once at onset
  expect_true(all(copd$n_unique_quintiles <= 2))
})


test_that("COPD density RR recovers Hayes 2024 multipliers at baseline +/-5%", {
  results <- simulate(
    jurisdiction = "us", n_agents = 100000,
    time_horizon = 1, return_events = TRUE
  )

  all_individuals <- results$events[results$events$event == 0, ]

  copd_counts <- tapply(
    all_individuals$gold > 0, all_individuals$adi_quintile, sum
  )
  non_copd_counts <- tapply(
    all_individuals$gold == 0, all_individuals$adi_quintile, sum
  )

  copd_share     <- copd_counts     / sum(copd_counts)
  non_copd_share <- non_copd_counts / sum(non_copd_counts)

  rel_density <- copd_share / non_copd_share
  rr          <- rel_density / rel_density["1"]

  expect_true(rr["2"] >= 0.95 && rr["2"] <= 1.05)
  expect_true(rr["3"] >= 1.14 && rr["3"] <= 1.26)
  expect_true(rr["4"] >= 1.52 && rr["4"] <= 1.68)
  expect_true(rr["5"] >= 2.28 && rr["5"] <= 2.52)
})

test_that("ADI distribution matches that observed in the general population +/-5%", {
  # Target proportions per quintile from config_us.json (Neighborhood Atlas)
  expected_proportions <- c(0.21392596, 0.23087204, 0.21450997, 0.18572808, 0.15496396)

  results <- simulate(jurisdiction = "us", n_agents = 100000, time_horizon = 1, return_events = TRUE)

  all_individuals <- results$events[results$events$event == 0, ]
  non_copd        <- all_individuals[all_individuals$gold == 0, ]
  quintile_counts <- as.vector(table(factor(non_copd$adi_quintile, levels = 1:5)))
  observed_prop   <- quintile_counts / sum(quintile_counts)

  for (q in 1:5) {
    expect_gt(observed_prop[q], expected_proportions[q] * 0.95,
              label = paste("Q", q, "proportion below expected -5%"))
    expect_lt(observed_prop[q], expected_proportions[q] * 1.05,
              label = paste("Q", q, "proportion above expected +5%"))
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

test_that("COPD density RR recovers Hayes 2024 multipliers per year", {
  #   relative density[q] = (COPD share in q) / (non-COPD share in q)
  #   rr[q]            = relative density[q] / relative density[Q1]
  # Non-COPD agents follow p_adi_quintiles by construction, serving as the
  # population baseline. Recovers [1, 1, 1.2, 1.6, 2.4] Hayes et al. 2024.
  results <- simulate(jurisdiction = "us", n_agents = 1000000,
                      time_horizon = 50, extended_results = TRUE)

  alive    <- results$extended$n_alive_by_ctime_adi
  copd     <- results$extended$n_COPD_by_ctime_adi
  non_copd <- alive - copd

  for (yr in seq_len(nrow(alive))) {
    copd_share     <- copd[yr, ]     / sum(copd[yr, ])
    non_copd_share <- non_copd[yr, ] / sum(non_copd[yr, ])
    rel_density <- copd_share / non_copd_share
    rr       <- rel_density / rel_density[1]

    expect_true(
      rr[2] >= 0.95 && rr[2] <= 1.05,
      label = sprintf("Yr%d Q2/Q1=%.3f (exp 1.0)", yr, rr[2]))
    expect_true(
      rr[3] >= 1.14 && rr[3] <= 1.26,
      label = sprintf("Yr%d Q3/Q1=%.3f (exp 1.2)", yr, rr[3]))
    expect_true(
      rr[4] >= 1.52 && rr[4] <= 1.68,
      label = sprintf("Yr%d Q4/Q1=%.3f (exp 1.6)", yr, rr[4]))
    expect_true(
      rr[5] >= 2.28 && rr[5] <= 2.52,
      label = sprintf("Yr%d Q5/Q1=%.3f (exp 2.4)", yr, rr[5]))
  }
})
