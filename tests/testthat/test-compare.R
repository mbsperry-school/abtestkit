test_that("pre_experiment_comparison returns a sample size and prior", {
  fake_data <- data.frame(converted = c(rep(1, 20), rep(0, 980)))
  result <- pre_experiment_comparison(mde = 0.01, data = fake_data,
                                      outcome_column_name = "converted")
  expect_true(result$n > 0)
  expect_true(result$experiment_prior$alpha > 0)
  expect_true(result$experiment_prior$beta > 0)
})

test_that("pre_experiment_comparison sample size increases with smaller mde", {
  fake_data <- data.frame(converted = c(rep(1, 20), rep(0, 980)))
  small_mde <- pre_experiment_comparison(mde = 0.001, data = fake_data,
                                         outcome_column_name = "converted")
  large_mde <- pre_experiment_comparison(mde = 0.05, data = fake_data,
                                         outcome_column_name = "converted")
  expect_true(small_mde$n > large_mde$n)
})

test_that("pre_experiment_comparison works with low conversion rates", {
  fake_data <- data.frame(converted = c(rep(1, 5), rep(0, 995)))
  result <- pre_experiment_comparison(mde = 0.01, data = fake_data,
                                      outcome_column_name = "converted")
  expect_true(result$experiment_prior$alpha > 0)
  expect_true(result$experiment_prior$beta > 0)
})

test_that("data_prep counts conversions correctly", {
  fake_data <- data.frame(
    group = c(rep("control", 100), rep("treatment", 100)),
    converted = c(rep(1, 10), rep(0, 90), rep(1, 20), rep(0, 80))
  )
  result <- data_prep(data = fake_data, outcome_column_name = "converted",
                      group_column_name = "group",
                      control_value = "control",
                      treatment_value = "treatment", n = 100)
  expect_equal(result$conversions_control, 10)
  expect_equal(result$conversions_treatment, 20)
})

test_that("data_prep gets n for result", {
  fake_data <- data.frame(
    group = c(rep("control", 100), rep("treatment", 100)),
    converted = c(rep(1, 50), rep(0, 50), rep(1, 50), rep(0, 50))
  )
  result <- data_prep(data = fake_data, outcome_column_name = "converted",
                      group_column_name = "group",
                      control_value = "control",
                      treatment_value = "treatment", n = 10)
  expect_equal(result$conversions_control, 10)
  expect_equal(result$conversions_treatment, 10)
})

test_that("post_experiment_comparison returns freq and bayes results", {
  fake_data <- data.frame(
    group = c(rep("control", 1000), rep("treatment", 1000)),
    converted = c(rep(1, 20), rep(0, 980), rep(1, 40), rep(0, 960))
  )
  prior <- list(alpha = 1, beta = 49)
  result <- post_experiment_comparison(
    data = fake_data, outcome_column_name = "converted",
    group_column_name = "group", control_value = "control",
    treatment_value = "treatment", n = 1000,
    experiment_prior = prior
  )
  expect_true(!is.null(result$freq_results))
  expect_true(!is.null(result$bayes_results))
  expect_true(!is.null(result$posterior_control))
  expect_true(!is.null(result$posterior_treatment))
})

test_that("post_experiment_comparison detects obvious difference", {
  fake_data <- data.frame(
    group = c(rep("control", 1000), rep("treatment", 1000)),
    converted = c(rep(1, 10), rep(0, 990), rep(1, 100), rep(0, 900))
  )
  prior <- list(alpha = 1, beta = 99)
  result <- post_experiment_comparison(
    data = fake_data, outcome_column_name = "converted",
    group_column_name = "group", control_value = "control",
    treatment_value = "treatment", n = 1000,
    experiment_prior = prior
  )
  expect_true(result$freq_results$significant)
  expect_true(result$bayes_results$prob_treatment_better > 0.95)
})

test_that("post_experiment_comparison freq and bayes agree on no effect", {
  set.seed(42)
  fake_data <- data.frame(
    group = c(rep("control", 1000), rep("treatment", 1000)),
    converted = c(rbinom(1000, 1, 0.05), rbinom(1000, 1, 0.05))
  )
  prior <- list(alpha = 1, beta = 19)
  result <- post_experiment_comparison(
    data = fake_data, outcome_column_name = "converted",
    group_column_name = "group", control_value = "control",
    treatment_value = "treatment", n = 1000,
    experiment_prior = prior
  )
  expect_false(result$freq_results$significant)
  expect_true(result$bayes_results$prob_treatment_better < 0.95)
})
