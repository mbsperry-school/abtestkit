test_that("elicit_beta_prior uses quantile-matching for low conversion", {
  result <- elicit_beta_prior(
    guess = 0.02, worst_case = 0.01,
    best_case = 0.03
  )
  expect_equal(result$method, "quantile")
})

test_that("elicit_beta_prior uses moment-matching near 0.5", {
  result <- elicit_beta_prior(
    guess = 0.50, worst_case = 0.40,
    best_case = 0.60
  )
  expect_equal(result$method, "moment")
})

test_that("elicit_beta_prior uses quantile-matching for high conversion", {
  result <- elicit_beta_prior(
    guess = 0.08, worst_case = 0.07,
    best_case = 0.09
  )
  expect_equal(result$method, "quantile")
})

test_that("elicit_beta_prior 95% interval covers worst_case and best_case", {
  result <- elicit_beta_prior(
    guess = 0.02, worst_case = 0.01,
    best_case = 0.03
  )
  q_low <- qbeta(0.025, result$alpha, result$beta)
  q_high <- qbeta(0.975, result$alpha, result$beta)
  expect_true(abs(q_low - 0.01) < 0.005)
  expect_true(abs(q_high - 0.03) < 0.005)
})


test_that("update_beta increases alpha by number of successes", {
  prior <- list(alpha = 2, beta = 98)
  posterior <- update_beta(prior, successes = 10, trials = 100)
  expect_equal(posterior$alpha, 12)
})

test_that("update_beta increases beta by number of failures", {
  prior <- list(alpha = 2, beta = 98)
  posterior <- update_beta(prior, successes = 10, trials = 100)
  expect_equal(posterior$beta, 188)
})

test_that("update_beta with zero observations returns the prior", {
  prior <- list(alpha = 2, beta = 98)
  posterior <- update_beta(prior, successes = 0, trials = 0)
  expect_equal(posterior$alpha, prior$alpha)
  expect_equal(posterior$beta, prior$beta)
})


test_that("bayes_ab_summary detects obvious winner", {
  posterior_a <- list(alpha = 10, beta = 990)
  posterior_b <- list(alpha = 100, beta = 900)
  set.seed(42)
  result <- bayes_ab_summary(posterior_a, posterior_b, n_sims = 10000)
  expect_true(result$prob_treatment_better > 0.99)
})

test_that("bayes_ab_summary returns prob_treatment_better between 0 and 1", {
  posterior_a <- list(alpha = 50, beta = 950)
  posterior_b <- list(alpha = 55, beta = 945)
  set.seed(42)
  result <- bayes_ab_summary(posterior_a, posterior_b, n_sims = 10000)
  expect_true(result$prob_treatment_better >= 0 &&
    result$prob_treatment_better <= 1)
})

test_that("bayes_ab_summary returns near 0.5 for identical posteriors", {
  posterior_a <- list(alpha = 50, beta = 950)
  posterior_b <- list(alpha = 50, beta = 950)
  set.seed(42)
  result <- bayes_ab_summary(posterior_a, posterior_b, n_sims = 10000)
  expect_equal(result$prob_treatment_better, 0.5, tolerance = 0.05)
})
