test_that("frequentist_visual runs without error", {
  fake_freq <- list(
    effect = 0.01, ci = c(0.002, 0.018), p_value = 0.01,
    significant = TRUE
  )
  expect_no_error(frequentist_visual(fake_freq))
})

test_that("bayesian_visual runs without error", {
  post_a <- list(alpha = 50, beta = 950)
  post_b <- list(alpha = 60, beta = 940)
  set.seed(42)
  expect_no_error(bayesian_visual(post_a, post_b))
})

test_that("both_visual runs without error", {
  fake_freq <- list(
    effect = 0.01, ci = c(0.002, 0.018), p_value = 0.01,
    significant = TRUE
  )
  post_a <- list(alpha = 50, beta = 950)
  post_b <- list(alpha = 60, beta = 940)
  set.seed(42)
  expect_no_error(both_visual(fake_freq, post_a, post_b))
})

test_that("interpret_results runs without error", {
  fake_freq <- list(
    effect = 0.01, ci = c(0.002, 0.018), p_value = 0.01,
    significant = TRUE
  )
  fake_bayes <- list(
    prob_treatment_better = 0.97, expected_lift = 0.01,
    ci_lift = c(0.002, 0.018)
  )
  expect_no_error(interpret_results(fake_freq, fake_bayes))
})

test_that("full_comparison runs without error", {
  fake_freq <- list(
    effect = 0.01, ci = c(0.002, 0.018), p_value = 0.01,
    significant = TRUE
  )
  fake_bayes <- list(
    prob_treatment_better = 0.97, expected_lift = 0.01,
    ci_lift = c(0.002, 0.018)
  )
  post_a <- list(alpha = 50, beta = 950)
  post_b <- list(alpha = 60, beta = 940)
  set.seed(42)
  expect_no_error(full_comparison(fake_freq, fake_bayes, post_a, post_b))
})
