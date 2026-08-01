test_that("ab_sample_size returns positive integer", {
  result <- ab_sample_size(
    p_current = 0.02, mde = 0.005, alpha = 0.05,
    power = 0.80
  )
  expect_true(result > 0)
  expect_true(is.integer(result) || is.numeric(result))
})

test_that("ab_sample_size matches stats::power.prop.test", {
  n_from_fn <- ab_sample_size(p_current = 0.02, mde = 0.005)
  base_result <- stats::power.prop.test(
    p1 = 0.02, p2 = 0.025, sig.level = 0.05, power = 0.80
  )
  n_from_base <- base_result$n
  expect_equal(n_from_fn, ceiling(n_from_base), tolerance = 1)
})

test_that("ab_sample_size increases with smaller MDE", {
  n_small_mde <- ab_sample_size(
    p_current = 0.02, mde = 0.001, alpha = 0.05,
    power = 0.80
  )
  n_large_mde <- ab_sample_size(
    p_current = 0.02, mde = 0.010, alpha = 0.05,
    power = 0.80
  )
  expect_true(n_small_mde > n_large_mde)
})

test_that("ab_freq_test detects a significant difference", {
  result <- ab_freq_test(
    conversions_control = 100, n_control = 10000,
    conversions_treatment = 150, n_treatment = 10000
  )
  expect_true(result$p_value < 0.05)
})

test_that("ab_freq_test returns non-significant for no difference", {
  result <- ab_freq_test(
    conversions_control = 100, n_control = 10000,
    conversions_treatment = 102, n_treatment = 10000
  )
  expect_true(result$p_value > 0.05)
})

test_that("ab_freq_test confidence interval excludes zero when significant", {
  result <- ab_freq_test(
    conversions_control = 100, n_control = 10000,
    conversions_treatment = 200, n_treatment = 10000
  )
  expect_true(result$ci[1] > 0 || result$ci[2] < 0)
})

test_that("ab_freq_test confidence interval contains zero when not
          significant", {
  result <- ab_freq_test(
    conversions_control = 100,
    n_control = 10000,
    conversions_treatment = 102,
    n_treatment = 10000
  )
  expect_true(result$ci[1] <= 0 && result$ci[2] >= 0)
})

test_that("ab_freq_test matches prop.test", {
  result <- ab_freq_test(
    conversions_control = 100, n_control = 10000,
    conversions_treatment = 150, n_treatment = 10000
  )
  base_result <- prop.test(
    x = c(150, 100), n = c(10000, 10000), correct = FALSE
  )
  expect_equal(result$p_value, base_result$p.value, tolerance = 0.01)
})
