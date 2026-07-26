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

test_that("ab_power returns numeric between 0 and 1", {
  result <- ab_power(
    n_total = 2000, p_current = 0.02, mde = 0.005,
    alpha = 0.05
  )
  expect_true(result >= 0 && result <= 1)
})

test_that("ab_power increases with larger sample size", {
  power_small_n <- ab_power(
    n_total = 200, p_current = 0.02, mde = 0.005,
    alpha = 0.05
  )
  power_large_n <- ab_power(
    n_total = 10000, p_current = 0.02, mde = 0.005,
    alpha = 0.05
  )
  expect_true(power_large_n > power_small_n)
})

test_that("ab_power returns high power when effect is large", {
  result <- ab_power(
    n_total = 2000, p_current = 0.02, mde = 0.48,
    alpha = 0.05
  )
  expect_true(result > 0.99)
})


test_that("ab_sample_size and ab_power are consistent", {
  n <- ab_sample_size(
    p_current = 0.02, mde = 0.005, alpha = 0.05,
    power = 0.80
  )
  result_power <- ab_power(
    n_total = n * 2, p_current = 0.02, mde = 0.005,
    alpha = 0.05
  )
  expect_equal(result_power, 0.80, tolerance = 0.02)
})
