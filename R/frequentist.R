#' Frequentist Sample Size Estimation
#'
#' This function will calculate how many observations are needed to detect a
#' minimum change. It also controls for the error rates that you are willing to
#' accept.
#'
#' @param p_current current conversion rate. This value should be from 0-1
#' @param mde "Minimum detectable effect" this is the smallest change that you
#' would like to see in the data
#' @param alpha how often you are willing to falsely declare a winner when
#' there isn't one. Lower means you have a stronger test.
#' @param power how often you want to correctly detect a real effect when it
#' exists. higher means you have a stronger test.
#'
#' @return An integer value of how many observations you need PER GROUP
#'
#' @examples
#' ab_sample_size(p_current = 0.02, mde = 0.005)
#' @export
ab_sample_size <- function(p_current, mde, alpha = 0.05, power = 0.80) {
  p_treatment <- p_current + mde

  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_power <- stats::qnorm(power)

  p_pooled <- (p_current + p_treatment) / 2

  n <- 2 * (z_alpha + z_power)^2 * p_pooled * (1 - p_pooled) / (mde^2)

  ceiling(n)
}


#' Frequentist Proportion Z-test
#'
#' This calculates the results of an AB test using a two-proportion z-test.
#' @param conversions_control how many conversions occurred in the control group
#' @param n_control how many observations were in the control group
#' @param conversions_treatment how many conversions occurred in the test group
#' @param n_treatment how many observations were in the test group
#' @param alpha how often you are willing to falsely declare a winner when
#' there isn't one.
#'
#' @return a list containing:
#'   \item{effect}{Observed difference (treatment - control)}
#'   \item{p_value}{Two-sided p-value from the z-test}
#'   \item{ci}{Confidence interval for the difference in proportions}
#'   \item{significant}{Logical. TRUE if p_value < alpha}
#'
#' @examples
#' ab_freq_test(
#'   conversions_control = 100, n_control = 5000,
#'   conversions_treatment = 150, n_treatment = 5000
#' )
#'
#' @export
ab_freq_test <- function(conversions_control, n_control,
                         conversions_treatment, n_treatment, alpha = 0.05) {
  p_control <- conversions_control / n_control
  p_treatment <- conversions_treatment / n_treatment

  effect <- p_treatment - p_control

  p_pooled <- (conversions_control + conversions_treatment) /
    (n_control + n_treatment)

  se <- sqrt(p_pooled * (1 - p_pooled) * (1 / n_control + 1 / n_treatment))

  z <- effect / se

  p_value <- 2 * stats::pnorm(-abs(z))

  se_unpooled <- sqrt(
    p_control * (1 - p_control) / n_control +
      p_treatment * (1 - p_treatment) / n_treatment
  )
  z_alpha <- stats::qnorm(1 - alpha / 2)
  ci <- c(effect - z_alpha * se_unpooled, effect + z_alpha * se_unpooled)

  list(
    effect = effect,
    p_value = p_value,
    ci = ci,
    significant = p_value < alpha
  )
}
