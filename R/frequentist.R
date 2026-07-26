#' Frequentist Sample Size Estimation
#' This is the inverse of ab_power. This function will calculate how many
#' observations are needed to detect a minimum change. It also controls for the
#' error rates that you are willing to accept.
#'
#' @param p_current current conversion rate. This value should be from 0-1
#' @param mde "Minimum detectable effect" this is the smallest change that you
#' would like to see in the data
#' @param alpha how often you are willing to falsely declare a winner when
#' there isn't one. Lower means you have a stronger test.
#' @param power how often you want to correctly detect a real effect when it
#' exists. higher means you have a stronger test.
#'
#' @return An integer value of how many obersvations you need PER GROUP
#'
#' @export
ab_sample_size <- function(p_current, mde, alpha = 0.05, power = 0.80) {
  p_treatment <- p_current + mde

  z_alpha <- qnorm(1 - alpha / 2)
  z_power <- qnorm(power)

  p_pooled <- (p_current + p_treatment) / 2

  n <- 2 * (z_alpha + z_power)^2 * p_pooled * (1 - p_pooled) / (mde^2)

  ceiling(n)
}

#' Frequentist Power Estimation
#'
#' This is the inverse of ab_sample_size. This will calculate the probability
#' of detecting an effect given how much data you will have.
#'
#' @param n_total total number of observations you will have across both
#' control and treatment
#' @param p_current current conversion rate. This value should be from 0-1
#' @param mde "Minimum detectable effect" this is the smallest change that you
#' would like to see in the data. The idea is that a change of anything less
#' than this would indicate the experiment did not have a large enough effect
#' and you would roll back the changes
#' @param alpha how often you are willing to falsely declare a winner when
#' there isn't one
#' @return a double that is how often you want to correctly detect a real
#' effect when it exists. higher is better.
#' @export
ab_power <- function(n_total, p_current, mde, alpha = 0.05) {
  n_per_group <- n_total / 2

  p_treatment <- p_current + mde

  z_alpha <- qnorm(1 - alpha / 2)
  p_pooled <- (p_current + p_treatment) / 2

  var_null <- p_pooled * (1 - p_pooled) * (2 / n_per_group)
  var_alt <- (p_current * (1 - p_current) +
    p_treatment * (1 - p_treatment)) / n_per_group
  se_alt <- sqrt(var_alt)
  z_beta <- (mde - z_alpha * sqrt(var_null)) / se_alt
  power <- pnorm(z_beta)

  power
}
