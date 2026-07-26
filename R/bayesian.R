#' Form a Beta Prior
#'
#' This takes beliefs about the prior's values and turns them into a real
#' distribution. If the conversion rate is near .5, we use moment matching, if
#' it is not, we use quantile matching.
#'
#' @param guess expected conversion rate
#' @param worst_case lower bound of what you think the conversion rate is
#' @param best_case upper bound of what you think the conversion rate is
#'
#' @return A list containing:
#'   \item{alpha}{Alpha parameter of the Beta distribution}
#'   \item{beta}{Beta parameter of the Beta distribution}
#'
#' @export
#'
elicit_beta_prior <- function(guess, worst_case, best_case) {
  use_moment <- abs(guess - 0.5) <= 0.15

  if (use_moment) {
    mu <- guess
    sigma <- (best_case - worst_case) / 4


    concentration <- mu * (1 - mu) / sigma^2 - 1
    alpha <- mu * concentration
    beta <- (1 - mu) * concentration
    method <- "moment"
  } else {
    mu <- guess
    sigma <- (best_case - worst_case) / 4
    conc_init <- mu * (1 - mu) / sigma^2 - 1
    start <- c(mu * conc_init, (1 - mu) * conc_init)


    objective <- function(params) {
      a <- params[1]
      b <- params[2]

      if (a <= 0 || b <= 0) {
        return(1e6)
      }
      q_low <- qbeta(0.025, a, b)
      q_high <- qbeta(0.975, a, b)
      (q_low - worst_case)^2 + (q_high - best_case)^2
    }

    fit <- optim(start, objective,
      method = "L-BFGS-B",
      lower = c(0.001, 0.001)
    )
    alpha <- fit$par[1]
    beta <- fit$par[2]
    method <- "quantile"
  }

  list(alpha = alpha, beta = beta, method = method)
}

#' Beta Conjugate Update to get posterior
#' @export
update_beta <- function(prior, successes, trials) {
  failures <- trials - successes

  list(
    alpha = prior$alpha + successes,
    beta = prior$beta + failures
  )
}
#' Bayesian Proportion Test
#' Calculates the results of an AB test by analyzing updated or posterior
#' values of control and treatment results in experiment.
#'
#' @param posterior_control list containing information for distribution of
#' control data
#' @param posterior_treatment list containing information for distribution of
#' treatment data
#' @param n_sims number of monte carlo draws, or number of simulated
#' experiments to run. Only drawback of higher values is that it takes longer.

#' @export
bayes_ab_summary <- function(posterior_control, posterior_treatment,
                             n_sims = 10000) {
  draws_control <- rbeta(
    n_sims, posterior_control$alpha,
    posterior_control$beta
  )
  draws_treatment <- rbeta(
    n_sims, posterior_treatment$alpha,
    posterior_treatment$beta
  )

  lift <- draws_treatment - draws_control

  prob_treatment_better <- mean(draws_treatment > draws_control)

  expected_lift <- mean(lift)

  ci_a <- quantile(draws_control, probs = c(0.025, 0.975))
  ci_b <- quantile(draws_treatment, probs = c(0.025, 0.975))
  ci_lift <- quantile(lift, probs = c(0.025, 0.975))

  list(
    prob_treatment_better = prob_treatment_better,
    expected_lift = expected_lift,
    ci_a = ci_a,
    ci_b = ci_b,
    ci_lift = ci_lift
  )
}
