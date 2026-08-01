#' Frequentist Results Visual
#'
#' Plots the observed effect size with a confidence interval.
#'
#' @param freq_results Output from ab_freq_test
#'
frequentist_visual <- function(freq_results) {
  effect <- freq_results$effect
  ci <- freq_results$ci

  graphics::plot(effect, 1,
    xlim = c(min(ci[1], 0) - 0.005, max(ci[2], 0) + 0.005),
    ylim = c(0.5, 1.5),
    pch = 19, cex = 2,
    xlab = "Difference in Conversion Rate",
    ylab = "",
    main = "Frequentist: Effect Size and 95% CI",
    yaxt = "n"
  )

  graphics::segments(ci[1], 1, ci[2], 1, lwd = 3)

  graphics::segments(ci[1], 0.95, ci[1], 1.05, lwd = 2)
  graphics::segments(ci[2], 0.95, ci[2], 1.05, lwd = 2)

  graphics::abline(v = 0, lty = 2, col = "red")
}

#' Bayesian Results Visual
#'
#' Plots the posterior distribution of the treatment - control.
#'
#' @param posterior_control List with alpha and beta for control posterior
#' @param posterior_treatment List with alpha and beta for treatment posterior
#' @param n_sims Number of simulated draws (default 10000)
#'

bayesian_visual <- function(posterior_control, posterior_treatment,
                            n_sims = 10000) {
  draws_control <- stats::rbeta(
    n_sims, posterior_control$alpha,
    posterior_control$beta
  )
  draws_treatment <- stats::rbeta(
    n_sims, posterior_treatment$alpha,
    posterior_treatment$beta
  )
  lift <- draws_treatment - draws_control

  graphics::hist(lift,
    breaks = 80,
    col = "steelblue",
    border = "white",
    main = "Bayesian: Posterior Distribution of Lift",
    xlab = "Difference in Conversion Rate (Treatment - Control)",
    ylab = "Frequency",
    prob = TRUE
  )

  graphics::abline(v = 0, lty = 2, col = "red", lwd = 2)

  graphics::abline(v = mean(lift), lty = 1, col = "black", lwd = 2)

  ci <- stats::quantile(lift, probs = c(0.025, 0.975))
  graphics::abline(v = ci[1], lty = 3, col = "darkblue", lwd = 2)
  graphics::abline(v = ci[2], lty = 3, col = "darkblue", lwd = 2)
  graphics::legend("topright",
    legend = c("Mean", "Zero", "95% Credible Interval"),
    col = c("black", "red", "darkblue"),
    lty = c(1, 2, 3),
    lwd = 2
  )
}

#' Side-by-Side Frequentist and Bayesian Visuals
#'
#' @param freq_results Output from ab_freq_test
#' @param posterior_control List with alpha and beta for control posterior
#' @param posterior_treatment List with alpha and beta for treatment posterior
#' @param n_sims Number of simulated draws (default 10000)
#'
both_visual <- function(freq_results, posterior_control,
                        posterior_treatment, n_sims = 10000) {
  old_par <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(old_par))
  frequentist_visual(freq_results)
  bayesian_visual(posterior_control, posterior_treatment, n_sims)
}


#' Interpret A/B Test Results
#'
#' Prints a guide comparing frequentist and Bayesian results,
#' explaining what each conclusion means and the difference between
#' confidence and credible intervals.
#'
#' @param freq_results Output from ab_freq_test
#' @param bayes_results Output from bayes_ab_summary
#' @param alpha Significance level used (default 0.05)
#'
interpret_results <- function(freq_results, bayes_results, alpha = 0.05) {
  cat("=== A/B Test Results Interpretation ===\n\n")

  cat(sprintf(
    paste0(
      "-- Frequentist Results --\n",
      "Observed effect: %.4f\n",
      "p-value: %.4f\n",
      "95%% Confidence Interval: [%.4f, %.4f]\n",
      "The frequentist confidence interval says: if we repeated this\n",
      "experiment many times, 95%% of the intervals we compute would\n",
      "contain the true effect.\n\n"
    ),
    freq_results$effect,
    freq_results$p_value,
    freq_results$ci[1],
    freq_results$ci[2]
  ))

  if (freq_results$significant) {
    cat(sprintf(
      paste0(
        "The result IS statistically significant at alpha = %g\n",
        "This means: if there were truly no difference between groups,\n",
        "we would see data this extreme less than %g%% of the time.\n",
        "We reject the null hypothesis that the groups are the same.\n\n"
      ),
      alpha,
      alpha * 100
    ))
  } else {
    cat(sprintf(
      paste0(
        "The result is NOT statistically significant at alpha = %g\n",
        "This means: we cannot rule out that the observed difference\n",
        "is due to chance alone. We fail to reject the null hypothesis.\n\n"
      ),
      alpha
    ))
  }

  cat(sprintf(
    paste0(
      "-- Bayesian Results --\n",
      "P(treatment > control): %.4f\n",
      "Expected lift: %.4f\n",
      "95%% Credible Interval for lift: [%.4f, %.4f]\n",
      "The Bayesian credible interval says: given our prior beliefs and\n",
      "the observed data, there is a 95%% probability that the true\n",
      "effect falls inside this interval.\n\n"
    ),
    bayes_results$prob_treatment_better,
    bayes_results$expected_lift,
    bayes_results$ci_lift[1],
    bayes_results$ci_lift[2]
  ))

  prob_pct <- bayes_results$prob_treatment_better * 100

  if (bayes_results$prob_treatment_better > 0.95) {
    cat(sprintf(
      paste0(
        "The Bayesian analysis gives strong evidence that treatment is\n",
        "better. There is a %.1f%% probability that the treatment group\n",
        "has a higher conversion rate.\n\n"
      ),
      prob_pct
    ))
  } else if (bayes_results$prob_treatment_better > 0.80) {
    cat(sprintf(
      paste0(
        "The Bayesian analysis gives moderate evidence that treatment is\n",
        "better. There is a %.1f%% probability that the treatment group\n",
        "has a higher conversion rate.\n",
        "More data may be needed to reach a stronger conclusion.\n\n"
      ),
      prob_pct
    ))
  } else {
    cat(sprintf(
      paste0(
        "The Bayesian analysis does not give strong evidence either way.\n",
        "There is only a %.1f%% probability that treatment is better.\n\n"
      ),
      prob_pct
    ))
  }

  cat("-- Do the two methods agree? --\n\n")

  freq_positive <- freq_results$significant && freq_results$effect > 0
  bayes_positive <- bayes_results$prob_treatment_better > 0.95

  if (freq_positive && bayes_positive) {
    cat(
      "Yes. Both methods agree that the treatment group performed better.\n",
      "The frequentist test found a significant positive effect, and the\n",
      "Bayesian analysis assigns a high probability to treatment being\n",
      "better.\n",
      sep = ""
    )
  } else if (!freq_positive && !bayes_positive) {
    cat(
      "Yes. Neither method found strong evidence that treatment is better.\n",
      "The frequentist test was not significant, and the Bayesian analysis\n",
      "does not assign high probability to treatment winning.\n",
      sep = ""
    )
  } else if (freq_positive && !bayes_positive) {
    cat(
      "They disagree. The frequentist test found significance, but the\n",
      "Bayesian analysis is less convinced. This can happen when the prior\n",
      "is skeptical of the effect or the effect is right at the boundary\n",
      "of significance.\n",
      sep = ""
    )
  } else {
    cat(
      "They disagree. The Bayesian analysis favors treatment, but the\n",
      "frequentist test did not reach significance. This can happen when\n",
      "the prior already leaned toward an effect, giving the Bayesian\n",
      "method a head start the frequentist method does not have.\n",
      sep = ""
    )
  }
}

#' Full A/B Test Comparison
#'
#' Displays side-by-side visuals and prints interpretation guide.
#'
#' @param freq_results Output from ab_freq_test
#' @param bayes_results Output from bayes_ab_summary
#' @param posterior_control List with alpha and beta for control posterior
#' @param posterior_treatment List with alpha and beta for treatment posterior
#' @param alpha Significance level (default 0.05)
#' @param n_sims Number of simulated draws (default 10000)
#'
#' @examples
#' freq <- ab_freq_test(
#'   conversions_control = 100, n_control = 5000,
#'   conversions_treatment = 150, n_treatment = 5000
#' )
#' prior <- elicit_beta_prior(
#'   guess = 0.02, worst_case = 0.01,
#'   best_case = 0.03
#' )
#' post_ctrl <- update_beta(prior, successes = 100, trials = 5000)
#' post_trt <- update_beta(prior, successes = 150, trials = 5000)
#' bayes <- bayes_ab_summary(post_ctrl, post_trt)
#' full_comparison(freq, bayes, post_ctrl, post_trt)
#'
#' @export
full_comparison <- function(freq_results, bayes_results,
                            posterior_control, posterior_treatment,
                            alpha = 0.05, n_sims = 10000) {
  both_visual(freq_results, posterior_control, posterior_treatment, n_sims)
  interpret_results(freq_results, bayes_results, alpha)
}
