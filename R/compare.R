#' Pre-Experiment Planning Comparison
#'
#' Runs both frequentist and Bayesian pre-test planning on historical data.
#' Uses the historical conversion rate to calculate the required sample size
#' (frequentist) and form a prior distribution (Bayesian).
#'
#' @param mde smallest change you want to detect in the experiment
#' @param alpha how often you are willing to falsely declare a winner when
#' there isn't one
#' @param power how often you want to correctly detect a real effect when it
#' exists
#' @param data a dataframe of historical (pre-experiment) data
#' @param outcome_column_name string name of the binary outcome column
#'
#' @return a list containing:
#'   \item{n}{sample size needed per group from frequentist planning}
#'   \item{experiment_prior}{Beta prior from Bayesian planning, a list with
#'   alpha, beta, and method}
#' @examples
#' data(historical_data)
#' pre_experiment_comparison(mde = .2, data = historical_data,
#' outcome_column_name = "converted")
#'
#' @export
pre_experiment_comparison <- function(mde, alpha = .05, power = .8, data,
                                      outcome_column_name) {
  # frequentist
  p_current <- sum(data[[outcome_column_name]]) /
    length(data[[outcome_column_name]])
  n <- ab_sample_size(
    p_current = p_current, mde = mde, alpha = alpha,
    power = power
  )

  # bayesian
  guess <- p_current
  previous_sd <- stats::sd(data[[outcome_column_name]])
  best_case <- min(guess + previous_sd, 0.999)
  worst_case <- max(guess - previous_sd, 0.001)
  experiment_prior <- elicit_beta_prior(
    guess = guess,
    worst_case = worst_case,
    best_case = best_case
  )

  list(n = n, experiment_prior = experiment_prior)
}


#' Prepare Experiment Data for Analysis
#'
#' Splits experiment data into control and treatment groups, takes the first
#' n observations from each, and counts conversions. This is a helper function.
#'
#' @param data a dataframe of experiment results
#' @param outcome_column_name string name of the binary outcome column
#' @param group_column_name string name of the column that identifies which
#' group each observation belongs to
#' @param control_value the value in group_column_name that marks control rows
#' @param treatment_value the value in group_column_name that marks treatment
#' rows
#' @param n how many observations to use from each group
#'
#' @return a list containing:
#'   \item{conversions_control}{number of conversions in the control group}
#'   \item{conversions_treatment}{number of conversions in the treatment group}
data_prep <- function(data, outcome_column_name, group_column_name,
                      control_value,
                      treatment_value, n) {
  control_group <- data[data[[group_column_name]] == control_value, ]
  control_first_n <- control_group[1:n, ]
  conversions_control <- sum(control_first_n[[outcome_column_name]])

  treatment_group <- data[data[[group_column_name]] == treatment_value, ]
  treatment_first_n <- treatment_group[1:n, ]
  conversions_treatment <- sum(treatment_first_n[[outcome_column_name]])

  list(
    conversions_control = conversions_control,
    conversions_treatment = conversions_treatment
  )
}

#' Post-Experiment Analysis Comparison
#'
#' Runs both frequentist and Bayesian analysis on experiment data. The
#' frequentist side runs a two-proportion z-test. The Bayesian side updates
#' the prior with observed data and compares the resulting posteriors.
#'
#' @param data a dataframe of experiment results
#' @param outcome_column_name string name of the binary outcome column
#' @param group_column_name string name of the column that identifies which
#' group each observation belongs to
#' @param control_value the value in group_column_name that marks control rows
#' @param treatment_value the value in group_column_name that marks treatment
#' rows
#' @param n how many observations to use from each group
#' @param alpha how often you are willing to declare a winner when there
#' isn't one
#' @param experiment_prior the prior from pre_experiment_comparison, a list
#' with alpha and beta
#'
#' @return a list containing:
#'   \item{freq_results}{output from ab_freq_test}
#'   \item{bayes_results}{output from bayes_ab_summary}
#' @examples
#' data(historical_data)
#' data(experiment_data)
#' experiment_prior <- list(alpha = 1, beta = 1, method = "quantile")
#'
#' post_experiment_data <- post_experiment_comparison(
#'   data = experiment_data,
#'   outcome_column_name = "converted",
#'   group_column_name = "test.group",
#'   control_value = "psa",
#'   treatment_value = "ad",
#'   n = 1000,
#'   experiment_prior = experiment_prior
#' )
#'
#' @export
post_experiment_comparison <- function(data, outcome_column_name,
                                       group_column_name,
                                       control_value, treatment_value,
                                       n, alpha = .05, experiment_prior) {
  prepped_data <- data_prep(
    data = data, outcome_column_name = outcome_column_name,
    group_column_name = group_column_name,
    control_value = control_value,
    treatment_value = treatment_value, n = n
  )
  freq_results <- ab_freq_test(
    conversions_control = prepped_data$conversions_control, n_control = n,
    conversions_treatment = prepped_data$conversions_treatment,
    n_treatment = n, alpha = alpha
  )


  posterior_control <- update_beta(
    prior = experiment_prior,
    successes = prepped_data$conversions_control,
    trials = n
  )
  posterior_treatment <- update_beta(
    prior = experiment_prior,
    successes = prepped_data$conversions_treatment,
    trials = n
  )

  bayes_results <- bayes_ab_summary(
    posterior_control = posterior_control,
    posterior_treatment = posterior_treatment
  )

  list(
    freq_results = freq_results,
    bayes_results = bayes_results,
    posterior_control = posterior_control,
    posterior_treatment = posterior_treatment
  )
}
