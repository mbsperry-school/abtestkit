# abtestkit

<!-- badges: start -->
[![R-CMD-check](https://github.com/mbsperry-school/abtestkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mbsperry-school/abtestkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

abtestkit plans and analyzes binary-outcome A/B tests, running frequentist and Bayesian approaches side by side from the same inputs.

## Installation

``` r
# install.packages("pak")
pak::pak("mbsperry-school/abtestkit")
```

## Example

``` r
library(abtestkit)

data(historical_data)
data(experiment_data)

# Plan: sample size (frequentist) and a Beta prior (Bayesian)
planning <- pre_experiment_comparison(
  mde = 0.01,
  data = historical_data,
  outcome_column_name = "converted"
)

# Analyze: z-test and posterior comparison
results <- post_experiment_comparison(
  data = experiment_data,
  outcome_column_name = "converted",
  group_column_name = "test.group",
  control_value = "psa",
  treatment_value = "ad",
  n = planning$n,
  experiment_prior = planning$experiment_prior
)

# Side-by-side plots and interpretation
full_comparison(
  freq_results = results$freq_results,
  bayes_results = results$bayes_results,
  posterior_control = results$posterior_control,
  posterior_treatment = results$posterior_treatment
)
```

See `vignette("abtestkit")` for a full walkthrough.
