#' Experiment Data
#'
#' A sample of 5,000 users per arm from a Kaggle marketing dataset where users
#' were exposed to either an advertisement or public service announcement, with
#' conversion recorded as the outcome.
#'
#' @format A dataframe with 10,000 rows and 2 columns:
#' \describe{
#' \item{test.group}{Treatment group: "ad" or "psa"}
#' \item{converted}{Outcome of 1 or 0}}
#'
#' @source \url{https://www.kaggle.com/datasets/faviovaz/marketing-ab-testing}
"experiment_data"

#' Historical Data
#'
#' The first 1,000 PSA users from the Kaggle marketing dataset, used as
#' pre-experiment baseline data to inform prior beliefs about conversion rates.
#'
#' @format A dataframe with 1,000 rows and 2 columns:
#' \describe{
#' \item{test.group}{Treatment group: "psa"}
#' \item{converted}{Outcome of 1 or 0}}
#'
#' @source \url{https://www.kaggle.com/datasets/faviovaz/marketing-ab-testing}
"historical_data"
