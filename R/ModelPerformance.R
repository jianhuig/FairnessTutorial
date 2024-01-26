# -----------------------------------------------------------------------------
# Model Performance Metrics
# -----------------------------------------------------------------------------

#' Calculate the true positive rate
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of true positive rates
#' @importFrom magrittr %>%
#' @noRd

get_tpr <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate TPR
  result <- data %>%
    dplyr::filter(!!outcome_sym == 1) %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(tpr = mean(!!probs_sym >= cutoff), .groups = "drop")

  return(result)
}

#' Calculate the false positive rate
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of false positive rates
#' @importFrom magrittr %>%
#' @noRd

get_fpr <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate FPR
  result <- data %>%
    dplyr::filter(!!outcome_sym == 0) %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(fpr = mean(!!probs_sym >= cutoff), .groups = "drop")

  return(result)
}
