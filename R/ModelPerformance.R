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

#' Calculate the positive prediction rate
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of false positive rates
#' @importFrom magrittr %>%
#' @noRd

get_ppr <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate PPR
  result <- data %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(ppr = mean(!!probs_sym >= cutoff), .groups = "drop")

  return(result)
}


#' Calculate the conditional positive prediction rate
#' @param data Data frame containing the outcome, predicted outcome, and
#' two sensitive attributes
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param group2 the name of the conditional sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param group2_cutoff the threshold for the conditional sensitive attribute.
#' @return a vector of false positive rates
#' @importFrom magrittr %>%
#' @noRd

get_cond_ppr <- function(data, outcome, group, group2, probs, cutoff = 0.5, group2_cutoff) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  group2_sym <- rlang::sym(group2)
  probs_sym <- rlang::sym(probs)

  data <- data %>% mutate(group2AboveBelow = ifelse(group2_sym >= group2_cutoff, paste("Above ", group2_cutoff), paste("Below ", group2_cutoff)))

  # Calculate FPR
  result <- data %>%
    dplyr::group_by(!!group_sym, group2AboveBelow) %>%
  dplyr::summarize(ppr = mean(!!probs_sym >= cutoff), .groups = "drop")

  return(result)
}

#' Calculate the positive predictive value
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of true positive rates
#' @importFrom magrittr %>%
#' @noRd

get_ppv <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate TPR
  result <- data %>%
    dplyr::filter(!!probs >= cutoff) %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(ppv = mean(!!outcome_sym == 1), .groups = "drop")

  return(result)
}

#' Calculate the negative predictive value
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of true positive rates
#' @importFrom magrittr %>%
#' @noRd

get_npv <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate TPR
  result <- data %>%
    dplyr::filter(!!probs <= cutoff) %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(ppv = mean(!!outcome_sym == 0), .groups = "drop")

  return(result)
}

#' Calculate the accuracy
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of true positive rates
#' @importFrom magrittr %>%
#' @noRd

get_acc <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  data <- data %>% mutate(pred = ifelse(probs_sym >= cutoff, 1, 0))

  # Calculate TPR
  result <- data %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(ppv = mean(!!outcome_sym == pred), .groups = "drop")

  return(result)
}

#' Calculate the error ratio
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of true positive rates
#' @importFrom magrittr %>%
#' @noRd

get_err_ratio <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  data <- data %>% mutate(pred = ifelse(probs_sym >= cutoff, 1, 0))

  # Calculate TPR
  result <- data %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(error_ratio = sum(!!outcome_sym == 1 & pred == 0)/sum(!!outcome_sym == 0 & pred == 1), .groups = "drop")

  return(result)
}

#' Calculate the expected positive score
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of true positive rates
#' @importFrom magrittr %>%
#' @noRd

get_exp_pos <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate TPR
  result <- data %>%
    dplyr::filter(!!outcome_sym == 1)
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(exp_pos = mean(probs_sym), .groups = "drop")

  return(result)
}

#' Calculate the expected negative score
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of true positive rates
#' @importFrom magrittr %>%
#' @noRd

get_exp_neg <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate TPR
  result <- data %>%
    dplyr::filter(!!outcome_sym == 0)
  dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(exp_pos = mean(probs_sym), .groups = "drop")

  return(result)
}
