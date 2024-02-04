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
#' @return a list of true positive rates
#' @noRd

get_tpr <- function(data, outcome, group, probs, cutoff = 0.5) {
  tpr <- list()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    p <- sum(data[, outcome] == 1 & data[, group] == i)
    tpr[[paste0("TPR_", i)]] <- tp / p
  }
  return(tpr)
}

#' Calculate the false positive rate
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a list of false positive rates
#' @noRd

get_fpr <- function(data, outcome, group, probs, cutoff = 0.5) {
  fpr <- list()
  for (i in unique(data[, group])) {
    fp <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] >= cutoff)
    p <- sum(data[, outcome] == 1 & data[, group] == i)
    fpr[[paste0("FPR_", i)]] <- fp / p
  }
  return(fpr)
}

#' Calculate the probability of positive prediction
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a list of probability of positive prediction
#' @noRd

get_ppr <- function(data, outcome, group, probs, cutoff = 0.5) {
  ppr <- list()
  for (i in unique(data[, group])) {
    pp <- sum(
      data[, group] == i &
        data[, probs] >= cutoff
    )
    n <- sum(data[, group] == i)
    ppr[[paste0("PPR_", i)]] <- pp / n
  }
  return(ppr)
}

#' Calculate the positive predictive value
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a list of positive predictive value
#' @noRd

get_ppv <- function(data, outcome, group, probs, cutoff = 0.5) {
  ppv <- list()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    pp <- sum(data[, group] == i &
      data[, probs] >= cutoff)
    ppv[[paste0("PPV_", i)]] <- tp / pp
  }
  return(ppv)
}

#' Calculate the negative predictive value
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return  a list of negative predictive value
#' @noRd

get_npv <- function(data, outcome, group, probs, cutoff = 0.5) {
  npv <- list()
  for (i in unique(data[, group])) {
    tn <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] < cutoff)
    nn <- sum(data[, group] == i &
      data[, probs] < cutoff)
    npv[[paste0("NPV_", i)]] <- tn / nn
  }
  return(npv)
}

#' Calculate the accuracy
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a list of accuracy
#' @noRd

get_acc <- function(data, outcome, group, probs, cutoff = 0.5) {
  acc <- list()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    tn <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] < cutoff)
    p <- sum(data[, group] == i)
    acc[[paste0("ACC_", i)]] <- (tp + tn) / p
  }
  return(acc)
}

#' Calculate the brier score
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a list of brier score
#' @noRd

get_brier_score <- function(data, outcome, group, probs, cutoff = 0.5) {
  brier_score <- list()
  for (i in unique(data[, group])) {
    sub_data <- data[data[, group] == i, ]
    brier_score[[paste0("Brier_", i)]] <-
      mean((sub_data[, outcome] - sub_data[, probs])^2)
  }
  return(brier_score)
}

#' Calculate the error ratio
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of error ratio and the difference
#' @importFrom magrittr %>%
#' @noRd

get_err_ratio <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate Error Ratio
  result <- data %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(
      err_ratio =
        sum(!!outcome_sym == 1 & !!probs_sym < cutoff) /
          sum(!!outcome_sym == 0 & !!probs_sym >= cutoff),
      .groups = "drop"
    ) %>%
    dplyr::mutate(err_ratio_diff = abs(diff(err_ratio)))

  return(result)
}

#' Calculate the expected positive score
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of expected positive score and the difference
#' @importFrom magrittr %>%
#' @noRd

get_exp_pos <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate expected positive score
  result <- data %>%
    dplyr::filter(!!outcome_sym == 1) %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(exp_pos = mean(!!probs_sym), .groups = "drop") %>%
    dplyr::mutate(expected_positive_diff = abs(diff(exp_pos)))

  return(result)
}

#' Calculate the expected negative score
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @return a vector of expected negative score and the difference
#' @importFrom magrittr %>%
#' @noRd

get_exp_neg <- function(data, outcome, group, probs, cutoff = 0.5) {
  # Convert strings to symbols if necessary
  outcome_sym <- rlang::sym(outcome)
  group_sym <- rlang::sym(group)
  probs_sym <- rlang::sym(probs)

  # Calculate expected negative score
  result <- data %>%
    dplyr::filter(!!outcome_sym == 0) %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarize(exp_neg = mean(!!probs_sym), .groups = "drop") %>%
    dplyr::mutate(expeced_negative_diff = abs(diff(exp_neg)))

  return(result)
}
