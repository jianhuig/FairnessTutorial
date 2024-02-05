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
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of true positive rates
#' @noRd

get_tpr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  tpr <- list()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    p <- sum(data[, outcome] == 1 & data[, group] == i)
    tpr[[paste0("TPR_", i)]] <- round(tp / p, digits)
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
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of false positive rates
#' @noRd

get_fpr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  fpr <- list()
  for (i in unique(data[, group])) {
    fp <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] >= cutoff)
    p <- sum(data[, outcome] == 1 & data[, group] == i)
    fpr[[paste0("FPR_", i)]] <- round(fp / p, digits)
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
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of probability of positive prediction
#' @noRd

get_ppr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  ppr <- list()
  for (i in unique(data[, group])) {
    pp <- sum(
      data[, group] == i &
        data[, probs] >= cutoff
    )
    n <- sum(data[, group] == i)
    ppr[[paste0("PPR_", i)]] <- round(pp / n, digits)
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
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of positive predictive value
#' @noRd

get_ppv <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  ppv <- list()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    pp <- sum(data[, group] == i &
      data[, probs] >= cutoff)
    ppv[[paste0("PPV_", i)]] <- round(tp / pp, digits)
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
#' @param digits the number of digits to round the result to, default is 2
#' @return  a list of negative predictive value
#' @noRd

get_npv <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  npv <- list()
  for (i in unique(data[, group])) {
    tn <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] < cutoff)
    nn <- sum(data[, group] == i &
      data[, probs] < cutoff)
    npv[[paste0("NPV_", i)]] <- round(tn / nn, digits)
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
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of accuracy
#' @noRd

get_acc <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  acc <- list()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    tn <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] < cutoff)
    p <- sum(data[, group] == i)
    acc[[paste0("ACC_", i)]] <- round((tp + tn) / p, digits)
  }
  return(acc)
}

#' Calculate the brier score
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of brier score
#' @noRd

get_brier_score <- function(data, outcome, group, probs, digits = 2) {
  brier_score <- list()
  for (i in unique(data[, group])) {
    sub_data <- data[data[, group] == i, ]
    brier_score[[paste0("Brier_", i)]] <-
      round(mean((sub_data[, outcome] - sub_data[, probs])^2), digits)
  }
  return(brier_score)
}

#' Calculate the the raitio of false negative to false positive
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of the ratio of false negative to false positive
#' @noRd

get_err_ratio <- function(data, outcome, group, probs, cutoff = 0.5,
                          digits = 2) {
  err_ratio <- list()
  for (i in unique(data[, group])) {
    sub_data <- data[data[, group] == i, ]
    fp <- sum(sub_data[, outcome] == 0 &
      sub_data[, probs] >= cutoff)
    fn <- sum(sub_data[, outcome] == 1 &
      sub_data[, probs] < cutoff)
    err_ratio[[paste0("FN/FP_", i)]] <- round(fn / fp, digits)
  }
  return(err_ratio)
}

#' Calculate the average predicted probability
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param digits the number of digits to round the result to, default is 2
#' @return a list of average predicted probability
#' @noRd

get_avg_prob <- function(data, outcome, group, probs, digits = 2) {
  avg_prob <- list()
  for (i in unique(data[, group])) {
    sub_data <- data[data[, group] == i, ]
    avg_prob[[paste0("Avg_Predicted_Prob_", i)]] <-
      round(mean(sub_data[, probs]), digits)
  }
  return(avg_prob)
}
