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
#' @return a vector of true positive rates
#' @noRd

get_tpr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  tpr <- c()  # Initialize empty vector for true positive rates
  groups <- sort(unique(data[, group]))  # Get sorted unique groups

  for (i in groups) {
    tp <- sum(data[, outcome] == 1 & data[, group] == i & data[, probs] >= cutoff)
    p <- sum(data[, outcome] == 1 & data[, group] == i)
    tpr <- c(tpr, round(tp / p, digits))  # Calculate TPR and add to vector
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
#' @return a vector of false positive rates
#' @noRd

get_fpr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  fpr <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    fp <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] >= cutoff)
    p <- sum(data[, outcome] == 1 & data[, group] == i)
    fpr <- c(fpr, round(fp / p, digits))
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
#' @return a vector of probability of positive prediction
#' @noRd

get_ppr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  ppr <- c()
  for (i in unique(data[, group])) {
    pp <- sum(
      data[, group] == i &
        data[, probs] >= cutoff
    )
    n <- sum(data[, group] == i)
    ppr <- c(ppr, round(pp / n, digits))
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
#' @return a vector of positive predictive value
#' @noRd

get_ppv <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  ppv <- c()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    pp <- sum(data[, group] == i &
      data[, probs] >= cutoff)
    ppv <- c(ppv, round(tp / pp, digits))
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
#' @return  a vector of negative predictive value
#' @noRd

get_npv <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  npv <- c()
  for (i in unique(data[, group])) {
    tn <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] < cutoff)
    nn <- sum(data[, group] == i &
      data[, probs] < cutoff)
    npv <- c(npv, round(tn / nn, digits))
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
#' @return a vector of accuracy
#' @noRd

get_acc <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  acc <- c()
  for (i in unique(data[, group])) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff)
    tn <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] < cutoff)
    p <- sum(data[, group] == i)
    acc <- c(acc, round((tp + tn) / p, digits))
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
#' @return a vector of brier score
#' @noRd

get_brier_score <- function(data, outcome, group, probs, digits = 2) {
  brier_score <- c()
  for (i in unique(data[, group])) {
    sub_data <- data[data[, group] == i, ]
    brier_score <- c(brier_score, round(mean((sub_data[, outcome] - sub_data[, probs])^2), digits))
  }
  return(brier_score)
}

#' Calculate the the ratio of false negative to false positive
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of the ratio of false negative to false positive
#' @noRd

get_err_ratio <- function(data, outcome, group, probs, cutoff = 0.5,
                          digits = 2) {
  err_ratio <- c()
  for (i in unique(data[, group])) {
    sub_data <- data[data[, group] == i, ]
    fp <- sum(sub_data[, outcome] == 0 &
      sub_data[, probs] >= cutoff)
    fn <- sum(sub_data[, outcome] == 1 &
      sub_data[, probs] < cutoff)
    err_ratio <- c(err_ratio, round(fn / fp, digits))
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
#' @return a vector of average predicted probability
#' @noRd

get_avg_prob <- function(data, outcome, group, probs, digits = 2) {
  avg_prob <- c()
  for (i in unique(data[, group])) {
    sub_data <- data[data[, group] == i, ]
    avg_prob <- c(avg_prob, round(mean(sub_data[, probs]), digits))
  }
  return(avg_prob)
}

#' Calculate the all metrics at once
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a Data frame of all metrics

get_all_metrics <- function(data, outcome, group, probs, cutoff = 0.5,
                            digits = 2) {
  all_metrics <- list()  # Use a list to store metrics temporarily
  metrics_names <- c("TPR", "FPR", "PPR", "PPV", "NPV", "ACC", "Brier Score",
                     "FN/FP Ratio", "Avg Pred Prob")

  # Append each metric to the list
  all_metrics[[1]] <- get_tpr(data, outcome, group, probs, cutoff, digits)
  all_metrics[[2]] <- get_fpr(data, outcome, group, probs, cutoff, digits)
  all_metrics[[3]] <- get_ppr(data, outcome, group, probs, cutoff, digits)
  all_metrics[[4]] <- get_ppv(data, outcome, group, probs, cutoff, digits)
  all_metrics[[5]] <- get_npv(data, outcome, group, probs, cutoff, digits)
  all_metrics[[6]] <- get_acc(data, outcome, group, probs, cutoff, digits)
  all_metrics[[7]] <- get_brier_score(data, outcome, group, probs, digits)
  all_metrics[[8]] <- get_err_ratio(data, outcome, group, probs, cutoff, digits)
  all_metrics[[9]] <- get_avg_prob(data, outcome, group, probs, digits)

  # Convert the list of metrics into a data frame
  all_metrics_df <- do.call(rbind, all_metrics)
  rownames(all_metrics_df) <- metrics_names  # Assign metric names as row names

  # Create column names based on unique groups
  group_names <- paste0("Group ", sort(unique(data[, group])))
  colnames(all_metrics_df) <- group_names

  return(all_metrics_df)
}
