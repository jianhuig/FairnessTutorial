# -----------------------------------------------------------------------------
# Customized Multi-Group Fairness Function Implementation
# -----------------------------------------------------------------------------

#' Examine Maximum-Minimum Difference of a Model
#'
#' This function evaluates the maximum and minimum differences in model
#'  performance metrics across different groups.
#'
#' @param data Data frame containing the outcome, predicted outcome, and group.
#' @param outcome Name of the outcome variable, which must be binary.
#' @param group Name of the group variable.
#' @param probs Name of the predicted outcome variable.
#' @param cutoff Threshold for the predicted outcome, default is 0.5.
#' @param digits Number of digits to round the results to, default is 2.
#'
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Max-Min: The maximum minus minimum metric difference for the model.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile

eval_max_min_diff <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Max-Min difference for each metric
  metric$`Max-Min` <- apply(metric[,-1], 1, function(x) max(x) - min(x))

  return(metric)
}


#' Examine Maximum Minimum Ratio of a model
#'
#' This function evaluates the maximum and minimum ratio in model
#' performance metrics across different groups.
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Max/Min: The maximum over minimum metric difference for the model
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile

eval_max_min_ratio <- function(data, outcome, group, probs, cutoff = 0.5,
                               digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  metric$`Max/Min` <- apply(metric[,-1], 1, function(x) max(x) / min(x))

  return(metric)
}


#' Examine max absolute difference of a model
#'
#' This function evaluates the maximum absolute difference in model
#' performance metrics across different groups.
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Max_abs_diff: The maximum absolute difference for the model
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile

eval_max_abs_diff <- function(data, outcome, group, probs, cutoff = 0.5,
                              digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Max absolute difference for each metric
  metric$`Max_Abs_Diff` <- apply(metric[,-1], 1, function(x) round(abs(max(x) - mean(x)), digits))

  return(metric)
}


#' Examine mean absolute deviation of a model
#
#' #' This function evaluates the mean absolute deviation in model
#' performance metrics across different groups.
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Mean_abs_dev: The mean absolute deviation for the model
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom stats mad

eval_mean_abs_dev <- function(data, outcome, group, probs, cutoff = 0.5,
                              digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Mean Absolute Deviation for each metric
  metric$`Mean_Abs_Dev` <- apply(metric[,-1], 1, function(x) round(stats::mad(x, center = mean(x)), digits))

  return(metric)
}


#' Examine variance of a model
#
#' #' This function evaluates the variance in model
#' performance metrics across different groups.
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Variance: The variance for the model
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom stats var

eval_variance <- function(data, outcome, group, probs, cutoff = 0.5,
                          digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Variance for each metric
  metric$`Variance` <- apply(metric[,-1], 1, function(x) round(stats::var(x), digits))

  return(metric)
}


#' Examine Generalized Entropy Index of a model
#
#' #' This function evaluates the generalized entropy index in model
#' performance metrics across different groups.
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Generalized_entropy_index: The generalized entropy index for the model
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile

eval_generalized_entropy_index <- function(data, outcome, group, probs,
                                           alpha = 2,cutoff = 0.5, digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  if (alpha %in% c(0, 1)){
    stop("Alpha cannot be 0 or 1. Please choose another alpha")
  }

  # Calculate Generalized Entropy Index for each metric
  K <- length(unique(data[[group]]))
  metric$`Generalized_Entropy_Index` <- apply(metric[,-1], 1, function(x)
    round(1/(K * alpha * (alpha - 1)) * (sum((x / mean(x)) ^ alpha - 1)), digits))

  return(metric)
}
