# -----------------------------------------------------------------------------
# Customized Multi-Group Fairness Function Implementation
# -----------------------------------------------------------------------------

#' Examine maximum minimum difference of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param get_metric Function of the model performance metric used to compute multi-group metrics
#' The input should take in: data, outcome, group, probs, cutoff, and digits
#' The output should be a list containing the following elements:
#' - Name of each group
#' - Calculated metric for each group
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - max_min_diff: maximum minimum metric difference for the model
#' If confidence intervals are computed (`confint = TRUE`):
#' - MAX_MIN_DIFF_CI: A vector of length 2 providing the lower and upper bounds of
#' the 95% confidence interval for the max-min metric difference
#' @export

eval_max_min_diff <- function(data, outcome, group, probs, get_metric, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000,
                              digits = 2, message = TRUE){
  metric <- get_metric(
    data, outcome, group, probs, cutoff, digits
  )

  # Check that the length of metric matches the number of unique groups
  unique_groups <- unique(data[[group]])
  if (!(length(unique_groups) == length(metric))) {
    stop("Number of metrics don't match the number of groups")
  }

  unlisted_metric <- unlist(metric)
  max_min_diff <- round(max(unlisted_metric) - min(unlisted_metric), digits)
  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      resampled_data <- data %>%
        group_by(!!group) %>%
        sapply(resample_group)

      metric <- get_metric(
        data = as.data.frame(resampled_data), outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      unlisted_metric <- unlist(metric)
      max(unlisted_metric) - min(unlisted_metric)
    })
    max_min_diff$MAX_MIN_DIFF_CI <- c(
      round(max_min_diff - 1.96 * sd(unlist(se)), digits),
      round(max_min_diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  # Add in messages!!!
  return(max_min_diff)
}


#' Examine maximum minimum ratio of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param get_metric Function of the model performance metric used to compute multi-group metrics
#' The input should take in: data, outcome, group, probs, cutoff, and digits
#' The output should be a list containing the following elements:
#' - Name of each group
#' - Calculated metric for each group
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Max_min_ratio: maximum minimum metric ratio for the model
#' If confidence intervals are computed (`confint = TRUE`):
#' - MAX_MIN_RATIO_CI: A vector of length 2 providing the lower and upper bounds of
#' the 95% confidence interval for the max-min metric ratio
#' @export

eval_max_min_ratio <- function(data, outcome, group, probs, get_metric, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000,
                              digits = 2, message = TRUE){
  metric <- get_metric(
    data, outcome, group, probs, cutoff, digits
  )

  # Check that the length of metric matches the number of unique groups
  unique_groups <- unique(data[[group]])
  if (!(length(unique_groups) == length(metric))) {
    stop("Number of metrics don't match the number of groups")
  }

  unlisted_metric <- unlist(metric)
  # Need to prevent from /0 from happening
  max_min_ratio <- round(max(unlisted_metric) / min(unlisted_metric), digits)
  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      resampled_data <- data %>%
        group_by(!!group) %>%
        sapply(resample_group)

      metric <- get_metric(
        data = as.data.frame(resampled_data), outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      unlisted_metric <- unlist(metric)
      max(unlisted_metric) / min(unlisted_metric)
    })
    max_min_ratio$MAX_MIN_RATIO_CI <- c(
      round(max_min_ratio - 1.96 * sd(unlist(se)), digits),
      round(max_min_ratio + 1.96 * sd(unlist(se)), digits)
    )
  }

  # Add in messages!!!
  return(max_min_ratio)
}


#' Examine max absolute difference of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param get_metric Function of the model performance metric used to compute multi-group metrics
#' The input should take in: data, outcome, group, probs, cutoff, and digits
#' The output should be a list containing the following elements:
#' - Name of each group
#' - Calculated metric for each group
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - max_abs_diff: max absolute difference for the model
#' If confidence intervals are computed (`confint = TRUE`):
#' - MAX_ABS_DIFF_CI: A vector of length 2 providing the lower and upper bounds of
#' the 95% confidence interval for the max absolute difference
#' @export

eval_max_abs_diff <- function(data, outcome, group, probs, get_metric, cutoff = 0.5,
                               confint = TRUE, bootstraps = 1000,
                               digits = 2, message = TRUE){
  metric <- get_metric(
    data, outcome, group, probs, cutoff, digits
  )

  # Check that the length of metric matches the number of unique groups
  unique_groups <- unique(data[[group]])
  if (!(length(unique_groups) == length(metric))) {
    stop("Number of metrics don't match the number of groups")
  }

  unlisted_metric <- unlist(metric)
  max_abs_diff <- round(abs(max(unlisted_metric) - mean(unlisted_metric)), digits)
  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      resampled_data <- data %>%
        group_by(!!group) %>%
        sapply(resample_group)

      metric <- get_metric(
        data = as.data.frame(resampled_data), outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      unlisted_metric <- unlist(metric)
      abs(max(unlisted_metric) - mean(unlisted_metric))
    })
    max_abs_diff$MAX_ABS_DIFF_CI <- c(
      round(max_abs_diff - 1.96 * sd(unlist(se)), digits),
      round(max_abs_diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  # Add in messages!!!
  return(max_abs_diff)
}


#' Examine mean absolute deviation of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param get_metric Function of the model performance metric used to compute multi-group metrics
#' The input should take in: data, outcome, group, probs, cutoff, and digits
#' The output should be a list containing the following elements:
#' - Name of each group
#' - Calculated metric for each group
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - mean_abs_dev: mean absolute difference for the model
#' If confidence intervals are computed (`confint = TRUE`):
#' - MEAN_ABS_DEV_CI: A vector of length 2 providing the lower and upper bounds of
#' the 95% confidence interval for the mean absolute deviation
#' @export

eval_mean_abs_dev <- function(data, outcome, group, probs, get_metric, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000,
                              digits = 2, message = TRUE){
  metric <- get_metric(
    data, outcome, group, probs, cutoff, digits
  )

  # Check that the length of metric matches the number of unique groups
  unique_groups <- unique(data[[group]])
  if (!(length(unique_groups) == length(metric))) {
    stop("Number of metrics don't match the number of groups")
  }

  unlisted_metric <- unlist(metric)
  mean_abs_dev <- round(mad(unlisted_metric, center = mean(unlisted_metric)), digits)
  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      resampled_data <- data %>%
        group_by(!!group) %>%
        sapply(resample_group)

      metric <- get_metric(
        data = as.data.frame(resampled_data), outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      unlisted_metric <- unlist(metric)
      mad(unlisted_metric, center = mean(unlisted_metric))
    })
    mean_abs_dev$MEAN_ABS_DEV_CI <- c(
      round(mean_abs_dev - 1.96 * sd(unlist(se)), digits),
      round(mean_abs_dev + 1.96 * sd(unlist(se)), digits)
    )
  }

  # Add in messages!!!
  return(mean_abs_dev)
}


#' Examine variance of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param get_metric Function of the model performance metric used to compute multi-group metrics
#' The input should take in: data, outcome, group, probs, cutoff, and digits
#' The output should be a list containing the following elements:
#' - Name of each group
#' - Calculated metric for each group
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - variance: variance for the model
#' If confidence intervals are computed (`confint = TRUE`):
#' - VAR_CI: A vector of length 2 providing the lower and upper bounds of
#' the 95% confidence interval for the variance
#' @export

eval_variance <- function(data, outcome, group, probs, get_metric, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000,
                              digits = 2, message = TRUE){
  metric <- get_metric(
    data, outcome, group, probs, cutoff, digits
  )

  # Check that the length of metric matches the number of unique groups
  unique_groups <- unique(data[[group]])
  if (!(length(unique_groups) == length(metric))) {
    stop("Number of metrics don't match the number of groups")
  }

  unlisted_metric <- unlist(metric)
  variance <- round(var(unlisted_metric), digits)
  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      resampled_data <- data %>%
        group_by(!!group) %>%
        sapply(resample_group)

      metric <- get_metric(
        data = as.data.frame(resampled_data), outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      unlisted_metric <- unlist(metric)
      var(unlisted_metric)
    })
    variance$VAR_CI <- c(
      round(variance - 1.96 * sd(unlist(se)), digits),
      round(variance + 1.96 * sd(unlist(se)), digits)
    )
  }

  # Add in messages!!!
  return(variance)
}


#' Examine Generalized Entropy Index of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' group
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the group
#' @param probs Name of the predicted outcome variable
#' @param get_metric Function of the model performance metric used to compute multi-group metrics
#' The input should take in: data, outcome, group, probs, cutoff, and digits
#' The output should be a list containing the following elements:
#' - Name of each group
#' - Calculated metric for each group
#' @param alpha Alpha for the generalized entropy index function (Cannot be 0 or 1), default is 2
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - generalized_entropy_index: generalized entropy index for the model
#' If confidence intervals are computed (`confint = TRUE`):
#' - GEN_ENTROPY_IND_CI: A vector of length 2 providing the lower and upper bounds of
#' the 95% confidence interval for the generalized entropy index
#' @export

eval_generalized_entropy_index <- function(data, outcome, group, probs, get_metric, alpha = 2,
                          cutoff = 0.5, confint = TRUE, bootstraps = 1000,
                          digits = 2, message = TRUE){
  metric <- get_metric(
    data, outcome, group, probs, cutoff, digits
  )

  # Check that the length of metric matches the number of unique groups
  unique_groups <- unique(data[[group]])
  if (!(length(unique_groups) == length(metric))) {
    stop("Number of metrics don't match the number of groups")
  }
  if (alpha %in% c(0, 1)){
    stop("Alpha cannot be 0 or 1. Please choose another alpha")
  }

  K <- length(unique_groups)
  unlisted_metric <- unlist(metric)
  entropy <- 0
  for (value in unlisted_metric){
    entropy <- entropy + ((value / mean(unlisted_metric)) ^ alpha - 1)
  }
  generalized_entropy_index <- round(1/(K * alpha * (alpha - 1)) * entropy, digits)
  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      resampled_data <- data %>%
        group_by(!!group) %>%
        sapply(resample_group)

      metric <- get_metric(
        data = as.data.frame(resampled_data), outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      unlisted_metric <- unlist(metric)
      entropy <- 0
      for (value in unlisted_metric){
        entropy <- entropy + ((value / mean(unlisted_metric)) ^ alpha - 1)
      }
      1/(K * alpha * (alpha - 1)) * entropy
    })
    generalized_entropy_index$GEN_ENTROPY_IND_CI <- c(
      round(generalized_entropy_index - 1.96 * sd(unlist(se)), digits),
      round(generalized_entropy_index + 1.96 * sd(unlist(se)), digits)
    )
  }

  # Add in messages!!!
  return(generalized_entropy_index)
}
