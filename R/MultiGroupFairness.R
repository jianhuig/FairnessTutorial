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
#' @param confint Whether to compute a 95% confidence interval, default is TRUE.
#' @param bootstraps Number of bootstrap samples, default is 1000.
#' @param digits Number of digits to round the results to, default is 2.
#'
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Max-Min: The maximum minus minimum metric difference for the model.
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - 95% CI: A string providing the lower and upper bounds of the 95%
#'   confidence interval for the max-min metric difference.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @export

eval_max_min_diff <- function(data, outcome, group, probs, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000, digits = 2) {
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Max-Min difference for each metric
  metric$`Max-Min` <- apply(metric[,-1], 1, function(x) max(x) - min(x))

  # Calculate confidence interval
  if (confint) {
    boot_metrics <- replicate(bootstraps, {
      resampled_data <- # Resample data within each group
        resampled_data <- data %>%
        dplyr::group_by(!!dplyr::sym(group)) %>%
        dplyr::sample_n(dplyr::n(), replace = TRUE) %>%
        dplyr::ungroup() %>%
        data.frame() # Important: ungroup after sampling
      boot_metric <- get_all_metrics(resampled_data, outcome, group, probs,
                                     cutoff, digits)
      apply(boot_metric[,-1], 1, function(x) max(x) - min(x))
    }, simplify = "array")

    # Calculate the confidence intervals
    CI_bounds <- apply(boot_metrics, 1, function(x)
      stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
    metric$`95% CI` <- apply(CI_bounds, 2, function(x)
      paste("[", round(x[1], digits), ",", round(x[2], digits), "]", sep = ""))
  }

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
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Max/Min: The maximum over minimum metric difference for the model
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - 95% CI: A string providing the lower and upper bounds of the 95%
#'   confidence interval for the max/min metric difference.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @export

eval_max_min_ratio <- function(data, outcome, group, probs, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000, digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  metric$`Max/Min` <- apply(metric[,-1], 1, function(x) max(x) / min(x))
  # Calculate confidence interval
  if (confint) {
    boot_metrics <- replicate(bootstraps, {
      resampled_data <- # Resample data within each group
        resampled_data <- data %>%
        dplyr::group_by(!!dplyr::sym(group)) %>%
        dplyr::sample_n(dplyr::n(), replace = TRUE) %>%
        dplyr::ungroup() %>%
        data.frame() # Important: ungroup after sampling
      boot_metric <- get_all_metrics(resampled_data, outcome, group, probs,
                                     cutoff, digits)

      apply(metric[,-1], 1, function(x) max(x) / min(x))
    }, simplify = "array")

    CI_bounds <- apply(boot_metrics, 1, function(x)
      stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
    metric$`95% CI` <- apply(CI_bounds, 2, function(x)
      paste("[", round(x[1], digits), ",", round(x[2], digits), "]", sep = ""))
  }
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
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Max_abs_diff: The maximum absolute difference for the model
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - 95% CI: A string providing the lower and upper bounds of the 95%
#'   confidence interval for the maximum absolute difference.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @export

eval_max_abs_diff <- function(data, outcome, group, probs, cutoff = 0.5,
                               confint = TRUE, bootstraps = 1000, digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Max absolute difference for each metric
  metric$`Max_Abs_Diff` <- apply(metric[,-1], 1, function(x) round(abs(max(x) - mean(x)), digits))
  # Calculate confidence interval
  if (confint) {
    boot_metrics <- replicate(bootstraps, {
      resampled_data <- # Resample data within each group
        resampled_data <- data %>%
        dplyr::group_by(!!dplyr::sym(group)) %>%
        dplyr::sample_n(dplyr::n(), replace = TRUE) %>%
        dplyr::ungroup() %>%
        data.frame() # Important: ungroup after sampling
      boot_metric <- get_all_metrics(resampled_data, outcome, group, probs,
                                     cutoff, digits)
      apply(metric[,-1], 1, function(x) abs(max(x) - mean(x)))
    }, simplify = "array")

    # Calculate the confidence intervals
    CI_bounds <- apply(boot_metrics, 1, function(x)
      stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
    metric$`95% CI` <- apply(CI_bounds, 2, function(x)
      paste("[", round(x[1], digits), ",", round(x[2], digits), "]", sep = ""))
  }
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
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Mean_abs_dev: The mean absolute deviation for the model
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - 95% CI: A string providing the lower and upper bounds of the 95%
#'   confidence interval for the mean absolute deviation.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom stats mad
#' @export

eval_mean_abs_dev <- function(data, outcome, group, probs, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000, digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Mean Absolute Deviation for each metric
  metric$`Max_abs_dev` <- apply(metric[,-1], 1, function(x) round(stats::mad(x, center = mean(x)), digits))

  # Calculate confidence interval
  if (confint) {
    boot_metrics <- replicate(bootstraps, {
      resampled_data <- # Resample data within each group
        resampled_data <- data %>%
        dplyr::group_by(!!dplyr::sym(group)) %>%
        dplyr::sample_n(dplyr::n(), replace = TRUE) %>%
        dplyr::ungroup() %>%
        data.frame() # Important: ungroup after sampling
      boot_metric <- get_all_metrics(resampled_data, outcome, group, probs,
                                     cutoff, digits)
      apply(boot_metric[,-1], 1, function(x) stats::mad(x, center = mean(x)))
    }, simplify = "array")

    # Calculate the confidence intervals
    CI_bounds <- apply(boot_metrics, 1, function(x)
      stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
    metric$`95% CI` <- apply(CI_bounds, 2, function(x)
      paste("[", round(x[1], digits), ",", round(x[2], digits), "]", sep = ""))
  }

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
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Variance: The variance for the model
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - 95% CI: A string providing the lower and upper bounds of the 95%
#'   confidence interval for the variance.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom stats var
#' @export

eval_variance <- function(data, outcome, group, probs, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000, digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  # Calculate Variance for each metric
  metric$`Variance` <- apply(metric[,-1], 1, function(x) round(stats::var(x), digits))

  # Calculate confidence interval
  if (confint) {
    boot_metrics <- replicate(bootstraps, {
      resampled_data <- # Resample data within each group
        resampled_data <- data %>%
        dplyr::group_by(!!dplyr::sym(group)) %>%
        dplyr::sample_n(dplyr::n(), replace = TRUE) %>%
        dplyr::ungroup() %>%
        data.frame() # Important: ungroup after sampling
      boot_metric <- get_all_metrics(resampled_data, outcome, group, probs,
                                     cutoff, digits)
      apply(boot_metric[,-1], 1, function(x) stats::var(x))
    }, simplify = "array")

    # Calculate the confidence intervals
    CI_bounds <- apply(boot_metrics, 1, function(x)
      stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
    metric$`95% CI` <- apply(CI_bounds, 2, function(x)
      paste("[", round(x[1], digits), ",", round(x[2], digits), "]", sep = ""))
  }

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
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @return A data frame containing the following elements:
#'   - Metric: The names of the metrics calculated.
#'   - Values for each group.
#'   - Generalized_entropy_index: The generalized entropy index for the model
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - 95% CI: A string providing the lower and upper bounds of the 95%
#'   confidence interval for the generalized entropy index.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @export

eval_generalized_entropy_index <- function(data, outcome, group, probs, alpha = 2,
                          cutoff = 0.5, confint = TRUE, bootstraps = 1000, digits = 2){
  metric <- get_all_metrics(data, outcome, group, probs, cutoff, digits)

  if (alpha %in% c(0, 1)){
    stop("Alpha cannot be 0 or 1. Please choose another alpha")
  }

  # Calculate Generalized Entropy Index for each metric
  K <- length(unique(data[[group]]))
  metric$`Generalized_Entropy_Index` <- apply(metric[,-1], 1, function(x)
    round(1/(K * alpha * (alpha - 1)) * (sum((x / mean(x)) ^ alpha - 1)), digits))

  # Calculate confidence interval
  if (confint) {
    boot_metrics <- replicate(bootstraps, {
      resampled_data <- # Resample data within each group
        resampled_data <- data %>%
        dplyr::group_by(!!dplyr::sym(group)) %>%
        dplyr::sample_n(dplyr::n(), replace = TRUE) %>%
        dplyr::ungroup() %>%
        data.frame() # Important: ungroup after sampling
      boot_metric <- get_all_metrics(resampled_data, outcome, group, probs,
                                     cutoff, digits)
      apply(boot_metric[,-1], 1, function(x)
        round(1/(K * alpha * (alpha - 1)) * (sum((x / mean(x)) ^ alpha - 1)), digits))
    }, simplify = "array")

    # Calculate the confidence intervals
    CI_bounds <- apply(boot_metrics, 1, function(x)
      stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
    metric$`95% CI` <- apply(CI_bounds, 2, function(x)
      paste("[", round(x[1], digits), ",", round(x[2], digits), "]", sep = ""))
  }

  return(metric)
}
