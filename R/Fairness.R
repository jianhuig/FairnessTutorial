# -----------------------------------------------------------------------------
# Customized Fairness Function Implementation
# -----------------------------------------------------------------------------

#' Evaluate Equal Opportunity Compliance of a Predictive Model
#'
#' This function evaluates the equal opportunity compliance of a predictive model
#' by comparing the False Negative Rates (FNR) across different groups defined by
#' a sensitive attribute. It is used to determine if a model exhibits bias
#' towards any group for binary outcomes.
#'
#' @param data A dataframe containing the actual outcomes, predicted probabilities,
#' and sensitive attributes necessary for evaluating model fairness.
#' @param outcome The name of the outcome variable in the data; it must be binary.
#' @param group The name of the sensitive attribute variable used to define groups
#' for comparison in the fairness evaluation.
#' @param probs The name of the variable containing predicted probabilities or scores.
#' @param cutoff The threshold for converting predicted probabilities into binary
#' predictions; defaults to 0.5.
#' @param bootstraps The number of bootstrap samples used for estimating the
#' confidence interval; defaults to 2500.
#' @param alpha The 1 - significance level for the confidence interval; defaults to 0.05.
#' @param digits The number of decimal places to which numerical results are rounded;
#' defaults to 2.
#' @param differnce Logical; whether to return FNR difference or ratio
#' @param message Logical; whether to print summary results to the console; defaults to TRUE.
#' @return Returns a dataframe with the following columns:
#' \itemize{
#'   \item Metric: Describes the metric being reported (FNR for each group, difference).
#'   \item Group1: False Negative Rate for the first group.
#'   \item Group2: False Negative Rate for the second group.
#'   \item Difference/Ratio: The difference or ratio in False Negative Rates between the two groups.
#'   \item CI: The 95% confidence interval for the FNR difference/ratio.
#' }
#' @examples
#' # Example usage:
#' eval_eq_opp(
#'   data = your_data, outcome = "actual_outcome",
#'   group = "sensitive_attribute", probs = "predicted_probs"
#' )
#' @export

eval_eq_opp <- function(data, outcome, group, probs, cutoff = 0.5,
                        bootstraps = 2500, alpha = 0.05, digits = 2,
                        difference = TRUE, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  fnr <- 1 - get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  fnr_diff <- fnr[[1]] - fnr[[2]]
  fnr_ratio <- fnr[[2]] / fnr[[1]]

  # Calculate difference confidence interval
  se <- replicate(bootstraps, {
    # Bootstrap within each group
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])

    fnr_boot <- 1 - get_tpr(
      data = data_boot, outcome = outcome, group = group,
      probs = probs, cutoff = cutoff
    )
    return(fnr_boot[[1]] - fnr_boot[[2]])
  })

  # Calculate ratio condience interval
  ratio_se <- replicate(bootstraps, {
    # Bootstrap within each group
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
                     replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
                     replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])

    fnr_boot <- 1 - get_tpr(
      data = data_boot, outcome = outcome, group = group,
      probs = probs, cutoff = cutoff
    )
    return(log(fnr_boot[[2]]/fnr_boot[[1]]))
  })


  if(difference){
    lower_ci <- round(fnr_diff - qnorm(1 - alpha/2) * sd(se), digits)
    upper_ci <- round(fnr_diff + qnorm(1 - alpha/2) * sd(se), digits)

    # Create a dataframe for the results
    results_df <- data.frame(
      "FNR",
      fnr[[1]],
      fnr[[2]],
      fnr_diff,
      paste0("[", lower_ci, ", ", upper_ci, "]")
    )

    colnames(results_df) <- c(
      "Metric", paste0("Group", sort(unique(data[[group]]))[[1]]),
      paste0("Group", sort(unique(data[[group]]))[[2]]),
      "Difference", "95% CI"
    )
  } else {
    lower_ci <- round(exp(log(fnr_ratio) - 1.96 * sd(ratio_se)), digits)
    upper_ci <- round(exp(log(fnr_ratio) + 1.96 * sd(ratio_se)), digits)

    # Create a dataframe for the results
    results_df <- data.frame(
      "FNR",
      fnr[[1]],
      fnr[[2]],
      fnr_ratio,
      paste0("[", lower_ci, ", ", upper_ci, "]")
    )

    colnames(results_df) <- c(
      "Metric", paste0("Group", sort(unique(data[[group]]))[[1]]),
      paste0("Group", sort(unique(data[[group]]))[[2]]),
      "Ratio", "95% CI"
    )
  }


  # Print message if desired
  if (message) {
    if (lower_ci > 0 | upper_ci < 0) {
      cat("There is evidence that model does not satisfy equal opportunity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy equal
          opportunity.\n")
    }
  }

  return(results_df)
}


#' Examine Equalized Odds of a Predictive Model
#'
#' This function examines the equalized odds of a predictive model by comparing
#' both the False Negative Rates (FNR) and False Positive Rates (FPR) across different
#' groups defined by a sensitive attribute. It assesses if a model performs unbiasedly
#' for binary outcomes across these groups, adhering to the equalized odds fairness criterion.
#'
#' @param data A dataframe containing the actual outcomes, predicted outcomes,
#' and sensitive attributes necessary for evaluating model fairness.
#' @param outcome The name of the outcome variable in the data; it must be binary.
#' @param group The name of the sensitive attribute variable used to define groups
#' for comparison in the fairness evaluation.
#' @param probs The name of the variable containing predicted probabilities or scores.
#' @param cutoff The threshold for converting predicted probabilities into binary
#' predictions; defaults to 0.5.
#' @param bootstraps The number of bootstrap samples used for estimating the
#' uncertainty in the fairness metrics; defaults to 1000.
#' @param alpha The 1 - significance level for the confidence interval; defaults to 0.05.
#' @param digits The number of decimal places to which numerical results are rounded;
#' defaults to 2.
#' @param message Logical; whether to print summary results to the console; defaults to TRUE.
#' @return Returns a dataframe with the following columns:
#'   - Metric: Describes the metric being reported (FNR and FPR for each group, difference).
#'   - Group1: Rate for the first group.
#'   - Group2: Rate for the second group.
#'   - Difference: The difference in rates between the two groups.
#'   - 95% CI: The 95% confidence interval for the rate difference。
#' @examples
#' # Example usage:
#' eval_eq_odds(
#'   data = your_data, outcome = "actual_outcome",
#'   group = "sensitive_attribute", probs = "predicted_probs"
#' )

eval_eq_odds <- function(data, outcome, group, probs, cutoff = 0.5,
                         bootstraps = 2500, alpha = 0.05, digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  fnr <- 1 - get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  fnr_diff <- fnr[[1]] - fnr[[2]]
  fpr_diff <- fpr[[1]] - fpr[[2]]

  # Calculate confidence interval
  se <- replicate(bootstraps, {
    indices1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    indices2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    boot_data <- rbind(data[indices1, ], data[indices2, ])

    boot_fnr <- 1 - get_tpr(
      data = boot_data, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    boot_fpr <- get_fpr(
      data = boot_data, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    c(boot_fnr[[1]] - boot_fnr[[2]], boot_fpr[[1]] - boot_fpr[[2]])
  })

  # Calculate confidence intervals
  fnr_lower <- round(fnr_diff - qnorm(1 - alpha/4) * sd(se[1, ]), digits)
  fnr_upper <- round(fnr_diff + qnorm(1 - alpha/4) * sd(se[1, ]), digits)
  fpr_lower <- round(fpr_diff - qnorm(1 - alpha/4) * sd(se[2, ]), digits)
  fpr_upper <- round(fpr_diff + qnorm(1 - alpha/4) * sd(se[2, ]), digits)

  # Structure the results as a dataframe
  results_df <- data.frame(
    Metric = c("FNR; FPR"),
    Group1 = paste0(fnr[[1]],"; ", fpr[[1]]),
    Group2 = paste0(fnr[[2]],"; ",fpr[[2]]),
    Difference = paste0(fnr_diff,"; ", fpr_diff),
    CI =
      paste0("[", fnr_lower, ", ", fnr_upper, "]", "; ",
      "[", fpr_lower, ", ", fpr_upper, "]")
  )

  # Set proper column names, especially for '95% CI' to ensure it displays correctly
  colnames(results_df) <- c(
    "Metric",
    paste0("Group ", sort(unique(data[[group]]))[[1]]),
    paste0("Group ", sort(unique(data[[group]]))[[2]]),
    "Difference", "95% CR"
  )

  # Print summary message if desired
  if (message) {
    if (any(fnr_lower > 0) || any(fnr_upper < 0) || any(fpr_lower > 0) ||
      any(fpr_upper < 0)) {
      cat("There is evidence that model does not satisfy equalized odds.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy the
          equalized odds criterion.\n")
    }
  }

  return(results_df)
}


#' Examine statistical parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#'   - PPR_Group1: Positive Prediction Rate for the first group
#'   - PPR_Group2: Positive Prediction Rate for the second group
#'   - PPR_Diff: Difference in Positive Prediction Rate
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - PPR_Diff_CI: A vector of length 2 containing the lower and upper bounds
#'   of the 95% confidence interval for the difference in Positive Prediction
#'   Rate
#' @export

eval_stats_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                              bootstraps = 2500, alpha = 0.05, digits = 2,
                              message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppr <- get_ppr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  ppr_diff <- ppr[[1]] - ppr[[2]]

  # Calculate confidence interval
  se <- replicate(bootstraps, {
    indices1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    indices2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    boot_data <- rbind(data[indices1, ], data[indices2, ])
    ppr <- get_ppr(
      data = boot_data, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    ppr[[1]] - ppr[[2]]
  })

  lower_ci <- round(ppr_diff - qnorm(1 - alpha/2) * sd(unlist(se)), digits)
  upper_ci <- round(ppr_diff + qnorm(1 - alpha/2) * sd(unlist(se)), digits)

  # Structure the results as a dataframe
  results_df <- data.frame(
    Metric = "PPR",
    Group1 = ppr[[1]],
    Group2 = ppr[[2]],
    Difference = ppr_diff,
    CI = paste0("[", lower_ci, ", ", upper_ci, "]")
  )

  colnames(results_df) <- c(
    "Metric",
    paste0("Group ", sort(unique(data[[group]]))[[1]]),
    paste0("Group ", sort(unique(data[[group]]))[[2]]),
    "Difference", "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that model does not satisfy statistical parity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            statistical parity.\n")
    }
  }

  return(results_df)
}

#' Examine conditional statistical parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param group2 Name of the group to condition on
#' @param condition If the conditional group is categorical, the condition
#' supplied must be a character of the levels to condition on. If the conditional
#' group is continuous, the conditions supplied must be a character containing
#' the sign of the condition and the value to threshold the continuous variable
#' (e.g. "<50", ">50", "<=50", ">=50").
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#'  - Conditions: The conditions used to calculate the conditional PPR
#'  - PPR_Group1: Positive Prediction Rate for the first group
#'  - PPR_Group2: Positive Prediction Rate for the second group
#'  - PPR_Diff: Difference in Positive Prediction Rate
#'  If confidence intervals are computed (`confint = TRUE`):
#'  - PPR_Diff_CI: A vector of length 2 containing the lower and upper bounds
#'  of the 95% confidence interval for the difference in Positive Prediction
#'  Rate
#' @export

eval_cond_stats_parity <- function(data, outcome, group,
                                   group2, condition, probs,
                                   cutoff = 0.5,
                                   bootstraps = 2500, alpha = 0.05,
                                   message = TRUE,
                                   digits = 2) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  # check if the group2 is categorical or continuous
  if (is.numeric(data[[group2]])) {
    if (!grepl("^[<>=]", condition)) {
      stop("Condition must be a character containing the sign of the condition
           and the value to threshold the continuous variable
           (e.g. '<50', '>50', '<=50', '>=50').")
    } else {
      subset_data <- subset(
        data,
        eval(parse(text = paste0("data$", group2, condition)))
      )
      return(
        eval_stats_parity(
          data = subset_data, outcome = outcome, group = group, probs = probs,
          cutoff = cutoff, bootstraps = bootstraps, alpha = alpha,
          digits = digits, message = message
        )
      )
    }
  } else {
    data[[group2]] <- as.factor(data[[group2]])
    if (!condition %in% levels(data[[group2]])) {
      stop("Condition must be a character of the levels to condition on.")
    } else {
      subset_data <- subset(data, data[[group2]] == condition)
      return(
        eval_stats_parity(
          data = subset_data, outcome = outcome, group = group, probs = probs,
          cutoff = cutoff, alpha = alpha, bootstraps = bootstraps,
          digits = digits, message = message
        )
      )
    }
  }
}

#' Examine predictive parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#'  - PPV_Group1: Positive Predictive Value for the first group
#'  - PPV_Group2: Positive Predictive Value for the second group
#'  - PPV_Diff: Difference in Positive Predictive Value
#'  If confidence intervals are computed (`confint = TRUE`):
#'  - PPV_Diff_CI: A vector of length 2 containing the lower and upper bounds
#'  of the 95% confidence interval for the difference in Positive Predictive
#'  Value
#' @export

eval_pred_parity <- function(data, outcome, group, probs, cutoff = 0.5, bootstraps = 2500, alpha = 0.05,
                             digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- sort(unique(data[[outcome]]))
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  ppv_dif <- ppv[[1]] - ppv[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == sort(unique(data[[group]]))[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == sort(unique(data[[group]]))[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    ppv_boot <- get_ppv(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    ppv_boot[[1]] - ppv_boot[[2]]
  })

  lower_ci <- round(ppv_dif - qnorm(1 - alpha/2) * sd(se), digits)
  upper_ci <- round(ppv_dif + qnorm(1 - alpha/2) * sd(se), digits)

  result_df <- data.frame(
    "PPV",
    ppv[[1]],
    ppv[[2]],
    ppv_dif,
    paste0("[", lower_ci, ", ", upper_ci, "]")
  )
  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that model does not satisfy predictive parity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            predictive parity.\n")
    }
  }

  return(result_df)
}

#' Examine predictive equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - FPR_Group1: False Positive Rate for the first group
#' - FPR_Group2: False Positive Rate for the second group
#' - FPR_Diff: Difference in False Positive Rate
#' If confidence intervals are computed (`confint = TRUE`):
#' - FPR_Diff_CI: A vector of length 2 containing the lower and upper bounds
#' of the 95% confidence interval for the difference in False Positive Rate
#' @export

eval_pred_equality <- function(data, outcome, group, probs, cutoff = 0.5,
                               alpha = 0.05, bootstraps = 2500,
                               digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  fpr_dif <- fpr[[1]] - fpr[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    fpr_boot <- get_fpr(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff
    )
    fpr_boot[[1]] - fpr_boot[[2]]
  })

  lower_ci <- round(fpr_dif - qnorm(1 - alpha/2) * sd(se), digits)
  upper_ci <- round(fpr_dif + qnorm(1 - alpha/2) * sd(se), digits)

  result_df <- data.frame(
    "FPR",
    fpr[[1]],
    fpr[[2]],
    fpr_dif,
    paste0("[", lower_ci, ", ", upper_ci, "]")
  )
  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that model does not satisfy predictive
            equality.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            predictive equality.\n")
    }
  }

  return(result_df)
}

#' Examine conditional use accuracy equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - PPV_Group1: Positive Predictive Value for the first group
#' - PPV_Group2: Positive Predictive Value for the second group
#' - PPV_Diff: Difference in Positive Predictive Value
#' - NPV_Group1: Negative Predictive Value for the first group
#' - NPV_Group2: Negative Predictive Value for the second group
#' - NPV_Diff: Difference in Negative Predictive Value
#' If confidence intervals are computed (`confint = TRUE`):
#' - PPV_Diff_CI: A vector of length 2 containing the lower and upper bounds
#' of the 95% confidence interval for the difference in Positive Predictive
#' Value
#' - NPV_Diff_CI: A vector of length 2 containing the lower and upper bounds
#' of the 95% confidence interval for the difference in Negative Predictive
#' Value

eval_cond_acc_equality <- function(data, outcome, group, probs, cutoff = 0.5,
                                   alpha = 0.05, bootstraps = 2500,
                                   digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  npv <- get_npv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  ppv_diff <- ppv[[1]] - ppv[[2]]
  npv_diff <- npv[[1]] - npv[[2]]

  # Calculate confidence interval
  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    ppv_boot <- get_ppv(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    npv_boot <- get_npv(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    c(ppv_boot[[1]] - ppv_boot[[2]], npv_boot[[1]] - npv_boot[[2]])
  })

  ppv_lower_ci <- round(ppv_diff - qnorm(1 - alpha/4) * sd(se[1, ]), digits)
  ppv_upper_ci <- round(ppv_diff + qnorm(1 - alpha/4) * sd(se[1, ]), digits)
  npv_lower_ci <- round(npv_diff - qnorm(1 - alpha/4)  * sd(se[2, ]), digits)
  npv_upper_ci <- round(npv_diff + qnorm(1 - alpha/4)  * sd(se[2, ]), digits)

  result_df <- data.frame(
    Metric = c("PPV; NPV"),
    Group1 = paste0(ppv[[1]],"; ", npv[[1]]),
    Group2 = paste0(ppv[[2]],"; ",npv[[2]]),
    Difference = paste0(ppv_diff,"; ", npv_diff),
    CI =
      paste0("[", ppv_lower_ci, ", ", ppv_upper_ci, "]", "; ",
             "[", npv_lower_ci, ", ", npv_upper_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (ppv_lower_ci > 0 || ppv_upper_ci < 0 || npv_lower_ci > 0 ||
      npv_upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            conditional use accuracy equality.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            conditional use accuracy equality.\n")
    }
  }

  return(result_df)
}

#' Examine accuracy parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param cutoff Cutoff value for the predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Accuracy for Group 1
#' - Accuracy for Group 2
#' - Difference in accuracy
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in accuracy
#' @export

eval_acc_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                            alpha = 0.05, bootstraps = 2500,
                            digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  acc <- get_acc(
    data = data, outcome = outcome, group = group, probs = probs,
    digits = digits, cutoff = cutoff
  )

  acc_diff <- acc[[1]] - acc[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    acc_boot <- get_acc(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      digits = digits, cutoff = cutoff
    )
    acc_boot[[1]] - acc_boot[[2]]
  })

  lower_ci <- round(acc_diff - qnorm(1 - alpha/2)  * sd(se), digits)
  upper_ci <- round(acc_diff + qnorm(1 - alpha/2)  * sd(se), digits)

  result_df <- data.frame(
    "Accuracy",
    acc[[1]],
    acc[[2]],
    acc_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            accuracy parity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            accuracy parity.\n")
    }
  }
  return(result_df)
}

#' Examine Brier Score parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Brier Score for Group 1
#' - Brier Score for Group 2
#' - Difference in Brier Score
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in Brier Score
#' @export

eval_bs_parity <- function(data, outcome, group, probs,
                           alpha = 0.05, bootstraps = 2500,
                           digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  bs <- get_brier_score(
    data = data, outcome = outcome, group = group, probs = probs,
    digits = digits
  )

  bs_diff <- bs[[1]] - bs[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    bs_boot <- get_brier_score(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      digits = digits
    )
    bs_boot[[1]] - bs_boot[[2]]
  })

  lower_ci <- round(bs_diff - qnorm(1 - alpha/2) * sd(se), digits)
  upper_ci <- round(bs_diff + qnorm(1 - alpha/2) * sd(se), digits)

  result_df <- data.frame(
    "Brier Score",
    bs[[1]],
    bs[[2]],
    bs_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            Brier Score parity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            Brier Score parity.\n")
    }
  }

  return(result_df)
}

#' Examine treatment equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param cutoff Cutoff value for the predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - False Negative / False Positive ratio for Group 1
#' - False Negative / False Positive ratio for Group 2
#' - Difference in False Negative / False Positive ratio
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in False Negative / False Positive ratio
#' @export

eval_treatment_equality <- function(data, outcome, group, probs, cutoff = 0.5,
                                    alpha = 0.05, bootstraps = 2500,
                                    digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  err_ratio <- get_err_ratio(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  print(err_ratio)
  err_ratio_diff <- err_ratio[[1]] - err_ratio[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    err_ratio_boot <- get_err_ratio(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    err_ratio_boot[[1]] - err_ratio_boot[[2]]
  })
  se <- se[!is.infinite(se)]

  lower_ci <- round(err_ratio_diff - qnorm(1 - alpha/2)  * sd(se, na.rm = TRUE), digits)
  upper_ci <- round(err_ratio_diff + qnorm(1 - alpha/2)  * sd(se, na.rm = TRUE), digits)

  result_df <- data.frame(
    "FN/FP Ratio",
    err_ratio[[1]],
    err_ratio[[2]],
    err_ratio_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            treatment equality.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            treatment equality.\n")
    }
  }

  return(result_df)
}

#' Examine balance for positive class of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Average predicted probability for Group 1
#' - Average predicted probability for Group 2
#' - Difference in average predicted probability
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in average predicted probability
#' @export

eval_pos_class_bal <- function(data, outcome, group, probs,
                               alpha = 0.05, bootstraps = 2500,
                               digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  pos_data <- data[data[[outcome]] == 1, ]
  avg_prob <- get_avg_prob(
    data = pos_data, group = group, probs = probs, digits = digits
  )

  avg_prob_diff <- avg_prob[[1]] - avg_prob[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    pos_data_boot <- data_boot[data_boot[[outcome]] == 1, ]
    avg_prob_boot <- get_avg_prob(
      data = pos_data_boot, group = group, probs = probs
    )
    avg_prob_boot[[1]] - avg_prob_boot[[2]]
  })

  lower_ci <- round(avg_prob_diff - qnorm(1 - alpha/2)  * sd(se, na.rm = TRUE), digits)
  upper_ci <- round(avg_prob_diff + qnorm(1 - alpha/2)  * sd(se, na.rm = TRUE), digits)

  result_df <- data.frame(
    "Avg. Predicted Prob.",
    avg_prob[[1]],
    avg_prob[[2]],
    avg_prob_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            balance for positive class.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            balance for positive class.\n")
    }
  }

  return(result_df)
}

#' Examine balance for negative class of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Average predicted probability for Group 1
#' - Average predicted probability for Group 2
#' - Difference in average predicted probability
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in average predicted probability
#' @export

eval_neg_class_bal <- function(data, outcome, group, probs,
                               alpha = 0.05, bootstraps = 2500,
                               digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  neg_data <- data[data[[outcome]] == 0, ]
  avg_prob <- get_avg_prob(
    data = neg_data, group = group, probs = probs, digits = digits
  )

  avg_prob_diff <- avg_prob[[1]] - avg_prob[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    neg_data_boot <- data_boot[data_boot[[outcome]] == 0, ]
    avg_prob_boot <- get_avg_prob(
      data = neg_data_boot, group = group, probs = probs
    )
    avg_prob_boot[[1]] - avg_prob_boot[[2]]
  })

  lower_ci <- round(avg_prob_diff - qnorm(1 - alpha/2)  * sd(se, na.rm = TRUE), digits)
  upper_ci <- round(avg_prob_diff + qnorm(1 - alpha/2)  * sd(se, na.rm = TRUE), digits)

  result_df <- data.frame(
    "Avg. Predicted Prob.",
    avg_prob[[1]],
    avg_prob[[2]],
    avg_prob_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is enough evidence that the model does not satisfy
            balance for negative class.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            balance for negative class.\n")
    }
  }

  return(result_df)
}
