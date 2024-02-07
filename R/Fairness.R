# -----------------------------------------------------------------------------
# Customized Fairness Function Implementation
# -----------------------------------------------------------------------------

#' Examine equal opportunity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - TPR_Group1: True Positive Rate for the first group
#' - TPR_Group2: True Positive Rate for the second group
#' - TPR_Diff: The difference in True Positive Rates between the two groups
#' If confidence intervals are computed (`confint = TRUE`):
#' - TPR_Diff_CI: A vector of length 2 providing the lower and upper bounds of
#' the 95% confidence interval for the TPR difference
#' @export

eval_eq_opp <- function(data, outcome, group, probs, cutoff = 0.5,
                        confint = TRUE, bootstraps = 1000,
                        digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  tpr <- get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  tpr$TPR_Diff <- tpr[[1]] - tpr[[2]]

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      tpr <- get_tpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      tpr[[1]] - tpr[[2]]
    })

    tpr$TPR_Diff_CI <- c(
      round(tpr$TPR_Diff - 1.96 * sd(unlist(se)), digits),
      round(tpr$TPR_Diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "True positive rate (TPR) for Group", unique(data[[group]])[1], "is",
      tpr[[1]], "\n"
    )
    cat(
      "TPR for Group", unique(data[[group]])[2], "is",
      tpr[[2]], "\n"
    )
    cat("Difference in TPR is", tpr$TPR_Diff, "\n")
    if (confint) {
      cat(
        "95% CI for the difference in TPR is",
        tpr$TPR_Diff_CI[1], "to",
        tpr$TPR_Diff_CI[2], "\n"
      )
      if (tpr$TPR_Diff_CI[1] > 0 | tpr$TPR_Diff_CI[2] < 0) {
        cat("There is evidence that model does not satisfy equal opportunity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy equal
            opportunity.\n")
      }
    }
  }

  return(tpr)
}

#' Examine equalized odds of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#'  - TPR_Group1: True Positive Rate for the first group
#'  - TPR_Group2: True Positive Rate for the second group
#'  - TPR_Difference: The difference in True Positive Rates between the two
#'  groups
#'  - FPR_Group1: False Positive Rate for the first group
#'  - FPR_Group2: False Positive Rate for the second group
#'  - FPR_Difference: The difference in False Positive Rates between the two
#'  groups
#'  If confidence intervals are computed (`confint = TRUE`):
#'  - TPR_Diff_CI: A vector of length 2 providing the lower and upper bounds
#'  of the 95% confidence interval for the TPR difference
#'  - FPR_Diff_CI: A vector of length 2 providing the lower and upper bounds
#'  of the 95% confidence interval for the FPR difference
#' @export

eval_eq_odds <- function(data, outcome, group, probs, cutoff = 0.5,
                         confint = TRUE, bootstraps = 1000,
                         digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  tpr <- get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  tpr$TPR_Diff <- tpr[[1]] - tpr[[2]]
  fpr$FPR_Diff <- fpr[[1]] - fpr[[2]]

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      tpr <- get_tpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      fpr <- get_fpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )

      list(se_tpr = tpr[[1]] - tpr[[2]], se_fpr = fpr[[1]] - fpr[[2]])
    })

    tpr$TPR_Diff_CI <- c(
      round(tpr$TPR_Diff -
        1.96 * sd(unlist(lapply(se, function(x) x$se_tpr))), digits),
      round(tpr$TPR_Diff +
        1.96 * sd(unlist(lapply(se, function(x) x$se_tpr))), digits)
    )
    fpr$FPR_Diff_CI <- c(
      round(fpr$FPR_Diff -
        1.96 * sd(unlist(lapply(se, function(x) x$se_fpr))), digits),
      round(fpr$FPR_Diff +
        1.96 * sd(unlist(lapply(se, function(x) x$se_fpr))), digits)
    )
  }

  if (message) {
    cat(
      "True positive rate (TPR) for Group", unique(data[[group]])[1], "is",
      tpr[[1]], "\n"
    )
    cat(
      "TPR for Group", unique(data[[group]])[2], "is",
      tpr[[2]], "\n"
    )
    cat("Difference in TPR is", tpr$TPR_Diff, "\n")
    if (confint) {
      cat(
        "95% CI for the difference in TPR is",
        tpr$TPR_Diff_CI[1], "to",
        tpr$TPR_Diff_CI[2], "\n"
      )
    }
    cat(
      "False positive rate (FPR) for Group", unique(data[[group]])[1], "is",
      fpr[[1]], "\n"
    )
    cat(
      "FPR for Group", unique(data[[group]])[2], "is",
      fpr[[2]], "\n"
    )
    cat("Difference in FPR is", fpr$FPR_Diff, "\n")
    if (confint) {
      cat(
        "95% CI for the difference in FPR is",
        fpr$FPR_Diff_CI[1], "to",
        fpr$FPR_Diff_CI[2], "\n"
      )
      if ((tpr$TPR_Diff_CI[1] > 0 | tpr$TPR_Diff_CI[2] < 0) |
        (fpr$FPR_Diff_CI[1] > 0 | fpr$FPR_Diff_CI[2] < 0)) {
        cat("There is evidence that model does not satisfy the equalized odds.
            \n")
      } else {
        cat("There is not enough evidence that the model does not satisfy the
            equalized odds criterion.\n")
      }
    }
  }
  return(c(tpr, fpr))
}

#' Examine statistical parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
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
                              confint = TRUE, bootstraps = 1000, digits = 2,
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

  ppr$PPR_Diff <- ppr[[1]] - ppr[[2]]

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      ppr <- get_tpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      ppr[[1]] - ppr[[2]]
    })

    ppr$PPR_Diff_CI <- c(
      round(ppr$PPR_Diff - 1.96 * sd(unlist(se)), digits),
      round(ppr$PPR_Diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "Positive prediction rate (PPR) for Group", unique(data[[group]])[1], "is",
      round(ppr[[1]], digits), "\n"
    )
    cat(
      "PPR for Group", unique(data[[group]])[2], "is",
      round(ppr[[2]], digits), "\n"
    )
    cat("Difference in PPR is", round(ppr$PPR_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in PPR is",
        round(ppr$PPR_Diff_CI[1], digits), "to",
        round(ppr$PPR_Diff_CI[2], digits), "\n"
      )
      if (ppr$PPR_Diff_CI[1] > 0 | ppr$PPR_Diff_CI[2] < 0) {
        cat("There is evidence that model does not satisfy statistical parity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            statistical parity.\n")
      }
    }
  }

  return(ppr)
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
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
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
                                   cutoff = 0.5, confint = TRUE,
                                   bootstraps = 1000, message = TRUE,
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
      cat("Conditional on", group2, condition, ":\n")
      return(c(
        list(condition = paste(group2, condition)),
        eval_stats_parity(
          data = subset_data, outcome = outcome, group = group, probs = probs,
          cutoff = cutoff, confint = confint, bootstraps = bootstraps,
          digits = digits, message = message
        )
      ))
    }
  } else {
    data[[group2]] <- as.factor(data[[group2]])
    if (!condition %in% levels(data[[group2]])) {
      stop("Condition must be a character of the levels to condition on.")
    } else {
      subset_data <- subset(data, data[[group2]] == condition)
      cat("Conditional on", group2, "=", condition, ":\n")
      return(c(
        list(condition = paste(group2, condition)),
        eval_stats_parity(
          data = subset_data, outcome = outcome, group = group, probs = probs,
          cutoff = cutoff, confint = confint, bootstraps = bootstraps,
          digits = digits, message = message
        )
      ))
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
#' @param bootstraps Number of bootstrap samples, default is 1000
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

eval_pred_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                             confint = TRUE, bootstraps = 1000,
                             digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  ppv$PPV_Diff <- ppv[[1]] - ppv[[2]]

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      ppv <- get_ppv(
        data = data_boot, outcome = outcome, group = group, probs = probs,
        cutoff = cutoff
      )
      ppv[[1]] - ppv[[2]]
    })

    ppv$PPV_Diff_CI <- c(
      round(ppv$PPV_Diff - 1.96 * sd(unlist(se)), digits),
      round(ppv$PPV_Diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "Positive predictive value (PPV) for Group", unique(data[[group]])[1],
      "is", round(ppv[[1]], digits), "\n"
    )
    cat(
      "Positive predictive value (PPV) for Group", unique(data[[group]])[2],
      "is", round(ppv[[2]], digits), "\n"
    )
    cat("Difference in PPV is", round(ppv$PPV_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in PPV is",
        round(ppv$PPV_Diff_CI[1], digits), "to",
        round(ppv$PPV_Diff_CI[2], digits), "\n"
      )
      if (ppv$PPV_Diff_CI[1] > 0 | ppv$PPV_Diff_CI[2] < 0) {
        cat("There is evidence that model does not satisfy predictive parity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            predictive parity.\n")
      }
    }
  }

  return(ppv)
}

#' Examine predictive equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
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
                               confint = TRUE, bootstraps = 1000,
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

  fpr$FPR_Diff <- fpr[[1]] - fpr[[2]]

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      fpr <- get_fpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )

      fpr[[1]] - fpr[[2]]
    })

    fpr$FPR_Diff_CI <- c(
      round(fpr$FPR_Diff - 1.96 * sd(unlist(se)), digits),
      round(fpr$FPR_Diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "False positive rate (FPR) for Group", unique(data[[group]])[1], "is",
      round(fpr[[1]], digits), "\n"
    )
    cat(
      "FPR for Group", unique(data[[group]])[2], "is",
      round(fpr[[2]], digits), "\n"
    )
    cat("Difference in FPR is", round(fpr$FPR_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in FPR is",
        round(fpr$FPR_Diff_CI[1], digits), "to",
        round(fpr$FPR_Diff_CI[2], digits), "\n"
      )
      if (fpr$FPR_Diff_CI[1] > 0 | fpr$FPR_Diff_CI[2] < 0) {
        cat("There is evidence that model does not satisfy predictive
            equality.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            predictive equality.\n")
      }
    }
  }
  return(fpr)
}

#' Examine conditional use accuracy equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 1000
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
#' @export

eval_cond_acc_equality <- function(data, outcome, group, probs, cutoff = 0.5,
                                   confint = TRUE, bootstraps = 1000,
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
  ppv$PPV_Diff <- ppv[[1]] - ppv[[2]]
  npv$NPV_Diff <- npv[[1]] - npv[[2]]

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      ppv <- get_ppv(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      npv <- get_npv(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )

      list(se_ppv = ppv[[1]] - ppv[[2]], se_npv = npv[[1]] - npv[[2]])
    })

    ppv$PPV_Diff_CI <- c(
      round(
        ppv$PPV_Diff - 1.96 * sd(unlist(lapply(se, function(x) x$se_ppv))),
        digits
      ),
      round(
        ppv$PPV_Diff + 1.96 * sd(unlist(lapply(se, function(x) x$se_ppv))),
        digits
      )
    )
    npv$NPV_Diff_CI <- c(
      round(
        npv$NPV_Diff - 1.96 * sd(unlist(lapply(se, function(x) x$se_npv))),
        digits
      ),
      round(
        npv$NPV_Diff + 1.96 * sd(unlist(lapply(se, function(x) x$se_npv))),
        digits
      )
    )
  }

  if (message) {
    cat(
      "Positive predictive value (PPV) for Group", unique(data[[group]])[1], "is",
      round(ppv[[1]], digits), "\n"
    )
    cat(
      "PPV for Group", unique(data[[group]])[2], "is",
      round(ppv[[2]], digits), "\n"
    )
    cat("Difference in PPV is", round(ppv$PPV_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in PPV is",
        round(ppv$PPV_Diff_CI[1], digits), "to",
        round(ppv$PPV_Diff_CI[2], digits), "\n"
      )
    }
    cat(
      "Negative predictive value (NPV) for Group", unique(data[[group]])[1], "is",
      round(npv[[1]], digits), "\n"
    )
    cat(
      "NPV for Group", unique(data[[group]])[2], "is",
      round(npv[[2]], digits), "\n"
    )
    cat("Difference in NPV is", round(npv$NPV_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in NPV is",
        round(npv$NPV_Diff_CI[1], digits), "to",
        round(npv$NPV_Diff_CI[2], digits), "\n"
      )

      if (ppv$PPV_Diff_CI[1] > 0 | ppv$PPV_Diff_CI[2] < 0 |
        npv$NPV_Diff_CI[1] > 0 | npv$NPV_Diff_CI[2] < 0) {
        cat("There is enough evidence that the model does not satisfy
            conditional use accuracy equality.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            conditional use accuracy equality.\n")
      }
    }
  }
  return(c(ppv, npv))
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
                            confint = TRUE, bootstraps = 1000,
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

  acc$ACC_Diff <- acc[[1]] - acc[[2]]

  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      acc <- get_acc(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      acc[[1]] - acc[[2]]
    })
    acc$ACC_Diff_CI <- c(
      round(acc$ACC_Diff - 1.96 * sd(unlist(se)), digits),
      round(acc$ACC_Diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "Accuracy for Group", unique(data[[group]])[1], "is",
      round(acc[[1]], digits), "\n"
    )
    cat(
      "Accuracy for Group", unique(data[[group]])[2], "is",
      round(acc[[2]], digits), "\n"
    )
    cat("Difference in accuracy is", round(acc$ACC_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in accuracy is",
        round(acc$ACC_Diff_CI[1], digits), "to",
        round(acc$ACC_Diff_CI[2], digits), "\n"
      )
      if (acc$ACC_Diff_CI[1] > 0 | acc$ACC_Diff_CI[2] < 0) {
        cat("There is enough evidence that the model does not satisfy
            accuracy parity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            accuracy parity.\n")
      }
    }
  }
  return(acc)
}

#' Examine Brier Score parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
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
                           confint = TRUE, bootstraps = 1000,
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

  bs$BS_Diff <- bs[[1]] - bs[[2]]

  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      bs <- get_brier_score(
        data = data_boot, outcome = outcome, group = group,
        probs = probs
      )
      bs[[1]] - bs[[2]]
    })
    bs$BS_Diff_CI <- c(
      round(bs$BS_Diff - 1.96 * sd(unlist(se)), digits),
      round(bs$BS_Diff + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "Brier Score for Group", unique(data[[group]])[1], "is",
      round(bs[[1]], digits), "\n"
    )
    cat(
      "Brier Score for Group", unique(data[[group]])[2], "is",
      round(bs[[2]], digits), "\n"
    )
    cat("Difference in Brier Score is", round(bs$BS_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in Brier Score is",
        round(bs$BS_Diff_CI[1], digits), "to",
        round(bs$BS_Diff_CI[2], digits), "\n"
      )
      if (bs$BS_Diff_CI[1] > 0 | bs$BS_Diff_CI[2] < 0) {
        cat("There is enough evidence that the model does not satisfy
            Brier Score parity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            Brier Score parity.\n")
      }
    }
  }

  return(bs)
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
                                    confint = TRUE, bootstraps = 1000,
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

  err_ratio$"FN/FP_Diff" <- err_ratio[[1]] - err_ratio[[2]]

  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
        replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
        replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      err_ratio <- get_err_ratio(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      err_ratio[[1]] - err_ratio[[2]]
    })
    err_ratio$"FN/FP_Diff_CI" <- c(
      round(err_ratio$"FN/FP_Diff" - 1.96 * sd(unlist(se)), digits),
      round(err_ratio$"FN/FP_Diff" + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "False negative (FN) / false positive (FP) ratio for Group", unique(data[[group]])[1], "is",
      round(err_ratio[[1]], digits), "\n"
    )
    cat(
      "FN/FP ratio for Group", unique(data[[group]])[2], "is",
      round(err_ratio[[2]], digits), "\n"
    )
    cat(
      "Difference in FN/FP ratio is", round(err_ratio$"FN/FP_Diff", digits),
      "\n"
    )
    if (confint) {
      cat(
        "95% CI for the difference in FN/FP ratio is",
        round(err_ratio$"FN/FP_Diff_CI"[1], digits), "to",
        round(err_ratio$"FN/FP_Diff_CI"[2], digits), "\n"
      )
      if (err_ratio$"FN/FP_Diff_CI"[1] > 0 | err_ratio$"FN/FP_Diff_CI"[2] < 0) {
        cat("There is enough evidence that the model does not satisfy
            treatment equality.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            treatment equality.\n")
      }
    }
  }

  return(err_ratio)
}

#' Examine balance for positive class of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
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
                               confint = TRUE, bootstraps = 1000,
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

  avg_prob$Avg_Prob_Diff <- avg_prob[[1]] - avg_prob[[2]]

  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
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
    avg_prob$"Avg_Prob_Diff_CI" <- c(
      round(avg_prob$"Avg_Prob_Diff" - 1.96 * sd(unlist(se)), digits),
      round(avg_prob$"Avg_Prob_Diff" + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "Average predicted probability for Group", unique(data[[group]])[1], "is",
      round(avg_prob[[1]], digits), "\n"
    )
    cat(
      "Average predicted probability for Group", unique(data[[group]])[2], "is",
      round(avg_prob[[2]], digits), "\n"
    )
    cat(
      "Difference in average predicted probability is",
      round(avg_prob$"Avg_Prob_Diff", digits), "\n"
    )
    if (confint) {
      cat(
        "95% CI for the difference in average predicted probability is",
        round(avg_prob$"Avg_Prob_Diff_CI"[1], digits), "to",
        round(avg_prob$"Avg_Prob_Diff_CI"[2], digits), "\n"
      )
      if (avg_prob$"Avg_Prob_Diff_CI"[1] > 0 |
        avg_prob$"Avg_Prob_Diff_CI"[2] < 0) {
        cat("There is enough evidence that the model does not satisfy
            balance for positive class.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            balance for positive class.\n")
      }
    }
  }

  return(avg_prob)
}

#' Examine balance for negative class of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
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
                               confint = TRUE, bootstraps = 1000,
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

  avg_prob$Avg_Prob_Diff <- avg_prob[[1]] - avg_prob[[2]]

  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      # bootstrap within each group
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
    avg_prob$"Avg_Prob_Diff_CI" <- c(
      round(avg_prob$"Avg_Prob_Diff" - 1.96 * sd(unlist(se)), digits),
      round(avg_prob$"Avg_Prob_Diff" + 1.96 * sd(unlist(se)), digits)
    )
  }

  if (message) {
    cat(
      "Average predicted probability for Group", unique(data[[group]])[1], "is",
      round(avg_prob[[1]], digits), "\n"
    )
    cat(
      "Average predicted probability for Group", unique(data[[group]])[2], "is",
      round(avg_prob[[2]], digits), "\n"
    )
    cat(
      "Difference in average predicted probability is",
      round(avg_prob$"Avg_Prob_Diff", digits), "\n"
    )
    if (confint) {
      cat(
        "95% CI for the difference in average predicted probability is",
        round(avg_prob$"Avg_Prob_Diff_CI"[1], digits), "to",
        round(avg_prob$"Avg_Prob_Diff_CI"[2], digits), "\n"
      )
      if (avg_prob$"Avg_Prob_Diff_CI"[1] > 0 |
        avg_prob$"Avg_Prob_Diff_CI"[2] < 0) {
        cat("There is enough evidence that the model does not satisfy
            balance for negative class.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            balance for negative class.\n")
      }
    }
  }

  return(avg_prob)
}
