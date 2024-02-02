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
#'   - TPR_Group1: True Positive Rate for the first group
#'   - TPR_Group2: True Positive Rate for the second group
#'   - TPR_Difference: The difference in True Positive Rates between the two
#'   groups
#'   If confidence intervals are computed (`confint = TRUE`):
#'     - TPR_Diff_CI: A vector of length 2 providing the lower and upper bounds
#'     of the 95% confidence interval for the TPR difference
#' @export

check_equal_opportunity <- function(data, outcome, group, probs, cutoff = 0.5,
                                    confint = TRUE, bootstraps = 1000,
                                    digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  tpr <- get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
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
      tpr$TPR_Diff - 1.96 * sd(unlist(se)),
      tpr$TPR_Diff + 1.96 * sd(unlist(se))
    )
  }

  if (message) {
    cat(
      "True positive rate (TPR) for Group", unique(data[[group]])[1], "is",
      round(tpr[[1]], digits), "\n"
    )
    cat(
      "TPR for Group", unique(data[[group]])[2], "is",
      round(tpr[[2]], digits), "\n"
    )
    cat("Difference in TPR is", round(tpr$TPR_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in TPR is",
        round(tpr$TPR_Diff_CI[1], digits), "to",
        round(tpr$TPR_Diff_CI[2], digits), "\n"
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

check_equalized_odds <- function(data, outcome, group, probs, cutoff = 0.5,
                                 confint = TRUE, bootstraps = 1000,
                                 digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  tpr <- get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )
  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
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
      tpr$TPR_Diff - 1.96 * sd(unlist(lapply(se, function(x) x$se_tpr))),
      tpr$TPR_Diff + 1.96 * sd(unlist(lapply(se, function(x) x$se_tpr)))
    )
    fpr$FPR_Diff_CI <- c(
      fpr$FPR_Diff - 1.96 * sd(unlist(lapply(se, function(x) x$se_fpr))),
      fpr$FPR_Diff + 1.96 * sd(unlist(lapply(se, function(x) x$se_fpr)))
    )
  }

  if (message) {
    cat(
      "True positive rate (TPR) for Group", unique(data[[group]])[1], "is",
      round(tpr[[1]], digits), "\n"
    )
    cat(
      "TPR for Group", unique(data[[group]])[2], "is",
      round(tpr[[2]], digits), "\n"
    )
    cat("Difference in TPR is", round(tpr$TPR_Diff, digits), "\n")
    if (confint) {
      cat(
        "95% CI for the difference in TPR is",
        round(tpr$TPR_Diff_CI[1], digits), "to",
        round(tpr$TPR_Diff_CI[2], digits), "\n"
      )
    }
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
      if ((tpr$TPR_Diff_CI[1] > 0 | tpr$TPR_Diff_CI[2] < 0) |
        (fpr$FPR_Diff_CI[1] > 0 | fpr$FPR_Diff_CI[2] < 0)) {
        cat("There is evidence that model does not satisfy the equalized odds.\n")
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
#'   - PPR_Diff_CI: 95% confidence interval for the difference in Positive
#'   Prediction Rate
#' @export

check_statistical_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                                     confint = TRUE, bootstraps = 1000, digits = 2,
                                     message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppr <- get_ppr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
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
      ppr$PPR_Diff - 1.96 * sd(unlist(se)),
      ppr$PPR_Diff + 1.96 * sd(unlist(se))
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
        "95% CI for the difference in TPR is",
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
#'  - PPR_Diff_CI: 95% confidence interval for the difference in Positive
#'  Prediction Rate
#' @export

check_cond_statistical_parity <- function(data, outcome, group,
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
        check_statistical_parity(
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
        check_statistical_parity(
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
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of positive predictive value (ppv), the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

predictive_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    ppv_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_ppv(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(ppv_se = ppv, ppv_diff_se = ppv_diff)

    ppv <- ppv %>%
      dplyr::left_join(ppv_se, by = group) %>%
      dplyr::mutate(
        ppv_lower = expit(logit(ppv) - 1.96 * ppv_se),
        ppv_upper = expit(logit(ppv) + 1.96 * ppv_se),
        ppv_diff_lower = expit(logit(ppv_diff) - 1.96 * ppv_diff_se),
        ppv_diff_upper = expit(logit(ppv_diff) + 1.96 * ppv_diff_se)
      ) %>%
      dplyr::select(!!rlang::sym(group), ppv, ppv_lower, ppv_upper, ppv_diff, ppv_diff_lower, ppv_diff_upper) %>%
      dplyr::mutate(
        ppv_ci = paste0("[", round(ppv_lower, 3), ", ", round(ppv_upper, 3), "]"),
        ppv_diff_ci = paste0("[", round(ppv_lower, 3), ", ", round(ppv_upper, 3), "]")
      ) %>%
      dplyr::select(-ppv_lower, -ppv_upper, -ppv_diff_lower, -ppv_diff_upper)
    ppv[, c(1, 2, 4, 3, 5)]
  } else {
    return(ppv)
  }
}

#' Examine predictive equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of false positive rate (fpr), the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

predictive_equality <- function(data, outcome, group, probs, cutoff = 0.5,
                                confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    fpr_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_fpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(fpr_se = fpr, fpr_diff_se = fpr_diff)

    fpr <- fpr %>%
      dplyr::left_join(fpr_se, by = group) %>%
      dplyr::mutate(
        fpr_lower = expit(logit(fpr) - 1.96 * fpr_se),
        fpr_upper = expit(logit(fpr) + 1.96 * fpr_se),
        fpr_diff_lower = expit(logit(fpr_diff) - 1.96 * fpr_diff_se),
        fpr_diff_upper = expit(logit(fpr_diff) + 1.96 * fpr_diff_se)
      ) %>%
      dplyr::select(!!rlang::sym(group), fpr, fpr_lower, fpr_upper, fpr_diff, fpr_diff_lower, fpr_diff_upper) %>%
      dplyr::mutate(
        fpr_ci = paste0("[", round(fpr_lower, 3), ", ", round(fpr_upper, 3), "]"),
        fpr_diff_ci = paste0("[", round(fpr_diff_lower, 3), ", ", round(fpr_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-fpr_lower, -fpr_upper, -fpr_diff_lower, -fpr_diff_upper)
    fpr[, c(1, 2, 4, 3, 5)]
  } else {
    return(fpr)
  }
}

#' Examine conditional use accuracy equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of positive predictive value (ppv), negative predictive value (npv), the differences, and their confidence interval
#' @importFrom magrittr %>%
#' @export

conditional_use_accuracy_equality <- function(data, outcome, group, probs, cutoff = 0.5,
                                              confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )
  npv <- get_npv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      ppv_se <- get_ppv(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      npv_se <- get_npv(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      cbind(ppv_se, npv_se[, -1])
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(~ stats::sd(logit(.))) %>%
      dplyr::rename(ppv_se = ppv, ppv_diff_se = ppv_diff, npv_se = npv, npv_diff_se = npv_diff)

    cbind(ppv, npv[-1]) %>%
      dplyr::inner_join(se, by = group) %>%
      dplyr::mutate(
        ppv_lower = expit(logit(ppv) - 1.96 * ppv_se),
        ppv_upper = expit(logit(ppv) + 1.96 * ppv_se),
        ppv_diff_lower = expit(logit(ppv_diff) - 1.96 * ppv_diff_se),
        ppv_diff_upper = expit(logit(ppv_diff) + 1.96 * ppv_diff_se),
        npv_lower = expit(logit(npv) - 1.96 * npv_se),
        npv_upper = expit(logit(npv) + 1.96 * npv_se),
        npv_diff_lower = expit(logit(npv_diff) - 1.96 * npv_diff_se),
        npv_diff_upper = expit(logit(npv_diff) + 1.96 * npv_diff_se)
      ) %>%
      dplyr::select(
        !!rlang::sym(group), ppv, ppv_lower, ppv_upper, ppv_diff, ppv_diff_lower, ppv_diff_upper,
        npv, npv_lower, npv_upper, npv_diff_lower, npv_diff_upper
      ) %>%
      dplyr::mutate(
        ppv_ci = paste0("[", round(ppv_lower, 3), ", ", round(ppv_upper, 3), "]"),
        ppv_diff_ci = paste0("[", round(ppv_diff_lower, 3), ", ", round(ppv_diff_upper, 3), "]"),
        npv_ci = paste0("[", round(npv_lower, 3), ", ", round(npv_upper, 3), "]"),
        npv_diff_ci = paste0("[", round(npv_diff_lower, 3), ", ", round(npv_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-ppv_lower, -ppv_upper, -ppv_diff_lower, -ppv_diff_upper, -npv_lower, -npv_upper, -npv_diff_lower, -npv_diff_upper)
  } else {
    return(cbind(ppv, npv[-1]))
  }
}

#' Examine accuracy parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of model accuracy, the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

accuracy_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                            confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  acc <- get_acc(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    acc_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_acc(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(acc_se = acc, acc_diff_se = acc_diff)

    acc <- acc %>%
      dplyr::left_join(acc_se, by = group) %>%
      dplyr::mutate(
        acc_lower = expit(logit(acc) - 1.96 * acc_se),
        acc_upper = expit(logit(acc) + 1.96 * acc_se),
        acc_diff_lower = expit(logit(acc_diff) - 1.96 * acc_diff_se),
        acc_diff_upper = expit(logit(acc_diff) + 1.96 * acc_diff_se)
      ) %>%
      dplyr::select(!!rlang::sym(group), acc, acc_lower, acc_upper, acc_diff, acc_diff_lower, acc_diff_upper) %>%
      dplyr::mutate(
        acc_ci = paste0("[", round(acc_lower, 3), ", ", round(acc_upper, 3), "]"),
        acc_diff_ci = paste0("[", round(acc_diff_lower, 3), ", ", round(acc_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-acc_lower, -acc_upper, -acc_diff_lower, -acc_diff_upper)
    acc[, c(1, 2, 4, 3, 5)]
  } else {
    return(acc)
  }
}

#' Examine Brier Score parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of Brier Score, the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

brier_score_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                               confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  brier_score <- get_brier_score(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    brier_score_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_brier_score(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(brier_se = brier, diff_se = brier_diff)

    brier_score <- brier_score %>%
      dplyr::left_join(brier_score_se, by = group) %>%
      dplyr::mutate(
        brier_score_lower = brier - 1.96 * brier_se,
        brier_score_upper = brier + 1.96 * brier_se,
        brier_diff_lower = brier_diff - 1.96 * diff_se,
        brier_diff_upper = brier_diff + 1.96 * diff_se
      ) %>%
      dplyr::select(!!rlang::sym(group), brier, brier_score_lower, brier_score_upper, brier_diff, brier_diff_lower, brier_diff_upper) %>%
      dplyr::mutate(
        brier_score_ci = paste0("[", round(brier_score_lower, 3), ", ", round(brier_score_upper, 3), "]"),
        brier_diff_ci = paste0("[", round(brier_diff_lower, 3), ", ", round(brier_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-brier_score_lower, -brier_score_upper, -brier_diff_lower, -brier_diff_upper)
    brier_score[, c(1, 2, 4, 3, 5)]
  } else {
    return(brier_score)
  }
}

#' Examine treatment equality of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of error ratio (FN/FP), the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

treatment_equality <- function(data, outcome, group, probs, cutoff = 0.5,
                               confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  err_ratio <- get_err_ratio(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    err_ratio_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_err_ratio(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(err_ratio_se = err_ratio, err_ratio_diff_se = err_ratio_diff)

    err_ratio <- err_ratio %>%
      dplyr::left_join(err_ratio_se, by = group) %>%
      dplyr::mutate(
        err_ratio_lower = expit(logit(err_ratio) - 1.96 * err_ratio_se),
        err_ratio_upper = expit(logit(err_ratio) + 1.96 * err_ratio_se),
        err_ratio_diff_lower = expit(logit(err_ratio_diff) - 1.96 * err_ratio_diff_se),
        err_ratio_diff_upper = expit(logit(err_ratio_diff) + 1.96 * err_ratio_diff_se)
      ) %>%
      dplyr::select(!!rlang::sym(group), err_ratio, err_ratio_lower, err_ratio_upper, err_ratio_diff, err_ratio_diff_lower, err_ratio_diff_upper) %>%
      dplyr::mutate(
        err_ratio_ci = paste0("[", round(err_ratio_lower, 3), ", ", round(err_ratio_upper, 3), "]"),
        err_ratio_diff_ci = paste0("[", round(err_ratio_diff_lower, 3), ", ", round(err_ratio_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-err_ratio_lower, -err_ratio_upper, -err_ratio_diff_lower, -err_ratio_diff_upper)
    err_ratio[, c(1, 2, 4, 3, 5)]
  } else {
    return(err_ratio)
  }
}

#' Examine balance for positive class of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of expected positive score, the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

balance_positive <- function(data, outcome, group, probs, cutoff = 0.5,
                             confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  expected_positive_score <- get_exp_pos(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    expected_positive_score_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_exp_pos(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(expected_positive_score_se = exp_pos, expected_positive_score_diff_se = expected_positive_diff)

    expected_positive_score <- expected_positive_score %>%
      dplyr::left_join(expected_positive_score_se, by = group) %>%
      dplyr::mutate(
        expected_positive_score_lower = expit(logit(exp_pos) - 1.96 * expected_positive_score_se),
        expected_positive_score_upper = expit(logit(exp_pos) + 1.96 * expected_positive_score_se),
        expected_positive_score_diff_lower = expit(logit(expected_positive_diff) - 1.96 * expected_positive_score_diff_se),
        expected_positive_score_diff_upper = expit(logit(expected_positive_diff) + 1.96 * expected_positive_score_diff_se)
      ) %>%
      dplyr::select(
        !!rlang::sym(group), exp_pos, expected_positive_score_lower, expected_positive_score_upper,
        expected_positive_diff, expected_positive_score_diff_lower, expected_positive_score_diff_upper
      ) %>%
      dplyr::mutate(
        expected_positive_score_ci = paste0("[", round(expected_positive_score_lower, 3), ", ", round(expected_positive_score_upper, 3), "]"),
        expected_positive_score_diff_ci = paste0("[", round(expected_positive_score_diff_lower, 3), ", ", round(expected_positive_score_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-expected_positive_score_lower, -expected_positive_score_upper, -expected_positive_score_diff_lower, -expected_positive_score_diff_upper)
    expected_positive_score[, c(1, 2, 4, 3, 5)]
  } else {
    return(expected_positive_score)
  }
}

#' Examine balance for negative class of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of expected negative score, the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

balance_negative <- function(data, outcome, group, probs, cutoff = 0.5,
                             confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  expected_negative_score <- get_exp_neg(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    expected_negative_score_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_exp_neg(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(expected_negative_se = exp_neg, expected_negative_diff_se = expeced_negative_diff)

    expected_negative_score <- expected_negative_score %>%
      dplyr::left_join(expected_negative_score_se, by = group) %>%
      dplyr::mutate(
        expected_negative_score_lower = expit(logit(exp_neg) - 1.96 * expected_negative_se),
        expected_negative_score_upper = expit(logit(exp_neg) + 1.96 * expected_negative_se),
        expected_negative_score_diff_lower = expit(logit(expeced_negative_diff) - 1.96 * expected_negative_diff_se),
        expected_negative_score_diff_upper = expit(logit(expeced_negative_diff) + 1.96 * expected_negative_diff_se)
      ) %>%
      dplyr::select(
        !!rlang::sym(group), exp_neg, expected_negative_score_lower, expected_negative_score_upper,
        expeced_negative_diff, expected_negative_score_diff_lower, expected_negative_score_diff_upper
      ) %>%
      dplyr::mutate(
        expected_negative_score_ci = paste0("[", round(expected_negative_score_lower, 3), ", ", round(expected_negative_score_upper, 3), "]"),
        expected_negative_score_diff_ci = paste0("[", round(expected_negative_score_diff_lower, 3), ", ", round(expected_negative_score_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-expected_negative_score_lower, -expected_negative_score_upper, -expected_negative_score_diff_lower, -expected_negative_score_diff_upper)
    expected_negative_score[, c(1, 2, 4, 3, 5)]
  } else {
    return(expected_negative_score)
  }
}
