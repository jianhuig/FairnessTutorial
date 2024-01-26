# -----------------------------------------------------------------------------
# Customized Fairness Function Implementation
# -----------------------------------------------------------------------------

#' Examine equal opportunity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of equal opportunity and its confidence interval
#' @importFrom magrittr %>%
#' @export

equal_opportunity <- function(data, outcome, group, probs, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  tpr <- get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    tpr_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_tpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(se = tpr)

    tpr %>%
      dplyr::left_join(tpr_se, by = group) %>%
      dplyr::mutate(
        tpr_lower = expit(logit(tpr) - 1.96 * se),
        tpr_upper = expit(logit(tpr) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), tpr, tpr_lower, tpr_upper) %>%
      dplyr::mutate(
        tpr_ci = paste0("[", round(tpr_lower, 3), ", ", round(tpr_upper, 3), "]")
      ) %>%
      dplyr::select(-tpr_lower, -tpr_upper)
  } else {
    return(tpr)
  }
}

#' Examine equalized odds of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of equalized odds and its confidence interval
#' @importFrom magrittr %>%
#' @export

equalized_odds <- function(data, outcome, group, probs, cutoff = 0.5,
                           confint = TRUE, bootstraps = 1000) {
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

  # Calculate confidence interval
  if (confint) {
    se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      tpr_se <- get_tpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      fpr_se <- get_fpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      cbind(tpr_se, fpr_se[, -1])
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(~ stats::sd(logit(.))) %>%
      dplyr::rename(se_tpr = tpr, se_fpr = fpr)

    cbind(tpr, fpr[-1]) %>%
      dplyr::inner_join(se, by = group) %>%
      dplyr::mutate(
        tpr_lower = expit(logit(tpr) - 1.96 * se_tpr),
        tpr_upper = expit(logit(tpr) + 1.96 * se_tpr),
        fpr_lower = expit(logit(fpr) - 1.96 * se_fpr),
        fpr_upper = expit(logit(fpr) + 1.96 * se_fpr)
      ) %>%
      dplyr::select(
        !!rlang::sym(group), tpr, tpr_lower, tpr_upper,
        fpr, fpr_lower, fpr_upper
      ) %>%
      dplyr::mutate(
        tpr_ci = paste0("[", round(tpr_lower, 3), ", ", round(tpr_upper, 3), "]"),
        fpr_ci = paste0("[", round(fpr_lower, 3), ", ", round(fpr_upper, 3), "]")
      ) %>%
      dplyr::select(-tpr_lower, -tpr_upper, -fpr_lower, -fpr_upper)
  } else {
    return(cbind(tpr, fpr[-1]))
  }
}
