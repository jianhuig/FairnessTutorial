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
#' @return a list of true positive rate (tpr), the difference, and their confidence interval
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
      dplyr::rename(tpr_se = tpr, diff_se = tpr_diff)

    tpr <- tpr %>%
      dplyr::left_join(tpr_se, by = group) %>%
      dplyr::mutate(
        tpr_lower = expit(logit(tpr) - 1.96 * tpr_se),
        tpr_upper = expit(logit(tpr) + 1.96 * tpr_se),
        tpr_diff_lower = expit(logit(tpr_diff) - 1.96 * diff_se),
        tpr_diff_upper = expit(logit(tpr_diff) + 1.96 * diff_se)
      ) %>%
      dplyr::select(!!rlang::sym(group), tpr, tpr_lower, tpr_upper, tpr_diff, tpr_diff_lower, tpr_diff_upper) %>%
      dplyr::mutate(
        tpr_ci = paste0("[", round(tpr_lower, 3), ", ", round(tpr_upper, 3), "]"),
        tpr_diff_ci = paste0("[", round(tpr_diff_lower, 3), ", ", round(tpr_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-tpr_lower, -tpr_upper, -tpr_diff_lower, -tpr_diff_upper)
    tpr[, c(1, 2, 4, 3, 5)]
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
#' @return a list of true positive rate (tpr), false positive rate (fpr), the differences, and their confidence interval
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
      dplyr::rename(tpr_se = tpr, tpr_diff_se = tpr_diff, fpr_se = fpr, fpr_diff_se = fpr_diff)

    tpr_fpr <- cbind(tpr, fpr[-1]) %>%
      dplyr::inner_join(se, by = group) %>%
      dplyr::mutate(
        tpr_lower = expit(logit(tpr) - 1.96 * tpr_se),
        tpr_upper = expit(logit(tpr) + 1.96 * tpr_se),
        tpr_diff_lower = expit(logit(tpr_diff) - 1.96 * tpr_diff_se),
        tpr_diff_upper = expit(logit(tpr_diff) + 1.96 * tpr_diff_se),
        fpr_lower = expit(logit(fpr) - 1.96 * fpr_se),
        fpr_upper = expit(logit(fpr) + 1.96 * fpr_se),
        fpr_diff_lower = expit(logit(fpr_diff) - 1.96 * fpr_diff_se),
        fpr_diff_upper = expit(logit(fpr_diff) + 1.96 * fpr_diff_se)
      ) %>%
      dplyr::select(
        !!rlang::sym(group), tpr, tpr_lower, tpr_upper, tpr_diff, tpr_diff_lower, tpr_diff_upper,
        fpr, fpr_lower, fpr_upper, fpr_diff, fpr_diff_lower, fpr_diff_upper
      ) %>%
      dplyr::mutate(
        tpr_ci = paste0("[", round(tpr_lower, 3), ", ", round(tpr_upper, 3), "]"),
        tpr_diff_ci = paste0("[", round(tpr_diff_lower, 3), ", ", round(tpr_diff_upper, 3), "]"),
        fpr_ci = paste0("[", round(fpr_lower, 3), ", ", round(fpr_upper, 3), "]"),
        fpr_diff_ci = paste0("[", round(fpr_diff_lower, 3), ", ", round(fpr_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-tpr_lower, -tpr_upper, -tpr_diff_lower, -tpr_diff_upper, -fpr_lower, -fpr_upper, -fpr_diff_lower, -fpr_diff_upper)
      tpr_fpr[, c('gender', 'tpr', 'tpr_ci', 'fpr', 'fpr_ci', 'tpr_diff', 'tpr_diff_ci', 'fpr_diff', 'fpr_diff_ci')]
  } else {
    return(cbind(tpr, fpr[-1]))
  }
}

#' Examine statistical parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of positive prediction rate (ppr), the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

statistical_parity <- function(data, outcome, group, probs, cutoff = 0.5,
                              confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppr <- get_ppr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    ppr_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_ppr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(ppr_se = ppr, ppr_diff_se = ppr_diff)

    ppr <- ppr %>%
      dplyr::left_join(ppr_se, by = group) %>%
      dplyr::mutate(
        ppr_lower = expit(logit(ppr) - 1.96 * ppr_se),
        ppr_upper = expit(logit(ppr) + 1.96 * ppr_se),
        ppr_diff_lower = expit(logit(ppr_diff) - 1.96 * ppr_diff_se),
        ppr_diff_upper = expit(logit(ppr_diff) + 1.96 * ppr_diff_se),
      ) %>%
      dplyr::select(!!rlang::sym(group), ppr, ppr_lower, ppr_upper, ppr_diff, ppr_diff_lower, ppr_diff_upper) %>%
      dplyr::mutate(
        ppr_ci = paste0("[", round(ppr_lower, 3), ", ", round(ppr_upper, 3), "]"),
        ppr_diff_ci = paste0("[", round(ppr_diff_lower, 3), ", ", round(ppr_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-ppr_lower, -ppr_upper, -ppr_diff_lower, -ppr_diff_upper)
      ppr[, c(1, 2, 4, 3, 5)]
  } else {
    return(ppr)
  }
}

#' Examine conditional statistical parity of a model
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attributes
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the sensitive attribute
#' @param group2 the name of the conditional sensitive attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param group2_cutoff the threshold for the conditional sensitive attribute.
#' @param confint whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps the number of bootstrap samples, default is 1000
#' @return a list of conditional positive predictive rate (cond_ppr), the difference, and their confidence interval
#' @importFrom magrittr %>%
#' @export

conditional_statistical_parity <- function(data, outcome, group, group2, probs, cutoff = 0.5, group2_cutoff,
                               confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  cond_ppr <- get_cond_ppr(
    data = data, outcome = outcome, group = group, group2 = group2, probs = probs,
    cutoff = cutoff, group2_cutoff = group2_cutoff
  )

  # Calculate confidence interval
  if (confint) {
    group2_sym <- rlang::sym(group2)
    data <- data %>% mutate(group2AboveBelow = ifelse(!!group2_sym >= group2_cutoff, paste("Above ", group2_cutoff), paste("Below ", group2_cutoff)))
    cond_ppr_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_cond_ppr(
        data = data, outcome = outcome, group = group, group2 = group2, probs = probs,
        cutoff = cutoff, group2_cutoff = group2_cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group), group2AboveBelow) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(cond_ppr_se = cond_ppr, cond_ppr_diff_se = cond_ppr_diff)

    cond_ppr <- cond_ppr %>%
      dplyr::left_join(cond_ppr_se, by = c(group, group2AboveBelow)) %>%
      dplyr::mutate(
        cond_ppr_lower = expit(logit(cond_ppr) - 1.96 * cond_ppr_se),
        cond_ppr_upper = expit(logit(cond_ppr) + 1.96 * cond_ppr_se),
        cond_ppr_diff_lower = expit(logit(cond_ppr_diff) - 1.96 * cond_ppr_diff_se),
        cond_ppr_diff_lower = expit(logit(cond_ppr_diff) + 1.96 * cond_ppr_diff_se)
      ) %>%
      dplyr::select(!!rlang::sym(group), group2AboveBelow, cond_ppr, cond_ppr_lower, cond_ppr_upper, cond_ppr_diff, cond_ppr_diff_lower, cond_ppr_diff_upper) %>%
      dplyr::mutate(
        cond_ppr_ci = paste0("[", round(cond_ppr_lower, 3), ", ", round(cond_ppr_upper, 3), "]"),
        cond_ppr_diff_ci = paste0("[", round(cond_ppr_diff_lower, 3), ", ", round(cond_ppr_diff_upper, 3), "]")
      ) %>%
      dplyr::select(-cond_ppr_lower, -cond_ppr_upper, -cond_ppr_diff_lower, -cond_ppr_diff_upper)
    cond_ppr[, c(1, 2, 4, 3, 5)]
  } else {
    return(cond_ppr)
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
      dplyr::select(!!rlang::sym(group), exp_pos, expected_positive_score_lower, expected_positive_score_upper,
                    expected_positive_diff, expected_positive_score_diff_lower, expected_positive_score_diff_upper) %>%
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
      dplyr::select(!!rlang::sym(group), exp_neg, expected_negative_score_lower, expected_negative_score_upper,
                    expeced_negative_diff, expected_negative_score_diff_lower, expected_negative_score_diff_upper) %>%
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
