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

#' Examine statistical parity of a model
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
      dplyr::rename(se = ppr)

    tpr %>%
      dplyr::left_join(ppr_se, by = group) %>%
      dplyr::mutate(
        ppr_lower = expit(logit(ppr) - 1.96 * se),
        ppr_upper = expit(logit(ppr) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), ppr, ppr_lower, ppr_upper) %>%
      dplyr::mutate(
        ppr_ci = paste0("[", round(ppr_lower, 3), ", ", round(ppr_upper, 3), "]")
      ) %>%
      dplyr::select(-ppr_lower, -ppr_upper)
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
#' @return a list of equal opportunity and its confidence interval
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
    data <- data %>% mutate(group2AboveBelow = ifelse(group2_sym >= group2_cutoff, paste("Above ", group2_cutoff), paste("Below ", group2_cutoff)))
    cond_ppr_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_cond_ppr(
        data = data, outcome = outcome, group = group, group2 = group2, probs = probs,
        cutoff = cutoff, group2_cutoff = group2_cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      # Need to get group two too
      dplyr::group_by(!!rlang::sym(group), group2AboveBelow) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(se = cond_ppr)

    tpr %>%
      dplyr::left_join(cond_ppr_se, by = c(group, group2AboveBelow)) %>%
      dplyr::mutate(
        cond_ppr_lower = expit(logit(cond_ppr) - 1.96 * se),
        cond_ppr_upper = expit(logit(cond_ppr) + 1.96 * se)
      ) %>%
      # Need to get group two here too
      dplyr::select(!!rlang::sym(group), group2AboveBelow, cond_ppr, cond_ppr_lower, cond_ppr_upper) %>%
      dplyr::mutate(
        cond_ppr_ci = paste0("[", round(cond_ppr_lower, 3), ", ", round(cond_ppr_upper, 3), "]")
      ) %>%
      dplyr::select(-cond_ppr_lower, -cond_ppr_upper)
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
#' @return a list of equal opportunity and its confidence interval
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
      dplyr::rename(se = ppv)

    tpr %>%
      dplyr::left_join(ppv_se, by = group) %>%
      dplyr::mutate(
        ppv_lower = expit(logit(ppv) - 1.96 * se),
        ppv_upper = expit(logit(ppv) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), ppv, ppv_lower, ppv_upper) %>%
      dplyr::mutate(
        ppv_ci = paste0("[", round(ppv_lower, 3), ", ", round(ppv_upper, 3), "]")
      ) %>%
      dplyr::select(-ppv_lower, -ppv_upper)
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
#' @return a list of equal opportunity and its confidence interval
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
      dplyr::rename(se = fpr)

    tpr %>%
      dplyr::left_join(fpr_se, by = group) %>%
      dplyr::mutate(
        fpr_lower = expit(logit(fpr) - 1.96 * se),
        fpr_upper = expit(logit(fpr) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), fpr, fpr_lower, fpr_upper) %>%
      dplyr::mutate(
        fpr_ci = paste0("[", round(fpr_lower, 3), ", ", round(fpr_upper, 3), "]")
      ) %>%
      dplyr::select(-fpr_lower, -fpr_upper)
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
#' @return a list of equalized odds and its confidence interval
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
      dplyr::rename(se_ppv = ppv, se_npv = npv)

    cbind(ppv, npv[-1]) %>%
      dplyr::inner_join(se, by = group) %>%
      dplyr::mutate(
        ppv_lower = expit(logit(ppv) - 1.96 * se_ppv),
        ppv_upper = expit(logit(ppv) + 1.96 * se_ppv),
        npv_lower = expit(logit(npv) - 1.96 * se_npv),
        npv_upper = expit(logit(npv) + 1.96 * se_npv)
      ) %>%
      dplyr::select(
        !!rlang::sym(group), ppv, ppv_lower, ppv_upper,
        npv, npv_lower, npv_upper
      ) %>%
      dplyr::mutate(
        ppv_ci = paste0("[", round(ppv_lower, 3), ", ", round(ppv_upper, 3), "]"),
        npv_ci = paste0("[", round(npv_lower, 3), ", ", round(npv_upper, 3), "]")
      ) %>%
      dplyr::select(-ppv_lower, -ppv_upper, -npv_lower, -npv_upper)
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
#' @return a list of equal opportunity and its confidence interval
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
      dplyr::rename(se = acc)

    acc %>%
      dplyr::left_join(tpr_se, by = group) %>%
      dplyr::mutate(
        acc_lower = expit(logit(acc) - 1.96 * se),
        acc_upper = expit(logit(acc) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), acc, acc_lower, acc_upper) %>%
      dplyr::mutate(
        acc_ci = paste0("[", round(acc_lower, 3), ", ", round(acc_upper, 3), "]")
      ) %>%
      dplyr::select(-acc_lower, -acc_upper)
  } else {
    return(acc)
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
#' @return a list of equal opportunity and its confidence interval
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
      dplyr::rename(se = err_ratio)

    acc %>%
      dplyr::left_join(err_ratio_se, by = group) %>%
      dplyr::mutate(
        err_ratio_lower = expit(logit(err_ratio) - 1.96 * se),
        err_ratio_upper = expit(logit(err_ratio) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), err_ratio, err_ratio_lower, err_ratio_upper) %>%
      dplyr::mutate(
        err_ratio_ci = paste0("[", round(err_ratio_lower, 3), ", ", round(err_ratio_upper, 3), "]")
      ) %>%
      dplyr::select(-err_ratio_lower, -err_ratio_upper)
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
#' @return a list of equal opportunity and its confidence interval
#' @importFrom magrittr %>%
#' @export

balance_positive <- function(data, outcome, group, probs, cutoff = 0.5,
                               confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  exp_pos <- get_exp_pos(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    exp_pos_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_exp_pos(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(se = exp_pos)

    acc %>%
      dplyr::left_join(exp_pos_se, by = group) %>%
      dplyr::mutate(
        exp_pos_lower = expit(logit(exp_pos) - 1.96 * se),
        exp_pos_upper = expit(logit(exp_pos) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), exp_pos, exp_pos_lower, exp_pos_upper) %>%
      dplyr::mutate(
        exp_pos_ci = paste0("[", round(exp_pos_lower, 3), ", ", round(exp_pos_upper, 3), "]")
      ) %>%
      dplyr::select(-exp_pos_lower, -exp_pos_upper)
  } else {
    return(exp_pos)
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
#' @return a list of equal opportunity and its confidence interval
#' @importFrom magrittr %>%
#' @export

balance_negative <- function(data, outcome, group, probs, cutoff = 0.5,
                             confint = TRUE, bootstraps = 1000) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  exp_neg <- get_exp_neg(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  # Calculate confidence interval
  if (confint) {
    exp_neg_se <- lapply(1:bootstraps, function(j) {
      data_boot <- data[sample(nrow(data), replace = TRUE), ]
      get_exp_neg(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
    }) %>%
      do.call(rbind, .) %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarize_all(function(x) stats::sd(logit(x))) %>%
      dplyr::rename(se = exp_neg)

    acc %>%
      dplyr::left_join(exp_neg_se, by = group) %>%
      dplyr::mutate(
        exp_neg_lower = expit(logit(exp_neg) - 1.96 * se),
        exp_neg_upper = expit(logit(exp_neg) + 1.96 * se)
      ) %>%
      dplyr::select(!!rlang::sym(group), exp_neg, exp_neg_lower, exp_neg_upper) %>%
      dplyr::mutate(
        exp_neg_ci = paste0("[", round(exp_neg_lower, 3), ", ", round(exp_neg_upper, 3), "]")
      ) %>%
      dplyr::select(-exp_neg_lower, -exp_neg_upper)
  } else {
    return(exp_neg)
  }
}
