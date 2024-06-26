% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fairness.R
\name{eval_pred_equality}
\alias{eval_pred_equality}
\title{Examine predictive equality of a model}
\usage{
eval_pred_equality(
  data,
  outcome,
  group,
  probs,
  cutoff = 0.5,
  alpha = 0.05,
  bootstraps = 2500,
  digits = 2,
  message = TRUE
)
}
\arguments{
\item{data}{Data frame containing the outcome, predicted outcome, and
sensitive attribute}

\item{outcome}{Name of the outcome variable, it must be binary}

\item{group}{Name of the sensitive attribute}

\item{probs}{Name of the predicted outcome variable}

\item{cutoff}{Threshold for the predicted outcome, default is 0.5}

\item{alpha}{The 1 - significance level for the confidence interval, default is 0.05}

\item{bootstraps}{Number of bootstrap samples, default is 2500}

\item{digits}{Number of digits to round the results to, default is 2}

\item{message}{Whether to print the results, default is TRUE}

\item{confint}{Whether to compute 95\% confidence interval, default is TRUE}
}
\value{
A list containing the following elements:
\itemize{
\item FPR_Group1: False Positive Rate for the first group
\item FPR_Group2: False Positive Rate for the second group
\item FPR_Diff: Difference in False Positive Rate
If confidence intervals are computed (\code{confint = TRUE}):
\item FPR_Diff_CI: A vector of length 2 containing the lower and upper bounds
of the 95\% confidence interval for the difference in False Positive Rate
}
}
\description{
Examine predictive equality of a model
}
