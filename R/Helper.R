# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Define logit function
#' @param p a numeric object
#' @noRd

logit <- function(p) {
  epsilon <- 1e-6
  # Ensure p is not exactly 0 or 1 by adding or subtracting epsilon
  p <- ifelse(p <= epsilon, epsilon, ifelse(p >= 1 - epsilon, 1 - epsilon, p))
  return(log(p / (1 - p)))
}

#' Define expit function
#' @param x a numeric object
#' @noRd

expit <- function(x) {
  1 / (1 + exp(-x))
}

#' Define resample by group function
#' @param group_data a dataframe object
#' @noRd
resample_group <- function(group_data) {
  sample(group_data, size = length(group_data), replace = TRUE)
}
