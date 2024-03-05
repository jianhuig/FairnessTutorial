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

#' Fit a random forest model
#' @param data a dataframe object
#' @return Dataframe with outcome, predicted probability, and protected
#' attributes
#' @importFrom randomForest randomForest
#' @export

fit_rf <- function(data) {
  # Calculate the number of missing values per column
  missing_values <- sapply(data, function(x) sum(is.na(x)))

  # Calculate the percentage of missing values per column
  missing_values_percentage <- sapply(data, function(x) {
    sum(is.na(x)) / length(x) * 100
  })

  # Identify columns with more than 10% missing values
  columns_to_remove <- names(
    missing_values_percentage[missing_values_percentage > 10]
  )

  # Remove these columns
  data <- data[, !(names(data) %in% columns_to_remove)]

  # Impute remaining missing values with median
  for (i in which(sapply(data, function(x) any(is.na(x))))) {
    data[[i]][is.na(data[[i]])] <- median(data[[i]], na.rm = TRUE)
  }

  # Identify columns that have only one unique value
  cols_with_one_value <- sapply(data, function(x) length(unique(x)) == 1)

  # Subset the dataframe to remove these columns
  data <- data[, !cols_with_one_value]

  # Remove specified columns
  data <- data[ , !(names(data) %in% c("hosp_exp_flg", "icu_exp_flg",
                                       "mort_day_censored", "censor_flg"))]

  # Use 700 labels to train the data
  train_data <- data[1:700, ]
  # Fit a random forest model
  rf_model <- randomForest::randomForest(factor(train_data$day_28_flg) ~ .,
                           data = train_data, seed = 123)

  # Test the model on the remaining data
  test_data <- data[701:nrow(data), ]
  test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[,2]

  return(test_data)
}

