# Load data
library(PheCAP)
data(ehr_data)
data <- PheCAP::PhecapData(ehr_data, "healthcare_utilization", "label", 0.4)$frame
# Binarize outcome (main_ICD)
data <- data %>% mutate(outcome = ifelse(main_ICD > 0, 1, 0))
# Split data into groups (1, 2, 3, 4) based on healthcare_utilization quantiles
data <- data %>% mutate(HU_group = ifelse(healthcare_utilization <= quantile(healthcare_utilization, 0.25), 1,
                                          ifelse(healthcare_utilization <= quantile(healthcare_utilization, 0.5), 2,
                                                 ifelse(healthcare_utilization <= quantile(healthcare_utilization, 0.75), 3, 4))))
# Keep only interested
target_data <- data %>% select(outcome, main_NLP, HU_group, healthcare_utilization, starts_with("NLP"))
# Check if any missing data
sum(is.na(target_data))

# 75% of the sample size
train_size <- floor(0.75 * nrow(target_data))
# Split data
set.seed(321)
train_ind <- sample(seq_len(nrow(target_data)), size=train_size)
train <- target_data[train_ind, ]
x_train <- as.matrix(train %>% select(-outcome, -HU_group))
y_train <- as.matrix(train %>% select(outcome))

test <- target_data[-train_ind, ]
x_test <- as.matrix(test %>% select(-outcome, -HU_group))
y_test <- test %>% select(outcome)

# Manually create 10-folds to make sure the results are reproducible
set.seed(30)
folds <- caret::createFolds(y_train, k = 10, list = TRUE)

# Convert the list to a vector where each element indicates the fold number
fold_ids <- rep(NA, length(y_train))
for(i in 1:10) {
  fold_ids[folds[[i]]] <- i
}

# To choose lambda (regularization parameter), cross-validation is typically used
# Pass the fold_ids to cv.glmnet through the foldid argument
cv_fit <- glmnet::cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, foldid = fold_ids)

# Best lambda value
best_lambda <- cv_fit$lambda.min

# Fit logistic Regression
fit <- glmnet::glmnet(x_train, y_train, family = "binomial", alpha=1, lambda = best_lambda)

# Predictions
cut_off <- 0.4
test$prob <- predict(fit, newx = x_test, type = "response", s = best_lambda)[,1]
mean(ifelse(test$prob > cut_off, 1, 0) == y_test)

# The dataframe would need predictions, outcome, HU_group
test <- test %>%
  mutate(pred = ifelse(prob > cut_off, 1, 0))

# Calculate the fairness metrics
test <- test %>%
  mutate(hu_group = ifelse(HU_group == 1, paste0("0 to ", quantile(data$healthcare_utilization, 0.25)),
                           ifelse(HU_group ==2, paste0(quantile(data$healthcare_utilization, 0.25), " to ", quantile(data$healthcare_utilization, 0.5)),
                                  ifelse(HU_group == 3, paste0(quantile(data$healthcare_utilization, 0.5), " to ", quantile(data$healthcare_utilization, 0.75)), paste0(quantile(data$healthcare_utilization, 0.75), " to ", quantile(data$healthcare_utilization, 1))))))


# Calculate the Max Min Difference
max_min_diff <- test %>% dplyr::group_by(hu_group) %>%
  summarise(max_min_diff = max(test$prob) - min(test$prob))

max_min_diff

# Calculate the Max-Min Ratio
max_min_ratio <- test %>% dplyr::group_by(hu_group) %>%
  summarise(max_min_ratio = max(test$prob)/min(test$prob))

max_min_ratio

# Calculate the Max Absolute Difference
mean_ab_diff <- test %>% dplyr::group_by(hu_group) %>%
  summarise(mean_ab_diff = abs(max(test$prob) - mean(test$prob)))

mean_ab_diff

# Calculate the Mean Absolute Deviance (MAD)
mad <- test %>% dplyr::group_by(hu_group) %>%
  summarise(MAD = mad(test$prob, center=mean(test$prob)))

mad

# Calculate the variance
variance <- test %>% dplyr::group_by(hu_group) %>%
  summarise(MSE = mean((test$pred - mean(test$outcome))^2))

variance

# generalized entropy index

