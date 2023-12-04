# Note to self: remove rows from rmse_summary with this code:
# rmse_summary <- rmse_summary %>% filter(!row_number() %in% c(5:7))

# If I need longer data for any reason:
#longer_data <- abalone %>% select(-Sex) %>% 
  # pivot_longer(LongestShell:ShellWeight, names_to = "variable", values_to = "response")

# Install packages and open libraries
package_names <- c("AppliedPredictiveModeling", 
                   "ggplot2", "dplyr", "tidyverse", "caret",
                   "randomForest", "corrplot", "glmnet", "rmarkdown",
                   "RColorBrewer", "xgboost")
# Load libraries
libraries_to_load <- c("AppliedPredictiveModeling", 
                       "ggplot2", "dplyr", "tidyverse", "caret",
                       "randomForest", "corrplot", "glmnet", "rmarkdown",
                       "RColorBrewer", "xgboost")
for (lib in libraries_to_load) {
  library(lib, character.only = TRUE)
}

# Load abalone Data
data(abalone)
dim(abalone)
str(abalone)
head(abalone)

# Clean Data
# Add column for abalone age based on # of rings
# remove old Rings column so as not to introduce issues associated with perfect multicollinearity when running models
# Add age column: The age of an abalone is represented by its number of rings plus 1.5 as number of years lived.

abalone <- abalone %>% mutate(Age = Rings + 1.5) %>% rename(Sex = Type) %>% select(-Rings)

# Clean data - remove obvious errors/outliers

apply(abalone,2,min)
# looks like height has some values of 0 that should be removed

# Check for erroneous values: where shucked weight is greater than whole weight and viscera weight is greater than whole weight
sum(abalone$ShuckedWeight >= abalone$WholeWeight)
# there are 4 values here that should be removed

# Check that there aren't any viscera weights greater than or equal to whole weight that might need removal
sum(abalone$VisceraWeight >= abalone$WholeWeight)
# There are no values here that need to be removed

# Generate a clean dataset based on results

abalone_clean <- abalone %>% filter(ShuckedWeight <= WholeWeight) %>% filter(Height != 0)

# EDA
# Use color-blind friendly color palette. Looks like palettes, "Paired", "Set2", and "Dark2"
# all fall into this category
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n = 8, name = 'Paired')
brewer.pal(n = 8, name = "Paired")

# Univariate EDA

# define shortcut for applying general formatting to plots
gen_formatting <- theme(plot.title = element_text(face = "bold"),
                        axis.title.y = element_text(margin = unit(c(0, 20, 0, 0), "pt"))
)

# Visualize distribution of abalone weights in the full dataset
plot1 <- ggplot(abalone_clean, aes(x = WholeWeight)) + 
  geom_histogram(fill = "#FDBF6F", color = "#FF7F00", alpha = 0.7) +
  labs(title = "Distribution of Abalone Weight",
       x = "Weight (grams)",
       y = "Count") +
  gen_formatting
plot1

# Visualize distribution of abalone weights by Sex
plot2 <- ggplot(abalone_clean, aes(x = Sex, y = WholeWeight, fill = Sex)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Abalone Weight by Sex",
       x = "Sex",
       y = "Weight (grams)") +
  scale_fill_brewer(palette = "Set2", labels = c("Female", "Immature", "Male")) +
  scale_x_discrete(labels = c("Female", "Immature", "Male")) +
  gen_formatting
plot2

# Conduct independent t-test: Are females significantly larger than males?
# test that homogeneity of variance is achieved (prereq for t-test)
abalone_test <- abalone_clean %>% filter(Sex %in% c("M", "F"))
res <- var.test(WholeWeight ~ Sex, data = abalone_test)
res
# Ratio of variances is around 0.84, showing that variance is similar between male and female groups
# no significant difference between these two variances, remove "I" and move forward with t-test

t.test(WholeWeight ~ Sex, var.equal = TRUE, data = abalone_test)
# significant differences in weight by sex
# The t-value is 3.2305. This represents the number of standard deviations that the sample mean (mean in group F minus mean in group M) is from the null hypothesis mean (0, assuming no difference between the groups). 
# A higher absolute t-value indicates a larger difference between the groups.
# In summary, the low p-value (0.00125) and the 95% confidence interval (not including 0) suggest that there is evidence to reject the null hypothesis.

# Visualize average abalone weight by age group
plot3 <- abalone_clean %>% filter(Sex %in% c("M", "F")) %>% group_by(Age) %>% 
  summarise(avg_weight = mean(WholeWeight)) %>% 
  ggplot(aes(x = Age, y = avg_weight)) + 
  geom_point(size = 3, shape = 23, color = "#FF7F00", fill = "#FDBF6F") +
  labs(title = "Average Weight per Abalone Age Group",
       x = "Age Group",
       y = "Average Weight by Age Group (grams)") +
  gen_formatting
plot3

# Visualize age distribution within the abalone dataset
plot4 <- abalone %>% select(-Sex) %>% group_by(Age) %>%  ggplot(aes(Age)) + 
  geom_bar(fill = "#66C2A5", color = "#B3B3B3") +
  labs(title = "Distribution of Abalone Age",
       x = "Age",
       y = "Count") +
  gen_formatting
plot4

# Visualize distribution of abalone age by Sex
facet_colors_fill <- c("#A6CEE3", "#FB9A99", "#B2DF8A")
facet_colors_color <- c("#1F78B4", "#E31A1C", "#33A02C")

plot5 <- ggplot(abalone_clean, aes(x = Age, fill = as.factor(Sex), color = as.factor(Sex))) +
  geom_histogram(binwidth = 2, alpha = 0.7) +  # Custom bar color
  scale_fill_manual(values = setNames(facet_colors_fill, unique(abalone_clean$Sex)),
                    name = "Sex",
                    labels = c(F = "Female", I = "Immature", M = "Male")) +
  scale_color_manual(values = setNames(facet_colors_color, unique(abalone_clean$Sex)),
                     name = "Sex",
                     labels = c(F = "Female", I = "Immature", M = "Male")) + 
  facet_grid(Sex ~ ., labeller = as_labeller(c(F = "Female", I = "Immature", M = "Male"))) +
  labs(title = "Distribution of Abalone Age by Sex",
       x = "Age",
       y = "Count") +
  gen_formatting
plot5

# Multivariate EDA

cor_table = abalone_clean %>% select(-Sex) %>% cor()
corplot1 <- corrplot(cor_table, method = 'number', diag = FALSE) # colorful number
corplot1
# Indicates that we will need to be careful re: multicollinearity when running ML algos
# Features that are highly correlated with the age (e.g., ShellWeight, Diameter) might be good candidates for inclusion in your machine learning model.
# Viscera Weight and Whole Weight are highly correlated, would include using dimensionality reduction here if it was a larger dataset
# Shows us that shell characteristics (ShellWeight, Diameter, LongestShell) may be more predictive of age than full body or fleshy features

# Incorporate cross-validation into LR models
# Use regularization to deal with the small sample sizes of abalones in the higher age groups
# use multiple models (LR for prediction of age as continuous variable and random forest for prediction of age as categorical/interval variable)

train_index <- createDataPartition(abalone_clean$Age, p = 0.8, list = FALSE)
train_set <- abalone_clean[train_index, ]
test_set <- abalone_clean[-train_index, ]

# Let's run a simple model (to use for comparison) with just Shell Weight as a predictor

lm_fit <- train_set %>% lm(Age ~ ShellWeight, data = .)
pred <- predict(lm_fit, test_set)
head(pred)
rmse_1 <- RMSE(test_set$Age, pred)

rmse_summary <- tibble(Model = "Linear Reg - Shell Weight", RMSE = rmse_1)
rmse_summary

# Visualize predicted vs actual values
plot_data <- data.frame(Predicted_value = pred,   
                        Observed_value = test_set$Age) 

ggplot(plot_data, aes(x = Observed_value, y = Predicted_value)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "#66C2A5", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")

# Let's see what happens when we add in Height
# Chose height because it still has a relatively high correlation with age (0.56)
# but is less closely correlated than Diameter with Age (0.82 for Height vs 0.91 for Diameter)
# Multicollinearity still high, but not as high as it would be with Diameter

lm_fit <- train_set %>% lm(Age ~ ShellWeight + Height, data = .)
pred <- predict(lm_fit, test_set)
rmse_2 <- RMSE(test_set$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - Shell Weight + Height",
                                 RMSE = rmse_2))
rmse_summary

# Worse predictive power

# Let's try using all of the other predictors in the linear regression model

lm_fit <- train_set %>% lm(Age ~ ., data = .)
pred <- predict(lm_fit, test_set)
rmse_3 <- RMSE(test_set$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - All Predictor Variables",
                                 RMSE = rmse_3))
rmse_summary

# That's much better! Can we improve even further with regularization?

# First, transform Sex to integer so that all predictor variables are numeric
train_set_numeric <- train_set %>% mutate(Sex_Integer = as.integer(train_set$Sex))
test_set_numeric <- test_set %>% mutate(Sex_Integer = as.integer(test_set$Sex))

# Since regularization methods are sensitive to the scale of the input features, 
# let's (a) ensure our features are on a similar scale and (b) if not, use a feature
# scaling technique such as standardization

# (a) Ensure features are on a similar scale:
scale_names <- colnames(train_set_numeric)[!colnames(train_set_numeric) %in% c("Age", "Sex")]

# Check the means and standard deviations of the features
feature_summary <- data.frame(
  Feature = scale_names,
  Mean = colMeans(train_set_numeric[, scale_names]),
  SD = apply(train_set_numeric[, scale_names], 2, sd)
)

feature_summary
# Looks like features are not on a similar scale

# Implement feature scaling using the scale() function
train_set_scaled <- train_set_numeric
train_set_scaled[, scale_names] <- scale(train_set_scaled[, scale_names])

# Check means and SDs after scaling
# Now, check the means and standard deviations after scaling
scaled_feature_summary <- data.frame(
  Feature = scale_names,
  Mean = colMeans(train_set_scaled[, scale_names]),
  SD = apply(train_set_scaled[, scale_names], 2, sd)
)

scaled_feature_summary

# Apply same scaling transformation to the test set to ensure consistency
test_set_scaled <- test_set_numeric
test_set_scaled[, scale_names] <- scale(test_set_scaled[, scale_names],
                                            center = colMeans(train_set_numeric[, scale_names]),
                                            scale = apply(train_set_numeric[, scale_names], 2, sd))


# Perform elastic net regression regularization technique, determining lambda using cross-validation 
# Lasso (L1) regression works best when your model contains lot of useless variables, shrinks some parameters to 0 to create a simpler model
# Ridge (L2) regression works best when most variables in the model are useful, shrinks parameters but does not remove them
# Elastic net regression combines the two penalties and is especially good at dealing with situations
# when there are correlations between parameters

# Perform Ridge Regression and see if it reduces RMSE

# Define predictor names so that Age is not included as a predictor during model fitting
# and so that the "Sex" variable does not coerce the entire matrix variable classes to characters

predictor_names <- colnames(train_set_scaled)[!colnames(train_set_scaled) %in% c("Age", "Sex")]

alpha0.fit <- cv.glmnet(x= as.matrix(train_set_scaled[,predictor_names]), y= train_set_scaled$Age, family= "gaussian", 
          type.measure = "mse", alpha = 0, nlambda = 100)

alpha0.predicted <- predict(alpha0.fit, s = alpha0.fit$lambda.min, newx = as.matrix(test_set_scaled[,predictor_names]))
rmse_4 <- RMSE(test_set_scaled$Age, alpha0.predicted)
rmse_4
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg w Ridge (L2) Regression",
                                 RMSE = rmse_4))
rmse_summary

# Perform Lasso Regression and see if it reduces RMSE

alpha1.fit <- cv.glmnet(x= as.matrix(train_set_scaled[,predictor_names]), y= train_set_scaled$Age, family= "gaussian", 
                        type.measure = "mse", alpha = 1, nlambda = 100)

alpha1.predicted <- predict(alpha1.fit, s = alpha1.fit$lambda.min, newx = as.matrix(test_set_scaled[ , predictor_names]))
rmse_5 <- RMSE(test_set_scaled$Age, alpha1.predicted)
rmse_5
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg w Lasso (L1) Regression",
                                 RMSE = rmse_5))
rmse_summary

# Perform Elastic Net Regression
alpha0.5.fit <- cv.glmnet(x= as.matrix(train_set_scaled[ , predictor_names]), y= train_set_scaled$Age, family= "gaussian", 
                        type.measure = "mse", alpha = 0.5, nlambda = 100)

alpha0.5.predicted <- predict(alpha0.5.fit, s = alpha0.5.fit$lambda.min, newx = as.matrix(test_set_scaled[ , predictor_names]))
rmse_6 <- RMSE(test_set_scaled$Age, alpha0.5.predicted)
rmse_6
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Elastic Net Regression",
                                 RMSE = rmse_6))
rmse_summary

# TO DO: use weights based on correlation, see if it improves rmse
# see ?variable.names example for use of weights in linear regression

# Lasso (L1) Regression seems to yield the best result, but to understand whether this really wins, 
# we need to try many different values for alpha in elastic net.

fit_values <- list()

for (i in 0:10) {
  fit_name <- paste0("alpha", i/10)
  
  fit_values[[fit_name]] <-
    cv.glmnet(x = as.matrix(train_set_numeric[ , predictor_names]), y= train_set_numeric$Age, family= "gaussian", 
              type.measure = "mse", alpha = i/10, nlambda = 100)
}

results <- data.frame()

for (i in 0:10) {
  fit_name <- paste0("alpha", i/10)
  
  predicted <- 
    predict(fit_values[[fit_name]], 
            s = fit_values[[fit_name]]$lambda.min, newx = as.matrix(test_set_numeric[ , predictor_names]))
  
  rmse <- RMSE(test_set_numeric$Age, predicted)
  
  temp <- data.frame(alpha = i/10, mse = rmse, fit_name = fit_name)
  results <- rbind(results, temp)
}

results

results <- results %>% rename(rmse = mse, alphas = alpha)

# visualize results without the outlier (alpha0)
plot_results <-  ggplot(results, aes(x = alphas, y = rmse, color = fit_name)) +
    geom_point(position = position_jitter(width = 0.2), size = 3) +
    labs(title = "RMSE vs. Alphas",
         x = "Alphas",
         y = "Root Mean Squared Error") +
    coord_cartesian(ylim = c(2.315, 2.325)) +
    scale_y_continuous(breaks = seq(2.315, 2.325, by = 0.005)) +
  scale_y_continuous(trans='log10') +
  gen_formatting
plot_results

# since it's difficult to tell which alpha is the lowest just from the graph, print result below:
results$alphas[which.min(results$rmse)]

# See if there is a better result that can be obtained by finding the best combination of alpha and lambda

alpha_values <- seq(0, 1, by = 0.1)  # 0 for Ridge, 1 for Lasso, 0.5 for Elastic Net
lambda_values <- seq(from = 0.02, to = 0.0000001, length.out = 100)

# Create an empty matrix to store cross-validated errors
cv_errors <- matrix(NA, nrow = length(alpha_values), ncol = length(lambda_values),
                    dimnames = list(as.character(alpha_values), as.character(lambda_values)))

# Perform the grid search
for (i in seq_along(alpha_values)) {
  alpha <- alpha_values[i]
  
  # Run cross-validated glmnet with a vector of lambda values
  cv_model <- cv.glmnet(x = as.matrix(train_set_numeric[, predictor_names]), y = train_set_numeric$Age,
                        alpha = alpha, lambda = lambda_values)
  
  # Store the cross-validated errors
  cv_errors[i, ] <- cv_model$cvm
}

cv_errors

# try random forest model instead
rf_model <- randomForest(Age ~ ., data = train_set, ntree = 500)
predictions <- predict(rf_model, test_set)
rmse_7 <- RMSE(predictions, test_set$Age)
rmse_7

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Random Forest - Default Params",
                                 RMSE = rmse_7))
rmse_summary

# fine tune the model:

# Specify the training control settings for cross-validation (with a 10-fold cross validation)
ctrl <- trainControl(method = "cv", number = 10)

# We could use the tuneGrid argument of the train() function in the caret package to do this,
# but let's try adjusting parameters using the randomForest() function instead 

param_grid <- expand.grid(
  mtry = c(2, 4, 6),
  nodesize = c(1,3,5),
  ntree = c(100, 300, 500)
)

# Create an empty list to store the models
models <- list()

# Iterate over each combination of parameters
for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  # Train the model with the current set of parameters
  model <- randomForest(
    x = train_set[, colnames(train_set_numeric) != "Age"],
    y = train_set$Age,
    mtry = params$mtry,
    nodesize = params$nodesize,
    ntree = params$ntree
  )
  
  # Store the trained model
  models[[paste(params$mtry, params$nodesize, params$ntree, sep = "_")]] <- model
}

names(models)

# Create an empty vector to store the RMSE values
rmse_values <- numeric(length(models))

# Evaluate each model on the test set
for (i in seq_along(models)) {
  model <- models[[i]]
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = test_set[, colnames(test_set) != "Age"])
  
  # Calculate RMSE
  rmse_values[i] <- RMSE(predictions, test_set$Age)
}

# Find the index of the model with the lowest RMSE
best_model_index <- which.min(rmse_values)

# Access the best model
best_model <- models[[best_model_index]]
names(models[best_model_index])
# looks like the best parameters are mtry = 2, nodesize = 5, and ntree = 500

# Print the RMSE values
rmse_8 <- rmse_values[best_model_index]

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Random Forest - Tuned Params",
                                 RMSE = rmse_8))
rmse_summary

# We could try more values for fine tuning, but it would take a lot of run time
# and it doesn't seem like tuning is making a significant difference in RMSE, anyway

# Let's try a gradient boosting model

# Separate out x and y variables

x_train <- subset(train_set_numeric, select = -c(Age, Sex))
y_train <- train_set_numeric$Age
x_test <- subset(test_set_numeric, select = -c(Age, Sex))
y_test <- test_set_numeric$Age

# Convert data to the format expected by xgboost
dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)


# Set hyperparameters (you may need to tune these)
params <- list(
  objective = "reg:squarederror",  # Regression task
  eval_metric = "rmse",            # Evaluation metric (Root Mean Squared Error)
  max_depth = 2,                 # Maximum depth of a tree
  eta = 0.1,               # Learning rate
  nrounds = 400     # Number of boosting rounds
)

# Train the xgboost model
xgb_model <- xgboost(data = dtrain, params = params, nrounds = 100)

# Make predictions on the training set (you can use a separate test set for predictions)
predictions <- predict(xgb_model, as.matrix(x_test))
rmse9 <- RMSE(predictions, y_test)
rmse9

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Gradient Boosted Model",
                                 RMSE = rmse_9))
rmse_summary

# Tune the model

# Create a matrix for training and initialize testing data


# Define hyperparameter grid
param_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(3, 6, 9),
  gamma = c(0, 0.1, 0.2),
  subsample = c(0.8, 1.0),
  colsample_bytree = c(0.8, 1.0),
  min_child_weight = c(1, 3, 5)
)

# Function to train and evaluate model for a given set of hyperparameters
train_and_evaluate <- function(params, train_data, test_data) {
  # Convert non-numeric columns to numeric using one-hot encoding
  train_data_matrix <- model.matrix(~ . - 1, data = train_data)
  test_data_matrix <- model.matrix(~ . - 1, data = test_data)
  
  # Create DMatrix for training and testing data
  dtrain <- xgb.DMatrix(data = train_data_matrix, label = train_data$Age)
  dtest <- xgb.DMatrix(data = test_data_matrix, label = test_data$Age, missing = NA, nthread = 1)
  
  # Train the model
  xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)
  
  # Make predictions on the test set
  predictions <- predict(xgb_model, newdata = dtest)
  
  # Evaluate the model
  rmse <- sqrt(mean((predictions - test_data$Age)^2))
  
  return(data.frame(params, rmse))
}

# Apply the function to all combinations of hyperparameters
results <- apply(param_grid, 1, function(row) {
  params <- as.list(row)
  train_and_evaluate(params, train_set_numeric, test_set_numeric)
})


# Combine results into a data frame
results_df <- do.call(rbind, results)

# Find the best hyperparameters
best_params <- results_df[which.min(results_df$rmse), ]
print(best_params)

# Let's try running our model again with these "best" parameters
params <- list(
  objective = "reg:squarederror",  # Regression task
  eval_metric = "rmse",            # Evaluation metric (Root Mean Squared Error)
  max_depth = 3,                 # Maximum depth of a tree
  eta = 0.1,     
  gamma = 0,
  subsample = 0.8,
  colsample_bytree = 1,
  min_child_weight = 1
)

xgb_model <- xgboost(data = dtrain, params = params, nrounds = 100)
predictions <- predict(xgb_model, as.matrix(x_test))
rmse_10 <- RMSE(predictions, y_test)
rmse_10

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Gradient Boosted Model - Optimized Params",
                                 RMSE = rmse_10))
rmse_summary

# It seems like we are running into issues with overfitting and the RMSE is not improving
# even after optimizing parameters for the XG boosted model

# Let's try stacking models
# Re-split the data from the original abalone_clean dataset into training, validation, and test sets

# Assuming your dataset is in a data frame called 'data'
set.seed(123)  # Set seed for reproducibility

# Create a vector of indices for the stratified split
abalone_clean_df <- abalone_clean %>% mutate(Sex_Integer = as.integer(Sex)) %>% select(-c(Sex)) %>% mutate(Sex_Factor = as.factor(Sex_Integer))
abalone_clean_age_numeric = abalone_clean_df$Age
train_set_indices <- createDataPartition(abalone_clean_age_numeric, p = 0.7, list = FALSE)

# Create the training set
train_set <- abalone_clean_df[train_set_indices, ]

# Create a numeric with all the remaining indices.
# The purpose of this numeric is to be split 50/50 
# between validation and test set
abalone_clean_age_numeric_remaining <- abalone_clean_age_numeric[-train_set_indices]

# Create the validation and test sets
val_set_indices <- createDataPartition(abalone_clean_age_numeric_remaining, p = 0.5, list = FALSE)
val_set <- abalone_clean_df[val_set_indices, ]

test_set_indices = abalone_clean_age_numeric_remaining[-val_set_indices]
test_set <- abalone_clean_df[test_set_indices, ]

# Train base models
rf_model <- randomForest(Age ~ ., data = train_set)
xgb_model <- xgboost(data = as.matrix(subset(train_set, select = -c(Age, Sex_Factor))), label = train_set$Age, nrounds = 100, verbose = 0)

# Make predictions on the validation set
rf_pred <- predict(rf_model, newdata = val_set)
xgb_pred <- predict(xgb_model, newdata = as.matrix(subset(val_set, select = -c(Age, Sex_Factor))))

# Combine predictions for stacking
stacking_data <- data.frame(rf_pred, xgb_pred, Age = val_set$Age)

# Train meta-model (e.g., linear regression) on the stacked predictions
meta_model <- lm(Age ~ ., data = stacking_data)

# Make predictions on the test set
rf_test_pred <- predict(rf_model, newdata = test_set)
xgb_test_pred <- predict(xgb_model, newdata = as.matrix(subset(test_set, select = -c(Age, Sex_Factor))))

# Combine predictions for stacking on the test set and preserve the structure of stacking_data
stacking_test_data <- data.frame(rf_test_pred, xgb_test_pred, Age = test_set$Age)

# Make final predictions using the meta-model
final_predictions <- predict(meta_model, newdata = stacking_test_data)

# Evaluate the final predictions
final_rmse <- RMSE(final_predictions, test_set$Age)
print(final_rmse)





