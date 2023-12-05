# Abalone Age Prediction Project

# Install packages and open libraries
package_names <- c("AppliedPredictiveModeling", 
                   "ggplot2", "dplyr", "tidyverse", "caret",
                   "randomForest", "corrplot", "glmnet", "rmarkdown",
                   "RColorBrewer", "xgboost", "DAAG", "Metrics")
# Load libraries
libraries_to_load <- c("AppliedPredictiveModeling", 
                       "ggplot2", "dplyr", "tidyverse", "caret",
                       "randomForest", "corrplot", "glmnet", "rmarkdown",
                       "RColorBrewer", "xgboost", "DAAG", "Metrics")
for (lib in libraries_to_load) {
  library(lib, character.only = TRUE)
}

# Load abalone Data
data(abalone)
dim(abalone)
str(abalone)

# Clean Data

# Create a new column for "Age" (which is just Rings + 1.5), 
# remove the Rings column,
# and rename "Type" to "Sex"

abalone <- abalone %>% mutate(Age = Rings + 1.5) %>% rename(Sex = Type) %>% select(-Rings)

# Check that there are no "0" measurements present by observing minimum of each variable

apply(abalone, 2, min)
# looks like height has some values of 0 that should be removed

# Check for erroneous values: 
# where shucked weight is greater than whole weight
sum(abalone$ShuckedWeight >= abalone$WholeWeight)
# there are 4 values here that should be removed

# where viscera weight is greater than whole weight
sum(abalone$VisceraWeight >= abalone$WholeWeight)
# There are no values here that need to be removed

# Generate a clean dataset based on results

abalone_clean <- abalone %>% filter(ShuckedWeight <= WholeWeight) %>% filter(Height != 0)
dim(abalone_clean)

## Univariate EDA

# Prior to plotting and graphing,
# let's look into using a color-blind friendly color palette. Looks like palettes, "Paired", "Set2", and "Dark2"
# all fall into this category
display.brewer.all(colorblindFriendly = TRUE)

# We will be using the "Paired" and "Set2" palettes, so let's note their hexadecimal color names for future use
display.brewer.pal(n = 8, name = 'Paired')
brewer.pal(n = 8, name = "Paired")
display.brewer.pal(n = 8, name = 'Set2')
brewer.pal(n = 8, name = "Set2")

# define shortcut for applying general formatting to plots to save processing time later
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

# Based on the plot above, it's difficult to tell whether females are significantly heavier than males.

# Conduct independent t-test: Are females significantly heavier than males?
# test that homogeneity of variance is achieved (prerequisite for t-test)
abalone_test <- abalone_clean %>% filter(Sex %in% c("M", "F"))
res <- var.test(WholeWeight ~ Sex, data = abalone_test)
res
# Ratio of variances is around 0.84, showing that variance is similar between male and female groups
# no significant difference between these two variances, so we can move forward with our t-test
# Move forward with t-test

t.test(WholeWeight ~ Sex, var.equal = TRUE, data = abalone_test)
# there are significant differences in weight by sex

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

# This plot shows a general increasing trend and direct relationship between age group and average weight.
# However, there are some funky-looking values in the top right-hand corner of the plot (for age groups of around 18 yrs and above)
# Average is really only a good central tendency measure when the data in each age group is normally distributed or has low sample size.
# It is possible that the distribution of weight data in the higher age groups is not normally distributed or has low sample size.

# Let's investigate the number of observations in each age group:

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

# Looks like there is a generally similar distribution of males and females in each age group.
# Even immature abalones, although obviously much less are present in older age groups, show a similar data distribution
# to males and females

## Multivariate EDA

# Run a correlation plot to look at variable associations
cor_table = abalone_clean %>% select(-Sex) %>% cor()
corplot1 <- corrplot(cor_table, method = 'number', diag = FALSE) # colorful number
corplot1
# Indicates that we will need to be careful re: multicollinearity when running ML algos
# Correlation does not imply causation.

# Machine Learning Applications

# Prepare data for Linear Regression

# Define RMSE function
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

set.seed(100) # Set the seed so that the indices are consistent if running code again

# Generate train and test sets
train_index <- createDataPartition(abalone_clean$Age, p = 0.8, list = FALSE)
train_set <- abalone_clean[train_index, ]
test_set <- abalone_clean[-train_index, ]

# Let's run a simple model (to use for comparison) with just Shell Weight as a predictor

lm_fit <- train_set %>% lm(Age ~ ShellWeight, data = .)
pred <- predict(lm_fit, test_set)
head(pred)
rmse_1 <- RMSE(test_set$Age, pred)

rmse_summary <- tibble(Model = "Linear Reg - Shell Weight", RMSE = round(rmse_1, 3))
rmse_summary

# Visualize predicted vs actual values
plot_data <- data.frame(Predicted_value = pred,   
                        Observed_value = test_set$Age) 

ggplot(plot_data, aes(x = Observed_value, y = Predicted_value)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "#66C2A5", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values") +
  gen_formatting

# Let's try using all of the other predictors in the linear regression model:

lm_fit <- train_set %>% lm(Age ~ ., data = .)
pred <- predict(lm_fit, test_set)
rmse_2 <- RMSE(test_set$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - All Predictor Variables",
                                 RMSE = round(rmse_2, 3)))
rmse_summary

# Let's try to improve the LR model by adding interaction terms

train_set_intxns <- train_set %>% mutate(physical_measurements = Height * Diameter * WholeWeight, 
                                                    internal_features = ShuckedWeight * VisceraWeight,
                                                    shell_features = ShellWeight * Diameter)

test_set_intxns <- test_set %>% mutate(physical_measurements = Height * Diameter * WholeWeight, 
                                          internal_features = ShuckedWeight * VisceraWeight,
                                          shell_features = ShellWeight * Diameter)

lm_fit <- train_set_int_terms %>% lm(Age ~ ., data = .)
pred <- predict(lm_fit, test_set_intxns)
rmse_3 <- RMSE(test_set_intxns$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - All Predictor Variables + Interaction Terms",
                                 RMSE = round(rmse_3, 3)))
rmse_summary

# Apply Regularization

# Transform Sex to a numeric value so that all predictor variables are numeric:
train_set_continuous <- train_set_intxns %>% mutate(Sex_Integer = as.integer(train_set$Sex)) %>% select(-c(Sex))
test_set_continuous <- test_set_intxns %>% mutate(Sex_Integer = as.integer(test_set$Sex)) %>% select(-c(Sex))

# Initialize the scaled data frames
train_set_scaled <- train_set_continuous
test_set_scaled <- test_set_continuous

# Ensure features are on a similar scale:
scale_names_train <- colnames(train_set_continuous)[!colnames(train_set_continuous) %in% c("Age")]
scale_names_test <- colnames(test_set_continuous)[!colnames(test_set_continuous) %in% c("Age")]

# Check the means and standard deviations of the features
feature_summary <- data.frame(
  Feature = scale_names_train,
  Mean = colMeans(train_set_continuous[, scale_names_train]),
  SD = apply(train_set_continuous[, scale_names_train], 2, sd)
)

feature_summary
# Looks like features are not on a similar scale, so let's fix it

# Implement feature scaling using the scale() function
train_set_scaled[, scale_names_train] <- scale(train_set_continuous[, scale_names_train])
test_set_scaled[, scale_names_test] <- scale(test_set_continuous[, scale_names_test])

# Now, check the means and standard deviations after scaling
scaled_feature_summary <- data.frame(
  Feature = scale_names_train,
  Mean = colMeans(train_set_scaled[, scale_names_train]),
  SD = apply(train_set_scaled[, scale_names_train], 2, sd)
)

scaled_feature_summary
# the scaling process has centered the data (resulting in a mean close to zero) 

# Perform Ridge (L2) Regression
# Ridge regression may be more suitable for a dataset with a larger number of predictors, but let's see how our model performs.

# Define predictor names so that Age is not included as a predictor during model fitting
predictor_names <- colnames(train_set_scaled)[!colnames(train_set_scaled) %in% c("Age")]

# Run regularized model and use to output predictions
alpha0.fit <- cv.glmnet(x= as.matrix(train_set_scaled[,predictor_names]), y= train_set_scaled$Age, family= "gaussian", 
          type.measure = "mse", alpha = 0, nlambda = 100)

alpha0.predicted <- predict(alpha0.fit, s = alpha0.fit$lambda.min, newx = as.matrix(test_set_scaled[,predictor_names]))
rmse_4 <- RMSE(test_set_scaled$Age, alpha0.predicted)
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg w Ridge (L2) Regression",
                                 RMSE = round(rmse_4,3)))
rmse_summary

# Perform Lasso (L1) Regression and see if it reduces RMSE

alpha1.fit <- cv.glmnet(x= as.matrix(train_set_scaled[,predictor_names]), y= train_set_scaled$Age, family= "gaussian", 
                        type.measure = "mse", alpha = 1, nlambda = 100)

alpha1.predicted <- predict(alpha1.fit, s = alpha1.fit$lambda.min, newx = as.matrix(test_set_scaled[ , predictor_names]))
rmse_5 <- RMSE(test_set_scaled$Age, alpha1.predicted)
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg w Lasso (L1) Regression",
                                 RMSE = round(rmse_5, 3)))
rmse_summary

# Now, let's try elastic net regression to see if a combination of ridge and lasso penalties may offer 
# a better balance between variable selection and handling multicollinearity.

# Perform Elastic Net Regression
alpha0.5.fit <- cv.glmnet(x= as.matrix(train_set_scaled[ , predictor_names]), y= train_set_scaled$Age, family= "gaussian", 
                        type.measure = "mse", alpha = 0.5, nlambda = 100)

alpha0.5.predicted <- predict(alpha0.5.fit, s = alpha0.5.fit$lambda.min, newx = as.matrix(test_set_scaled[ , predictor_names]))
rmse_6 <- RMSE(test_set_scaled$Age, alpha0.5.predicted)
rmse_6
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Elastic Net Regression",
                                 RMSE = round(rmse_6, 3)))
rmse_summary

# Lasso (L1) Regression or Elastic Net Regression seems to yield the best result, 
# but to understand which combination of penalties is truly best,
# we need to try many different values for alpha by optimizing the elastic net regression.

fit_values <- list() # initialize empty list of fit_values

# for loop to generate models using different values of alpha
for (i in 0:10) {
  fit_name <- paste0("alpha", i/10) 
  
  fit_values[[fit_name]] <-
    cv.glmnet(x = as.matrix(train_set_continuous[ , predictor_names]), y= train_set_continuous$Age, family= "gaussian", 
              type.measure = "mse", alpha = i/10, nlambda = 100)
}

results <- data.frame() # initialize empty dataframe for results

# for loop to run models and showcase results
for (i in 0:10) {
  fit_name <- paste0("alpha", i/10)
  
  predicted <- 
    predict(fit_values[[fit_name]], 
            s = fit_values[[fit_name]]$lambda.min, newx = as.matrix(test_set_continuous[ , predictor_names]))
  
  rmse <- RMSE(test_set_continuous$Age, predicted)
  
  temp <- data.frame(alpha = i/10, mse = rmse, fit_name = fit_name)
  results <- rbind(results, temp)
}

results

results <- results %>% rename(rmse = mse, alphas = alpha)

# Print the best result:
results$alphas[which.min(results$rmse)] # It seems like an alpha value of 0.4 is best here

# Run elastic net regression model with alpha = 0.4

alpha0.4.fit <- cv.glmnet(x= as.matrix(train_set_scaled[ , predictor_names]), y= train_set_scaled$Age, family= "gaussian", 
                          type.measure = "mse", alpha = 0.4, nlambda = 100)

alpha0.4.predicted <- predict(alpha0.4.fit, s = alpha0.4.fit$lambda.min, newx = as.matrix(test_set_scaled[ , predictor_names]))

rmse_7 <- RMSE(test_set_scaled$Age, alpha0.4.predicted)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Elastic Net Regression - Best Alpha",
                                 RMSE = round(rmse_7, 3)))
rmse_summary

# Still not the best RMSE. Move on to testing ensemble methods.

# Random forest model

# Run a random forest model with default parameters:
rf_model <- randomForest(Age ~ ., data = train_set_intxns, ntree = 500)
predictions <- predict(rf_model, test_set_intxns)
rmse_8 <- RMSE(predictions, test_set_intxns$Age)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Random Forest - Default Params",
                                 RMSE = round(rmse_8, 3)))
rmse_summary

# This is our best result yet! 
# Let's try running the same random forest model, but with incorporation of class weights
# (by assigning a higher weight to older age groups with a lower sample size) to address
# the issue of unbalanced data:

# Assign class weights (higher weight for age groups 15 years and older)
class_weights <- ifelse(train_set_intxns$Age >= 15, yes = 2, no = 1)

# Run the model with class weights
rf_model_weights <- randomForest(
  formula = Age ~ .,
  data = train_set_intxns,
  ntree = 500,
  classwt = class_weights
  )

predictions <- predict(rf_model_weights, test_set_intxns)
rmse_rf_wt <- RMSE(predictions, test_set_intxns$Age)
rmse_rf_wt

# It looks like this didn't help improve our RMSE much. Let's try fine-tuning instead.

# Tune the random forest model:

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
# WARNING: Note that this code will take around 5 minutes to run.
for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  # Train the model with the current set of parameters
  model <- randomForest(
    x = train_set_intxns[, colnames(train_set_intxns) != "Age"],
    y = train_set_intxns$Age,
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
  predictions <- predict(model, newdata = test_set_intxns[, colnames(test_set_intxns) != "Age"])
  
  # Calculate RMSE
  rmse_values[i] <- RMSE(predictions, test_set_intxns$Age)
}

# Find the index of the model with the lowest RMSE
best_model_index <- which.min(rmse_values)

# Access the best model
best_model <- models[[best_model_index]]
names(models[best_model_index])
# looks like the best parameters are mtry = 2, nodesize = 5, and ntree = 500

# Print the RMSE values
rmse_9 <- rmse_values[best_model_index]
rmse_9

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Random Forest - Tuned Params",
                                 RMSE = rmse_9))
rmse_summary

# We could try more values for fine tuning, but it would take a lot of run time
# and it doesn't seem like tuning is making a significant difference in RMSE, anyway

# Let's try a gradient boosting model, since it's generally known for improved performance over 
# random forests for unbalanced data

# Separate out x and y variables

x_train <- subset(train_set_continuous, select = -c(Age))
y_train <- train_set_continuous$Age
x_test <- subset(test_set_continuous, select = -c(Age))
y_test <- test_set_continuous$Age

# Convert data to the format expected by xgboost
dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)

# Set hyperparameters (we may need to tune these)
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
rmse_10 <- RMSE(predictions, y_test)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Gradient Boosted Model - Default Params",
                                 RMSE = round(rmse_10, 3)))
rmse_summary

# Tune the model

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
  train_data <- train_set_continuous
  test_data <- test_set_continuous
  train_and_evaluate(params, train_data, test_data)
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
rmse_11 <- RMSE(predictions, y_test)
rmse_11

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Gradient Boosted Model - Tuned Params",
                                 RMSE = round(rmse_11,3)))
rmse_summary

# It is possible that the xgboost model ran into issues with overfitting

# Either way, our tuned random forest model is the winner! 
# Let's take a look at a sorted version of our RMSE Summary table:

rmse_summary_sorted <- rmse_summary %>%
  arrange(RMSE)
rmse_summary_sorted

