# Note to self: remove rows from rmse_summary with this code:
# rmse_summary <- rmse_summary %>% filter(!row_number() %in% c(5:7))

# If I need longer data for any reason:
#l onger_data <- abalone %>% select(-Sex) %>% 
  # pivot_longer(LongestShell:ShellWeight, names_to = "variable", values_to = "response")

install.packages("AppliedPredictiveModeling")
# load the library
library(AppliedPredictiveModeling)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(corrplot)
library(glmnet)
library(rmarkdown)
library(RColorBrewer)

# Abalone Data
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

cor_table = abalone %>% select(-Sex, -Age) %>% cor()
corrplot(cor_table, method = 'number', diag = FALSE) # colorful number

# Conduct independent t-test: Are males significantly larger than females?
# test that homogeneity of variance is achieved (prereq for t test)
abalone_test <- abalone_clean %>% filter(Sex %in% c("M", "F"))
res <- var.test(WholeWeight ~ Sex, data = abalone_test)
res

# no significant difference between these two variances, remove "I" and move forward with t-test

t.test(WholeWeight ~ Sex, var.equal = TRUE, data = abalone_test)
# significant differences in weight by sex

# Incorporate cross-validation into LR models
# Use regularization to deal with the small sample sizes of abalones in the higher age groups
# use multiple models (LR for prediction of age as continuous variable and random forest for prediction of age as categorical/interval variable)

train_index <- createDataPartition(abalone_clean$Age, p = 0.8, list = FALSE)
train_set <- abalone_clean[train_index, ]
test_set <- abalone_clean[-train_index, ]

lm_fit <- train_set %>% lm(Age ~ WholeWeight, data = .)
pred <- predict(lm_fit, test_set)
head(pred)
rmse_1 <- RMSE(test_set$Age, pred)

rmse_summary <- tibble(Model = "Linear Reg - Weight", RMSE = rmse_1)
rmse_summary

plot_data <- data.frame(Predicted_value = pred,   
                        Observed_value = test_set$Age) 

ggplot(plot_data, aes(x = Predicted_value, y = Observed_value)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "#66C2A5")

lm_fit <- train_set %>% lm(Age ~ WholeWeight + Height, data = .)
pred <- predict(lm_fit, test_set)
rmse_2 <- RMSE(test_set$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - Weight + Height",
                                 RMSE = rmse_2))
rmse_summary

lm_fit <- train_set %>% lm(Age ~ WholeWeight + Height + Diameter, data = .)
pred <- predict(lm_fit, test_set)
rmse_3 <- RMSE(test_set$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - Weight + Height + Diameter",
                                 RMSE = rmse_3))
rmse_summary

lm_fit <- train_set %>% lm(Age ~ WholeWeight + Height + Diameter + LongestShell, data = .)
pred <- predict(lm_fit, test_set)
rmse_4 <- RMSE(test_set$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - Weight + Height + Diameter + Longest Shell",
                                 RMSE = rmse_4))
rmse_summary

lm_fit <- train_set %>% lm(Age ~ ., data = .)
pred <- predict(lm_fit, test_set)
rmse_5 <- RMSE(test_set$Age, pred)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg - All Variables",
                                 RMSE = rmse_5))
rmse_summary


# Perform elastic net regression regularization technique, determining lambda using cross-validation 
# Lasso (L1) regression works best when your model contains lot of useless variables, shrinks some parameters to 0 to create a simpler model
# Ridge (L2) regression works best when most variables in the model are useful, shrinks parameters but does not remove them
# Elastic net regression combines the two penalties and is especially good at dealing with situations
# when there are correlations between parameters

# Perform Ridge Regression and see if it reduces RMSE
# First, transform Sex to integer so that we can run a confusion matrix on the results
train_set_numeric <- train_set %>% mutate(Sex_Integer = as.integer(train_set$Sex))
test_set_numeric <- test_set %>% mutate(Sex_Integer = as.integer(test_set$Sex))

# Next, define predictor names so that Age is not included as a predictor during model fitting
# and so that the "Sex" variable does not coerce the entire matrix variable classes to characters

predictor_names <- colnames(train_set_numeric)[!colnames(train_set_numeric) %in% c("Age", "Sex")]

alpha0.fit <- cv.glmnet(x= as.matrix(train_set_numeric[,predictor_names]), y= train_set_numeric$Age, family= "gaussian", 
          type.measure = "mse", alpha = 0, nlambda = 100)

alpha0.predicted <- predict(alpha0.fit, s = alpha0.fit$lambda.min, newx = as.matrix(test_set_numeric[,predictor_names]))
rmse_6 <- RMSE(test_set_numeric$Age, alpha0.predicted)
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg w Ridge (L2) Regression",
                                 RMSE = rmse_6))

# Perform Lasso Regression and see if it reduces RMSE

alpha1.fit <- cv.glmnet(x= as.matrix(train_set_numeric[,predictor_names]), y= train_set_numeric$Age, family= "gaussian", 
                        type.measure = "mse", alpha = 1, nlambda = 100)

alpha1.predicted <- predict(alpha1.fit, s = alpha1.fit$lambda.min, newx = as.matrix(test_set_numeric[ , predictor_names]))
rmse_7 <- RMSE(test_set_numeric$Age, alpha1.predicted)
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Linear Reg w Lasso (L1) Regression",
                                 RMSE = rmse_7))
rmse_summary

# Perform Elastic Net Regression
alpha0.5.fit <- cv.glmnet(x= as.matrix(train_set_numeric[ , predictor_names]), y= train_set_numeric$Age, family= "gaussian", 
                        type.measure = "mse", alpha = 0.5, nlambda = 100)

alpha0.5.predicted <- predict(alpha0.5.fit, s = alpha0.5.fit$lambda.min, newx = as.matrix(test_set_numeric[ , predictor_names]))
rmse_8 <- RMSE(test_set_numeric$Age, alpha0.5.predicted)
rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Elastic Net Regression",
                                 RMSE = rmse_8))
rmse_summary

# TO DO: use weights based on correlation, see if it improves rmse
# see ?variable.names example for use of weights in linear regression

# TO DO: Lasso (L1) Regression seems to yield the best result, but to understand whether this really wins, 
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
    coord_cartesian(ylim = c(2.375, 2.400)) +
    scale_y_continuous(breaks = seq(2.375, 2.400, by = 0.025)) +
    theme_minimal()
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
# TO DO: transform predictions into factors with same levels as abalone set so that confusion matrix can be run
rf_model <- randomForest(Age ~ ., data = train_set_numeric, ntree = 500)
predictions <- predict(rf_model, test_set_numeric)
rmse_9 <- RMSE(predictions, test_set_numeric$Age)

rmse_summary <- bind_rows(rmse_summary,
                          tibble(Model = "Random Forest",
                                 RMSE = rmse_9))
rmse_summary

# fine tune the model:

# Specify the training control settings for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Define the parameter grid
param_grid <- expand.grid(
  mtry = c(2, 4, 6)  # Adjust the number of features to consider at each split
  # nodesize = c(1, 5, 10),  # Minimum size of terminal nodes
  # ntree = c(100, 200, 300)  # Number of trees
)

# Create the Random Forest model with grid search
rf_model <- train(
  x = train_set_numeric[, colnames(train_set_numeric) != "Age"],
  y = train_set_numeric$Age,
  method = "rf",
  trControl = ctrl,
  tuneLength = 8, # Number of combinations in the grid
  tuneGrid = param_grid,
  ntree = 1000,
  nodesize = 5
)

# Display the best tuning parameters
print(rf_model)

# Access the best model with optimal parameters
predictions <- predict(rf_model, test_set)
rmse_10 <- RMSE(predictions, test_set_numeric$Age)
rmse_10
