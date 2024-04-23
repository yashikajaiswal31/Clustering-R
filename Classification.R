# Install necessary packages
install.packages("naniar")
install.packages("plotly")
install.packages("ggcorrplot")
install.packages("knitr")
install.packages("class")
install.packages("pROC")
install.packages("rpart.plot")

# Load required libraries
library(pROC)
library(class)
library(knitr)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)
library(naniar)
library(tidyverse)
library(ggcorrplot)  # For finding correlation with variables
library(caTools)      # For splitting data into training and test sets
library(caret)

# Read the dataset
data_cancer <- read.csv("data.csv")

# Explore the dataset
head(data_cancer)
str(data_cancer)

# Visualize numeric variables
data_1 <- data_cancer %>%
  as.data.frame() %>%
  select_if(is.numeric) %>%
  gather(key = "variable", value = "value")
ggplot(data_1, aes(value)) +
  geom_density() +
  facet_wrap(~variable)

# Prepare the target variable
data_cancer$diagnosis <- factor(data_cancer$diagnosis, levels = c("M", "B"), labels = c(0, 1))
data_cancer$diagnosis <- as.character(data_cancer$diagnosis)
data_cancer$diagnosis <- as.numeric(data_cancer$diagnosis)
data_cancer <- data_cancer %>% relocate(diagnosis, .after = fractal_dimension_worst)

# Check for missing values
sum(is.na(data_cancer))

# Split the data into training and test sets
split <- sample.split(data_cancer$diagnosis, SplitRatio = 0.7)
train_set <- subset(data_cancer, split == TRUE)
test_set <- subset(data_cancer, split == FALSE)

# Initialize vectors to store accuracy values
train_accuracy <- numeric()
test_accuracy <- numeric()





# Define a range of k values to try
k_values <- 1:20

# Loop through each k value
for (k in k_values) {
  # Fitting k-NN to the Training set
  y_pred_knn_train <- knn(train = train_set[, 2:31],
                          test = train_set[, 2:31],
                          cl = train_set[, 32],
                          k = k,
                          prob = TRUE)
  
  # Calculate training accuracy
  train_cm_knn <- table(train_set[, 32], y_pred_knn_train)
  train_accuracy_knn <- sum(diag(train_cm_knn)) / sum(train_cm_knn)
  train_accuracy <- c(train_accuracy, train_accuracy_knn)
  
  # Predicting the Test set results
  y_pred_knn_test <- knn(train = train_set[, 2:31],
                         test = test_set[, 2:31],
                         cl = train_set[, 32],
                         k = k,
                         prob = TRUE)
  
  # Calculate test accuracy
  cm_knn <- table(test_set[, 32], y_pred_knn_test)
  test_accuracy_knn <- sum(diag(cm_knn)) / sum(cm_knn)
  test_accuracy <- c(test_accuracy, test_accuracy_knn)
}

# Create a data frame with k values and corresponding accuracies
accuracy_df <- data.frame(k = k_values, train_accuracy = train_accuracy, test_accuracy = test_accuracy)

# Plot the graph
ggplot(accuracy_df, aes(x = k)) +
  geom_line(aes(y = train_accuracy, color = "Training Accuracy")) +
  geom_line(aes(y = test_accuracy, color = "Test Accuracy")) +
  labs(title = "Accuracy vs. k",
       x = "k",
       y = "Accuracy") +
  scale_color_manual(values = c("Training Accuracy" = "blue", "Test Accuracy" = "red")) +
  theme_minimal()

# Fitting K-NN to the Training set and Predicting the Test set results
y_predknn <- knn(train = train_set[, 2:31],
                 test = test_set[, 2:31],
                 cl = train_set[, 32],
                 k = 4,
                 prob = TRUE)

# Making the Confusion Matrix
cmknn <- table(test_set[, 32], y_predknn)

# Calculate training accuracy for k-NN
train_pred_knn <- knn(train = train_set[, 2:31],
                      test = train_set[, 2:31],
                      cl = train_set[, 32],
                      k = 5,
                      prob = TRUE)
train_cmknn <- table(train_set[, 32], train_pred_knn)
train_accuracy_knn <- sum(diag(train_cmknn)) / sum(train_cmknn)
print(paste("Training Accuracy of k-NN classifier:", round(train_accuracy_knn * 100, 2), "%"))

# Calculate test accuracy for k-NN
test_accuracy_knn <- sum(diag(cmknn)) / sum(cmknn)
print(paste("Test Accuracy of k-NN classifier:", round(test_accuracy_knn * 100, 2), "%"))

# Load e1071 package for Naive Bayes
library(e1071)

# Train Naive Bayes classifier
classifier_bayes <- naiveBayes(x = train_set[, 2:31],
                               y = train_set$diagnosis)

# Predicting the Test set results
y_pred_bayes <- predict(classifier_bayes, newdata = test_set[, 2:31])

# Making the Confusion Matrix
cm_bayes <- table(test_set[, 32], y_pred_bayes)

# Calculate training accuracy for Naive Bayes
train_pred_bayes <- predict(classifier_bayes, newdata = train_set[, 2:31])
train_cm_bayes <- table(train_set[, 32], train_pred_bayes)
train_accuracy_bayes <- sum(diag(train_cm_bayes)) / sum(train_cm_bayes)
print(paste("Training Accuracy of Naive Bayes classifier:", round(train_accuracy_bayes * 100, 2), "%"))

# Calculate test accuracy for Naive Bayes
test_accuracy_bayes <- sum(diag(cm_bayes)) / sum(cm_bayes)
print(paste("Test Accuracy of Naive Bayes classifier:", round(test_accuracy_bayes * 100, 2), "%"))

library("rpart")

# Fit Decision Tree to the Training set
classifier_tree <- rpart(diagnosis ~ ., data = train_set[, 2:32], method = "class")

# Predicting the Training set results
train_pred_tree <- predict(classifier_tree, newdata = train_set[, 2:31], type = "class")

# Making the Confusion Matrix for training set
train_cm_tree <- table(train_set[, 32], train_pred_tree)
train_accuracy_tree <- sum(diag(train_cm_tree)) / sum(train_cm_tree)
print(paste("Training Accuracy of Decision Tree classifier:", round(train_accuracy_tree * 100, 2), "%"))

# Predicting the Test set results
test_pred_tree <- predict(classifier_tree, newdata = test_set[, 2:31], type = "class")

# Making the Confusion Matrix for test set
cm_tree <- table(test_set[, 32], test_pred_tree)
test_accuracy_tree <- sum(diag(cm_tree)) / sum(cm_tree)
print(paste("Test Accuracy of Decision Tree classifier:", round(test_accuracy_tree * 100, 2), "%"))


# Load rpart.plot package
library(rpart.plot)

# Display the decision tree
rpart.plot(classifier_tree)

# Calculate ROC curves for all three models 
roc_knn <- roc(response = test_set[, 32], predictor = as.numeric(y_predknn))
roc_bayes <- roc(response = test_set[, 32], predictor = as.numeric(y_pred_bayes))
roc_tree <- roc(response = test_set[, 32], predictor = as.numeric(test_pred_tree))

# Create an empty plot
plot(roc_knn, col = "blue", main = "ROC Curves", lwd = 2)

# Add ROC curves for Naive Bayes and Decision Tree
lines(roc_bayes, col = "red", lwd = 2)
lines(roc_tree, col = "green", lwd = 2)

# Extract AUC values for all models
auc_knn <- round(auc(roc_knn), 2)
auc_bayes <- round(auc(roc_bayes), 2)
auc_tree <- round(auc(roc_tree), 2)

# Add legend with AUC values
legend_text <- c(paste("k-NN (AUC =", auc_knn, ")", sep = ""),
                 paste("Naive Bayes (AUC =", auc_bayes, ")", sep = ""),
                 paste("Decision Tree (AUC =", auc_tree, ")", sep = ""))
legend("bottomright", legend = legend_text, col = c("blue", "red", "green"), lwd = 2)

# Create a data frame with model names and their accuracies
accuracy_data <- data.frame(Model = c("k-NN", "Naive Bayes", "Decision Tree"),
                            Train_Accuracy = c(train_accuracy_knn, train_accuracy_bayes, train_accuracy_tree),
                            Test_Accuracy = c(test_accuracy_knn, test_accuracy_bayes, test_accuracy_tree))

# Print the table of accuracies
print(accuracy_data)
