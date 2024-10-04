library(e1071)  # For Naive Bayes
library(caret)  # For confusion matrix
library(ggplot2)  # For plotting
library(class)  # For KNN

# Read data
abalone <- read.csv("D:/titta/RPI2024/Courses/Data Analytics/DataAnalytics2024_Sampoorna/Lab 2/abalone.data", header = FALSE)

# Rename columns (fixed typos)
colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")

# Derive age group based on the number of rings
abalone$age.group <- cut(abalone$rings, breaks = c(-1, 8, 11, 35), labels = c("young", "adult", "old"))

# Summary of the age groups
summary(abalone$age.group)

# Drop the 'sex' and 'rings' columns for training the classifier
abalone_filtered <- abalone[, -c(1, 9)]  # Dropping 'sex' (1st column) and 'rings' (9th column)

# Train Naive Bayes classifier using all data
classifier <- naiveBayes(abalone_filtered[, 1:7], abalone_filtered[, 8])  # Train on other features to predict 'age.group'

# Predict classes
prediction <- predict(classifier, abalone_filtered[, 1:7])

# Add the prediction to the abalone dataframe
abalone$prediction <- prediction  # To view actual vs prediction

# Evaluate the prediction using a contingency table
contingency.table <- table(prediction, abalone$age.group, dnn = list('predicted', 'actual'))

# Print the contingency table
print(contingency.table)  # Diagonal gives correct predictions

# Get model parameters for 'whole_weight' for different age groups
parameters <- classifier$tables$whole_weight
print(parameters)

# Extract mean and standard deviations for each age group from the parameters
m1 <- parameters["young", 1][[1]]  # Mean for 'young'
sd1 <- parameters["young", 2][[1]]  # Standard deviation for 'young'
m2 <- parameters["adult", 1][[1]]  # Mean for 'adult'
sd2 <- parameters["adult", 2][[1]]  # Standard deviation for 'adult'
m3 <- parameters["old", 1][[1]]  # Mean for 'old'
sd3 <- parameters["old", 2][[1]]  # Standard deviation for 'old'

# Plot the normal distribution of 'whole_weight' for the 3 age groups
x_range <- seq(0, 3, length.out = 100)  # Adjust the range as necessary

# Plot the distributions
plot(x_range, dnorm(x_range, m1, sd1), type = "l", col = "red", 
     xlab = "Whole Weight", ylab = "Density", 
     main = "Whole Weight Distribution for Age Groups")
lines(x_range, dnorm(x_range, m2, sd2), col = "green")
lines(x_range, dnorm(x_range, m3, sd3), col = "blue")
legend("topright", legend = c("Young", "Adult", "Old"), 
       col = c("red", "green", "blue"), lty = 1)



contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(abalone[,8]) #diagonal has best value , sum of those values divided by the total number of datapoints to understand how best model fits



#For 3 dataset

# Function to train and evaluate Naive Bayes model
train_and_evaluate_nb <- function(data, features, target) {
  # Split data into training and test sets
  set.seed(123)
  trainIndex <- createDataPartition(data[[target]], p = 0.8, list = FALSE)
  trainData <- data[trainIndex, features]
  testData <- data[-trainIndex, features]
  trainTarget <- data[trainIndex, target]
  testTarget <- data[-trainIndex, target]
  
  # Train Naive Bayes model
  model <- naiveBayes(trainData, trainTarget)
  
  # Make predictions
  predictions <- predict(model, testData)
  
  # Evaluate model using confusion matrix
  confusion <- confusionMatrix(predictions, testTarget)
  
  # Return confusion matrix and model
  list(confusion_matrix = confusion, model = model)
}

# Function to plot the distribution of a class along a feature
plot_class_distribution <- function(data, feature, target_column) {
  ggplot(data, aes_string(x = feature, fill = target_column)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of", target_column, "across", feature),
         x = feature, y = "Density") +
    theme_minimal()
}

# Feature subsets for the analysis
subset1 <- c("length", "diameter", "height")
subset2 <- c("whole_weight", "shucked_weight", "viscera_weight")
subset3 <- c("sex", "shell_weight", "length")

# Train and evaluate models for each subset
result1 <- train_and_evaluate_nb(abalone, features = subset1, target = "age.group")
result2 <- train_and_evaluate_nb(abalone, features = subset2, target = "age.group")
result3 <- train_and_evaluate_nb(abalone, features = subset3, target = "age.group")

# Compare confusion matrices
print("Confusion Matrix for Subset 1 (length, diameter, height):")
print(result1$confusion_matrix)

print("Confusion Matrix for Subset 2 (whole_weight, shucked_weight, viscera_weight):")
print(result2$confusion_matrix)

print("Confusion Matrix for Subset 3 (sex, shell_weight, length):")
print(result3$confusion_matrix)

# Plot the distribution of classes along three different features
plot1 <- plot_class_distribution(abalone, "length", "age.group")
plot2 <- plot_class_distribution(abalone, "whole_weight", "age.group")
plot3 <- plot_class_distribution(abalone, "shell_weight", "age.group")

# Display the plots
print(plot1)
print(plot2)
print(plot3)


############################ 
#working with 2 Data young 
abalone= # subset
  
  
  #####################################################################

########## kNN ###########
n <- nrow(abalone)
print(n)

# Training set indexes, sample 70% of the data
set.seed(123)
train.indexes <- sample(1:n, size = 0.7 * n, replace = FALSE)

# Create training and test sets
abalone.train <- abalone[train.indexes, ]
abalone.test <- abalone[-train.indexes, ]

# Set k value based on sqrt of 2923 (approximately 55)
k <- 55

# Train KNN model
KNNpred <- knn(train = abalone.train[,1:7], test = abalone.test[,1:7], cl = abalone.train$age.group, k = k)

# Evaluate using contingency table
contingency.table <- table(predicted = KNNpred, actual = abalone.test$age.group)
print(contingency.table)

# Accuracy
contingency.matrix <- as.matrix(contingency.table)
accuracy <- sum(diag(contingency.matrix)) / length(abalone.test$age.group)
print(paste("Accuracy with k =", k, ":", accuracy))

# Run KNN with multiple k values and plot accuracy
accuracy <- c()
ks <- seq(5, 105, 10)

for (k in ks) {
  KNNpred <- knn(train = abalone.train[,1:7], test = abalone.test[,1:7], cl = abalone.train$age.group, k = k)
  cm <- as.matrix(table(predicted = KNNpred, actual = abalone.test$age.group))
  accuracy <- c(accuracy, sum(diag(cm)) / length(abalone.test$age.group))
}

# Plot the accuracy for different k values
plot(ks, accuracy, type = "b", col = "blue", xlab = "k value", ylab = "Accuracy", main = "Accuracy vs k in KNN")


data(iris)

# Set seed for reproducibility
set.seed(123)

# Split data into training (70%) and test (30%) sets
n <- nrow(iris)
train.indexes <- sample(1:n, size = 0.7 * n)
iris.train <- iris[train.indexes, ]
iris.test <- iris[-train.indexes, ]

# Subset 1: Use Sepal.Length and Sepal.Width
train.subset1 <- iris.train[, c("Sepal.Length", "Sepal.Width")]
test.subset1 <- iris.test[, c("Sepal.Length", "Sepal.Width")]

# Subset 2: Use Petal.Length and Petal.Width
train.subset2 <- iris.train[, c("Petal.Length", "Petal.Width")]
test.subset2 <- iris.test[, c("Petal.Length", "Petal.Width")]

# Define a function to perform kNN and evaluate accuracy
evaluate_knn <- function(train_data, test_data, train_labels, test_labels, k_values) {
  accuracy <- c()
  
  for (k in k_values) {
    # Train and predict using kNN
    knn_pred <- knn(train = train_data, test = test_data, cl = train_labels, k = k)
    
    # Create confusion matrix
    contingency.table <- table(Predicted = knn_pred, Actual = test_labels)
    print(paste("Confusion Matrix for k =", k))
    print(contingency.table)
    
    # Calculate accuracy
    correct_preds <- sum(diag(contingency.table))
    total_preds <- length(test_labels)
    accuracy <- c(accuracy, correct_preds / total_preds)
  }
  
  return(accuracy)
}

# Define the range of k values to test
k_values <- seq(1, 15, 2)

# Evaluate kNN on Subset 1 (Sepal Length and Sepal Width)
accuracy_subset1 <- evaluate_knn(train.subset1, test.subset1, iris.train$Species, iris.test$Species, k_values)

# Evaluate kNN on Subset 2 (Petal Length and Petal Width)
accuracy_subset2 <- evaluate_knn(train.subset2, test.subset2, iris.train$Species, iris.test$Species, k_values)

# Plot accuracy for both feature subsets
plot(k_values, accuracy_subset1, type = "b", col = "blue", ylim = c(0, 1),
     xlab = "k", ylab = "Accuracy", main = "Accuracy for kNN with Different Subsets of Features")
lines(k_values, accuracy_subset2, type = "b", col = "red")
legend("bottomright", legend = c("Sepal Features", "Petal Features"), col = c("blue", "red"), lty = 1)
