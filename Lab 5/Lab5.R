library(rattle)
library(e1071)
library(caret)

data(wine)

wine.new= wine[c("Type","Alcohol","Malic","Ash","Alcalinity","Magnesium","Nonflavanoids","Proanthocyanins","Color","Hue","Proline")]

set.seed(42)
trainIndex <- createDataPartition(wine.new$Type, p = .7, list = FALSE)
wine_train <- wine.new[trainIndex, ]
wine_test <- wine.new[-trainIndex, ]

# Tune SVM with Linear Kernel
#tune_linear <- tune.svm(Type ~ ., data = wine_train, kernel = "linear", gamma = 0.69, cost = .01)
#print(summary(tune_linear))
tuned.linear <- tune.svm(Type ~ ., data = wine_train, kernel = 'linear',gamma = seq(1/2^nrow(wine.new),1, .01), cost = 2^seq(-6, 4, 2))
tuned.linear
tuned.polynomial <- tune.svm(Type ~ ., data = wine_train, kernel = 'polynomial',gamma = seq(1/2^nrow(wine.new),1, .01), cost = 2^seq(-6, 4, 2))
tuned.polynomial

svm.modlin <- svm(Type ~ ., data = wine_train, kernel = 'linear', gamma = 2.610122e-54, cost = 0.015625)
svm.modlin 
svn.modpol <- svm(Type ~ ., data = wine_train,kernel = 'polynomial', gamma = 0.46, cost = 0.015625)
svn.modpol

ConfusionMatrix<- function(Type, pred){
  cm = as.matrix(table(Actual = Type, Predicted = pred))
  
  cm
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted 
  
  recall = diag / rowsums 
  precision = diag / colsums
  f1 = 2 * precision * recall / (precision + recall) 
  
  data.frame(precision, recall, f1)
}


train.pred <- predict(svm.modlin, wine_train)
conf_matrix_svmlin <- ConfusionMatrix(wine_train$Type, train.pred)

train.pred2 <- predict(svn.modpol, wine_train)
conf_matrix_svmpol<- ConfusionMatrix(wine_train$Type, train.pred2)


# Train Naive Bayes model
nb_model <- naiveBayes(Type ~ ., data = wine_train)
pred_nb <- predict(nb_model, wine_test)
conf_matrix_nb <- ConfusionMatrix(pred_nb, wine_test$Type)

getwd()
NYHouse <- read.csv('Lab 5/NY-House-Dataset.csv')

# Train an SVM regression model
tune_linear <- tune.svm(PRICE ~ PROPERTYSQFT, data = NYHouse, kernel = 'linear',gamma = seq(1/2^nrow(NYHouse),1, .01), cost = 2^seq(-6, 4, 2))
svm_model <- svm(PRICE ~ PROPERTYSQFT, data = NYHouse, kernel = 'linear', gamma = 2.610122e-54, cost = 0.015625)
svm_predictions <- predict(svm_model, NYHouse)

# Train a linear regression model
lm_model <- lm(PRICE ~ PROPERTYSQFT, data = NYHouse)
lm_predictions <- predict(lm_model, NYHouse)

# Plot SVM predictions
ggplot(NYHouse, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_line(aes(x = PROPERTYSQFT, y = svm_predictions), color = "red") +
  labs(title = "SVM Regression: Predicted vs Actual Prices",
       x = "Square Footage", y = "Price") +
  theme_minimal()

# Plot Linear Model predictions
ggplot(NYHouse, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_line(aes(x = PROPERTYSQFT, y = lm_predictions), color = "green") +
  labs(title = "Linear Regression: Predicted vs Actual Prices",
       x = "Square Footage", y = "Price") +
  theme_minimal()

# Model evaluation - SVM
svm_results <- postResample(svm_predictions, NYHouse$PRICE)
print(svm_results)

# Model evaluation - Linear Regression
lm_results <- postResample(lm_predictions, NYHouse$PRICE)
print(lm_results)





