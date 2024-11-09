library(rattle)
library(ggfortify)
library(e1071)
library(caret)
library(class)
library(psych)

data(wine)
str(wine)

sum(is.na(wine))

summary(wine)
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

wine.pc <- princomp(wine[,-1], cor = TRUE, score = TRUE)
principal_components<-prcomp(wine[,-1], center = TRUE, scale. = TRUE)

summary(principal_components)
principal_components$rotation


# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# Plot the first and second PCs using autoplot
autoplot(principal_components, data = wine, colour = 'Type',
         x = 1, y = 2, # specify the 1st and 2nd PC
         loadings = TRUE, loadings.colour = 'blue', 
         loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("Wine Dataset PCA - 1st and 2nd Principal Components") 

loadings <- principal_components$rotation[, 1]
top_contributors <- names(sort((loadings), decreasing = TRUE)[1:3])
print("Top contributors to the 1st PC:")
print(top_contributors)




knn.predall <- knn(train = wine[,-1], test = wine[,-1], cl = wine$Type, k = 5)

## evaluate
cm <- table(Predicted=knn.predall, Actual = wine$Type, dnn=list('predicted','actual'))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision_all = diag / colsums 
recall_all = diag / rowsums 
f1_all = 2 * precision * recall / (precision + recall) 

data.frame(recall_all, precision_all, f1_all)



knn.pred_pc3 <- knn(train = wine.pc$scores[,c(1,2,3)], test = wine.pc$scores[,c(1,2,3)], cl = wine$Type, k = 5)

## evaluate
cm <- table(Predicted=knn.pred_pc3, Actual = wine$Type, dnn=list('predicted','actual'))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision_pc3 = diag / colsums 
recall_pc3 = diag / rowsums 
f1_pc3 = 2 * precision * recall / (precision + recall) 

data.frame(recall_pc3, precision_pc3, f1_pc3)

loadings <- principal_components$rotation[, 1]
top_contributors <- names(sort((loadings), decreasing = FALSE)[1:3])
print("Least contributors to the 1st PC:")
print(top_contributors)

wine.new= wine[c("Type","Alcohol","Malic","Ash","Alcalinity","Magnesium","Nonflavanoids","Proanthocyanins","Color","Hue","Proline")]
principal_components.new<-prcomp(wine.new[,-1], center = TRUE, scale. = TRUE)

summary(principal_components.new)
principal_components.new$rotation


# using the plot() function, we can plot the principal components.
plot(principal_components.new)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components.new, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components.new)

## using autoplot() function to plot the components
autoplot(principal_components.new, data = wine.new, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

