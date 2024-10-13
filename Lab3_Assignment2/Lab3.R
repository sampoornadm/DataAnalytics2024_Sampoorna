library("class")
library("e1071")
library("ggplot2")
setwd("/Lab3_Assignment2")
epi<- read.csv('Lab3_Assignment2/epi2024results_DA_F24_lab03.csv')
attach(epi)
getwd()
episub1 <-subset(epi,region=='Southern Asia')
attach(episub1)

print(summary(episub1$EPI))
print(sd(episub1$EPI))
hist(episub1$EPI)
hist(episub1$EPI, seq(20, 60, 5.0), prob=TRUE)
lines(density(episub1$EPI,na.rm=TRUE,bw=2))
lines(density(episub1$EPI,na.rm=TRUE,bw="SJ"))

qqnorm(episub1$EPI,main = "QQ Plot for Region 1: Southern Asia")
qqline(episub1$EPI)


episub2 <-subset(epi,region=='Latin America & Caribbean')
attach(episub2)

print(summary(episub2$EPI))
print(sd(episub2$EPI))
hist(episub2$EPI)
hist(episub2$EPI, seq(20, 70, 5.0), prob=TRUE)
lines(density(episub2$EPI,na.rm=TRUE,bw=2))
lines(density(episub2$EPI,na.rm=TRUE,bw="SJ"))

qqnorm(episub2$EPI, main = "QQ Plot for Region 2: Latin America & Caribbean")
qqline(episub2$EPI)


# question 2
linear_model <- lm(EPI ~ ECO + BDH + SPI + CHA + GTI, data = epi)
summary(linear_model)
ggplot(epi, aes(x = ECO, y = BDH)) +  geom_point() +  stat_smooth(method = "lm")+ ggtitle("Scatter Plot with Fitted Line: ECO vs BDH Total") +  xlab("ECO") +  ylab("BDH")

#Residual standard error: 3.574 on 172 degrees of freedom
#Multiple R-squared:  0.9077,	Adjusted R-squared:  0.905 
#F-statistic: 338.2 on 5 and 172 DF,  p-value: < 2.2e-16

linear_model_region1 <- lm(EPI ~ ECO + BDH + SPI + CHA + GTI, data = episub1)
summary(linear_model_region1)
ggplot(episub1, aes(x = ECO, y = BDH)) +  geom_point() +  stat_smooth(method = "lm")+ ggtitle("Scatter Plot with Fitted Line: ECO vs BDH Total") +  xlab("ECO") +  ylab("BDH")

#Residual standard error: 1.491 on 1 degrees of freedom
#Multiple R-squared:  0.9912,	Adjusted R-squared:  0.9474 
#F-statistic:  22.6 on 5 and 1 DF,  p-value: 0.1583

linear_model_region2 <- lm(EPI ~ ECO + BDH + SPI + CHA + GTI, data = episub2)
summary(linear_model_region2)
ggplot(episub2, aes(x = ECO, y = BDH)) +  geom_point() +  stat_smooth(method = "lm")+ ggtitle("Scatter Plot with Fitted Line: ECO vs BDH Total") +  xlab("ECO") +  ylab("BDH")


#Residual standard error: 3.978 on 26 degrees of freedom
#Multiple R-squared:  0.6095,	Adjusted R-squared:  0.5344 
#F-statistic: 8.117 on 5 and 26 DF,  p-value: 0.0001011


# 3.1 Choose 5 variables and select 3 regions
region_filter1 <- epi[epi$region %in% c("Greater Middle East", "Eastern Europe", "Asia-Pacific"), ]

subset1 <- region_filter1[, c("ECO", "BDH", "SPI", "CHA", "GTI", "region")]
subset1 <- na.omit(subset1)

set.seed(123)
n = nrow(subset1)
train_index1 <- sample(n, n * 0.7) 
train_epi1 <- subset1[train_index1, ]
test_epi1 <- subset1[-train_index1, ]

train_x1 <- train_epi1[, -6]
train_y1 <- train_epi1[, 6]
test_x1 <- test_epi1[, -6]   
test_y1 <- test_epi1[, 6]

KNNpred <- knn(train = train_x1, test = test_x1, cl = train_y1, k = 3)


contingency_table1 <- table(Predicted = KNNpred, Actual = test_y1, dnn=list('predicted','actual'))
print(contingency_table1)

accuracy <- sum(diag(as.matrix(contingency_table1))) / length(test_y1)
print(paste("Accuracy for Model 1:", accuracy1))

accuracy <- c()
ks <- seq(1,20,1)

for (k in ks) {
  
  KNNpred <- knn(train = train_x1, test = test_x1, cl = train_y1, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = test_y1, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(test_y1)) 
  
}
print(length(accuracy))
plot(ks,accuracy,type = "b")


region_filter2 <- epi[epi$region %in% c("Sub-Saharan Africa", "Global West", "Latin America & Caribbean"), ]

# Select the same 5 variables and the 'region' column
subset2 <- region_filter2[, c("ECO", "BDH", "SPI", "CHA", "GTI", "region")]
subset2 <- na.omit(subset2)

# Split into training and testing sets
set.seed(123)
n = nrow(subset2)
train_index2 <- sample(n, n * 0.7)
train_epi2 <- subset2[train_index2, ]
test_epi2 <- subset2[-train_index2, ]

# Prepare the features and labels for training and testing
train_x2 <- train_epi2[, -6]  # Features
train_y2 <- train_epi2[, 6]   # Labels (regions)
test_x2 <- test_epi2[, -6]    # Features for testing
test_y2 <- test_epi2[, 6]     # Labels for testing

# Train the second kNN model
KNNpred2 <- knn(train = train_x2, test = test_x2, cl = train_y2, k = 1)

# Create a contingency table (confusion matrix)
contingency_table2 <- table(Predicted = KNNpred2, Actual = test_y2)
print(contingency_table2)

# Convert the contingency table to a matrix and calculate accuracy
accuracy2 <- sum(diag(as.matrix(contingency_table2))) / length(test_y2)
print(paste("Accuracy for Model 2:", accuracy2))


accuracy2 <- c()
ks2 <- seq(1,20,1)

for (k in ks) {
  
  KNNpred2 <- knn(train = train_x2, test = test_x2, cl = train_y2, k = k)
  cm2 = as.matrix(table(Actual=KNNpred2, Predicted = test_y2, dnn=list('predicted','actual')))
  
  accuracy2 <- c(accuracy2,sum(diag(cm2))/length(test_y2)) 
  
}
print(length(accuracy2))
plot(ks2,accuracy2,type = "b")

#4

ggplot(subset1, aes(x = subset1$ECO, y = subset1$BDH, colour = subset1$region)) +  geom_point()

set.seed(123)
km <- kmeans(subset1[,-6], centers = 3)
km$tot.withinss
assigned_clusters <- as.factor(km$cluster)

ggplot(subset1, aes(x = subset1$ECO, y = subset1$BDH, colour = assigned_clusters)) +  geom_point()

wss <- c()
ks<-1:10
for (k in ks) {
  km <- kmeans(subset1[,-6], centers = k)
  wss <- c(wss,km$tot.withinss)
}
plot(ks,wss,type = "b")


ggplot(subset2, aes(x = subset2$ECO, y = subset2$BDH, colour = subset2$region)) +  geom_point()
set.seed(123)

km2 <- kmeans(subset2[,-6], centers = 3)
km2$tot.withinss
assigned_clusters2 <- as.factor(km2$cluster)

ggplot(subset2, aes(x = subset2$ECO, y = subset2$BDH, colour = assigned_clusters2)) +  geom_point()

wss2 <- c()

ks<-1:10
for (k in ks) {
  km2 <- kmeans(subset2[,-6], centers = k)
  wss2 <- c(wss2,km2$tot.withinss)
}
plot(ks,wss2,type = "b")
