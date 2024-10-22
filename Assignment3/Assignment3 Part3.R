library(ggplot2)

getwd()
NYHouse <- read.csv('Assignment3/NY-House-Dataset.csv')
attach(NYHouse)


# Fit the linear model
model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = NYHouse)

# Summary of the model to check the significance of variables
summary(model)

# Scatterplot of PROPERTYSQFT vs PRICE with best fit line
ggplot(NYHouse, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatterplot of Property Size vs Price",
       x = "Property Size (sqft)",
       y = "Price") +
  theme_minimal()

# Plot the residuals
residuals <- model$residuals

# Residual plot
ggplot(NYHouse, aes(x = fitted(model), y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals of the Linear Model",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()


# Create a subset of the dataset where PRICE > 500000
subset_NYHouse <- subset(NYHouse, PRICE > 500000)

# Fit the linear model on the subset
model_subset <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = subset_NYHouse)

# Summary of the new model
summary(model_subset)

# Scatterplot of PROPERTYSQFT vs PRICE with best fit line for the subset
ggplot(subset_NYHouse, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatterplot of Property Size vs Price (Subset)",
       x = "Property Size (sqft)",
       y = "Price") +
  theme_minimal()

# Plot the residuals for the subset
residuals_subset <- model_subset$residuals

# Residual plot for the subset
ggplot(subset_NYHouse, aes(x = fitted(model_subset), y = residuals_subset)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals of the Linear Model (Subset)",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()