# Project Title: Your Project Name Here
# Description: Fetal Health Analysis: Predictive Modeling Using CTG Data
# Authors: Adam Elamin, Martina Mileva, Roy Yeoun, Basel Abdelhadi, Jasmine Taylor
# Date: 29 March 2024

# Import necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(caret)

df <- read.csv("fetal_health.csv")

# Quick exploration
head(df)
summary(df)
str(df)

# Check for NA values
sum(is.na(df))

# Depending on your findings, you may need to preprocess data (e.g., impute missing values, create new features, etc.)


set.seed(123) # For reproducibility
indexes <- sample(1:nrow(df), size = 0.7*nrow(df))
train_data <- df[indexes, ]
test_data <- df[-indexes, ]

# Adjust 'fetal_health' if your target variable is named differently
model <- randomForest(fetal_health ~ ., data = train_data)


predictions <- predict(model, test_data)
confusionMatrix <- table(test_data$fetal_health, predictions)
print(confusionMatrix)

# Calculate accuracy or other performance metrics as needed
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", accuracy))

importance(model)
varImpPlot(model)



