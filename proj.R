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
library(corrplot)

# set the working directory for the project
setwd("/Users/elamin/Courses/STAT 362/stat362-project")

# Load the dataset
df <- read.csv("fetal_health.csv")

# View the first few rows of the dataset
head(df)

# Get a summary of the dataset
summary(df)

# Get the structure of the dataset
str(df)

# Check for missing values
sum(is.na(df))

# check how is the target variable distributed 
class_distribution <- table(df$fetal_health)
print(class_distribution)

# DATA VISUALIZATION

# Calculate the correlation matrix
cor_matrix <- cor(df, use = "complete.obs") 
# png("corrplot.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", tl.cex = 0.6)
# dev.off()

# uncomment line 40 and 42 if you want to save file and get a cleaarer view

# BUILD CLASSIFICATION MODEL

# Split the data into training and testing sets, if you haven't already
set.seed(1) 
index <- sample(1:nrow(df), 200, replace = FALSE)
train_df <- df[index, ]
test_df <- df[-index, ]


train_df$fetal_health <- as.factor(train_df$fetal_health)
test_df$fetal_health <- as.factor(test_df$fetal_health)

# Fit the random forest model on training set
rf_model <- randomForest(fetal_health ~ ., data = train_df)

# Predict on the test set
test_pred <- predict(rf_model, newdata = test_df)

# Create the confusion matrix
conf_matrix <- confusionMatrix(test_pred, test_df$fetal_health)
print(conf_matrix)


