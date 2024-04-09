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
library(gbm)
library(e1071)


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

df$fetal_health <- factor(df$fetal_health,
                          levels = c("1", "2", "3"),
                          labels = c("Normal", "Suspect", "Pathological"))



# Split the data into training and testing using the 80, 20 rulw
set.seed(1) 
index <- sample(1:nrow(df), 425, replace = FALSE)
train_df <- df[index, ]
test_df <- df[-index, ]


train_df$fetal_health <- as.factor(train_df$fetal_health)
test_df$fetal_health <- as.factor(test_df$fetal_health)

train_df <- train_df %>%
  mutate_if(is.numeric, ~ (. - min(.)) / (max(.) - min(.)))

test_df <- test_df %>%
  mutate_if(is.numeric, ~ (. - min(.)) / (max(.) - min(.)))


# Define trainControl for cross-validation
control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)

# Train models
models <- list(
  rf = train(fetal_health ~ ., data = train_df, method = "rf", trControl = control),
  svm = train(fetal_health ~ ., data = train_df, method = "svmRadial", trControl = control),
  gbm = train(fetal_health ~ ., data = train_df, method = "gbm", trControl = control, verbose = FALSE),
  knn = train(fetal_health ~ ., data = train_df, method = "knn", trControl = control)
)

# Evaluate models and print accuracies
accuracies <- sapply(models, function(model) {
  predictions <- predict(model, newdata = test_df)
  cm <- confusionMatrix(predictions, test_df$fetal_health)
  cm$overall['Accuracy']
})

# Print model accuracies
sapply(names(accuracies), function(model_name) {
  cat(paste0(model_name, ":", round(accuracies[model_name], 4)), "\n")
})