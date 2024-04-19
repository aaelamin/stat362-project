# Project Title: Your Project Name Here
# Description: Fetal Health Analysis: Predictive Modeling Using CTG Data
# Authors: Adam Elamin, Martina Mileva, Roy Yeoun, Basel Abdelhadi, Jasmine Taylor
# Date: Apr 9 2024

# Import necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(caret)
library(corrplot)
library(e1071)
library(ROSE)
library(rpart)
library(nnet)
library(class)





# set the working directory for the project
#setwd("/Users/elamin/Courses/STAT 362/stat362-project")

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



# Update with actual column names present in your dataset
selected_features <- c("baseline.value", "uterine_contractions", "fetal_movement", "fetal_health")


# Extract variable importance
var_importance <- importance(rf_model)

# Convert to a data frame for plotting
importance_df <- data.frame(Variable = row.names(var_importance), Importance = var_importance[, "MeanDecreaseGini"])

# Sort by importance and select the top 5
top5_importance_df <- importance_df %>%
  dplyr::arrange(desc(Importance)) %>%
  head(5)

# Plot using ggplot2
ggplot(top5_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "turquoise3") +
  coord_flip() +  # For horizontal bars
  theme_minimal() +
  xlab("") +  # Removing the x-axis label
  ylab("Importance (Mean Decrease in Gini)") +
  ggtitle("Top 5 Important Variables - Random Forest")


library(e1071)

# Assuming your data is prepared and stored in train_df_selected and test_df_selected
svm_model <- svm(fetal_health ~ ., data=train_df_selected, kernel="linear")


# Extract model coefficients
model_coefs <- svm_model$coefs[[1]] %*% t(svm_model$SV)

# The names of the support vectors
feature_names <- colnames(svm_model$SV)

# Combine into a data frame
feature_importance <- data.frame(Feature = feature_names, Importance = abs(model_coefs))


# Sort by importance and select top 5
top5_features <- feature_importance %>%
  dplyr::arrange(desc(Importance)) %>%
  head(5)

# Plot using ggplot2
ggplot(top5_features, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_col(fill="dodgerblue") +
  coord_flip() +
  theme_minimal() +
  xlab("") +
  ylab("Feature Weight") +
  ggtitle("Top 5 Important Features - Linear SVM Model")


