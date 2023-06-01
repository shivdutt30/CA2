#Load the necessary libraries and read the dataset
# Install and load required libraries
#install.packages("caret")
#install.packages("dplyr")
#install.packages("ggplot2")

library(caret)
library(dplyr)
library(ggplot2)

# Read the dataset
data <- read.csv("diabetes.csv")


# View the structure of the dataset
str(data)

# Check summary statistics
summary(data)


# Plot scatter plots for numeric variables
scatter_data <- data[, c("BMI", "Age", "Income")]
ggplot(data = scatter_data, aes(x = BMI, y = Age, color = Income)) +
  geom_point() +
  theme_minimal()


# Compute correlation matrix
cor_matrix <- cor(data[, c("BMI", "Age", "Income")])

# Plot correlation matrix as a heatmap
ggplot(data = reshape2::melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Set the seed for reproducibility
set.seed(123)

# Split the data into 70% training and 30% testing
train_indices <- createDataPartition(data$Diabetes_binary, p = 0.7, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


# Train a logistic regression model
model <- glm(Diabetes_binary ~ ., data = train_data, family = binomial)

# Print the model summary
summary(model)


# Predict on the testing set
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to class labels
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix
confusion_matrix <- table(Actual = test_data$Diabetes_binary, Predicted = predicted_classes)

# Calculate accuracy, sensitivity, and specificity
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])

# Print evaluation metrics
print(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

