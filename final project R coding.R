# Load necessary libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate sample data
num_customers <- 100

# Create sample data frame
data <- data.frame(
  CustomerID = 1:num_customers,
  Age = sample(18:60, num_customers, replace = TRUE),
  Gender = sample(c("Male", "Female"), num_customers, replace = TRUE),
  TotalPurchases = sample(5:30, num_customers, replace = TRUE),
  AvgPurchaseAmount = round(runif(num_customers, min = 20, max = 100), 2),
  FrequencyOfVisits = sample(1:10, num_customers, replace = TRUE),
  Churned = sample(c("Yes", "No"), num_customers, replace = TRUE)
)

# Print first few rows of the data
head(data)
# Load necessary libraries
library(dplyr)

# Print the structure of the dataset
str(data)

# Summary statistics
summary(data)

# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Convert 'Churned' column to binary numeric format
data$Churned <- ifelse(data$Churned == "Yes", 1, 0)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Build logistic regression model
model <- glm(Churned ~ ., data = train_data, family = "binomial")

# Print summary of the model
summary(model)

# Predictions on test data
predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to class labels
predicted_labels <- ifelse(predictions > 0.5, "Yes", "No")

# Confusion matrix
conf_matrix <- table(test_data$Churned, predicted_labels)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# Calculate precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste("Precision:", precision))

# Calculate recall
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste("Recall:", recall))

# Calculate ROC-AUC
library(pROC)
roc_curve <- roc(as.numeric(test_data$Churned) - 1, as.numeric(predictions))
auc <- auc(roc_curve)
print(paste("ROC-AUC:", auc))

library(ggplot2)

# Age distribution by churn status
ggplot(data, aes(x = Age, fill = Churned)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  labs(title = "Age Distribution by Churn Status",
       x = "Age", y = "Count") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

# Total purchases distribution by churn status
ggplot(data, aes(x = TotalPurchases, fill = Churned)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  labs(title = "Total Purchases Distribution by Churn Status",
       x = "Total Purchases", y = "Count") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

# Frequency of visits distribution by churn status
ggplot(data, aes(x = FrequencyOfVisits, fill = Churned)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "Frequency of Visits Distribution by Churn Status",
       x = "Frequency of Visits", y = "Count") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()



