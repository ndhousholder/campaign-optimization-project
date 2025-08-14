library(dplyr)
library(randomForest)
library(caret)
library(pROC) # For ROC-AUC

# Load dataset
data = read.csv("Group Project/dataset-FE.csv", header = T)

# Add Response6 Column to data
data$Response6 = NA
data$Response6 <- ifelse(data$Total_Campaigns_Accepted > 0, 1, 0)
data$Response6 <- as.factor(data$Response6)

table(data$Response6)

# Ensure correct data types
data$AcceptedCmp1 <- as.factor(data$AcceptedCmp1)
data$AcceptedCmp2 <- as.factor(data$AcceptedCmp2)
data$AcceptedCmp3 <- as.factor(data$AcceptedCmp3)
data$AcceptedCmp4 <- as.factor(data$AcceptedCmp4)
data$AcceptedCmp5 <- as.factor(data$AcceptedCmp5)
data$Dt_Customer <- as.Date(data$Dt_Customer)
data$Education <- as.factor(data$Education)
data$Education_Level <- as.factor(data$Education_Level)
data$Marital_Status <- as.factor(data$Marital_Status)
data$Is_Partnered <- as.factor(data$Is_Partnered)
data$Recency_Category <- as.factor(data$Recency_Category)
data$Complain <- as.factor(data$Complain)

current_date <- as.Date("2025-08-01")
data <- data %>%
  mutate(Days_Since_Enrollment = as.numeric(current_date - as.Date(Dt_Customer)))

str(data)

# Select features from data for modeling
features = c("Age", "Education_Level", "Is_Partnered", "Kidhome", "Teenhome", "Total_Children",
             "Income", "MntMeatProducts", "MntFishProducts", "MntFruits", "MntVegProds",
             "MntSweetProducts", "MntWines", "Prop_Meat", "Prop_Wines", "Prop_Healthy",
             "Total_Spending", "MaxCategorySpend", "Spending_per_Child", "NumDealsPurchases",
             "NumStorePurchases", "NumWebPurchases", "Total_Purchases", "Income_per_Purchase",
             "Prop_StorePurchases", "Prop_WebPurchases", "NumWebVisitsMonth", "Complain",
             "Recency", "Days_Since_Enrollment")

# Split Data
set.seed(42) # For reproducibility
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train random forest with class weights to minimize false negatives
rf_model <- randomForest(Response6 ~ ., data = train_data[, c(features, "Response6")], 
                         ntree = 100, mtry = 5, importance = TRUE, 
                         classwt = c(1, 10))
# Print model summary
print(rf_model)

# Feature Importance
importance(rf_model)
varImpPlot(rf_model)

# Predictions on test set
predictions <- predict(rf_model, test_data, type = "class")

# Confusion matrix
conf_matrix <- table(test_data$Response6, predictions)
caret::confusionMatrix(conf_matrix)

# Accuracy
accuracy <- sum(predictions == test_data$Response6) / nrow(test_data)
print(paste("Accuracy:", accuracy))

# ROC-AUC
prob_predictions <- predict(rf_model, test_data, type = "prob")[,2]  # Probabilities for class 1
roc_curve <- roc(test_data$Response6, prob_predictions)
print(auc(roc_curve))

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

