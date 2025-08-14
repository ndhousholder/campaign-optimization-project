library(readxl)
require(tidyverse)
require(corrplot)
require(lubridate)
require(dplyr)

data = read_excel("Group Project/dataset.xlsx")
str(data)

# Convert Data Types
data$AcceptedCmp1 = as.factor(data$AcceptedCmp1)
data$AcceptedCmp2 = as.factor(data$AcceptedCmp2)
data$AcceptedCmp3 = as.factor(data$AcceptedCmp3)
data$AcceptedCmp4 = as.factor(data$AcceptedCmp4)
data$AcceptedCmp5 = as.factor(data$AcceptedCmp5)
data$Complain = as.factor(data$Complain)
data$Dt_Customer = as.Date(data$Dt_Customer)
data$Education = as.factor(data$Education)
data$Marital_Status = as.factor(data$Marital_Status)
str(data)

# Rename Year_Birth 2 to Year_Birth
colnames(data)[which(names(data) == "Year_Birth 2")] = "Year_Birth"

# Check for Missing Values
colSums(is.na(data))
# Impute with Median
data$Income = ifelse(is.na(data$Income), median(data$Income, na.rm = T), data$Income)

# Remove Outlier Responses
data = data %>% filter(Income < 500000)
data = data %>% filter(Year_Birth > 1925)
data = data %>% filter(Year_Birth < 2007)

data = data %>%
  select(ID, AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5,
         Dt_Customer, Year_Birth, Education,  Marital_Status, Kidhome, Teenhome, 
         Income, MntMeatProducts, MntFishProducts, MntFruits, MntVegProds,  MntSweetProducts, MntWines,
         NumDealsPurchases, NumStorePurchases, NumWebPurchases, NumWebVisitsMonth, Complain, Recency)



# save data as new file
write.csv(data, "Group Project/dataset-clean.csv", row.names = F)

# Descriptive Stats
summary(data)

# Evaluate Campaign Response Rates
cmp_responses = data %>%
  summarise(Cmp1 = mean(AcceptedCmp1 == 1, na.rm = T),
            Cmp2 = mean(AcceptedCmp2 == 1, na.rm = T),
            Cmp3 = mean(AcceptedCmp3 == 1, na.rm = T),
            Cmp4 = mean(AcceptedCmp4 == 1, na.rm = T),
            Cmp5 = mean(AcceptedCmp5 == 1, na.rm = T))
print(cmp_responses)

# Evaluate Spending Habits
spending_sum = data %>%
  summarise(Fish = mean(MntFishProducts, na.rm = T),
            Meat = mean(MntMeatProducts, na.rm = T),
            Fruits = mean(MntFruits, na.rm = T),
            Sweets = mean(MntSweetProducts, na.rm = T),
            Wines = mean(MntWines, na.rm = T),
            Veggies = mean(MntVegProds, na.rm = T))
print(spending_sum)

# Evaluate Purchase Channels
purchase_channels = data %>%
  summarise(Web = mean(NumWebPurchases, na.rm = T),
            Store = mean(NumStorePurchases, na.rm = T),
            Deals = mean(NumDealsPurchases, na.rm = T))
print(purchase_channels)

###

# Customer Demographics Plots
library(ggplot2)
# Age Distribution
data$Age = 2025 - data$Year_Birth
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Customer Age Distribution", x = "Age", y = "Count") +
  theme_minimal()
# Income Distribution
ggplot(data, aes(x = Income)) +
  geom_histogram(binwidth = 10000, fill = "lightgreen", color = "black") +
  labs(title = "Customer Income Distribution", x = "Income", y = "Count") +
  theme_minimal()
# Education Level Distribution
ggplot(data, aes(x = Education)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Customer Education Level Distribution", x = "Education Level", y = "Count") +
  theme_minimal()
# Marital Status Distribution
ggplot(data, aes(x = Marital_Status)) +
  geom_bar(fill = "lightgoldenrod", color = "black") +
  labs(title = "Customer Marital Status Distribution", x = "Marital Status", y = "Count") +
  theme_minimal()

###

# Correlation Analysis
cor_matrix = cor(data %>% select_if(is.numeric), use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
cor_matrix

# Create a dataset which consists only of customers who have accepted a previous campaign (AcceptedCmp ==1)
data1 = data %>%
  filter(AcceptedCmp1 == 1 | AcceptedCmp2 == 1 | AcceptedCmp3 == 1 | 
         AcceptedCmp4 == 1 | AcceptedCmp5 == 1)

# Correlation Analysis for data1
cor_matrix1 = cor(data1 %>% select_if(is.numeric), use = "pairwise.complete.obs")
corrplot(cor_matrix1, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
cor_matrix1
