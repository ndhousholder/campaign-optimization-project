### Feature Engineering

library(lubridate)
library(tidyverse)
library(caret)
require(dplyr)
require(corrplot)

dataFE = read.csv("Group Project/dataset-clean.csv", header = T)

# Add empty column 'Response6' to dataFE
dataFE$Response6 = NA
str(dataFE)

# Demographic Features
dataFE$Age = 2025 - dataFE$Year_Birth
dataFE$Dt_Customer <- as.Date(dataFE$Dt_Customer)
dataFE$Education <- as.factor(dataFE$Education)
dataFE$Education_Level <- as.numeric(factor(dataFE$Education, 
                                          levels = c("Basic", "High school or some post-secondary education", 
                                                     "Bachelor’s degree", "Master’s degree", "PhD"),
                                          ordered = T))
dataFE$Marital_Status <- as.factor(dataFE$Marital_Status)
dataFE$Is_Partnered <- ifelse(dataFE$Marital_Status %in% c("Married", "Together"), 1, 0)
dataFE$Total_Children <- dataFE$Kidhome + dataFE$Teenhome
dataFE$Recency_Category <- cut(dataFE$Recency, breaks = c(-Inf, 30, 60, Inf), 
                             labels = c("Recent", "Moderate", "Distant"))


# Aggregated Spending Features
dataFE$Total_Spending <- dataFE$MntFishProducts + dataFE$MntMeatProducts + dataFE$MntFruits + dataFE$MntVegProds + dataFE$MntSweetProducts + dataFE$MntWines
dataFE$Prop_Wines <- dataFE$MntWines / dataFE$Total_Spending
dataFE$Prop_Meat <- dataFE$MntMeatProducts / dataFE$Total_Spending
dataFE$Prop_Wines[is.na(dataFE$Prop_Wines)] <- 0
dataFE$Prop_Meat[is.na(dataFE$Prop_Meat)] <- 0
dataFE$Prop_Healthy <- ifelse(dataFE$Total_Spending > 0, 
                             (dataFE$MntFishProducts + dataFE$MntFruits + dataFE$MntVegProds) / dataFE$Total_Spending, 0)
dataFE$MaxCategorySpend <- pmax(dataFE$MntFishProducts, dataFE$MntMeatProducts, dataFE$MntFruits, 
                               dataFE$MntSweetProducts, dataFE$MntWines, dataFE$MntVegProds)

# Purchase Channel Features
dataFE$Total_Purchases <- dataFE$NumDealsPurchases + dataFE$NumStorePurchases + dataFE$NumWebPurchases
dataFE$Prop_WebPurchases <- dataFE$NumWebPurchases / dataFE$Total_Purchases
dataFE$Prop_StorePurchases <- dataFE$NumStorePurchases / dataFE$Total_Purchases
dataFE$Prop_WebPurchases[is.na(dataFE$Prop_WebPurchases)] <- 0
dataFE$Prop_StorePurchases[is.na(dataFE$Prop_StorePurchases)] <- 0

# Campaign Response Features
dataFE$Total_Campaigns_Accepted <- rowSums(dataFE[, c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", 
                                                        "AcceptedCmp4", "AcceptedCmp5")], na.rm = T)


# Interaction Features
dataFE$Spending_per_Child <- ifelse(dataFE$Total_Children > 0, 
                                    dataFE$Total_Spending / dataFE$Total_Children, 
                                    dataFE$Total_Spending)
dataFE$Income_per_Purchase <- dataFE$Income / dataFE$Total_Purchases
dataFE$Income_per_Purchase[is.na(dataFE$Income_per_Purchase) | is.infinite(dataFE$Income_per_Purchase)] <- 0

# Reorder dataFE
str(dataFE)
dataFE <- dataFE %>%
  select(ID, AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5, Total_Campaigns_Accepted, 
         Dt_Customer, Year_Birth, Age, Education, Education_Level, Marital_Status, Is_Partnered, Kidhome, Teenhome, Total_Children, 
         Income,  
         MntMeatProducts, MntFishProducts, MntFruits, MntVegProds, MntSweetProducts, MntWines, 
         Prop_Meat, Prop_Wines, Prop_Healthy, 
         Total_Spending, MaxCategorySpend, Spending_per_Child, 
         NumDealsPurchases, NumStorePurchases, NumWebPurchases, Total_Purchases, Income_per_Purchase, Prop_StorePurchases, Prop_WebPurchases, NumWebVisitsMonth, 
         Complain, Recency, Recency_Category)

# Convert data types
dataFE$AcceptedCmp1 <- as.factor(dataFE$AcceptedCmp1)
dataFE$AcceptedCmp2 <- as.factor(dataFE$AcceptedCmp2)
dataFE$AcceptedCmp3 <- as.factor(dataFE$AcceptedCmp3)
dataFE$AcceptedCmp4 <- as.factor(dataFE$AcceptedCmp4)
dataFE$AcceptedCmp5 <- as.factor(dataFE$AcceptedCmp5)
dataFE$Complain <- as.factor(dataFE$Complain)
dataFE$Dt_Customer <- as.Date(dataFE$Dt_Customer)
dataFE$Education_Level <- as.factor(dataFE$Education_Level)
dataFE$Is_Partnered <- as.factor(dataFE$Is_Partnered)

str(dataFE)

# Save dataFE as CSV
write.csv(dataFE, "Group Project/dataset-FE.csv", row.names = F)

summary(dataFE)

# Correlation Analysis using new features
features = c("Age", "Education_Level", "Is_Partnered", "Total_Children", "Recency_Category",
             "Total_Spending", "Prop_Meat", "Prop_Wines", "Prop_Healthy", "MaxCategorySpend", "Spending_per_Child", 
             "Total_Purchases", "Prop_StorePurchases", "Prop_WebPurchases", "Income_per_Purchase",
             "Total_Campaigns_Accepted")
# Correlation Matrix with new features
cormatrixFE = cor(dataFE %>% select(all_of(features)) %>% mutate(Education_Level = as.numeric(Education_Level),
                                                            Is_Partnered = as.numeric(Is_Partnered),
                                                            Recency_Category = as.numeric(Recency_Category)))
corrplot(cormatrixFE, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
cormatrixFE

# Remove any observations that has accepted a previous campaign (AcceptedCmp == 1)
data0 <- dataFE %>%
  filter(AcceptedCmp1 == 0 & AcceptedCmp2 == 0 & AcceptedCmp3 == 0 & 
           AcceptedCmp4 == 0 & AcceptedCmp5 == 0)
