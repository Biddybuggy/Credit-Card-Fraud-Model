# Setting Up
library(caret)
library(ggplot2)

df1 <- read.csv(file = 'path', nrows = 10000)
View(df1)
df1$fraud <- as.factor(df1$fraud)
df1$repeat_retailer <- as.factor(df1$repeat_retailer)
df1$used_chip <- as.factor(df1$used_chip)
df1$used_pin_number <- as.factor(df1$used_pin_number)
df1$online_order <- as.factor(df1$online_order)
preProcess1 <- c("center", "scale")

# Training and Testing Sets
i1 <- createDataPartition(y = df1$fraud, times = 1, p = 0.8, list = FALSE)
training_set1 <- df1[i1,]
testing_set1 <- df1[-i1,]

# Model 
model1 <- train(fraud ~ ., method = 'knn', data = training_set1,
               metric='Accuracy', preProcess=preProcess1)
testing_set1$pred <- predict(model1, testing_set1)
testing_set1$factor_pred <- as.factor(testing_set1$pred)
testing_set1$factor_truth <- as.factor(testing_set1$fraud)
print(testing_set1$factor_pred)
print(testing_set1$factor_truth)

# Data Visualization for Each Feature

# Distance From Home
ggplot(data=df1,aes(x=distance_from_home,fill=fraud))+geom_histogram(binwidth = 50) +
  ggtitle("Relationship Between Distance From Home and Fraud Count") 

# Distance From Last Transaction
ggplot(data=df1,aes(x=distance_from_last_transaction,fill=fraud))+geom_histogram(binwidth = 50) +
  ggtitle("Relationship Between Distance From Last Transaction and Fraud Count") 

# Ratio to Median Purchase Price
ggplot(data=df1,aes(x=ratio_to_median_purchase_price,fill=fraud))+geom_histogram(binwidth = 5) +
  ggtitle("Relationship Between Ratio to Median Purchase Price and Fraud Count") 

# Repeat Retailer
ggplot(data=df1,aes(x=repeat_retailer,fill=fraud))+geom_histogram(stat="count") +
  ggtitle("Relationship Between Repeat Retailer and Fraud Count") 

ggplot(df1[df1$repeat_retailer,], aes(x=repeat_retailer,y=fraud)) +
  geom_bar(stat="count")

# Used Chip
ggplot(data=df1,aes(x=used_chip,fill=fraud))+geom_histogram(stat="count") +
  ggtitle("Relationship Between Use of Chip and Fraud Count") 

# Used PIN Number
ggplot(data=df1,aes(x=used_pin_number,fill=fraud))+geom_histogram(stat="count") +
  ggtitle("Relationship Between Use of PIN Number and Fraud Count") 

# Online Order
ggplot(data=df1,aes(x=online_order,fill=fraud))+geom_histogram(stat="count") +
  ggtitle("Relationship Between Type of Order (Online/Offline) and Fraud Count") 

# Confusion Matrix
cm1 <- confusionMatrix(testing_set1$pred, testing_set1$fraud)
print(cm1)
