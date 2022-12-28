library(dplyr)
library(ggplot2)

#### 1. Load Data ####
df <- read.csv(
  file="E:/R/Customer-Value.csv", 
  header=TRUE,  sep = ","
)
View(df)

glimpse(df)

# Encode engaged customers as 0s and 1s
df <- df %>%
  mutate(Response=ifelse(Response=="No", 0, 1))
df$Engaged <- as.integer(df$Response)


mean(df$Engaged)

#### 2. Analytics on Engaged Customers ####

## - Overall Engagement Rates ##
engagementRate <- df %>% group_by(Response) %>%
  summarise(Count=n())  %>%
  mutate(EngagementRate=Count/nrow(df)*100.0)

ggplot(engagementRate, aes(x=Response, y=EngagementRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Engagement Rate') +
  xlab("Engaged") +
  ylab("Percentage (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Engagement Rates by Offer Type ##
engagementRateByOfferType <- df %>% 
  group_by(Renew.Offer.Type) %>%
  summarise(Count=n(), NumEngaged=sum(Engaged))  %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateByOfferType, aes(x=Renew.Offer.Type, y=EngagementRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Engagement Rates by Offer Type') +
  xlab("Offer Type") +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Offer Type & Vehicle Class ##
engagementRateByOfferTypeVehicleClass <- df %>% 
  group_by(Renew.Offer.Type, Vehicle.Class) %>%
  summarise(NumEngaged=sum(Engaged))  %>%
  left_join(engagementRateByOfferType[,c("Renew.Offer.Type", "Count")], by="Renew.Offer.Type") %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateByOfferTypeVehicleClass, aes(x=Renew.Offer.Type, y=EngagementRate, fill=Vehicle.Class)) +
  geom_bar(width=0.5, stat="identity", position = "dodge") +
  ggtitle('Engagement Rates by Offer Type & Vehicle Class') +
  xlab("Offer Type") +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Engagement Rates by Sales Channel ##
SalesChannel <- df %>%
  group_by(Engaged, Sales.Channel) %>%
  summarize(Count=n()) %>%
  arrange(Sales.Channel)

options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(SalesChannel, aes(x="", y=Count, fill=Sales.Channel)) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(x=1.25, label=Count), position = position_fill(vjust=0.5)) +
  coord_polar("y") + facet_wrap(~Engaged) +
  ggtitle("Sales Channel (0: Not Engaged, 1: Engaged)") + 
  theme(legend.position = "bottom", legend.text=element_text(size=20), plot.title = element_text(size=22))


## - Sales Channel & Vehicle Size ##
engagementRateBySalesChannelVehicleSize <- df %>% 
  group_by(Sales.Channel, Vehicle.Size) %>%
  summarise(NumEngaged=sum(Engaged))  %>%
  left_join(engagementRateBySalesChannel[,c("Sales.Channel", "Count")], by="Sales.Channel") %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateBySalesChannelVehicleSize, aes(x=Sales.Channel, y=EngagementRate, fill=Vehicle.Size)) +
  geom_bar(width=0.5, stat="identity", position = "dodge") +
  ggtitle('Engagement Rates by Sales Channel & Vehicle Size') +
  xlab("Sales Channel") +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Engagement Rates by Months Since Policy Inception ##
engagementRateByPolicyAge <- df %>% 
  group_by(Months.Since.Policy.Inception) %>%
  summarise(Count=n(), NumEngaged=sum(Engaged))  %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateByPolicyAge, aes(x=Months.Since.Policy.Inception, y=EngagementRate)) +
  geom_line() +
  ylab("Engagement Rate (%)") +
  xlab("Months Since Policy Inception") +
  ggtitle("Engagement Rates by Months Since Policy Inception") +
  theme(plot.title=element_text(hjust=0.5))


#### 3. Customer Segmentation by CLV & Months Since Inception ####
summary(df$Customer.Lifetime.Value)
summary(df$Months.Since.Policy.Inception)

engagementRateBySegment <- df %>% 
  group_by(CLV.Segment, Policy.Age.Segment) %>%
  summarise(Count=n(), NumEngaged=sum(Engaged))  %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateBySegment, aes(x=CLV.Segment, y=EngagementRate, fill=Policy.Age.Segment)) +
  geom_bar(width=0.5, stat="identity", position = "dodge") +
  ggtitle('Engagement Rates by Customer Segments') +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

conversionsEmp <- df %>%
  group_by(EmploymentStatus) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)

ggplot(conversionsEmp, aes(x=EmploymentStatus, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Employment Status") +
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 20), plot.title = element_text(size=22))

conversionsEdu <- df %>%
  group_by(Education) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)

ggplot(conversionsEdu, aes(x=Education, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Education") + 
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 20), plot.title = element_text(size=22))

categoricalVars = c(
  'Sales.Channel', 'Vehicle.Size', 'Vehicle.Class', 'Policy', 'Policy.Type',
  'EmploymentStatus', 'Marital.Status', 'Education', 'Coverage', 'Gender'
)

encodedDF <- model.matrix(~.-1, df[categoricalVars])

## 2.3. Continuous Features
continuousFeatures <- c(
  'Customer.Lifetime.Value', 'Income', 'Monthly.Premium.Auto',
  'Months.Since.Last.Claim', 'Months.Since.Policy.Inception',
  'Number.of.Open.Complaints', 'Number.of.Policies', 'Total.Claim.Amount'
)

encodedDF <- cbind(encodedDF, df[continuousFeatures])


#### 3. Training & Testing ####

# install.packages('caTools')
library(caTools)

sample <- sample.split(df$Customer, SplitRatio = .7)

trainX <- as.matrix(subset(encodedDF, sample == TRUE))
trainY <- as.double(as.matrix(subset(df$Engaged, sample == TRUE)))

testX <- as.matrix(subset(encodedDF, sample == FALSE))
testY <- as.double(as.matrix(subset(df$Engaged, sample == FALSE)))

## 3.1. Building Random Forest Model

# - Training
# install.packages('randomForest')
library(randomForest)

rfModel <- randomForest(x=trainX, y=factor(trainY), ntree=200, maxnodes=24)

# - Individual Tree Predictions
b = getTree(rfModel, 1)
View(b)
predict(rfModel, trainX, predict.all=TRUE)$individual

# - Feature Importances
a = importance(rfModel)
View(a)

## 3.2. Evaluating Models

inSamplePreds <- as.double(predict(rfModel, trainX)) - 1
outSamplePreds <- as.double(predict(rfModel, testX)) - 1

# - Accuracy, Precision, and Recall
inSampleAccuracy <- mean(trainY == inSamplePreds)
outSampleAccuracy <- mean(testY == outSamplePreds)
print(sprintf('In-Sample Accuracy: %0.4f', inSampleAccuracy))
print(sprintf('Out-Sample Accuracy: %0.4f', outSampleAccuracy))

inSamplePrecision <- sum(inSamplePreds & trainY) / sum(inSamplePreds)
outSamplePrecision <- sum(outSamplePreds & testY) / sum(outSamplePreds)
print(sprintf('In-Sample Precision: %0.4f', inSamplePrecision))
print(sprintf('Out-Sample Precision: %0.4f', outSamplePrecision))

inSampleRecall <- sum(inSamplePreds & trainY) / sum(trainY)
outSampleRecall <- sum(outSamplePreds & testY) / sum(testY)
print(sprintf('In-Sample Recall: %0.4f', inSampleRecall))
print(sprintf('Out-Sample Recall: %0.4f', outSampleRecall))

# - ROC & AUC
# install.packages('ROCR')
library(ROCR)

inSamplePredProbs <- as.double(predict(rfModel, trainX, type='prob')[,2])
outSamplePredProbs <- as.double(predict(rfModel, testX, type='prob')[,2])

pred <- prediction(outSamplePredProbs, testY)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc <- performance(pred, measure='auc')@y.values[[1]]

plot(
  perf, 
  main=sprintf('Random Forest Model ROC Curve (AUC: %0.2f)', auc), 
  col='darkorange', 
  lwd=2
) + grid()
abline(a = 0, b = 1, col='darkgray', lty=3, lwd=2)

