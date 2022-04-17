library(data.table)
library(tidyverse) 
library(data.table)
library(caret)
library(gbm)
library(chron)
library(jsonlite)
library(Hmisc)

data = fread("C:/Users/evamu/OneDrive/Desktop/Machie Learning 2/project/data_agg.csv", header = TRUE,
             colClasses=c(fullVisitorId='character'), stringsAsFactors = TRUE)


data_cat <- data[,c(1:20,28,29)]
data_num <- data[,-c(1:20,28,29)]
data_cat <- data.frame(apply(data_cat, 2,function(x){as.factor(x)}))
data_num <- data.frame(apply(data_num, 2,function(x){as.numeric(x)}))
data <- cbind(data_cat,data_num)
data$channelGrouping <- as.factor(data$channelGrouping)
data$value <-as.factor(data$value)
data$browser <-as.factor(data$browser)
data$operatingSystem <- as.factor(data$operatingSystem)
data$isMobile <-as.factor(data$isMobile)
data$deviceCategory <-as.factor(data$deviceCategory)
data$continent <- as.factor(data$continent)
data$subContinent <-as.factor(data$subContinent)
data$country <-as.factor(data$country)
data$region <- as.factor(data$region)
data$metro <-as.factor(data$metro)
data$city <-as.factor(data$city)
data$campaign <- as.factor(data$campaign)
data$source <-as.factor(data$source)
data$medium <-as.factor(data$medium)
data$isTrueDirect <- as.factor(data$isTrueDirect)
data$adContent <-as.factor(data$adContent)
data$adwordsClickInfo.slot <- as.factor(data$adwordsClickInfo.slot)
data$adwordsClickInfo.adNetworkType <-as.factor(data$adwordsClickInfo.adNetworkType)
data$dayOfMonth <-as.factor(data$dayOfMonth)
data$month <-as.factor(data$month)


set.seed(272)
train_test_split<-createDataPartition(data$transactionRevenue, p=0.7, list = FALSE)
train_data<-data[train_test_split,]
test_data<-data[-train_test_split,]


library(gbm)
set.seed(272)
# train GBM model
gbm.fit <- gbm(
  formula = transactionRevenue ~ .,
  distribution = "gaussian",
  data = train_data[,-c(1)],
  n.trees = 3500,
  interaction.depth = 5,
  shrinkage = 0.001,
  cv.folds = 3,
  n.cores = NULL, 
  verbose = FALSE
)
print(gbm.fit)


# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))

best.iter <- gbm.perf(gbm.fit, method = "cv")
print(best.iter)

# predict values for test split
pred <- predict(gbm.fit, n.trees = best.iter, test_data)
# results
caret::RMSE(pred, test_data$transactionRevenue)


test_data$target <- ifelse(pred<0,0,pred)
tsf_data <- data.frame(fullVisitorId=unique(test_data$fullVisitorId))


temp<-test_data %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = log(sum(target)+1))
tsf_data <- merge(tsf_data,temp,by="fullVisitorId",sort = TRUE,all.x=TRUE)
head(tsf_data)
