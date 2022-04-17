library(tidyverse) 
library(data.table)
library(caret)
library(gbm)
library(chron)
library(jsonlite)
library(Hmisc)


data = fread("C:/Users/evamu/OneDrive/Desktop/Machie Learning 2/project/data_clean.csv", header = TRUE,
             colClasses=c(fullVisitorId='character'))


print('data:'); dim(data)


# correct the data types
data_num <- data[,c(4,17:22)]
data_cat <- data[,-c(4,17:22)]
data_cat <- data.frame(apply(data_cat, 2,function(x){as.factor(x)}))
data_num <- data.frame(apply(data_num, 2,function(x){as.numeric(x)}))
data <- cbind(data_cat,data_num)

data$date <- as.Date(as.character(data$date), format= "%Y-%m-%d")
data$dayOfMonth <- as.factor(days(data$date))
data$month <- as.factor(month(data$date))

calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}



tr <- data.frame(fullVisitorId=unique(data$fullVisitorId))
temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(channelGrouping = calculate_mode(channelGrouping))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(value = calculate_mode(value))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(browser = calculate_mode(browser))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(operatingSystem = calculate_mode(operatingSystem))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(isMobile = calculate_mode(isMobile))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(deviceCategory = calculate_mode(deviceCategory))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(continent = calculate_mode(continent))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(subContinent = calculate_mode(subContinent))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(country = calculate_mode(country))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(region = calculate_mode(region))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(metro = calculate_mode(metro))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(city = calculate_mode(city))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(campaign = calculate_mode(campaign))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(source = calculate_mode(source))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(medium = calculate_mode(medium))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(isTrueDirect = calculate_mode(isTrueDirect))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(adContent = calculate_mode(adContent))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(adwordsClickInfo.slot = calculate_mode(adwordsClickInfo.slot))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(adwordsClickInfo.adNetworkType = calculate_mode(adwordsClickInfo.adNetworkType))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(visitNumber = last(visitNumber))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(pageviews = mean(pageviews))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(sessionQualityDim = mean(sessionQualityDim))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(timeOnSite = mean(timeOnSite))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(transactions = sum(transactions))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(transactionRevenue = sum(transactionRevenue))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(totalTransactionRevenue = mean(totalTransactionRevenue))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(dayOfMonth = calculate_mode(dayOfMonth))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

temp<-data %>%
  group_by(fullVisitorId) %>%
  summarise(month = calculate_mode(month))
tr <- merge(tr,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)

head(tr,5)
write.csv(tr, "data_agg.csv",row.names = FALSE)
