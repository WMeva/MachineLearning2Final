library(tidyverse) 
library(data.table)
library(caret)
library(gbm)
library(chron)
library(jsonlite)
library(Hmisc)



data = fread("C:/Users/evamu/OneDrive/Desktop/Machie Learning 2/project/data.csv", header = TRUE,
             colClasses=c(fullVisitorId='character'), nrow=20000)

# json formatting  https://www.kaggle.com/code/flubber/preprocessing-of-the-v2-datasets
# Used help from one of the submissions because json formatting was new to us
# The goal of this was to make new columns (the main one being transactionRevenue), 
# in order to make prediction about each customers revenue


data$customDimensions<-gsub("[]","[{}]",data$customDimensions,fixed=TRUE)
data$customDimensions<-gsub("'","\"",data$customDimensions,fixed=TRUE)
data$customDimensions<-gsub("[","",data$customDimensions,fixed=TRUE)
data$customDimensions<-gsub("]","",data$customDimensions,fixed=TRUE)
data$customDimensions <- factor(data$customDimensions)
data.customDimensions <- fromJSON(paste('[',paste(data$customDimensions,collapse = ','),']'),flatten = TRUE)
data.customDimensions <- data.frame(apply(data.customDimensions, 2,function(x){as.factor(x)}))

data$device<-gsub("\"\"","\"",data$device,fixed=TRUE)
data.device <- fromJSON(paste('[',paste(data$device,collapse = ','),']'),flatten = TRUE)
data.device <- data.frame(apply(data.device, 2,function(x){as.factor(x)}))

data$geoNetwork<-gsub("\"\"","\"",data$geoNetwork,fixed=TRUE)
data.geoNetwork <- fromJSON(paste('[',paste(data$geoNetwork,collapse = ','),']'),flatten = TRUE)
data.geoNetwork <- data.frame(apply(data.geoNetwork, 2,function(x){as.factor(x)}))

data$totals<-gsub("\"\"","\"",data$totals,fixed=TRUE)
data.totals <- fromJSON(paste('[',paste(data$totals,collapse = ','),']'),flatten = TRUE)
data.totals <- data.frame(apply(data.totals, 2,function(x){as.numeric(x)}))

data$trafficSource<-gsub("\"\"","\"",data$trafficSource,fixed=TRUE)
data.trafficSource <- fromJSON(paste('[',paste(data$trafficSource,collapse = ','),']'),flatten = TRUE)
data.trafficSource <- data.frame(apply(data.trafficSource, 2,function(x){as.factor(x)}))

data_ <- cbind(data,data.customDimensions,data.device,data.geoNetwork,data.totals,data.trafficSource)
data_ <- data_[,-c(2,4,6,8,9)]
data_$date <- as.Date(as.character(data_$date), format= "%Y%m%d")
class(data_$visitStartTime) <- c('POSIXt','POSIXct')
data_$channelGrouping <- as.factor(data_$channelGrouping)
data_$fullVisitorId <- as.factor(data_$fullVisitorId)
data_$socialEngagementType <- as.factor(data_$socialEngagementType)
data_$visitId <- as.factor(data_$visitId)

data_<-select(data_,-hits,-trafficSource,-socialEngagementType, -visitStartTime, 
                  -browserVersion, -browserSize, -operatingSystemVersion,
                  -mobileDeviceBranding, -mobileDeviceModel, -mobileInputSelector,
                  -mobileDeviceInfo, -mobileDeviceMarketingName, -flashVersion,
                  -language, -screenColors, -screenResolution, -cityId,
                  -latitude, -longitude, -networkLocation, 
                  -adwordsClickInfo.criteriaParameters, -index, -visits, -bounces,
                  -newVisits, -visitId, -adwordsClickInfo.gclId)


data_$transactionRevenue <- impute(data_$transactionRevenue,0)
data_$totalTransactionRevenue <- impute(data_$totalTransactionRevenue,0)
data_$transactions <- impute(data_$transactions,0)
data_$adwordsClickInfo.page <- impute(data_$adwordsClickInfo.page)
data_$adwordsClickInfo.slot <- impute(data_$adwordsClickInfo.slot)
data_$adwordsClickInfo.adNetworkType <- impute(data_$adwordsClickInfo.adNetworkType)
data_$adwordsClickInfo.isVideoAd <- impute(data_$adwordsClickInfo.isVideoAd)
data_$isTrueDirect[is.na(data_$isTrueDirect) & data_$source == '(direct)'] <- TRUE
data_$isTrueDirect <- impute(data_$isTrueDirect,FALSE)
data_$isTrueDirect <- factor(data_$isTrueDirect)
data_$timeOnSite <- impute(data_$timeOnSite,0)
data_$value <- impute(data_$value)
data_$keyword <- impute(data_$keyword)
data_$pageviews <- impute(data_$pageviews,0)

head(data_,3)
write.csv(data_, "data_clean.csv",row.names = FALSE)

