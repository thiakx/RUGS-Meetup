#I will setwd("/MY_Working_Directory/data") where the kaggle train/test data are placed
source("../baseFunctions_model.R")

library(randomForest)
library(foreach)
library(doMC)
library(parallel)

coreNumber<-max(detectCores(),1)
registerDoMC(coreNumber)

set.seed(2291033)
result<-rfModel(trainDataMod,testDataMod,1000,coreNumber)
save(result,file="resultRF.Rdata")

#if count <sampleSize use the global median for !NA for each city 
#we only use city here on purpose, dont further split by tag/source as the count of each category will be too small and skew the median
trainData2<-data.table(trainData2)
tagSummary<-trainData2[,list(num_views=median(num_views),num_votes=median(num_votes),num_comments=median(num_comments),
                              count=length(num_views)),by="city"][order(-count)]
testData2<-testData2[!(testData2$id %in% result$id),]
result2<-merge(testData2,tagSummary,by=c("city"))
result2<-result2[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]

resultFinal<-rbind(result,result2)

write.csv(resultFinal[1:4], file = "randomForest.csv", row.names = FALSE)
