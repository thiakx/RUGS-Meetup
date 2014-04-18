#30min on mac
source("../baseFunctions_model.R")

#load cleaned data
load("trainDataMod.Rdata")
load("testDataMod.Rdata")
load("trainData2.Rdata")
load("testData2.Rdata")
load("trainDataModChi.Rdata")
load("testDataModChi.Rdata")

library(randomForest)
library(foreach)
library(doMC)
library(parallel)

coreNumber<-max(detectCores()-1,1)
registerDoMC(coreNumber)

#only consider local moran and hex average if count of hex >30
trainDataMod$LM_avgView[trainDataMod$count<30]<-NA
trainDataMod$LM_avgComment[trainDataMod$count<30]<-NA
trainDataMod$LM_avgVote[trainDataMod$count<30]<-NA
trainDataMod<-na.omit(trainDataMod)
testDataMod<-na.omit(testDataMod)

set.seed(2291033)
result<-rfModel(trainDataMod,testDataMod,1500,coreNumber)
save(result,file="resultRF.Rdata")

#summary(trainDataModChi): vote =1, comments, view =0, no need run model
#result_chi<-rfModelChi(trainDataModChi,testDataModChi)
result_chi<-data.frame(testDataModChi$id,median(trainDataModChi$num_views),median(trainDataModChi$num_votes),median(trainDataModChi$num_comments),
                       testDataModChi$city,testDataModChi$tag_type,testDataModChi$source)
names(result_chi)<-c("id","num_views","num_votes","num_comments","city","tag_type","source")
result<-rbind(result,result_chi)

#if count <sampleSize use the global median for !NA for each city 
#we only use city here on purpose, dont further split by tag/source as the count of each category will be too small and skew the median
trainData2<-data.table(trainData2)
tagSummary<-trainData2[,list(num_views=median(num_views),num_votes=median(num_votes),num_comments=median(num_comments),
                              count=length(num_views)),by="city"][order(-count)]
testData2<-testData2[!(testData2$id %in% result$id),]
result2<-merge(testData2,tagSummary,by=c("city"))
result2<-result2[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]

resultFinal<-rbind(result,result2)

write.csv(resultFinal[1:4], file = "randomForest_sub.csv", row.names = FALSE)
