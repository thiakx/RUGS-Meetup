setwd("~/Documents/datascience SG/MusingsOfKaggler/scf_RCode/data")
source("../baseFunctions_model.R")

library(foreach)
library(doMC)
library(parallel)

coreNumber<-max(detectCores(),1)
registerDoMC(coreNumber)

result<-foreach(1:300,.combine='cbind',.inorder=FALSE) %dopar%{ 
  trainDataModTemp<-trainDataMod[1:nrow(trainDataMod) %in% sample(nrow(trainDataMod), size=nrow(trainDataMod),replace = TRUE),]
  result<-glmModel(trainDataModTemp,testDataMod)
  result[,2:4]
}

result_num_views<-apply(result[,grep("num_views",names(result))],1,mean)
result_num_votes<-apply(result[,grep("num_votes",names(result))],1,mean)
result_num_comments<-apply(result[,grep("num_comments",names(result))],1,mean)

#only selected cols are chosen
result<-cbind(testDataMod[,c("id","city","tag_type","source")],result_num_views,result_num_votes,result_num_comments)  
names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]

#!< 0, votes cannot be less than 1
result[result$num_votes<1,"num_votes"]<-1
result[result$num_comments<0,"num_comments"]<-0
result[result$num_views<0,"num_views"]<-0
save(result,file="resultGLM.Rdata")

#if count <sampleSize use the global median for !NA for each city 
#we only use city here on purpose, dont further split by tag/source as the count of each category will be too small and skew the median
trainData2<-data.table(trainData2)
tagSummary<-trainData2[,list(num_views=median(num_views),num_votes=median(num_votes),num_comments=median(num_comments),
                              count=length(num_views)),by="city"][order(-count)]
testData2<-testData2[!(testData2$id %in% result$id),]
result2<-merge(testData2,tagSummary,by=c("city"))
result2<-result2[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]

resultFinal<-rbind(result,result2)

write.csv(resultFinal[1:4], file = "baggedGLM.csv", row.names = FALSE)
