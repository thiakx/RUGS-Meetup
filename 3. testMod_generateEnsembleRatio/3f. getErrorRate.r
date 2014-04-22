#I will setwd("/MY_Working_Directory/data") where the kaggle train/test data are placed

load("testDataEnsembleAns.Rdata")
rfAns<-read.csv("randomForest_e.csv")
gbmAns<-read.csv("gbm_e.csv")
svmAns<-read.csv("svm_e.csv")
glmBagAns<-read.csv("baggedGLM_e.csv")

conCatCol<-function(dataIn){
  num_votes<-dataIn[,c("id","num_votes")]
  names(num_votes)<-c("id","combineCol")
  num_comments<-dataIn[,c("id","num_comments")]
  names(num_comments)<-c("id","combineCol")
  num_views<-dataIn[,c("id","num_views")]
  names(num_views)<-c("id","combineCol")
  combine<-rbind(num_views,num_votes,num_comments)
  combine<-combine[order(combine[,1]),]
  row.names(combine)<-NULL
  return(combine)
}
  
rmsle<-function(pred,actual){
  
  result<-sqrt(mean((log(pred+1)-log(actual+1))^2))
  result<-round(result,3)
  return(result)
}

#get error rate
testDataAns<-conCatCol(testDataAns)
rfAns<-conCatCol(rfAns)
gbmAns<-conCatCol(gbmAns)
svmAns<-conCatCol(svmAns)
glmBagAns<-conCatCol(glmBagAns)
rmsle(rfAns$combineCol,testDataAns$combineCol)
rmsle(gbmAns$combineCol,testDataAns$combineCol)
rmsle(svmAns$combineCol,testDataAns$combineCol)
rmsle(glmBagAns$combineCol,testDataAns$combineCol)
