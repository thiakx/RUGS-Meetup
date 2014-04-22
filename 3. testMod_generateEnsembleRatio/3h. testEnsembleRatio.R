#I will setwd("/MY_Working_Directory/data") where the kaggle train/test data are placed
load("eRatioList.RData")
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

eRatio_num_view<-eRatioList[[1]]
eRatio_num_votes<-eRatioList[[2]]
eRatio_num_comments<-eRatioList[[3]]

#make sure all 3 same sort order
rfAns<-rfAns[order(rfAns[,1]),]
gbmAns<-gbmAns[order(gbmAns[,1]),]
svmAns<-svmAns[order(svmAns[,1]),]
glmBagAns<-glmBagAns[order(glmBagAns[,1]),]

ensembleAns<-rfAns
ensembleAns$num_views<-eRatio_num_view[1]*rfAns$num_views+eRatio_num_view[2]*gbmAns$num_views+eRatio_num_view[3]*svmAns$num_views+eRatio_num_view[4]*glmBagAns$num_views+eRatio_num_view[5]
ensembleAns$num_votes<-eRatio_num_votes[1]*rfAns$num_votes+eRatio_num_votes[2]*gbmAns$num_votes+eRatio_num_votes[3]*svmAns$num_votes+eRatio_num_votes[4]*glmBagAns$num_votes+eRatio_num_votes[5]
ensembleAns$num_comments<-eRatio_num_comments[1]*rfAns$num_comments+eRatio_num_comments[2]*gbmAns$num_comments+eRatio_num_comments[3]*svmAns$num_comments+eRatio_num_comments[4]*glmBagAns$num_comments+eRatio_num_comments[5]

#<0.5 = 0, votes cannot be less than 1
ensembleAns[ensembleAns$num_votes<1,"num_votes"]<-1
ensembleAns[ensembleAns$num_comments<0.05,"num_comments"]<-0
ensembleAns[ensembleAns$num_views<0.05,"num_views"]<-0

ensembleAns<-conCatCol(ensembleAns)
testDataAns<-conCatCol(testDataAns)

rmsle(ensembleAns$combineCol,testDataAns$combineCol)