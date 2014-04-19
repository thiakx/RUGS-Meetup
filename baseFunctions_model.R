library(stringr)
library(caret)
library(data.table)

#load cleaned data
load("trainDataMod.Rdata")
load("testDataMod.Rdata")
load("trainData2.Rdata")
load("testData2.Rdata")

#only consider local moran and hex average if count of hex >30
trainDataMod$LM_avgView[trainDataMod$count<30]<-NA
trainDataMod$LM_avgVote[trainDataMod$count<30]<-NA
trainDataMod$LM_avgComment[trainDataMod$count<30]<-NA
trainDataMod<-na.omit(trainDataMod)
testDataMod<-na.omit(testDataMod)

#views, votes, comments each use thier own Local Moran
rowsNames<-names(trainDataMod)
drops <- c("hexNumber","id","num_votes","num_comments","num_views","count",
           "LM_count","LM_avgView","LM_avgComment","LM_avgVote",
           "avgComment","avgVote","avgView")
rowsNames<-rowsNames[!rowsNames %in% drops]

#use predicted views inside votes and comments as well, since logically, they are correlated
mod_viewsVar<-c(rowsNames,"LM_avgView")
mod_votesVar<-c(rowsNames,"num_views","LM_avgVote")
mod_commentsVar<-c(rowsNames,"num_views","LM_avgComment")


rfModel<-function(dataIn,dataTest,ntreeTotal,coreNumber){
  
  rf_views<- foreach(ntree=rep(ceiling(ntreeTotal/coreNumber),coreNumber), .combine=combine, .packages='randomForest',.inorder=FALSE) %dopar%
    randomForest(dataIn[,mod_viewsVar], dataIn$num_views, ntree=ntree)
  
  rf_votes<- foreach(ntree=rep(ceiling(ntreeTotal/coreNumber),coreNumber), .combine=combine, .packages='randomForest',.inorder=FALSE) %dopar%
    randomForest(dataIn[,mod_votesVar], dataIn$num_votes, ntree=ntree)
  
  rf_comments<- foreach(ntree=rep(ceiling(ntreeTotal/coreNumber),coreNumber), .combine=combine, .packages='randomForest',.inorder=FALSE) %dopar%
    randomForest(dataIn[,mod_commentsVar], dataIn$num_comments, ntree=ntree)

  rf_views_Test<-predict(rf_views, dataTest[,mod_viewsVar], type="response")
  dataTest$num_views<-rf_views_Test

  rf_votes_Test<-predict(rf_votes, dataTest[,mod_votesVar], type="response")
  
  rf_comments_Test<-predict(rf_comments, dataTest[,mod_commentsVar], type="response")
  dataTest$num_views<-NULL
  
  #only selected cols are chosen
  result<-cbind(dataTest[,c("id","city","tag_type","source")],rf_views_Test,rf_votes_Test,rf_comments_Test)  
  names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
  result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]
  
  #undo the log for view
  result$num_views<-exp(result$num_views)-1
  
  #!< 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0,"num_comments"]<-0
  result[result$num_views<0,"num_views"]<-0
  return (result)
}

glmModel<-function(dataIn,dataTest){
  
  glm_views<- glm(paste("num_views~",paste(mod_viewsVar, collapse = "+"),sep=""),data=dataIn) 
  glm_votes<- glm(paste("num_votes~",paste(mod_votesVar, collapse = "+"),sep=""),data=dataIn)
  glm_comments<- glm(paste("num_comments~",paste(mod_commentsVar, collapse = "+"),sep=""),data=dataIn)
  
  glm_views_Test<-predict(glm_views, dataTest[,mod_viewsVar], type="response")
  dataTest$num_views<-glm_views_Test
  
  glm_votes_Test<-predict(glm_votes, dataTest[,mod_votesVar], type="response")
  
  glm_comments_Test<-predict(glm_comments, dataTest[,mod_commentsVar], type="response")
  dataTest$num_views<-NULL
  
  #only selected cols are chosen
  result<-cbind(dataTest[,c("id","city","tag_type","source")],glm_views_Test,glm_votes_Test,glm_comments_Test)  
  names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
  result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]
  
  #undo the log
  result$num_views<-exp(result$num_views)-1
  
  #!< 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0,"num_comments"]<-0
  result[result$num_views<0,"num_views"]<-0
  return (result)
}


gbmModel <-function(dataIn,dataTest){

#parameters to be tested
treeAmt     =10000
shinkAmt    =0.001
depthAmt    =5
minobsAmt   =25

#gbm submission
gbm_views<-gbm.fit(dataIn[,mod_viewsVar], dataIn$num_views,n.trees=treeAmt, shrinkage=shinkAmt, distribution="gaussian",
                   interaction.depth=depthAmt,n.minobsinnode = minobsAmt)

gbm_votes<-gbm.fit(dataIn[,mod_votesVar], dataIn$num_votes,n.trees=treeAmt, shrinkage=shinkAmt, distribution="poisson",
                interaction.depth=depthAmt,n.minobsinnode = minobsAmt)

gbm_comments<-gbm.fit(dataIn[,mod_commentsVar], dataIn$num_comments,n.trees=treeAmt, shrinkage=shinkAmt, distribution="poisson",
                   interaction.depth=depthAmt,n.minobsinnode = minobsAmt)

#oob best iter tend to under estimate, hence need to increase it by 20%
best.iter_views <- 1.2*gbm.perf(gbm_views,method="OOB")
best.iter_votes <- 1.2*gbm.perf(gbm_votes,method="OOB")
best.iter_comments <- 1.2*gbm.perf(gbm_comments,method="OOB")

gbm_views_Test <- predict(gbm_views,  dataTest[,mod_viewsVar],best.iter_views, type="response")
dataTest$num_views<-gbm_views_Test

gbm_votes_Test <- predict(gbm_votes,  dataTest[,mod_votesVar],best.iter_votes, type="response")

gbm_comments_Test <- predict(gbm_comments,  dataTest[,mod_commentsVar],best.iter_comments, type="response")
dataTest$num_views<-NULL

#only selected cols are chosen
result<-cbind(dataTest[,c("id","city","tag_type","source")],gbm_views_Test,gbm_votes_Test,gbm_comments_Test)  
names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]

#undo the log
result$num_views<-exp(result$num_views)-1

#!< 0, votes cannot be less than 1
result[result$num_votes<1,"num_votes"]<-1
result[result$num_comments<0,"num_comments"]<-0
result[result$num_views<0,"num_views"]<-0
return (result)

}


svmModel <-function(dataIn,dataTest){

  svm_views<- svm(as.formula(paste("num_views~",paste(mod_viewsVar, collapse = "+"),sep="")),data=dataIn) 
  svm_votes<- svm(as.formula(paste("num_votes~",paste(mod_votesVar, collapse = "+"),sep="")),data=dataIn)
  svm_comments<- svm(as.formula(paste("num_comments~",paste(mod_commentsVar, collapse = "+"),sep="")),data=dataIn)

  svm_views_Test <- predict(svm_views,  dataTest[,mod_viewsVar],type="response")
  dataTest$num_views<-svm_views_Test
  
  svm_votes_Test <- predict(svm_votes,  dataTest[,mod_votesVar], type="response")
  
  svm_comments_Test <- predict(svm_comments,  dataTest[,mod_commentsVar], type="response")
  
  dataTest$num_views<-NULL
  
  #only selected cols are chosen
  result<-cbind(dataTest[,c("id","city","tag_type","source")],svm_views_Test,svm_votes_Test,svm_comments_Test)  
  names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
  result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]
  
  #undo the log
  result$num_views<-exp(result$num_views)-1
  
  #!< 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0,"num_comments"]<-0
  result[result$num_views<0,"num_views"]<-0
  return (result)
}
