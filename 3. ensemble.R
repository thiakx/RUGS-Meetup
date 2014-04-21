
load("eRatioList.RData")

rfAns<-read.csv("randomForest.csv")
gbmAns<-read.csv("gbm.csv")
svmAns<-read.csv("svm.csv")
glmBagAns<-read.csv("baggedGLM.csv")

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

#votes cannot be less than 1
ensembleAns[ensembleAns$num_votes<1,"num_votes"]<-1
ensembleAns[ensembleAns$num_comments<0,"num_comments"]<-0
ensembleAns[ensembleAns$num_views<0,"num_views"]<-0

# submit prediction
write.csv(ensembleAns, file = "ensemble.csv", row.names = FALSE)