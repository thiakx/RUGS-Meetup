#I will setwd("/MY_Working_Directory/data") where the kaggle train/test data are placed

load("testDataEnsembleAns.Rdata")
rfAns<-read.csv("randomForest_e.csv")
gbmAns<-read.csv("gbm_e.csv")
svmAns<-read.csv("svm_e.csv")
glmBagAns<-read.csv("baggedGLM_e.csv")

##getting eRatio###

#make sure all 3 same sort order
rfAns<-rfAns[order(rfAns[,1]),]
names(rfAns)<-c("id","rfAns_num_views","rfAns_num_votes","rfAns_num_comments")
gbmAns<-gbmAns[order(gbmAns[,1]),]
names(gbmAns)<-c("id","gbmAns_num_views","gbmAns_num_votes","gbmAns_num_comments")
svmAns<-svmAns[order(svmAns[,1]),]
names(svmAns)<-c("id","svmAns_num_views","svmAns_num_votes","svmAns_num_comments")
glmBagAns<-glmBagAns[order(glmBagAns[,1]),]
names(glmBagAns)<-c("id","glmBagAns_num_views","glmBagAns_num_votes","glmBagAns_num_comments")

testDataAns<-testDataAns[order(testDataAns[,1]),]
ensembleData<-cbind(testDataAns,rfAns[2:4],gbmAns[2:4],svmAns[2:4],glmBagAns[2:4])

eGlm_num_views<-glm(num_views~rfAns_num_views+gbmAns_num_views+svmAns_num_views+glmBagAns_num_views,data=ensembleData)
eRatio_num_view<-c(eGlm_num_views$coef[2],eGlm_num_views$coef[3],eGlm_num_views$coef[4],eGlm_num_views$coef[5],eGlm_num_views$coef[1])

eGlm_num_votes<-glm(num_votes~rfAns_num_votes+gbmAns_num_votes+svmAns_num_votes+glmBagAns_num_votes,data=ensembleData)
eRatio_num_votes<-c(eGlm_num_votes$coef[2],eGlm_num_votes$coef[3],eGlm_num_votes$coef[4],eGlm_num_votes$coef[5],eGlm_num_votes$coef[1])

eGlm_num_comments<-glm(num_comments~rfAns_num_comments+gbmAns_num_comments+svmAns_num_comments+glmBagAns_num_comments,data=ensembleData)
eRatio_num_comments<-c(eGlm_num_comments$coef[2],eGlm_num_comments$coef[3],eGlm_num_comments$coef[4],eGlm_num_comments$coef[5],eGlm_num_comments$coef[1])

#scale ratio to sum up as 1. We want the % contribution by each model, which adds up to 100% or 1
eRatio_num_view<-eRatio_num_view/sum(eRatio_num_view)
eRatio_num_votes<-eRatio_num_votes/sum(eRatio_num_votes)
eRatio_num_comments<-eRatio_num_comments/sum(eRatio_num_comments)

eRatioList<-list(eRatio_num_view,eRatio_num_votes,eRatio_num_comments)
save(eRatioList, file="eRatioList.RData")