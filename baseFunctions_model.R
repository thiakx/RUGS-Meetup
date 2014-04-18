library(stringr)
library(caret)
library(data.table)

month<-function(dataIn){
  return (as.numeric(format(as.Date(dataIn), "%m")))
}

year<-function(dataIn){
  return (as.numeric(format(as.Date(dataIn), "%Y")))
}

#requires data.table as input
madRemove<-function(dataIn,cutOff){
  madSummary<-dataIn[,list(num_views=median(num_views)+cutOff*mad(num_views),
                                num_votes=median(num_votes)+cutOff*mad(num_votes),
                                num_comments=median(num_comments)+cutOff*mad(num_comments),
                                count=length(num_views)),by="city,tag_type"][order(-count,city,tag_type)]
  for(i in 1:nrow(madSummary)){
    dataIn<-dataIn[!(dataIn$city==madSummary[[i,1]]&dataIn$tag_type==madSummary[[i,2]]&
                                 (dataIn$num_views>madSummary[[i,3]]|
                                    dataIn$num_votes>madSummary[[i,4]]|
                                    dataIn$num_comments>madSummary[[i,5]])),]
  }
  return(dataIn)
}

rfModel<-function(dataIn,dataTest,ntreeTotal,coreNumber){
  
  #views, votes, comments each use thier own Local Moran
  rowsNames<-names(dataIn)
  drops <- c("hexNumber","id","num_votes","num_comments","num_views","count",
             "LM_count","LM_avgView","LM_avgComment","LM_avgVote",
             "avgComment","avgVote","avgView")
  rowsNames<-rowsNames[!rowsNames %in% drops]
  
  #based on cor below, we shld do: view,votes, comments
  #cor(trainDataRF[,c("num_votes","num_comments","num_views")])
  rf_views<- foreach(ntree=rep(ceiling(ntreeTotal/coreNumber),coreNumber), .combine=combine, .packages='randomForest',.inorder=FALSE) %dopar%
    randomForest(dataIn[,c(rowsNames,"LM_avgView")], dataIn$num_views, ntree=ntree)
  
  rf_votes<- foreach(ntree=rep(ceiling(ntreeTotal/coreNumber),coreNumber), .combine=combine, .packages='randomForest',.inorder=FALSE) %dopar%
    randomForest(dataIn[,c(rowsNames,"num_views","LM_avgVote")], dataIn$num_votes, ntree=ntree)
  
  rf_comments<- foreach(ntree=rep(ceiling(ntreeTotal/coreNumber),coreNumber), .combine=combine, .packages='randomForest',.inorder=FALSE) %dopar%
    randomForest(dataIn[,c(rowsNames,"num_views","num_votes","LM_avgComment")], dataIn$num_comments, ntree=ntree)

  rf_views_Test<-predict(rf_views, dataTest[,c(rowsNames,"LM_avgView")], type="response")
  dataTest$num_views<-rf_views_Test

  rf_votes_Test<-predict(rf_votes, dataTest[,c(rowsNames,"num_views","LM_avgVote")], type="response")
  dataTest$num_votes<-rf_votes_Test
  
  rf_comments_Test<-predict(rf_comments, dataTest[,c(rowsNames,"num_views","num_votes","LM_avgComment")], type="response")
  dataTest$num_views<-NULL
  dataTest$num_votes<-NULL
  
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

rfModelBag <-function(dataIn,dataTest,nTree,nLoop){
  
  #need to add names below if new col is added
  rowsNames<-c("city","tag_type","source")
  
  resultPrev=data.frame(0)
  
  for(i in 1:nLoop){
  dataInTemp<-dataIn[1:nrow(dataIn) %in% sample(nrow(dataIn), size=nTree,replace = FALSE),]
  result<-rfModel(dataInTemp,dataTest,999)
    if(nrow(resultPrev)==1){
    result<-result[,2:4]
    resultPrev<-result
    }else{ 
    result<-result[,2:4]+resultPrev
    resultPrev<-result
    }
  }
  
  result<-result/nLoop
  #<0.5 = 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0.5,"num_comments"]<-0
  result[result$num_views<0.5,"num_views"]<-0
  
  result<-cbind(dataTest,result)    
  result<-result[,c("id","num_views","num_votes","num_comments",rowsNames)]
  return (result)
}

gbmModel <-function(dataIn,dataTest){

#need to add names below if new col is added
rowsNames<-names(dataIn[,5:length(dataIn)])

#parameters to be tested
treeAmt     =10000
shinkAmt    =0.001
depthAmt    =5
minobsAmt   =25

#gbm submission
gbm_views<-gbm.fit(dataIn[,rowsNames], dataIn$num_views,n.trees=treeAmt, shrinkage=shinkAmt, distribution="gaussian",
                   interaction.depth=depthAmt,n.minobsinnode = minobsAmt)

gbm_votes<-gbm.fit(dataIn[,c(rowsNames,"num_views")], dataIn$num_votes,n.trees=treeAmt, shrinkage=shinkAmt, distribution="poisson",
                interaction.depth=depthAmt,n.minobsinnode = minobsAmt)

gbm_comments<-gbm.fit(dataIn[,c(rowsNames,"num_views","num_votes")], dataIn$num_comments,n.trees=treeAmt, shrinkage=shinkAmt, distribution="poisson",
                   interaction.depth=depthAmt,n.minobsinnode = minobsAmt)

best.iter_views <- 1.2*gbm.perf(gbm_views,method="OOB")
best.iter_votes <- 1.2*gbm.perf(gbm_votes,method="OOB")
best.iter_comments <- 1.2*gbm.perf(gbm_comments,method="OOB")

gbm_views_Test <- predict(gbm_views,  dataTest[,rowsNames],best.iter_views, type="response")
dataTest$num_views<-gbm_views_Test

gbm_votes_Test <- predict(gbm_votes,  dataTest[,c(rowsNames,"num_views")],best.iter_votes, type="response")
dataTest$num_votes<-gbm_votes_Test

gbm_comments_Test <- predict(gbm_comments,  dataTest[,c(rowsNames,"num_views","num_votes")],best.iter_comments, type="response")
dataTest$num_views<-NULL
dataTest$num_votes<-NULL

#only selected cols are chosen
result<-cbind(dataTest[,c("id","city","tag_type","source")],gbm_views_Test,gbm_votes_Test,gbm_comments_Test)  
names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]

#undo the log
#   result$num_votes<-exp(result$num_votes)-1
#   result$num_comments<-exp(result$num_comments)-1
result$num_views<-exp(result$num_views)-1

#!< 0, votes cannot be less than 1
result[result$num_votes<1,"num_votes"]<-1
result[result$num_comments<0,"num_comments"]<-0
result[result$num_views<0,"num_views"]<-0
return (result)

}


svmModel <-function(dataIn,dataTest){
  
  #need to add names below if new col is added
  rowsNames<-names(dataIn[,5:length(dataIn)])
#   
#   #x,y and tune.svm,$best.model why does not work...why? try with subset of data
#   #svm S3 method and tune best model only works with numeric values, convert factors to numeric
#   dataIn$city<-as.numeric(dataIn$city)
#   dataIn$tag_type<-as.numeric(dataIn$tag_type)
#   dataIn$source<-as.numeric(dataIn$source)
#   dataTest$city<-as.numeric(dataTest$city)
#   dataTest$tag_type<-as.numeric(dataTest$tag_type)
#   dataTest$source<-as.numeric(dataTest$source)
#   
  svm_views<-svm(num_views~city+tag_type+source, dataIn)
  svm_votes<-svm(num_votes~city+tag_type+source+num_views, dataIn)
  svm_comments<-svm(num_comments~city+tag_type+source+num_views+num_votes, dataIn)

  svm_views_Test <- predict(svm_views,  dataTest[,rowsNames],type="response")
  dataTest$num_views<-svm_views_Test
  
  svm_votes_Test <- predict(svm_votes,  dataTest[,c(rowsNames,"num_views")], type="response")
  dataTest$num_votes<-svm_votes_Test
  
  svm_comments_Test <- predict(svm_comments,  dataTest[,c(rowsNames,"num_views","num_votes")], type="response")
  
  dataTest$num_views<-NULL
  dataTest$num_votes<-NULL
  
  #only selected cols are chosen
  result<-cbind(dataTest[,c("id","city","tag_type","source")],svm_views_Test,svm_votes_Test,svm_comments_Test)  
  names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
  result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]
  
  #undo the log
  #   result$num_votes<-exp(result$num_votes)-1
  #   result$num_comments<-exp(result$num_comments)-1
  result$num_views<-exp(result$num_views)-1
  
  #!< 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0,"num_comments"]<-0
  result[result$num_views<0,"num_views"]<-0
  return (result)

}


glmModel<-function(dataIn,dataTest){
  
  #need to add names below if new col is added
  rowsNames<-names(dataIn[,5:length(dataIn)])
  
  glm_views<- glm(paste("num_views~",paste(rowsNames, collapse = "+"),sep=""),data=dataIn) 
  glm_votes<- glm(paste("num_votes~",paste(rowsNames, collapse = "+"),"+num_views",sep=""),data=dataIn)
  glm_comments<- glm(paste("num_comments~",paste(rowsNames, collapse = "+"),"+num_views","+num_votes",sep=""),data=dataIn)
  
  glm_views_Test<-predict(glm_views, dataTest[,c(rowsNames)], type="response")
  dataTest$num_views<-glm_views_Test
  
  glm_votes_Test<-predict(glm_votes, dataTest[,c(rowsNames,"num_views")], type="response")
  dataTest$num_votes<-glm_votes_Test
  
  glm_comments_Test<-predict(glm_comments, dataTest[,c(rowsNames,"num_views","num_votes")], type="response")
  dataTest$num_views<-NULL
  dataTest$num_votes<-NULL
  
  #only selected cols are chosen
  result<-cbind(dataTest[,c("id","city","tag_type","source")],glm_views_Test,glm_votes_Test,glm_comments_Test)  
  names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
  result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]
  
  #undo the log
  #     result$num_votes<-exp(result$num_votes)
  #     result$num_comments<-exp(result$num_comments)-1
  result$num_views<-exp(result$num_views)-1
  
  #!< 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0,"num_comments"]<-0
  result[result$num_views<0,"num_views"]<-0
  return (result)
}

glmNetModel<-function(dataIn,dataTest){
  
  #need to add names below if new col is added
  rowsNames<-names(dataIn[,5:length(dataIn)])
  
  #model matrix help prepare data, esp factor for glmnet 
  #may need to standardize = FALSE due to eqns having factors
  glm_views<-cv.glmnet(model.matrix(as.formula(paste("num_views~",paste(rowsNames, collapse = "+"),sep="")),dataIn),as.matrix(dataIn$num_views,ncol=1),family="gaussian",alpha=0.25)
  glm_votes<-cv.glmnet(model.matrix(as.formula(paste("num_votes~",paste(rowsNames, collapse = "+"),"+num_views",sep="")),dataIn),as.matrix(dataIn$num_votes,ncol=1),family="gaussian",alpha=0.25)
  glm_comments<-cv.glmnet(model.matrix(as.formula(paste("num_comments~",paste(rowsNames, collapse = "+"),"+num_views","+num_votes",sep="")),dataIn),as.matrix(dataIn$num_comments,ncol=1),family="gaussian",alpha=0.25)
  
  glm_views_Test<-predict(glm_views, model.matrix(as.formula(paste("~",paste(rowsNames, collapse = "+"),sep="")),dataTest),type="response",s="lambda.min")
  dataTest$num_views<-glm_views_Test
  
  glm_votes_Test<-predict(glm_votes, model.matrix(as.formula(paste("~",paste(rowsNames, collapse = "+"),"+num_views",sep="")),dataTest), type="response",s="lambda.min")
  dataTest$num_votes<-glm_votes_Test
  
  glm_comments_Test<-predict(glm_comments, model.matrix(as.formula(paste("~",paste(rowsNames, collapse = "+"),"+num_views","+num_votes",sep="")),dataTest), type="response",s="lambda.min")
  dataTest$num_views<-NULL
  dataTest$num_votes<-NULL
  
  #only selected cols are chosen
  result<-cbind(dataTest[,c("id","city","tag_type","source")],glm_views_Test,glm_votes_Test,glm_comments_Test)  
  names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
  result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]
  
  #undo the log
  #     result$num_votes<-exp(result$num_votes)
  #     result$num_comments<-exp(result$num_comments)-1
  result$num_views<-exp(result$num_views)-1
  
  #!< 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0,"num_comments"]<-0
  result[result$num_views<0,"num_views"]<-0
  return (result)
}

glmModelBag <-function(dataIn,dataTest,nLoop){

  result<-foreach(1:nLoop,.combine='cbind',.inorder=FALSE) %dopar%{
    
    dataInTemp<-dataIn[1:nrow(dataIn) %in% sample(nrow(dataIn), size=nrow(dataIn),replace = TRUE),]
    result<-glmModel(dataInTemp,dataTest)
    result[,2:4]
  }
  
  result_num_views<-apply(result[,grep("num_views",names(result))],1,mean)
  result_num_votes<-apply(result[,grep("num_votes",names(result))],1,mean)
  result_num_comments<-apply(result[,grep("num_comments",names(result))],1,mean)
 
  #only selected cols are chosen
  result<-cbind(dataTest[,c("id","city","tag_type","source")],result_num_views,result_num_votes,result_num_comments)  
  names(result)<-c("id","city","tag_type","source","num_views","num_votes","num_comments")
  result<-result[,c("id","num_views","num_votes","num_comments","city","tag_type","source")]
  
  #!< 0, votes cannot be less than 1
  result[result$num_votes<1,"num_votes"]<-1
  result[result$num_comments<0,"num_comments"]<-0
  result[result$num_views<0,"num_views"]<-0
  return (result)
}


remapAPI <- function(dataIn){
  
  dataIn$tag_type<-as.character(dataIn$tag_type)
  
  #simplify tag
  dataIn$tag_type[dataIn$tag_type=="animal_problem"]<-"animal"
  dataIn$tag_type[dataIn$tag_type=="City Bldg"]<-"cityProb"
  dataIn$tag_type[dataIn$tag_type=="Unknown"]<-"unknown"
  dataIn$tag_type[dataIn$tag_type=="Park"]<-"park"
  dataIn$tag_type[dataIn$tag_type=="road_safety"]<-"road"
  dataIn$tag_type[dataIn$tag_type=="abandoned_vehicle"]<-"abandon"
  dataIn$tag_type[dataIn$tag_type=="street_light"]<-"street light"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$summary=="Abandoned Vehicle"]<-"abandon"
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$summary=="Homeless Encampment"]<-"homeless"
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$summary=="Pedestrian Signal - Broken/Damaged"]<-"pedestrian_light"
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$summary=="Pothole in Street"]<-"pothole"
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$summary=="Rodent Baiting / Rat Complaint"]<-"rodents"
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$summary=="Tree Debris"]<-"tree"
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$summary=="Restaurant Complaint"]<-"restaurant"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="Graffiti - Advertising (posters, signs, etc.)"
                                                        |dataIn$summary=="Graffiti (report from SeeClickFix)"
                                                        |dataIn$summary=="Graffiti in a Park"
                                                        |dataIn$summary=="Graffiti on Private Property"
                                                        |dataIn$summary=="Graffiti on Street, Street Light, Traffic Signal,"
                                                        |dataIn$summary=="Graffiti Removal")]<-"graffiti"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="Street Cut Complaints"
                                                        |dataIn$summary=="Street Medians (Landscaped)"
                                                        |dataIn$summary=="Streets - Berm Repair or Install"
                                                        |dataIn$summary=="Streets - Guardrail Repair"
                                                        |dataIn$summary=="Streets - Mudslides / Landslides"
                                                        |dataIn$summary=="Streets - Potholes/Depression"
                                                        |dataIn$summary=="Streets - Speed Bump Repair"
                                                        |dataIn$summary=="Streets - Street Deterioration")]<-"road"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="Pavement Cave-In Survey"
                                                        |dataIn$summary=="Sidewalk - Damage"
                                                        |dataIn$summary=="Streets/Sidewalks - Curb & Gutter Repair"
                                                        |dataIn$summary=="Streets/Sidewalks - Curb &amp; Gutter Repair"
                                                        |dataIn$summary=="Streets/Sidewalks - Pathway Repair"
                                                        |dataIn$summary=="Streets/Sidewalks - Pooling Water"
                                                        |dataIn$summary=="Streets/Sidewalks - Portable Barriers Maint"
                                                        |dataIn$summary=="Streets/Sidewalks Maintenance - General"
                                                        |dataIn$summary=="Electrical Curb Box - Damaged/Missing"
                                                        |dataIn$summary=="Fence - Repair or Install Along Street")]<-"sidewalk"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="Alley Light Out"
                                                        |dataIn$summary=="Necklace of Lights Broken/Damaged"
                                                        |dataIn$summary=="Street Light - Outage/Damaged"
                                                        |dataIn$summary=="Street Light - Pole Down"
                                                        |dataIn$summary=="Street Light - Pole Leaning"
                                                        |dataIn$summary=="Street Light - Request for New/Upgrade"
                                                        |dataIn$summary=="Street Light 1 / Out"
                                                        |dataIn$summary=="Street Lights All / Out")]<-"street light"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="Traffic Signal - Knocked Down"
                                                        |dataIn$summary=="Traffic Signal - Outage/Damaged"
                                                        |dataIn$summary=="Traffic Signal - Turned in Wrong Direction"
                                                        |dataIn$summary=="Traffic Signal Out")]<-"street_signal"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="Illegal Dumping - debris, appliances, etc."
                                                        |dataIn$summary=="Illegal Dumping (Enforcement Potential)"
                                                        |dataIn$summary=="Illegal Dumping (report from SeeClickFix)"
                                                        |dataIn$summary=="Litter - Green Bag Pickup"
                                                        |dataIn$summary=="Litter - In Public Right of Way"
                                                        |dataIn$summary=="Litter - Street Litter Container"
                                                        |dataIn$summary=="Litter in Parks"
                                                        |dataIn$summary=="Sanitation Code Violation")]<-"trash"
 
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="City Bldg - Clean / Custodial"
                                                        |dataIn$summary=="City Bldg - Electrical Inside/Outside"
                                                        |dataIn$summary=="City Bldg - Plumbing"
                                                        |dataIn$summary=="Building Violation"
                                                        |dataIn$summary=="Shrine Removal With OPD")]<-"cityProb"
  
  dataIn$tag_type[dataIn$source=="remote_api_created" &(dataIn$summary=="Park - Landscape Maintenance"
                                                        |dataIn$summary=="Park - Mowing"
                                                        |dataIn$summary=="Park - Pathways, Hardscape and Paving"
                                                        |dataIn$summary=="Park - Pests"
                                                        |dataIn$summary=="Park - Tot Lots, Tables, Benches"
                                                        |dataIn$summary=="Parks and Street Medians - Irrigation")]<-"park"
  
  #the remaining all contains illegal dumping (with special characters), aka mapped to trash
  dataIn$tag_type[dataIn$source=="remote_api_created" &dataIn$tag_type=="unknown"]<-"trash"
  
  #recode most of the remaining unknown tags 
  dataInTemp<-dataIn[dataIn$tag_type=="unknown",]
  dataIn<-dataIn[dataIn$tag_type!="unknown",]
  dataInTemp$tag_type[grep("trash|Bulk|Discard|removal|furniture|dump|alley|Street|st|Avenue|Ave|rd|Dr|Drive|ct|Boulevard|blvd|abandon",dataInTemp$summary,ignore.case = TRUE)]<-"trash"
  dataInTemp$tag_type[grep("road",dataInTemp$summary,ignore.case = TRUE)]<-"road"
  dataInTemp$tag_type[grep("tree|brush|trim|limb",dataInTemp$summary,ignore.case = TRUE)]<-"tree"
  dataInTemp$tag_type[grep("Pothole",dataInTemp$summary,ignore.case = TRUE)]<-"pothole"
  dataInTemp$tag_type[grep("burn",dataInTemp$summary,ignore.case = TRUE)]<-"street light"
  dataInTemp$tag_type[grep("cat|dog",dataInTemp$summary,ignore.case = TRUE)]<-"animal"
  dataInTemp$tag_type[grep("park",dataInTemp$summary,ignore.case = TRUE)]<-"park"
  dataInTemp$tag_type[grep("other|misc",dataInTemp$summary,ignore.case = TRUE)]<-"unknown"
  dataIn<-rbind(dataIn,dataInTemp)
  
  #remap the low count tags to higher count tags
  dataIn$tag_type[dataIn$tag_type=="street_signal"|dataIn$tag_type=="traffic"|dataIn$tag_type=="roadkill"
                  |dataIn$tag_type=="illegal_idling"|dataIn$tag_type=="bike_concern"|dataIn$tag_type=="pedestrian_light"
                  |dataIn$tag_type=="bad_driving"|dataIn$tag_type=="crosswalk"]<-"road"
  
  dataIn$tag_type[dataIn$tag_type=="zoning"|dataIn$tag_type=="bridge"|dataIn$tag_type=="blighted_property"
                  |dataIn$tag_type=="drain_problem"|dataIn$tag_type=="overgrowth"|dataIn$tag_type=="odor"
                  |dataIn$tag_type=="bench"|dataIn$tag_type=="parking_meter"]<-"cityProb"

  dataIn$tag_type[dataIn$tag_type=="snow"|dataIn$tag_type=="flood"|dataIn$tag_type=="heat"]<-"weather"
  
  dataIn$tag_type[dataIn$tag_type=="robbery"|dataIn$tag_type=="drug_dealing"|dataIn$tag_type=="prostitution"
                  |dataIn$tag_type=="homeless"|dataIn$tag_type=="noise_complaint"|dataIn$tag_type=="lost_and_found"]<-"crime"
  
  dataIn$tag_type[dataIn$tag_type=="rodents"]<-"animal"
  dataIn$tag_type[dataIn$tag_type=="test"]<-"unknown"
  
  dataIn$tag_type<-as.factor(dataIn$tag_type)
  
return(dataIn)
}   

#split the tag into multiple columns
splitTag<-function (dataIn){
  splitTags <- as.data.frame(t(sapply(dataIn[,"tag_type"], function(x) {
    y <- rep(0, length(levels(dataIn[,"tag_type"])))
    y
  })))
  names(splitTags)<-levels(dataIn[,"tag_type"])
  result <- cbind(dataIn, splitTags) 
  return(result)  
}


wordMine<-function(dataIn,testCol){
  
  startCol<-ncol(dataIn)+1
  dataIn<-splitTag(dataIn)
  
  dataIn$summary<-as.character(dataIn$summary)
  dataIn$description<-as.character(dataIn$description)
  dataIn[,startCol:ncol(dataIn)]<-sapply(dataIn[,startCol:ncol(dataIn)],as.numeric)
  
  if(length(testCol)>1){
    dataIn<-dataIn[,testCol]
  }
  
  #sum up counts for those that match col header  
  for(i in startCol:ncol(dataIn)){
      dataIn[,i] <- sapply(dataIn$summary, function(x) str_count(x,ignore.case(names(dataIn)[i])))
  }
  for(i in startCol:ncol(dataIn)){
    dataIn[,i] <-  dataIn[,i]+sapply(dataIn$description, function(x) str_count(x,ignore.case(names(dataIn)[i])))
  }
  
  #sum up counts for those that match rules
  try(dataIn[,"road"] <- dataIn[,"road"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("street","signal","traffic","roadkill","idling","bike","pedestrian","driving","crosswalk")
  )))),silent = TRUE)
  try(dataIn[,"road"] <- dataIn[,"road"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("street","signal","traffic","roadkill","idling","bike","pedestrian","driving","crosswalk")
  )))),silent = TRUE)
  
  try(dataIn[,"cityProb"] <- dataIn[,"cityProb"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("city","zone","zoning","bridge","blight","property","drain","overgrowth","odor","bench","parking")
  )))),silent = TRUE)
  try(dataIn[,"cityProb"] <- dataIn[,"cityProb"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("city","zone","zoning","bridge","blight","property","drain","overgrowth","odor","bench","parking")
  )))),silent = TRUE)
  
  try(dataIn[,"weather"] <- dataIn[,"weather"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("snow","flood","heat")
  )))),silent = TRUE)
  try(dataIn[,"weather"] <- dataIn[,"weather"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("snow","flood","heat")
  )))),silent = TRUE)
  
  try(dataIn[,"crime"] <- dataIn[,"crime"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("rob","drug","prostitut","homeless","noise","lost")
  )))),silent = TRUE)
  try(dataIn[,"crime"] <- dataIn[,"crime"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("rob","drug","prostitut","homeless","noise","lost")
  )))),silent = TRUE)
  
  try(dataIn[,"animal"] <- dataIn[,"animal"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("rodents","dog","cat")
  )))),silent = TRUE)
  try(dataIn[,"animal"] <- dataIn[,"animal"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("rodents","dog","cat")
  )))),silent = TRUE)
  
  try(dataIn[,"trash"] <- dataIn[,"trash"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("trash","Bulk","Discard","removal","furniture","dump","alley")
  )))),silent = TRUE)
  try(dataIn[,"trash"] <- dataIn[,"trash"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("trash","Bulk","Discard","removal","furniture","dump","alley")
  )))),silent = TRUE)
  
  try(dataIn[,"tree"] <- dataIn[,"tree"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("brush","trim","limb")
  )))),silent = TRUE)
  try(dataIn[,"tree"] <- dataIn[,"tree"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("brush","trim","limb")
  )))),silent = TRUE)
  
  try(dataIn[,"street light"] <- dataIn[,"street light"]+sapply(dataIn$summary, function(x) sum(str_count(x,ignore.case(
    c("burn","light","streetlight")
  )))),silent = TRUE)
  try(dataIn[,"street light"] <- dataIn[,"street light"]+sapply(dataIn$description, function(x) sum(str_count(x,ignore.case(
    c("burn","light","streetlight")
  )))),silent = TRUE)
  
  if (length(testCol)==1){
    nearZero<-nearZeroVar(dataIn)
    nearZero<-subset(nearZero,nearZero>=startCol)
    dataIn<-dataIn[,!(1:ncol(dataIn) %in% nearZero)]
  }
  
  return (dataIn)
}
