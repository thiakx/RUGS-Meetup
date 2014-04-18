setwd("~/Documents/datascience SG/MusingsOfKaggler/scf_RCode/data")
source("../baseFunctions_model.R")

trainData<-read.csv("train.csv")
testData<-read.csv("test.csv")

#classify by cities based on lat long
trainData$longitude<-trunc(trainData$longitude)
trainData$city<-""
trainData$city[trainData$longitude=="-77"]<-"richmond"
trainData$city[trainData$longitude=="-72"]<-"new_haven"
trainData$city[trainData$longitude=="-87"]<-"chicargo"
trainData$city[trainData$longitude=="-122"]<-"oakland"
trainData$city<-as.factor(trainData$city)

testData$longitude<-trunc(testData$longitude)
testData$city<-""
testData$city[testData$longitude=="-77"]<-"richmond"
testData$city[testData$longitude=="-72"]<-"new_haven"
testData$city[testData$longitude=="-87"]<-"chicargo"
testData$city[testData$longitude=="-122"]<-"oakland"
testData$city<-as.factor(testData$city)

trainData2<-trainData[,!(names(trainData) %in% c("latitude","longitude"))]

#replace string NA with unknown
trainData2$tag_type<-as.character(trainData2$tag_type)
trainData2$tag_type[is.na(trainData2$tag_type)]<-"unknown"
trainData2$tag_type<-as.factor(trainData2$tag_type)

testData$tag_type<-as.character(testData$tag_type)
testData$tag_type[is.na(testData$tag_type)]<-"unknown"
testData$tag_type<-as.factor(testData$tag_type)

trainData2$source<-as.character(trainData2$source)
trainData2$source[is.na(trainData2$source)]<-"unknown"
trainData2$source<-as.factor(trainData2$source)

testData$source<-as.character(testData$source)
testData$source[is.na(testData$source)]<-"unknown"
testData$source<-as.factor(testData$source)

#get data table of median values by city, tag type
trainData2$num_views<-as.numeric(trainData2$num_views)
trainData2$num_votes<-as.numeric(trainData2$num_votes)
trainData2$num_comments<-as.numeric(trainData2$num_comments)

trainData2$month<-month(trainData2$created_time)
trainData2$year<-year(trainData2$created_time)

#remove first 10 month 2012 data as they have higher d from 2013 Apr
#remove march 2013 as it has high d as well.
#d is like the distance of difference, smaller d = the two data sets are closer
ksTest<-ks.test(trainData2$num_views[trainData2$month==4&trainData2$year==2013],
                trainData2$num_views[trainData2$month==9&trainData2$year==2012])
trainData2<-trainData2[trainData2$month==11|trainData2$month==12|trainData2$year==2013,]
trainData2<-trainData2[trainData2$month!=3,]

#no longer need the date cols
trainData2$created_time<-NULL
trainData2$year<-NULL
trainData2$month<-NULL

#combine the remote_API tags with tag_type
trainData2<-remapAPI(trainData2)
testData2<-testData[,!(names(testData) %in% c("latitude","longitude","created_time"))]
testData2<-remapAPI(testData2)

#chicargo remote_api_created is one of the kind weird and should be modelled seperately
ksTest_city<- ks.test(trainData2$num_views[trainData2$city=="oakland"],
              trainData2$num_views[trainData2$city=="chicargo"&trainData2$source!="remote_api_created"])

ksTest_city2<-ks.test(trainData2$num_views[trainData2$city=="chicargo"],
              trainData2$num_views[trainData2$city=="chicargo"&trainData2$source!="remote_api_created"])

#richmond, new_haven have d=0 which is strange
ksTest_city3<-ks.test(trainData2$num_views[trainData2$city=="new_haven"],
        trainData2$num_views[trainData2$city=="new_haven"&trainData2$source!="remote_api_created"])

trainDataMod<-trainData2
trainDataMod<-trainDataMod[!(trainDataMod$city=="chicargo"&trainDataMod$source=="remote_api_created"),]
testDataMod<-testData2[!(testData2$city=="chicargo"&testData2$source=="remote_api_created"),]
trainDataMod<-data.table(trainDataMod)

#remove summary/desc
trainData2<-trainData2[,!(names(trainData2) %in% c("summary","description"))]
testData2<-testData2[,!(names(testData2) %in% c("summary","description"))]

trainDataModChi<-trainData2
trainDataModChi<-trainDataModChi[trainDataModChi$city=="chicargo"&trainDataModChi$source=="remote_api_created",]
testDataModChi<-testData2[testData2$city=="chicargo"&testData2$source=="remote_api_created",]
trainDataModChi<-data.table(trainDataModChi)

#remove train data more than 3 Median Absolute Deviation away from median (outliers), 
#dont do need -3*mad they will all result in -ve
trainDataMod<-madRemove(trainDataMod,3)
trainDataModChi<-madRemove(trainDataModChi,3)

#convert traindataRf back to data frame
trainDataMod<-data.frame(trainDataMod)
trainDataModChi<-data.frame(trainDataModChi)

#log view (it has large variation)
trainDataMod$num_views<-log(trainDataMod$num_views+1)

#relevel test data to top tags and source and remove those test data with non top tags& source first
testDataMod$tag_type<-factor(testDataMod$tag_type,levels=levels(trainDataMod$tag_type))
testDataMod$source<-factor(testDataMod$source,levels=levels(trainDataMod$source))
testDataMod<-testDataMod[!is.na(testDataMod$tag_type),]
testDataMod<-testDataMod[!is.na(testDataMod$source),]

#relevel test data to top tags and source and remove those test data with non top tags& source first
testDataModChi$tag_type<-factor(testDataModChi$tag_type,levels=levels(trainDataModChi$tag_type))
testDataModChi$source<-factor(testDataModChi$source,levels=levels(trainDataModChi$source))
testDataModChi<-testDataModChi[!is.na(testDataModChi$tag_type),]
testDataModChi<-testDataModChi[!is.na(testDataModChi$source),]

#reclassify text + split tags
trainDataMod<-wordMine(trainDataMod,"all")
testCol<-names(trainDataMod)
testCol<-testCol[testCol!="num_views"&testCol!="num_votes"&testCol!="num_comments"]
testDataMod<-wordMine(testDataMod,testCol)
trainDataMod<-trainDataMod[,!(names(trainDataMod) %in% c("summary","description"))]
testDataMod<-testDataMod[,!(names(testDataMod) %in% c("summary","description"))]

#save the data
save(trainDataMod,file="trainDataMod.Rdata")
save(testDataMod,file="testDataMod.Rdata")
save(trainData2,file="trainData2.Rdata")
save(testData2,file="testData2.Rdata")
save(trainDataModChi,file="trainDataModChi.Rdata")
save(testDataModChi,file="testDataModChi.Rdata")