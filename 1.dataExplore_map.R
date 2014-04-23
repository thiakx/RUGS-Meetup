#I will setwd("/MY_Working_Directory/data") where the kaggle train/test data are placed
source("../baseFunctions_map.R")
trainData<-read.csv("train.csv")
testData<-read.csv("test.csv")

#load in the boundry box for each city
regionPt<-read.csv("../leafletMaps/regionPoints.csv")

#classify by cities based on lat long
trainData$longitudeCut<-trunc(trainData$longitude)
trainData$city<-""
trainData$city[trainData$longitudeCut=="-77"]<-"richmond"
trainData$city[trainData$longitudeCut=="-72"]<-"new_haven"
trainData$city[trainData$longitudeCut=="-87"]<-"chicargo"
trainData$city[trainData$longitudeCut=="-122"]<-"oakland"
trainData$city<-as.factor(trainData$city)
trainData$longitudeCut<-NULL

#classify by cities based on lat long
testData$longitudeCut<-trunc(testData$longitude)
testData$city<-""
testData$city[testData$longitudeCut=="-77"]<-"richmond"
testData$city[testData$longitudeCut=="-72"]<-"new_haven"
testData$city[testData$longitudeCut=="-87"]<-"chicargo"
testData$city[testData$longitudeCut=="-122"]<-"oakland"
testData$city<-as.factor(testData$city)
testData$longitudeCut<-NULL

trainData_map<-as.matrix(trainData[,c("longitude","latitude")])
trainData_map<-SpatialPointsDataFrame(trainData_map,trainData,proj4string = baseCRS)

testData_map<-as.matrix(testData[,c("longitude","latitude")])
testData_map<-SpatialPointsDataFrame(testData_map,testData,proj4string = baseCRS)

# #write shp file
# writeSpatialShape(trainData_map,"trainData_map")

oakland_PtData<-generateHex(regionPt[regionPt$region=="oakland",])
new_haven_PtData<-generateHex(regionPt[regionPt$region=="new_haven",])
richmond_PtData<-generateHex(regionPt[regionPt$region=="richmond",])
chicargo_PtData<-generateHex(regionPt[regionPt$region=="chicargo",])

baseHex<-rbind(oakland_PtData,new_haven_PtData,richmond_PtData,chicargo_PtData)

# #write shp file
# writeSpatialShape(baseHex,"baseHexagon")

#point in polygon analysis, count number of trainData pts in each hex
# remove the polygons with no data pts in them
HexPt<-over(trainData_map,baseHex)
DataTemp<-data.frame(trainData_map,HexPt)
DataTemp$longitude.1<-NULL
DataTemp$latitude.1<-NULL

#add hexNumber to traindata
train_wHex<-DataTemp

#only keep columns that's useful for mapping
coltoKeep<-c("latitude","longitude","num_votes","num_comments","num_views","source","tag_type","city","hexNumber")
DataTemp<-DataTemp[,coltoKeep]
DataTemp<-data.table(DataTemp)
HexPt<-DataTemp[,list(count=length(num_votes),avgVote=round(mean(num_votes),2),
                      avgComment=round(mean(num_comments),2),avgView=round(mean(num_views),2)),by=hexNumber]
Hex<-merge(baseHex,HexPt,by="hexNumber",all.x=FALSE)

#add hexNumber to testdata
HexPt<-over(testData_map,baseHex)
DataTemp<-data.frame(testData_map,HexPt)
DataTemp$longitude.1<-NULL
DataTemp$latitude.1<-NULL
test_wHex<-DataTemp

# #write shp file
# writeSpatialShape(Hex,"Hex")

#output to geojson for leafletR function below, delete file if exists
unlink("Hex.geojson")
writeOGR(Hex,"Hex.geojson","Hex", driver="GeoJSON")

#fisher natural break. +1 to last cutoff so that the max value will be colored.
#colorblind friendly colors from http://colorbrewer2.org/
hexVar="avgView"
n = 5
breakInt<-round(classIntervals(data.frame(Hex[,hexVar])[,1], n, style = "fisher")$brks)
breakInt[n+1]<-breakInt[n+1]+1
q.style <- styleGrad(prop=hexVar, breaks=breakInt, 
                     style.val=c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c"), 
                     leg="Average View", 
                     fill.alpha=0.7, rad=8)

# create map
unlink("../leafletMaps/scf",recursive = TRUE)
q.map <- leaflet(data="Hex.geojson", dest="../leafletMaps",title="scf", center=c(37.8044, -122.2708), zoom=12,
                 base.map="osm", style=q.style, popup="*")

#localMoran's I (beta)
#localM function defined in baseFunction.R
HexCount<-localM(Hex,"count")
HexView<-localM(Hex,"avgView")
HexComment<-localM(Hex,"avgComment")
HexVote<-localM(Hex,"avgVote")

#merge the localM values
HexTemp2<-data.frame(HexCount$hexNumber,HexCount$mType,HexView$mType,HexComment$mType,HexVote$mType)
names(HexTemp2)<-c("hexNumber","LM_count","LM_avgView","LM_avgComment","LM_avgVote")

Hex_localM<-merge(Hex,HexTemp2,by="hexNumber")

train_wHex<-merge(train_wHex,Hex_localM,by="hexNumber", all.x=TRUE)
test_wHex<-merge(test_wHex,Hex_localM,by="hexNumber", all.x=TRUE)

#write new train and hex data
write.csv(train_wHex, file = "train_wHex.csv", row.names = FALSE)
write.csv(test_wHex, file = "test_wHex.csv", row.names = FALSE)

#output to geojson, delete file if exists
unlink("Hex_localM.geojson")
writeOGR(Hex_localM,"Hex_localM.geojson","Hex_localM", driver="GeoJSON")

q.styleLocalM <- styleCat(prop="LM_avgView", c("None","LH","LL","HL","HH"),
                          style.val=c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c"),  
                          leg="Local Morans I", 
                          fill.alpha=0.7, rad=8)

# create map
unlink("../leafletMaps/scfLocalM",recursive = TRUE)
q.map <- leaflet(data="Hex_localM.geojson", dest="../leafletMaps",title="scfLocalM", center=c(37.8044, -122.2708), zoom=12,
                 base.map="osm", style=q.styleLocalM, popup="*")