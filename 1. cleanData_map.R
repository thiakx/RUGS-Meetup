#setwd("myDirectory")
source("../baseFunctions.R")
trainData<-read.csv("train.csv")

#classify by cities based on lat long
trainData$longitudeCut<-trunc(trainData$longitude)
trainData$city<-""
trainData$city[trainData$longitudeCut=="-77"]<-"richmond"
trainData$city[trainData$longitudeCut=="-72"]<-"new_haven"
trainData$city[trainData$longitudeCut=="-87"]<-"chicargo"
trainData$city[trainData$longitudeCut=="-122"]<-"oakland"
trainData$city<-as.factor(trainData$city)

coltoKeep<-c("latitude","longitude","num_votes","num_comments","num_views","source","tag_type","city")
trainData<-trainData[,coltoKeep]

trainData_map<-as.matrix(trainData[,c("longitude","latitude")])
trainData_map<-SpatialPointsDataFrame(trainData_map,trainData[,c("num_votes","num_comments","num_views","source","tag_type","city")]
                          ,proj4string = baseCRS)

# #write shp file
# writeSpatialShape(trainData_map,"trainData_map")

#generate hexagonal grid by region
regionPt<-read.csv("regionPoints.csv")

oakland_PtData<-generateHex(regionPt[regionPt$region=="oakland",])
new_haven_PtData<-generateHex(regionPt[regionPt$region=="new_haven",])
richmond_PtData<-generateHex(regionPt[regionPt$region=="richmond",])
chicargo_PtData<-generateHex(regionPt[regionPt$region=="chicargo",])

Hex<-rbind(oakland_PtData,new_haven_PtData,richmond_PtData,chicargo_PtData)

# #write shp file
# writeSpatialShape(Hex,"baseHexagon")

#point in polygon analysis, count number of trainData pts in each hex
# remove the polygons with no data pts in them

HexPt<-over(trainData_map,Hex)
DataTemp<-data.frame(trainData_map,HexPt)
DataTemp<-data.table(DataTemp)
HexPt<-DataTemp[,list(count=length(num_votes),avgVote=round(mean(num_votes),2),
                      avgComment=round(mean(num_comments),2),avgView=round(mean(num_views),2)),by=hexNumber]

Hex<-merge(Hex,HexPt,by="hexNumber",all.x=FALSE)

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
unlink("scf",recursive = TRUE)
q.map <- leaflet(data="Hex.geojson", title="scf", center=c(37.8044, -122.2708), zoom=12,
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

#output to geojson, delete file if exists
unlink("Hex_localM.geojson")
writeOGR(Hex_localM,"Hex_localM.geojson","Hex_localM", driver="GeoJSON")

q.styleLocalM <- styleCat(prop="LM_avgView", c("None","LH","LL","HL","HH"),
                          style.val=c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c"),  
                          leg="Local Morans I", 
                          fill.alpha=0.7, rad=8)

# create map
unlink("scf_localM",recursive = TRUE)
q.map <- leaflet(data="Hex_localM.geojson", title="scf localM", center=c(37.8044, -122.2708), zoom=12,
                  base.map="osm", style=q.styleLocalM, popup="*")