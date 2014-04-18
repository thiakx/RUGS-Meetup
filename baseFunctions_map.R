library(maptools)
library(rgeos)
library(rgdal)
library(sp)
library(leafletR)
library(classInt)
library(spdep)
library(data.table)

#define boundry to draw hexagon
#m is for miles
baseCRS<-CRS("+proj=longlat +ellps=WGS84 +units=km")
LambertCRS<-" +proj=lcc +lat_1=60 +lat_2=30 +lon_0=-100 +units=km" 

generateHex<-function(regionPtData){
  
  region<-regionPtData[1,1]
  regionPtData<-as.matrix(regionPtData[,2:3])
  coords <- project(regionPtData,LambertCRS)
  
  #The reason for this triple conversion below is that a Polygon is a simple ring, 
  #a Polygons object can be several rings with an ID (here set to 1) (so is like a single feature in a GIS) 
  #and a SpatialPolygons can have a CRS. 
  p  <- Polygon(coords)
  ps <- Polygons(list(p),1)
  pg <- SpatialPolygons(list(ps))
  
  #Sample hexagonal points, convert to polygons
  # cellsize is in map units (somehow always defaults to miles, hence / 1.60934)
  HexPts=spsample(pg, type="hexagonal",  cellsize = 1 / 1.60934, offset=c(0,0)) 
  HexPols = HexPoints2SpatialPolygons(HexPts)
  proj4string(HexPols)<-LambertCRS
  
  #transform back into lon, lat coordinates 
  HexPols<-spTransform(HexPols,baseCRS)
  hexNumber<-data.frame(paste(region,1:length(HexPols)))
  names(hexNumber)<-"hexNumber"
  HexPols<-SpatialPolygonsDataFrame(HexPols,hexNumber,match.ID = FALSE)
  
  #define unique id for each hex
  HexPols<-spChFIDs(HexPols, as.character(HexPols$hexNumber))
  
  return(HexPols)
}

localM<-function(Hex,HexCol){
  #calculate local moran (where the H / Ls are)
  HexB <- poly2nb(Hex, queen=T)
  HexB <- nb2listw(HexB, style="W", zero.policy=TRUE)
  HexTemp<-data.frame(Hex[,HexCol])[,1]
  localM<-localmoran(HexTemp,HexB,  zero.policy=TRUE, na.action=na.exclude)

  localM<-data.frame(Hex$hexNumber,HexTemp,localM)
  names(localM)[1:2]<-c("hexNumber",HexCol)
  names(localM)[7]<-"p"
  
  #asume anything above defined percentile is considered high
  highCutOff<-round(quantile(localM[,HexCol],.5))
  
  #if p>0.05, dont bother, no significant differnce from random, default we set to none
  localM$mType<-"None"
  
  #if Ii +ve, means it is same with its surroundings, -ve, means it is opposite from its surroundings
  #just happens for this data set, all p < 0.05 no -ve
  localM$mType[localM$p<0.05 & localM[,HexCol]<=highCutOff & localM$Ii<0]<-"LH"
  localM$mType[localM$p<0.05 & localM[,HexCol]<=highCutOff & localM$Ii>0]<-"LL"
  
  localM$mType[localM$p<0.05 & localM[,HexCol]>highCutOff & localM$Ii<0]<-"HL"
  localM$mType[localM$p<0.05 & localM[,HexCol]>highCutOff & localM$Ii>0]<-"HH"

  localM<-subset(localM,select=c("hexNumber","mType"))
  Hex<-merge(Hex,localM,by="hexNumber")
  
  return(Hex)
}