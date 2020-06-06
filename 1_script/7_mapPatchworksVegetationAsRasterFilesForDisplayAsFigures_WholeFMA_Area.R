## this script is used after bird densities are calculated in cure4insect 
#for different Patchworks scenarios

#Once densities and abundances are generated for a particular species,
#match the quarter-sections in these files with the quarter-sections in xy,
#then use these files to subset from the xy spatial points data frame
library(cure4insect)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(maps)
library(maptools)
library(mapproj)
library(mapdata)
library(mefa4)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(sp)
library(tidyr)
library(viridis)
library(zoo)

## load this file from where you put it
load("0_data/raw/hf-an-xy-for patchworks.RData")
qsid <- rownames(coordinates(xy))

#No Pine20 by Year 50 at least in PFM Scenario
names<-c("Pine40",
         "Pine60",
         "Pine80",
         "Pine100",
         "Pine120",
         "Pine140",
         "WhiteSpruce20",
         "WhiteSpruce40",
         "WhiteSpruce60",
         "WhiteSpruce80",
         "WhiteSpruce100",
         "WhiteSpruce120",
         "WhiteSpruce140")
for (SPP in names){
  #CARIBOU (PFM) SCENARIO
  SCEN<-"caribou"
  
  #Year 0
  a14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a14_",SCEN,"_0.csv"), header=TRUE)
  a15<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a15_",SCEN,"_0.csv"), header=TRUE)
  l1<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l1_",SCEN,"_0.csv"), header=TRUE)
  l2<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l2_",SCEN,"_0.csv"), header=TRUE)
  l3<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l3_",SCEN,"_0.csv"), header=TRUE)
  l8<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l8_",SCEN,"_0.csv"), header=TRUE)
  l11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l11_",SCEN,"_0.csv"), header=TRUE)
  s11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s11_",SCEN,"_0.csv"), header=TRUE)
  s14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s14_",SCEN,"_0.csv"), header=TRUE)
  s18<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s18_",SCEN,"_0.csv"), header=TRUE)
  s22<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s22_",SCEN,"_0.csv"), header=TRUE)
  s23<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s23_",SCEN,"_0.csv"), header=TRUE)
  
  fma.0C<-bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.0C)#89 variables, X (column 1) being QSID
  #columns 3-89 being areas of different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.0C$Area<-rowSums(fma.0C[,3:89], na.rm=TRUE)
  fma.0C$Habitat<-ifelse(is.na(fma.0C[,SPP]),0,fma.0C[,SPP])
  fma.0C$PercOfHabitat<-fma.0C$Habitat/fma.0C$Area*100
  
  str(xy)#look at coordinates and quarter-section names in master file
  xy@coords#stores quarter section, longitude, latitude
  str(xy@coords)
  xy@coords[,1]#quarter-section and longitude
  xy@coords[,2]#quarter-section and latitude
  xy@data#quarter-section MER, RGE, TWP, SEC, QS, TWNSHIP, SECTION, Area_km2
  xy@data$QSID<-paste0(xy@data$MER,"-",xy@data$RGE,"-",xy@data$TWP,"-",xy@data$SEC,"-",xy@data$QS)
  #create the column that will be used to match records in xy to the placeholder
  xy@coords.nrs
  xy@bbox#bounding box (min & max POINT_X and POINT_Y)
  xy@proj4string  #CRS arguments:+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
  
  
  ###NOW SUBSET THE SPATIAL POINTS DATA FRAME AND ADD NEW VARIABLES TO ITS
  ###ATTRIBUTE TABLE
  xyreduced<-subset(xy[xy@data$QSID %in% fma.0C$QSLinkID, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0C$X, ])  
  
  nrow(xyreduced)#93177 obs instead of 1008568
  nrow(fma.0C)#93399 obs
  nrow(fma.0C[!fma.0C$QSLinkID %in% xyreduced@data$QSID,])#2
  fma.0C.red<-fma.0C[,c("QSLinkID","PercOfHabitat")]
  nrow(fma.0C.red)#93399 obs (why are there more than in xyreduced)
  fma.0C.red$dupl<-duplicated(fma.0C.red$QSLinkID)
  nrow(fma.0C.red[fma.0C.red$dupl==TRUE,])#221 duplicates
  
  #There are 221 fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are 221 duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those 221 quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.0C.red.u<-fma.0C.red[!fma.0C.red$dupl==TRUE,]
  nrow(fma.0C.red.u)#93178
  nrow(fma.0C.red.u[!fma.0C.red.u$QSLinkID %in% xyreduced@data$QSID,])#1
  fma.0C.red.u<-fma.0C.red.u[fma.0C.red.u$QSLinkID %in% xyreduced@data$QSID,]
  nrow(fma.0C.red.u[!fma.0C.red.u$QSLinkID %in% xyreduced@data$QSID,])#0
  xyreduced@data$Abund<-fma.0C.red.u$PercOfHabitat
  
  #Now create the Year 0 raster from this information (lat, long, abund)
  #a. Read it into R as a data frame, with columns x, y, yield
  pts.0C<-data.frame(x=data.frame(xyreduced@coords)$POINT_X,
                     y=data.frame(xyreduced@coords)$POINT_Y,
                     abund=xyreduced@data$Abund)
  #b. Convert the data frame to a SpatialPointsDataFrame using the sp package and something like:
  coordinates(pts.0C)=~x+y
  #c. Convert to your regular km system by first telling it what CRS it is, and then spTransform to the destination.
  proj4string(pts.0C)=CRS("+init=epsg:4326") # set it to lat-long
  pts.0C = spTransform(pts.0C, CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))#
  #d. Tell R that this is gridded:
  #gridded(pts.0C) = TRUE
  #Error message indicating that points are not on a proper grid
  
  #e. Now use the raster package to convert to a raster and set its CRS
  r.0C = raster()#default
  extent(r.0C) = extent(pts.0C)
  projection(r.0C) = CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  res(r.0C)<-1000#set resolution to 1 sq. km
  
  pts_ras.0C<- rasterize(pts.0C, r.0C, fun=mean)
  plot(pts_ras.0C$abund)#in basic R
  
  levelplot(pts_ras.0C$abund, margin=F)#in rasterVis
  
  #Year 50
  a14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a14_",SCEN,"_5.csv"), header=TRUE)
  a15<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a15_",SCEN,"_5.csv"), header=TRUE)
  l1<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l1_",SCEN,"_5.csv"), header=TRUE)
  l2<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l2_",SCEN,"_5.csv"), header=TRUE)
  l3<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l3_",SCEN,"_5.csv"), header=TRUE)
  l8<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l8_",SCEN,"_5.csv"), header=TRUE)
  l11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l11_",SCEN,"_5.csv"), header=TRUE)
  s11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s11_",SCEN,"_5.csv"), header=TRUE)
  s14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s14_",SCEN,"_5.csv"), header=TRUE)
  s18<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s18_",SCEN,"_5.csv"), header=TRUE)
  s22<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s22_",SCEN,"_5.csv"), header=TRUE)
  s23<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s23_",SCEN,"_5.csv"), header=TRUE)
  
  fma.50C<-bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.50C)#72 variables, X (column 1) being QSID
  #columns 3-89 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.50C$Area<-rowSums(fma.50C[,3:89], na.rm=TRUE)
  fma.50C$Habitat<-ifelse(is.na(fma.50C[,SPP]),0,fma.50C[,SPP])
  fma.50C$PercOfHabitat<-fma.50C$Habitat/fma.50C$Area*100
  
  
  str(xy)#look at coordinates and quarter-section names in master file
  xy@coords#stores quarter section, longitude, latitude
  str(xy@coords)
  xy@coords[,1]#quarter-section and longitude
  xy@coords[,2]#quarter-section and latitude
  xy@data#quarter-section MER, RGE, TWP, SEC, QS, TWNSHIP, SECTION, Area_km2
  xy@data$QSID<-paste0(xy@data$MER,"-",xy@data$RGE,"-",xy@data$TWP,"-",xy@data$SEC,"-",xy@data$QS)
  #create the column that will be used to match records in xy to the placeholder
  xy@coords.nrs
  xy@bbox#bounding box (min & max POINT_X and POINT_Y)
  xy@proj4string  #CRS arguments:+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
  
  
  ###NOW SUBSET THE SPATIAL POINTS DATA FRAME AND ADD NEW VARIABLES TO ITS
  ###ATTRIBUTE TABLE
  xyreduced<-subset(xy[xy@data$QSID %in% fma.50C$QSLinkID, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0C$QSLinkID, ])  
  
  nrow(xyreduced)#93176 obs instead of 1008568
  nrow(fma.50C)#93398 obs
  nrow(fma.50C[!fma.50C$QSLinkID %in% xyreduced@data$QSID,])#2
  fma.50C.red<-fma.50C[,c("QSLinkID","PercOfHabitat")]
  nrow(fma.50C.red)#93398 obs (why are there more than in xyreduced)
  fma.50C.red$dupl<-duplicated(fma.50C.red$QSLinkID)
  nrow(fma.50C.red[fma.50C.red$dupl==TRUE,])#221 duplicates
  
  #There are 221 fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are 221 duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those 221 quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.50C.red.u<-fma.50C.red[!fma.50C.red$dupl==TRUE,]
  nrow(fma.50C.red.u[!fma.50C.red.u$QSLinkID %in% xyreduced@data$QSID,])#1
  fma.50C.red.u<-fma.50C.red.u[fma.50C.red.u$QSLinkID %in% xyreduced@data$QSID,]
  nrow(fma.50C.red.u[!fma.50C.red.u$QSLinkID %in% xyreduced@data$QSID,])#0
  xyreduced@data$Abund<-fma.50C.red.u$PercOfHabitat
  
  #Now create the Year 50 raster from this information (lat, long, abund)
  #a. Read it into R as a data frame, with columns x, y, yield
  pts.50C<-data.frame(x=data.frame(xyreduced@coords)$POINT_X,
                      y=data.frame(xyreduced@coords)$POINT_Y,
                      abund=xyreduced@data$Abund)
  #b. Convert the data frame to a SpatialPointsDataFrame using the sp package and something like:
  coordinates(pts.50C)=~x+y
  #c. Convert to your regular km system by first telling it what CRS it is, and then spTransform to the destination.
  proj4string(pts.50C)=CRS("+init=epsg:4326") # set it to lat-long
  pts.50C = spTransform(pts.50C, CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))#
  #d. Tell R that this is gridded:
  #gridded(pts.50C) = TRUE
  #Error message indicating that points are not on a proper grid
  
  #e. Now use the raster package to convert to a raster and set its CRS
  r.50C = raster()#default
  extent(r.50C) = extent(pts.50C)
  projection(r.50C) = CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  res(r.50C)<-1000#set resolution to 1 sq. km
  
  pts_ras.50C<- rasterize(pts.50C, r.50C, fun=mean)
  plot(pts_ras.50C$abund)#in basic R
  
  #levelplot(pts_ras.50C$abund, margin=F)#in rasterVis
  
  #EBM/NDM SCENARIO
  SCEN2<-"ebm"
  
  #Year 0
  a14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a14_",SCEN2,"_0.csv"), header=TRUE)
  a15<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a15_",SCEN2,"_0.csv"), header=TRUE)
  l1<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l1_",SCEN2,"_0.csv"), header=TRUE)
  l2<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l2_",SCEN2,"_0.csv"), header=TRUE)
  l3<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l3_",SCEN2,"_0.csv"), header=TRUE)
  l8<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l8_",SCEN2,"_0.csv"), header=TRUE)
  l11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l11_",SCEN2,"_0.csv"), header=TRUE)
  s11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s11_",SCEN2,"_0.csv"), header=TRUE)
  s14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s14_",SCEN2,"_0.csv"), header=TRUE)
  s18<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s18_",SCEN2,"_0.csv"), header=TRUE)
  s22<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s22_",SCEN2,"_0.csv"), header=TRUE)
  s23<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s23_",SCEN2,"_0.csv"), header=TRUE)
  
  fma.0E<-bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.0E)#89 variables, X (column 1) being QSID
  #columns 3-89 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.0E$Area<-rowSums(fma.0E[,3:89], na.rm=TRUE)
  fma.0E$Habitat<-ifelse(is.na(fma.0E[,SPP]),0,fma.0E[,SPP])
  fma.0E$PercOfHabitat<-fma.0E$Habitat/fma.0E$Area*100
  
  str(xy)#look at coordinates and quarter-section names in master file
  xy@coords#stores quarter section, longitude, latitude
  str(xy@coords)
  xy@coords[,1]#quarter-section and longitude
  xy@coords[,2]#quarter-section and latitude
  xy@data#quarter-section MER, RGE, TWP, SEC, QS, TWNSHIP, SECTION, Area_km2
  xy@data$QSID<-paste0(xy@data$MER,"-",xy@data$RGE,"-",xy@data$TWP,"-",xy@data$SEC,"-",xy@data$QS)
  #create the column that will be used to match records in xy to the placeholder
  xy@coords.nrs
  xy@bbox#bounding box (min & max POINT_X and POINT_Y)
  xy@proj4string  #CRS arguments:+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
  
  
  ###NOW SUBSET THE SPATIAL POINTS DATA FRAME AND ADD NEW VARIABLES TO ITS
  ###ATTRIBUTE TABLE
  xyreduced<-subset(xy[xy@data$QSID %in% fma.0E$QSLinkID, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0E$QSLinkID, ])  
  
  nrow(xyreduced)#93177 obs instead of 1008568
  nrow(fma.0E)#93399 obs
  nrow(fma.0E[!fma.0E$QSLinkID %in% xyreduced@data$QSID,])#0
  fma.0E.red<-fma.0E[,c("QSLinkID","PercOfHabitat")]
  nrow(fma.0E.red)#93399 obs (why are there more than in xyreduced)
  fma.0E.red$dupl<-duplicated(fma.0E.red$QSLinkID)
  nrow(fma.0E.red[fma.0E.red$dupl==TRUE,])#221 duplicates
  
  #There are 221 fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are 221 duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those 221 quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.0E.red.u<-fma.0E.red[!fma.0E.red$dupl==TRUE,]
  nrow(fma.0E.red.u)#93178
  nrow(fma.0E.red.u[!fma.0E.red.u$QSLinkID %in% xyreduced@data$QSID,])#1
  fma.0E.red.u<-fma.0E.red.u[fma.0E.red.u$QSLinkID %in% xyreduced@data$QSID,]
  nrow(fma.0E.red.u[!fma.0E.red.u$QSLinkID %in% xyreduced@data$QSID,])#0
  xyreduced@data$Abund<-fma.0E.red.u$PercOfHabitat
  
  
  #Now create the Year 0 raster from this information (lat, long, abund)
  #a. Read it into R as a data frame, with columns x, y, yield
  pts.0E<-data.frame(x=data.frame(xyreduced@coords)$POINT_X,
                     y=data.frame(xyreduced@coords)$POINT_Y,
                     abund=xyreduced@data$Abund)
  #b. Convert the data frame to a SpatialPointsDataFrame using the sp package and something like:
  coordinates(pts.0E)=~x+y
  #c. Convert to your regular km system by first telling it what CRS it is, and then spTransform to the destination.
  proj4string(pts.0E)=CRS("+init=epsg:4326") # set it to lat-long
  pts.0E = spTransform(pts.0E, CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))#
  #d. Tell R that this is gridded:
  #gridded(pts.0E) = TRUE
  #Error message indicating that points are not on a proper grid
  
  #e. Now use the raster package to convert to a raster and set its CRS
  r.0E = raster()#default
  extent(r.0E) = extent(pts.0E)
  projection(r.0E) = CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  res(r.0E)<-1000#set resolution to 1 sq. km
  
  pts_ras.0E<- rasterize(pts.0E, r.0E, fun=mean)
  plot(pts_ras.0E$abund)#in basic R
  
  #levelplot(pts_ras.0E$abund, margin=F)#in rasterVis
  
  #Year 50
  a14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a14_",SCEN2,"_5.csv"), header=TRUE)
  a15<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_a15_",SCEN2,"_5.csv"), header=TRUE)
  l1<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l1_",SCEN2,"_5.csv"), header=TRUE)
  l2<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l2_",SCEN2,"_5.csv"), header=TRUE)
  l3<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l3_",SCEN2,"_5.csv"), header=TRUE)
  l8<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l8_",SCEN2,"_5.csv"), header=TRUE)
  l11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_l11_",SCEN2,"_5.csv"), header=TRUE)
  s11<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s11_",SCEN2,"_5.csv"), header=TRUE)
  s14<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s14_",SCEN2,"_5.csv"), header=TRUE)
  s18<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s18_",SCEN2,"_5.csv"), header=TRUE)
  s22<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s22_",SCEN2,"_5.csv"), header=TRUE)
  s23<-read.csv(paste0("0_data/raw/patchworks/bothhabitatHF_s23_",SCEN2,"_5.csv"), header=TRUE)
  
  fma.50E<-bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.50E)#89 variables, X (column 1) being QSID
  #columns 3-89 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.50E$Area<-rowSums(fma.50E[,3:89], na.rm=TRUE)
  fma.50E$Habitat<-ifelse(is.na(fma.50E[,SPP]),0,fma.50E[,SPP])
  fma.50E$PercOfHabitat<-fma.50E$Habitat/fma.50E$Area*100
  
  
  str(xy)#look at coordinates and quarter-section names in master file
  xy@coords#stores quarter section, longitude, latitude
  str(xy@coords)
  xy@coords[,1]#quarter-section and longitude
  xy@coords[,2]#quarter-section and latitude
  xy@data#quarter-section MER, RGE, TWP, SEC, QS, TWNSHIP, SECTION, Area_km2
  xy@data$QSID<-paste0(xy@data$MER,"-",xy@data$RGE,"-",xy@data$TWP,"-",xy@data$SEC,"-",xy@data$QS)
  #create the column that will be used to match records in xy to the placeholder
  xy@coords.nrs
  xy@bbox#bounding box (min & max POINT_X and POINT_Y)
  xy@proj4string  #CRS arguments:+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
  
  
  ###NOW SUBSET THE SPATIAL POINTS DATA FRAME AND ADD NEW VARIABLES TO ITS
  ###ATTRIBUTE TABLE
  xyreduced<-subset(xy[xy@data$QSID %in% fma.50E$QSLinkID, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0E$QSLinkID, ])  
  
  nrow(xyreduced)#93176 obs instead of 1008568
  nrow(fma.50E)#93398 obs
  nrow(fma.50E[!fma.50E$QSLinkID %in% xyreduced@data$QSID,])#0
  fma.50E.red<-fma.50E[,c("QSLinkID","PercOfHabitat")]
  nrow(fma.50E.red)#93397 obs (why are there more than in xyreduced)
  fma.50E.red$dupl<-duplicated(fma.50E.red$QSLinkID)
  nrow(fma.50E.red[fma.50E.red$dupl==TRUE,])#221 duplicates
  
  #There are 221 fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are 221 duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those 221 quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.50E.red.u<-fma.50E.red[!fma.50E.red$dupl==TRUE,]
  nrow(fma.50E.red.u)#93177
  nrow(fma.50E.red.u[!fma.50E.red.u$QSLinkID %in% xyreduced@data$QSID,])#1
  fma.50E.red.u<-fma.50E.red.u[fma.50E.red.u$QSLinkID %in% xyreduced@data$QSID,]
  nrow(fma.50E.red.u[!fma.50E.red.u$QSLinkID %in% xyreduced@data$QSID,])#0
  xyreduced@data$Abund<-fma.50E.red.u$PercOfHabitat
  
  
  #Now create the Year 50 raster from this information (lat, long, abund)
  #a. Read it into R as a data frame, with columns x, y, yield
  pts.50E<-data.frame(x=data.frame(xyreduced@coords)$POINT_X,
                      y=data.frame(xyreduced@coords)$POINT_Y,
                      abund=xyreduced@data$Abund)
  #b. Convert the data frame to a SpatialPointsDataFrame using the sp package and something like:
  coordinates(pts.50E)=~x+y
  #c. Convert to your regular km system by first telling it what CRS it is, and then spTransform to the destination.
  proj4string(pts.50E)=CRS("+init=epsg:4326") # set it to lat-long
  pts.50E = spTransform(pts.50E, CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))#
  #d. Tell R that this is gridded:
  #gridded(pts.50E) = TRUE
  #Error message indicating that points are not on a proper grid
  
  #e. Now use the raster package to convert to a raster and set its CRS
  r.50E = raster()#default
  extent(r.50E) = extent(pts.50E)
  projection(r.50E) = CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  res(r.50E)<-1000#set resolution to 1 sq. km
  
  pts_ras.50E<- rasterize(pts.50E, r.50E, fun=mean)
  plot(pts_ras.50E$abund)#in basic R
  
  #levelplot(pts_ras.50E$abund, margin=F)#in rasterVis
  
  miat=c(-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90,100) 
  
  ## Modify the palette with your colors
  divPal <- brewer.pal(n=9, 'PuOr')
  divPal[5] <- "#FFFFFF"
  divTheme <- rasterTheme(region=divPal)
  
  #Year 50 - Year 0 - Caribou Scenario
  caribouplot<-levelplot(pts_ras.50C$abund-pts_ras.0C$abund, margin=F,
                         par.settings = rasterTheme(region=divPal, axis.line = list(col = "transparent"), 
                                                    strip.background = list(col = 'transparent'), 
                                                    strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                         main=list(label='PFM (Caribou) Scenario: Year 50 - Year 0',cex=1), at=miat)#in rasterVis
  
  
  #Year 50 - Year 0 - EBM Scenario
  ebmplot<-levelplot(pts_ras.50E$abund-pts_ras.0E$abund, margin=F,
                     par.settings = rasterTheme(region=divPal, axis.line = list(col = "transparent"), 
                                                strip.background = list(col = 'transparent'), 
                                                strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                     main=list(label='EBM (Ecosytem-Based Mgmt. Scenario: Year 50 - Year 0',cex=1), at=miat)#in rasterVis
  
  
  
  #options for multi-panel plotting
  lattice.options(
    layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
    layout.widths=list(left.padding=list(x=0), right.padding=list(x=0.5))
  )
  
  tg<-textGrob(paste0("Change in ",SPP," Density"),gp=gpar(fontsize=20,font=1))
  margin <- unit(0.5, "line")
  grided<-grid.arrange(caribouplot, ebmplot, ncol=2)
  
  tiff(paste0("3_output/maps/whole fma/veg/",SPP,"PatchworksDensityMaps.tiff"), units="in", width=15, height=9, res=300)
  
  grid.arrange(tg, grided,
               heights = unit.c(grobHeight(tg) + 1.2*margin, 
                                unit(1,"null")))
  dev.off()
}

