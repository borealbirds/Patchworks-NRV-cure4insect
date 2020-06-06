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

names<-c("AmericanRobin",
         "AmericanRedstart",
         "BlackcappedChickadee",
         "BlueheadedVireo",
         "CedarWaxwing",
         "ChippingSparrow",
         "HairyWoodpecker",
         "LincolnsSparrow",
         "MagnoliaWarbler",
         "PhiladelphiaVireo",
         "PineSiskin",
         "PurpleFinch",
         "RedbreastedNuthatch",
         "RedeyedVireo",
         "RubycrownedKinglet",
         "TennesseeWarbler",
         "TreeSwallow",
         "WarblingVireo",
         "WinterWren",
         "YellowbelliedFlycatcher",
         "YellowrumpedWarbler",
         "YellowWarbler",
         "AlderFlycatcher",
         "AmericanThreetoedWoodpecker",
         "BaybreastedWarbler",
         "BarnSwallow",
         "BaltimoreOriole",
         "BlackpollWarbler",
         "BlackbackedWoodpecker",
         "BlackthroatedGreenWarbler",
         "BlackAndWhiteWarbler",
         "BorealChickadee",
         "BrownCreeper",
         "CanadaWarbler",
         "CapeMayWarbler",
         "ClaycoloredSparrow",
         "CommonYellowthroat",
         "ConnecticutWarbler",
         "DarkeyedJunco",
         "EveningGrosbeak",
         "EasternPhoebe",
         "GoldencrownedKinglet",
         "GrayJay",
         "HermitThrush",
         "LeastFlycatcher",
         "LeContesSparrow",
         "MourningWarbler",
         "NorthernFlicker",
         "OlivesidedFlycatcher",
         "Ovenbird",
         "PalmWarbler",
         "PileatedWoodpecker",
         "RosebreastedGrosbeak",
         "RustyBlackbird",
         "SolitarySandpiper",
         "SwainsonsThrush",
         "WesternTanager",
         "WesternWoodPewee",
         "WhitewingedCrossbill",
         "WilsonsSnipe",
         "WilsonsWarbler",
         "WhitethroatedSparrow",
         "YellowbelliedSapsucker")

for (SPP in names){
  #CARIBOU (PFM) SCENARIO
  SCEN<-"caribou"
  
  #Year 0
  a14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_",SCEN,"_0.csv"), header=TRUE)
  # a15<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_",SCEN,"_0.csv"), header=TRUE)
  # l1<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_",SCEN,"_0.csv"), header=TRUE)
  # l2<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_",SCEN,"_0.csv"), header=TRUE)
  # l3<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_",SCEN,"_0.csv"), header=TRUE)
  # l8<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_",SCEN,"_0.csv"), header=TRUE)
  # l11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_",SCEN,"_0.csv"), header=TRUE)
  # s11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_",SCEN,"_0.csv"), header=TRUE)
  # s14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_",SCEN,"_0.csv"), header=TRUE)
  # s18<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_",SCEN,"_0.csv"), header=TRUE)
  # s22<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_",SCEN,"_0.csv"), header=TRUE)
  # s23<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_",SCEN,"_0.csv"), header=TRUE)
  
  fma.0C<-a14#bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.0C)#67 variables, X (column 1) being QSID
  #columns 2-67 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.0C$Abund<-rowSums(fma.0C[,2:67], na.rm=TRUE)
  max(fma.0C$Abund)
  
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
  xyreduced<-subset(xy[xy@data$QSID %in% fma.0C$X, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0C$X, ])  
  
  nrow(xyreduced)#93177 obs instead of 1008568
  nrow(fma.0C)#93397 obs
  nrow(fma.0C[!fma.0C$X %in% xyreduced@data$QSID,])#0
  fma.0C.red<-fma.0C[,c("X","Abund")]
  nrow(fma.0C.red)#93397 obs (why are there more than in xyreduced)
  fma.0C.red$dupl<-duplicated(fma.0C.red$X)
  nrow(fma.0C.red[fma.0C.red$dupl==TRUE,])#N duplicates
  
  #There are N fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are 220 duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those 220 quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.0C.red.u<-fma.0C.red[!fma.0C.red$dupl==TRUE,]
  nrow(fma.0C.red.u)#93177
  xyreduced@data$Abund<-fma.0C.red.u$Abund
  
  #Now create the Year 0 raster from this information (lat, long, abund)
  #a. Read it into R as a data frame, with columns x, y, yield
  pts.0C<-data.frame(x=data.frame(xyreduced@coords)$POINT_X,
                     y=data.frame(xyreduced@coords)$POINT_Y,
                     abund=xyreduced@data$Abund)
  #b. Convert the data frame to a SpatialPointsDataFrame using the sp package and something like:
  coordinates(pts.0C)=~x+y
  #c. Convert to your regular km system by first telling it what CRS it is, and then spTransform to the destination.
  proj4string(pts.0C)=CRS("+init=epsg:4326") # set it to AEP Forest 10TM
  pts.0C = spTransform(pts.0C, CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))#
  #d. Tell R that this is gridded:
  #gridded(pts.0C) = TRUE
  #Error message indicating that points are not on a proper grid
  
  #e. Now use the raster package to convert to a raster and set its CRS
  r.0C = raster()#default resolution 1x1 degrees
  extent(r.0C) = extent(pts.0C)
  projection(r.0C) = CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  res(r.0C)<-1000#set resolution to 1000 m resolution after setting extent and projecting
  
  #pts_ras.0C <- shp2raster(shp = pts.0C,
  #                        mask.raster = rastermask, label = "abund", transform = FALSE, value = pts.0C@data$abund)
  
  pts_ras.0C<- rasterize(pts.0C, r.0C, fun=mean)
  try(plot(pts_ras.0C$abund))#in basic R
  
  print(paste0("PFM Year 0 map generated for ", SPP))
  
  levelplot(pts_ras.0C$abund, margin=F)#in rasterVis
  
  
  
  #library(gstat) # Use gstat's idw routine
  #library(sp)    # Used for the spsample function
  
  # Create an empty grid where n is the total number of cells
  #grd              <- as.data.frame(spsample(pts.0C, "regular", n=16518))
  #names(grd)       <- c("x", "y")
  #coordinates(grd) <- c("x", "y")
  #gridded(grd)     <- TRUE  # Create SpatialPixel object
  #fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  #proj4string(grd) <- proj4string(pts.0C)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  #P.idw <- gstat::idw(abund ~ 1, pts.0C, newdata=grd, idp=2.0)
  
  # Convert to raster object then clip to Texas
  #r       <- raster(P.idw)
  #r.m     <- mask(r, pts.0C)#Need a mask of the Al-Pac FMA
  
  # Plot
  # tm_shape(r.m) + 
  #   tm_raster(n=10, palette = "RdBu",
  #             title="Predicted abundance \n(per 200-m cell)") + 
  #   tm_shape(pts.0C) + tm_dots(size=.002) +
  #   tm_legend(legend.outside=TRUE)
  # 
  # Plot
  # tm_shape(r.m) + 
  #   tm_raster(palette = colorRampPalette(c("blue", "red"))(5),
  #             title="Predicted abundance \n(per 200-m cell)") + 
  #   tm_shape(pts.0C) + tm_dots(size=.002) +
  #   tm_legend(legend.outside=TRUE)
  # 
  # tm_shape(r.m) +
  #   tm_borders(alpha=0.5) +
  #   tm_fill("X2013", breaks = breaks, alpha=0, 
  #           palette=colorRampPalette(c("white", "red"))(5), 
  #           popup.vars = c("name","X2000","X2013") ) + 
  #   tm_scale_bar()
  
  #Year 50
  a14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_",SCEN,"_5.csv"), header=TRUE)
  # a15<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_",SCEN,"_5.csv"), header=TRUE)
  # l1<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_",SCEN,"_5.csv"), header=TRUE)
  # l2<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_",SCEN,"_5.csv"), header=TRUE)
  # l3<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_",SCEN,"_5.csv"), header=TRUE)
  # l8<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_",SCEN,"_5.csv"), header=TRUE)
  # l11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_",SCEN,"_5.csv"), header=TRUE)
  # s11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_",SCEN,"_5.csv"), header=TRUE)
  # s14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_",SCEN,"_5.csv"), header=TRUE)
  # s18<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_",SCEN,"_5.csv"), header=TRUE)
  # s22<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_",SCEN,"_5.csv"), header=TRUE)
  # s23<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_",SCEN,"_5.csv"), header=TRUE)
  
  fma.50C<-a14#bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.50C)#60 variables, X (column 1) being QSID
  #columns 2-60 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.50C$Abund<-rowSums(fma.50C[,2:60], na.rm=TRUE)
  max(fma.50C$Abund)
  
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
  xyreduced<-subset(xy[xy@data$QSID %in% fma.50C$X, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0C$X, ])  
  
  nrow(xyreduced)#93176 obs instead of 1008568
  nrow(fma.50C)#93396 obs
  nrow(fma.50C[!fma.50C$X %in% xyreduced@data$QSID,])#0
  fma.50C.red<-fma.50C[,c("X","Abund")]
  nrow(fma.50C.red)#93397 obs (why are there more than in xyreduced)
  fma.50C.red$dupl<-duplicated(fma.50C.red$X)
  nrow(fma.50C.red[fma.50C.red$dupl==TRUE,])#220 duplicates
  
  #There are 220 fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are 220 duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those 220 quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.50C.red.u<-fma.50C.red[!fma.50C.red$dupl==TRUE,]
  nrow(fma.50C.red.u)#93177
  xyreduced@data$Abund<-fma.50C.red.u$Abund
  
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
  try(plot(pts_ras.50C$abund))#in basic R

  print(paste0("PFM Year 50 map generated for ", SPP))
  
  levelplot(pts_ras.50C$abund, margin=F)#in rasterVis
  
  #EBM/NDM SCENARIO
  SCEN2<-"ebm"
  
  #Year 0
  a14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_",SCEN2,"_0.csv"), header=TRUE)
  # a15<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_",SCEN2,"_0.csv"), header=TRUE)
  # l1<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_",SCEN2,"_0.csv"), header=TRUE)
  # l2<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_",SCEN2,"_0.csv"), header=TRUE)
  # l3<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_",SCEN2,"_0.csv"), header=TRUE)
  # l8<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_",SCEN2,"_0.csv"), header=TRUE)
  # l11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_",SCEN2,"_0.csv"), header=TRUE)
  # s11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_",SCEN2,"_0.csv"), header=TRUE)
  # s14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_",SCEN2,"_0.csv"), header=TRUE)
  # s18<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_",SCEN2,"_0.csv"), header=TRUE)
  # s22<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_",SCEN2,"_0.csv"), header=TRUE)
  # s23<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_",SCEN2,"_0.csv"), header=TRUE)
  
  fma.0E<-a14#bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.0E)#67 variables, X (column 1) being QSID
  #columns 2-67 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.0E$Abund<-rowSums(fma.0E[,2:67], na.rm=TRUE)
  max(fma.0E$Abund)
  
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
  xyreduced<-subset(xy[xy@data$QSID %in% fma.0E$X, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0E$X, ])  
  
  nrow(xyreduced)#93177 obs instead of 1008568
  nrow(fma.0E)#93397 obs
  nrow(fma.0E[!fma.0E$X %in% xyreduced@data$QSID,])#0
  fma.0E.red<-fma.0E[,c("X","Abund")]
  nrow(fma.0E.red)#93397 obs (why are there more than in xyreduced)
  fma.0E.red$dupl<-duplicated(fma.0E.red$X)
  nrow(fma.0E.red[fma.0E.red$dupl==TRUE,])#N duplicates
  
  #There are N fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are N duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those N quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.0E.red.u<-fma.0E.red[!fma.0E.red$dupl==TRUE,]
  nrow(fma.0E.red.u)#93177
  xyreduced@data$Abund<-fma.0E.red.u$Abund
  
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
  r.0E = raster()#very roughly quarter-section resolution
  extent(r.0E) = extent(pts.0E)
  projection(r.0E) = CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  res(r.0E)<-1000#set resolution to 1 sq. km
  
  pts_ras.0E<- rasterize(pts.0E, r.0E, fun=mean)
  try(plot(pts_ras.0E$abund))#in basic R

  print(paste0("EBM Year 0 map generated for ", SPP))
  
  #levelplot(pts_ras.0E$abund, margin=F)#in rasterVis
  
  #Year 50
  a14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_",SCEN2,"_5.csv"), header=TRUE)
  # a15<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_",SCEN2,"_5.csv"), header=TRUE)
  # l1<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_",SCEN2,"_5.csv"), header=TRUE)
  # l2<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_",SCEN2,"_5.csv"), header=TRUE)
  # l3<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_",SCEN2,"_5.csv"), header=TRUE)
  # l8<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_",SCEN2,"_5.csv"), header=TRUE)
  # l11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_",SCEN2,"_5.csv"), header=TRUE)
  # s11<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_",SCEN2,"_5.csv"), header=TRUE)
  # s14<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_",SCEN2,"_5.csv"), header=TRUE)
  # s18<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_",SCEN2,"_5.csv"), header=TRUE)
  # s22<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_",SCEN2,"_5.csv"), header=TRUE)
  # s23<-read.csv(paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_",SCEN2,"_5.csv"), header=TRUE)
  
  fma.50E<-a14#bind_rows(a14,a15,l1,l2,l3,l8,l11,s11,s14,s18,s22,s23)
  str(fma.50E)#60 variables, X (column 1) being QSID
  #columns 2-60 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
  #currently it looks like densities are being calculated per square metre
  
  fma.50E$Abund<-rowSums(fma.50E[,2:60], na.rm=TRUE)
  max(fma.50E$Abund)
  
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
  xyreduced<-subset(xy[xy@data$QSID %in% fma.50E$X, ])
  #alternatively:
  #xyreduced<-subset(xy[qsid %in% fma.0E$X, ])  
  
  nrow(xyreduced)#93176 obs instead of 1008568
  nrow(fma.50E)#93396 obs
  nrow(fma.50E[!fma.50E$X %in% xyreduced@data$QSID,])#0
  fma.50E.red<-fma.50E[,c("X","Abund")]
  nrow(fma.50E.red)#93397 obs (why are there more than in xyreduced)
  fma.50E.red$dupl<-duplicated(fma.50E.red$X)
  nrow(fma.50E.red[fma.50E.red$dupl==TRUE,])#N duplicates
  
  #There are N fewer rows in xyreduced than my combined Patchworks file
  #All of the QSIDs in my Patchworks file are apparently in xyreduced
  #There are 220 duplicated QSID values (probably because some quarter-
  #sections are straddling two different FMUs)
  
  #I could add up the predicted abundance in those 220 quarter-sections
  #or I could treat them as decimal dust (as Erin would put it) and remove
  #them
  
  fma.50E.red.u<-fma.50E.red[!fma.50E.red$dupl==TRUE,]
  nrow(fma.50E.red.u)#93177
  xyreduced@data$Abund<-fma.50E.red.u$Abund
  
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
  r.50E = raster()#very roughly quarter-section resolution
  extent(r.50E) = extent(pts.50E)
  projection(r.50E) = CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  res(r.50E)<-1000#set resolution to 1 sq. km
  
  pts_ras.50E<- rasterize(pts.50E, r.50E, fun=mean)
  try(plot(pts_ras.50E$abund))#in basic R
  
  print(paste0("EBM Year 50 map generated for ", SPP))
  
  #levelplot(pts_ras.50E$abund, margin=F)#in rasterVis
  pts.50C$abund.T<-ifelse(pts.50C$abund>quantile(pts.50C$abund,0.95),quantile(pts.50C$abund,0.95),pts.50C$abund)
  pts.0C$abund.T<-ifelse(pts.0C$abund>quantile(pts.0C$abund,0.95),quantile(pts.0C$abund,0.95),pts.0C$abund)
  pts.50E$abund.T<-ifelse(pts.50E$abund>quantile(pts.50E$abund,0.95),quantile(pts.50E$abund,0.95),pts.50E$abund)
  pts.0E$abund.T<-ifelse(pts.0E$abund>quantile(pts.0E$abund,0.95),quantile(pts.0E$abund,0.95),pts.0E$abund)
  
  maxabund<-max(max(pts.50C$abund.T),
                max(pts.0C$abund.T),
                max(pts.50E$abund.T),
                max(pts.0E$abund.T))
  miat=c(-maxabund,
         -maxabund*0.875,
         -maxabund*0.75,
         -maxabund*0.625,
         -maxabund*0.5,
         -maxabund*0.375,
         -maxabund*0.25,
         -maxabund*0.125,
         0,
         maxabund*0.125,
         maxabund*0.25,
         maxabund*0.375,
         maxabund*0.5,
         maxabund*0.625,
         maxabund*0.75,
         maxabund*0.875,
         maxabund)  
  
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
  
  print(paste0("PFM scenario map generated for ",SPP))
  
  #Year 50 - Year 0 - EBM Scenario
  ebmplot<-levelplot(pts_ras.50E$abund-pts_ras.0E$abund, margin=F,
                     par.settings = rasterTheme(region=divPal, axis.line = list(col = "transparent"), 
                                                strip.background = list(col = 'transparent'), 
                                                strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                     main=list(label='EBM (Ecosytem-Based Mgmt. Scenario: Year 50 - Year 0',cex=1), at=miat)#in rasterVis
  
  print(paste0("EBM scenario map generated for ",SPP))
  
  #options for multi-panel plotting
  lattice.options(
    layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
    layout.widths=list(left.padding=list(x=0), right.padding=list(x=0.5))
  )
  
  tg<-textGrob(paste0("Change in ",SPP," Density in F.M.U. A-14"),gp=gpar(fontsize=20,font=1))
  margin <- unit(0.5, "line")
  grided<-grid.arrange(caribouplot, ebmplot, ncol=2)
  
  tiff(paste0("3_output/maps/a14/",SPP,"_A14_PatchworksDensityMaps.tiff"), units="in", width=15, height=8, res=300)
  
  grid.arrange(tg, grided,
               heights = unit.c(grobHeight(tg) + 1.2*margin, 
                                unit(1,"null")))
  dev.off()
  print(paste0("Map generated for ",SPP))
}

