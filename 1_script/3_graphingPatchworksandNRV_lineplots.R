library(mefa4)
library(rgdal)
library(rgeos)
library(sp)
library(maps)
library(maptools)
library(mapproj)
library(mapdata)
library(raster)

library(ggplot2)
my.theme <- theme_classic() +
  theme(text=element_text(size=12, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))
#devtools::install_github("ABbiodiversity/cure4insect", ref="v2018")

names<-c("AlderFlycatcher",
         "AmericanThreetoedWoodpecker",
         "BarnSwallow",
         "BaltimoreOriole",
         "BlackAndWhiteWarbler",
         "BlackbackedWoodpecker",
         "BlackpollWarbler",
         "BorealChickadee",
         "BrownCreeper",
         "CanadaWarbler",
         "CapeMayWarbler",
         "ClaycoloredSparrow",
         "CommonYellowthroat",
         "DarkeyedJunco",
         "EasternPhoebe",
         "EveningGrosbeak",
         "GoldencrownedKinglet",
         "GrayJay",
         "HermitThrush",
         "LeastFlycatcher",
         "LeContesSparrow",
         "MourningWarbler",
         "NorthernFlicker",
         "Ovenbird",
         "PalmWarbler",
         "PileatedWoodpecker",
         "RosebreastedGrosbeak",
         "RustyBlackbird",
         "SolitarySandpiper",
         "SwainsonsThrush",
         "WesternTanager",
         "WilsonsWarbler",
         "WilsonsSnipe",
         "WhitethroatedSparrow",
         "WhitewingedCrossbill",
         "WesternWoodPewee",
         "YellowbelliedSapsucker")


for (SPP in names){
  library(cure4insect)
  load_common_data()
  veg <- get_levels()$veg
  ## load this file from where you put it
  load("0_data/raw/hf-an-xy-for patchworks.RData")
  qsid <- rownames(coordinates(xy))
  xy@data$QSID<-paste0(xy@data$MER,"-",xy@data$RGE,"-",xy@data$TWP,"-",xy@data$SEC,"-",xy@data$QS)
  #create the variable that will be used to subset the spatial points data frame by matching to quarter sections
  #in each scenario file
  
  #Now read in and process Caribou Scenarios
  
  caribou.a14.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_caribou_0.csv"), header=TRUE)
  caribou.a14.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_caribou_1.csv"), header=TRUE)
  caribou.a14.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_caribou_2.csv"), header=TRUE)
  caribou.a14.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_caribou_5.csv"), header=TRUE)
  caribou.a15.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_caribou_0.csv"), header=TRUE)
  caribou.a15.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_caribou_1.csv"), header=TRUE)
  caribou.a15.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_caribou_2.csv"), header=TRUE)
  caribou.a15.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_caribou_5.csv"), header=TRUE)
  caribou.l1.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_caribou_0.csv"), header=TRUE)
  caribou.l1.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_caribou_1.csv"), header=TRUE)
  caribou.l1.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_caribou_2.csv"), header=TRUE)
  caribou.l1.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_caribou_5.csv"), header=TRUE)
  caribou.l2.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_caribou_0.csv"), header=TRUE)
  caribou.l2.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_caribou_1.csv"), header=TRUE)
  caribou.l2.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_caribou_2.csv"), header=TRUE)
  caribou.l2.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_caribou_5.csv"), header=TRUE)
  caribou.l3.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_caribou_0.csv"), header=TRUE)
  caribou.l3.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_caribou_1.csv"), header=TRUE)
  caribou.l3.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_caribou_2.csv"), header=TRUE)
  caribou.l3.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_caribou_5.csv"), header=TRUE)
  caribou.l8.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_caribou_0.csv"), header=TRUE)
  caribou.l8.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_caribou_1.csv"), header=TRUE)
  caribou.l8.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_caribou_2.csv"), header=TRUE)
  caribou.l8.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_caribou_5.csv"), header=TRUE)
  caribou.l11.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_caribou_0.csv"), header=TRUE)
  caribou.l11.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_caribou_1.csv"), header=TRUE)
  caribou.l11.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_caribou_2.csv"), header=TRUE)
  caribou.l11.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_caribou_5.csv"), header=TRUE)
  caribou.s11.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_caribou_0.csv"), header=TRUE)
  caribou.s11.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_caribou_1.csv"), header=TRUE)
  caribou.s11.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_caribou_2.csv"), header=TRUE)
  caribou.s11.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_caribou_5.csv"), header=TRUE)
  caribou.s14.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_caribou_0.csv"), header=TRUE)
  caribou.s14.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_caribou_1.csv"), header=TRUE)
  caribou.s14.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_caribou_2.csv"), header=TRUE)
  caribou.s14.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_caribou_5.csv"), header=TRUE)
  caribou.s18.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_caribou_0.csv"), header=TRUE)
  caribou.s18.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_caribou_1.csv"), header=TRUE)
  caribou.s18.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_caribou_2.csv"), header=TRUE)
  caribou.s18.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_caribou_5.csv"), header=TRUE)
  caribou.s22.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_caribou_0.csv"), header=TRUE)
  caribou.s22.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_caribou_1.csv"), header=TRUE)
  caribou.s22.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_caribou_2.csv"), header=TRUE)
  caribou.s22.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_caribou_5.csv"), header=TRUE)
  caribou.s23.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_caribou_0.csv"), header=TRUE)
  caribou.s23.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_caribou_1.csv"), header=TRUE)
  caribou.s23.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_caribou_2.csv"), header=TRUE)
  caribou.s23.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_caribou_5.csv"), header=TRUE)
  
  
  caribou.a14.0$Abund<-rowSums(caribou.a14.0[,c(2:ncol(caribou.a14.0))], na.rm = TRUE)
  caribou.a14.1$Abund<-rowSums(caribou.a14.1[,c(2:ncol(caribou.a14.1))], na.rm = TRUE)
  caribou.a14.2$Abund<-rowSums(caribou.a14.2[,c(2:ncol(caribou.a14.2))], na.rm = TRUE)
  caribou.a14.5$Abund<-rowSums(caribou.a14.5[,c(2:ncol(caribou.a14.5))], na.rm = TRUE)
  caribou.a15.0$Abund<-rowSums(caribou.a15.0[,c(2:ncol(caribou.a15.0))], na.rm = TRUE)
  caribou.a15.1$Abund<-rowSums(caribou.a15.1[,c(2:ncol(caribou.a15.1))], na.rm = TRUE)
  caribou.a15.2$Abund<-rowSums(caribou.a15.2[,c(2:ncol(caribou.a15.2))], na.rm = TRUE)
  caribou.a15.5$Abund<-rowSums(caribou.a15.5[,c(2:ncol(caribou.a15.5))], na.rm = TRUE)
  caribou.l1.0$Abund<-rowSums(caribou.l1.0[,c(2:ncol(caribou.l1.0))], na.rm = TRUE)
  caribou.l1.1$Abund<-rowSums(caribou.l1.1[,c(2:ncol(caribou.l1.1))], na.rm = TRUE)
  caribou.l1.2$Abund<-rowSums(caribou.l1.2[,c(2:ncol(caribou.l1.2))], na.rm = TRUE)
  caribou.l1.5$Abund<-rowSums(caribou.l1.5[,c(2:ncol(caribou.l1.5))], na.rm = TRUE)
  caribou.l2.0$Abund<-rowSums(caribou.l2.0[,c(2:ncol(caribou.l2.0))], na.rm = TRUE)
  caribou.l2.1$Abund<-rowSums(caribou.l2.1[,c(2:ncol(caribou.l2.1))], na.rm = TRUE)
  caribou.l2.2$Abund<-rowSums(caribou.l2.2[,c(2:ncol(caribou.l2.2))], na.rm = TRUE)
  caribou.l2.5$Abund<-rowSums(caribou.l2.5[,c(2:ncol(caribou.l2.5))], na.rm = TRUE)
  caribou.l3.0$Abund<-rowSums(caribou.l3.0[,c(2:ncol(caribou.l3.0))], na.rm = TRUE)
  caribou.l3.1$Abund<-rowSums(caribou.l3.1[,c(2:ncol(caribou.l3.1))], na.rm = TRUE)
  caribou.l3.2$Abund<-rowSums(caribou.l3.2[,c(2:ncol(caribou.l3.2))], na.rm = TRUE)
  caribou.l3.5$Abund<-rowSums(caribou.l3.5[,c(2:ncol(caribou.l3.5))], na.rm = TRUE)
  caribou.l8.0$Abund<-rowSums(caribou.l8.0[,c(2:ncol(caribou.l8.0))], na.rm = TRUE)
  caribou.l8.1$Abund<-rowSums(caribou.l8.1[,c(2:ncol(caribou.l8.1))], na.rm = TRUE)
  caribou.l8.2$Abund<-rowSums(caribou.l8.2[,c(2:ncol(caribou.l8.2))], na.rm = TRUE)
  caribou.l8.5$Abund<-rowSums(caribou.l8.5[,c(2:ncol(caribou.l8.5))], na.rm = TRUE)
  caribou.l11.0$Abund<-rowSums(caribou.l11.0[,c(2:ncol(caribou.l11.0))], na.rm = TRUE)
  caribou.l11.1$Abund<-rowSums(caribou.l11.1[,c(2:ncol(caribou.l11.1))], na.rm = TRUE)
  caribou.l11.2$Abund<-rowSums(caribou.l11.2[,c(2:ncol(caribou.l11.2))], na.rm = TRUE)
  caribou.l11.5$Abund<-rowSums(caribou.l11.5[,c(2:ncol(caribou.l11.5))], na.rm = TRUE)
  caribou.s11.0$Abund<-rowSums(caribou.s11.0[,c(2:ncol(caribou.s11.0))], na.rm = TRUE)
  caribou.s11.1$Abund<-rowSums(caribou.s11.1[,c(2:ncol(caribou.s11.1))], na.rm = TRUE)
  caribou.s11.2$Abund<-rowSums(caribou.s11.2[,c(2:ncol(caribou.s11.2))], na.rm = TRUE)
  caribou.s11.5$Abund<-rowSums(caribou.s11.5[,c(2:ncol(caribou.s11.5))], na.rm = TRUE)
  caribou.s14.0$Abund<-rowSums(caribou.s14.0[,c(2:ncol(caribou.s14.0))], na.rm = TRUE)
  caribou.s14.1$Abund<-rowSums(caribou.s14.1[,c(2:ncol(caribou.s14.1))], na.rm = TRUE)
  caribou.s14.2$Abund<-rowSums(caribou.s14.2[,c(2:ncol(caribou.s14.2))], na.rm = TRUE)
  caribou.s14.5$Abund<-rowSums(caribou.s14.5[,c(2:ncol(caribou.s14.5))], na.rm = TRUE)
  caribou.s18.0$Abund<-rowSums(caribou.s18.0[,c(2:ncol(caribou.s18.0))], na.rm = TRUE)
  caribou.s18.1$Abund<-rowSums(caribou.s18.1[,c(2:ncol(caribou.s18.1))], na.rm = TRUE)
  caribou.s18.2$Abund<-rowSums(caribou.s18.2[,c(2:ncol(caribou.s18.2))], na.rm = TRUE)
  caribou.s18.5$Abund<-rowSums(caribou.s18.5[,c(2:ncol(caribou.s18.5))], na.rm = TRUE)
  caribou.s22.0$Abund<-rowSums(caribou.s22.0[,c(2:ncol(caribou.s22.0))], na.rm = TRUE)
  caribou.s22.1$Abund<-rowSums(caribou.s22.1[,c(2:ncol(caribou.s22.1))], na.rm = TRUE)
  caribou.s22.2$Abund<-rowSums(caribou.s22.2[,c(2:ncol(caribou.s22.2))], na.rm = TRUE)
  caribou.s22.5$Abund<-rowSums(caribou.s22.5[,c(2:ncol(caribou.s22.5))], na.rm = TRUE)
  caribou.s23.0$Abund<-rowSums(caribou.s23.0[,c(2:ncol(caribou.s23.0))], na.rm = TRUE)
  caribou.s23.1$Abund<-rowSums(caribou.s23.1[,c(2:ncol(caribou.s23.1))], na.rm = TRUE)
  caribou.s23.2$Abund<-rowSums(caribou.s23.2[,c(2:ncol(caribou.s23.2))], na.rm = TRUE)
  caribou.s23.5$Abund<-rowSums(caribou.s23.5[,c(2:ncol(caribou.s23.5))], na.rm = TRUE)
  
  #a14 - get the quarter-section centroid locations for mapping clipped to A14
  xy.caribou.a14.0<-subset(xy[xy@data$QSID %in% caribou.a14.0$X, ])
  xy.caribou.a14.1<-subset(xy[xy@data$QSID %in% caribou.a14.1$X, ])
  xy.caribou.a14.2<-subset(xy[xy@data$QSID %in% caribou.a14.2$X, ])
  xy.caribou.a14.5<-subset(xy[xy@data$QSID %in% caribou.a14.5$X, ])
  
  xy.caribou.a14.0@data$Abund<-caribou.a14.0$Abund
  xy.caribou.a14.0@data$POINT_X<-coordinates(xy.caribou.a14.0)[,1]
  xy.caribou.a14.0@data$POINT_Y<-coordinates(xy.caribou.a14.0)[,2]
  
  xy.caribou.a14.1@data$Abund<-caribou.a14.1$Abund
  xy.caribou.a14.1@data$POINT_X<-coordinates(xy.caribou.a14.1)[,1]
  xy.caribou.a14.1@data$POINT_Y<-coordinates(xy.caribou.a14.1)[,2]
  
  xy.caribou.a14.2@data$Abund<-caribou.a14.2$Abund
  xy.caribou.a14.2@data$POINT_X<-coordinates(xy.caribou.a14.2)[,1]
  xy.caribou.a14.2@data$POINT_Y<-coordinates(xy.caribou.a14.2)[,2]
  
  xy.caribou.a14.5@data$Abund<-caribou.a14.5$Abund
  xy.caribou.a14.5@data$POINT_X<-coordinates(xy.caribou.a14.5)[,1]
  xy.caribou.a14.5@data$POINT_Y<-coordinates(xy.caribou.a14.5)[,2]
  
  #a15 - get the quarter-section centroid locations for mapping clipped to A15
  xy.caribou.a15.0<-subset(xy[xy@data$QSID %in% caribou.a15.0$X, ])
  xy.caribou.a15.1<-subset(xy[xy@data$QSID %in% caribou.a15.1$X, ])
  xy.caribou.a15.2<-subset(xy[xy@data$QSID %in% caribou.a15.2$X, ])
  xy.caribou.a15.5<-subset(xy[xy@data$QSID %in% caribou.a15.5$X, ])
  
  xy.caribou.a15.0@data$Abund<-caribou.a15.0$Abund
  xy.caribou.a15.0@data$POINT_X<-coordinates(xy.caribou.a15.0)[,1]
  xy.caribou.a15.0@data$POINT_Y<-coordinates(xy.caribou.a15.0)[,2]
  
  xy.caribou.a15.1@data$Abund<-caribou.a15.1$Abund
  xy.caribou.a15.1@data$POINT_X<-coordinates(xy.caribou.a15.1)[,1]
  xy.caribou.a15.1@data$POINT_Y<-coordinates(xy.caribou.a15.1)[,2]
  
  xy.caribou.a15.2@data$Abund<-caribou.a15.2$Abund
  xy.caribou.a15.2@data$POINT_X<-coordinates(xy.caribou.a15.2)[,1]
  xy.caribou.a15.2@data$POINT_Y<-coordinates(xy.caribou.a15.2)[,2]
  
  xy.caribou.a15.5@data$Abund<-caribou.a15.5$Abund
  xy.caribou.a15.5@data$POINT_X<-coordinates(xy.caribou.a15.5)[,1]
  xy.caribou.a15.5@data$POINT_Y<-coordinates(xy.caribou.a15.5)[,2]
  
  #l1 - get the quarter-section centroid locations for mapping clipped to L1
  xy.caribou.l1.0<-subset(xy[xy@data$QSID %in% caribou.l1.0$X, ])
  xy.caribou.l1.1<-subset(xy[xy@data$QSID %in% caribou.l1.1$X, ])
  xy.caribou.l1.2<-subset(xy[xy@data$QSID %in% caribou.l1.2$X, ])
  xy.caribou.l1.5<-subset(xy[xy@data$QSID %in% caribou.l1.5$X, ])
  
  xy.caribou.l1.0@data$Abund<-caribou.l1.0$Abund
  xy.caribou.l1.0@data$POINT_X<-coordinates(xy.caribou.l1.0)[,1]
  xy.caribou.l1.0@data$POINT_Y<-coordinates(xy.caribou.l1.0)[,2]
  
  xy.caribou.l1.1@data$Abund<-caribou.l1.1$Abund
  xy.caribou.l1.1@data$POINT_X<-coordinates(xy.caribou.l1.1)[,1]
  xy.caribou.l1.1@data$POINT_Y<-coordinates(xy.caribou.l1.1)[,2]
  
  xy.caribou.l1.2@data$Abund<-caribou.l1.2$Abund
  xy.caribou.l1.2@data$POINT_X<-coordinates(xy.caribou.l1.2)[,1]
  xy.caribou.l1.2@data$POINT_Y<-coordinates(xy.caribou.l1.2)[,2]
  
  xy.caribou.l1.5@data$Abund<-caribou.l1.5$Abund
  xy.caribou.l1.5@data$POINT_X<-coordinates(xy.caribou.l1.5)[,1]
  xy.caribou.l1.5@data$POINT_Y<-coordinates(xy.caribou.l1.5)[,2]
  
  #l2 - get the quarter-section centroid locations for mapping clipped to L2
  xy.caribou.l2.0<-subset(xy[xy@data$QSID %in% caribou.l2.0$X, ])
  xy.caribou.l2.1<-subset(xy[xy@data$QSID %in% caribou.l2.1$X, ])
  xy.caribou.l2.2<-subset(xy[xy@data$QSID %in% caribou.l2.2$X, ])
  xy.caribou.l2.5<-subset(xy[xy@data$QSID %in% caribou.l2.5$X, ])
  
  xy.caribou.l2.0@data$Abund<-caribou.l2.0$Abund
  xy.caribou.l2.0@data$POINT_X<-coordinates(xy.caribou.l2.0)[,1]
  xy.caribou.l2.0@data$POINT_Y<-coordinates(xy.caribou.l2.0)[,2]
  
  xy.caribou.l2.1@data$Abund<-caribou.l2.1$Abund
  xy.caribou.l2.1@data$POINT_X<-coordinates(xy.caribou.l2.1)[,1]
  xy.caribou.l2.1@data$POINT_Y<-coordinates(xy.caribou.l2.1)[,2]
  
  xy.caribou.l2.2@data$Abund<-caribou.l2.2$Abund
  xy.caribou.l2.2@data$POINT_X<-coordinates(xy.caribou.l2.2)[,1]
  xy.caribou.l2.2@data$POINT_Y<-coordinates(xy.caribou.l2.2)[,2]
  
  xy.caribou.l2.5@data$Abund<-caribou.l2.5$Abund
  xy.caribou.l2.5@data$POINT_X<-coordinates(xy.caribou.l2.5)[,1]
  xy.caribou.l2.5@data$POINT_Y<-coordinates(xy.caribou.l2.5)[,2]
  
  #l3 - get the quarter-section centroid locations for mapping clipped to L3
  xy.caribou.l3.0<-subset(xy[xy@data$QSID %in% caribou.l3.0$X, ])
  xy.caribou.l3.1<-subset(xy[xy@data$QSID %in% caribou.l3.1$X, ])
  xy.caribou.l3.2<-subset(xy[xy@data$QSID %in% caribou.l3.2$X, ])
  xy.caribou.l3.5<-subset(xy[xy@data$QSID %in% caribou.l3.5$X, ])
  
  xy.caribou.l3.0@data$Abund<-caribou.l3.0$Abund
  xy.caribou.l3.0@data$POINT_X<-coordinates(xy.caribou.l3.0)[,1]
  xy.caribou.l3.0@data$POINT_Y<-coordinates(xy.caribou.l3.0)[,2]
  
  xy.caribou.l3.1@data$Abund<-caribou.l3.1$Abund
  xy.caribou.l3.1@data$POINT_X<-coordinates(xy.caribou.l3.1)[,1]
  xy.caribou.l3.1@data$POINT_Y<-coordinates(xy.caribou.l3.1)[,2]
  
  xy.caribou.l3.2@data$Abund<-caribou.l3.2$Abund
  xy.caribou.l3.2@data$POINT_X<-coordinates(xy.caribou.l3.2)[,1]
  xy.caribou.l3.2@data$POINT_Y<-coordinates(xy.caribou.l3.2)[,2]
  
  xy.caribou.l3.5@data$Abund<-caribou.l3.5$Abund
  xy.caribou.l3.5@data$POINT_X<-coordinates(xy.caribou.l3.5)[,1]
  xy.caribou.l3.5@data$POINT_Y<-coordinates(xy.caribou.l3.5)[,2]
  
  #l8 - get the quarter-section centroid locations for mapping clipped to L8
  xy.caribou.l8.0<-subset(xy[xy@data$QSID %in% caribou.l8.0$X, ])
  xy.caribou.l8.1<-subset(xy[xy@data$QSID %in% caribou.l8.1$X, ])
  xy.caribou.l8.2<-subset(xy[xy@data$QSID %in% caribou.l8.2$X, ])
  xy.caribou.l8.5<-subset(xy[xy@data$QSID %in% caribou.l8.5$X, ])
  
  xy.caribou.l8.0@data$Abund<-caribou.l8.0$Abund
  xy.caribou.l8.0@data$POINT_X<-coordinates(xy.caribou.l8.0)[,1]
  xy.caribou.l8.0@data$POINT_Y<-coordinates(xy.caribou.l8.0)[,2]
  
  xy.caribou.l8.1@data$Abund<-caribou.l8.1$Abund
  xy.caribou.l8.1@data$POINT_X<-coordinates(xy.caribou.l8.1)[,1]
  xy.caribou.l8.1@data$POINT_Y<-coordinates(xy.caribou.l8.1)[,2]
  
  xy.caribou.l8.2@data$Abund<-caribou.l8.2$Abund
  xy.caribou.l8.2@data$POINT_X<-coordinates(xy.caribou.l8.2)[,1]
  xy.caribou.l8.2@data$POINT_Y<-coordinates(xy.caribou.l8.2)[,2]
  
  xy.caribou.l8.5@data$Abund<-caribou.l8.5$Abund
  xy.caribou.l8.5@data$POINT_X<-coordinates(xy.caribou.l8.5)[,1]
  xy.caribou.l8.5@data$POINT_Y<-coordinates(xy.caribou.l8.5)[,2]
  
  #l11 - get the quarter-section centroid locations for mapping clipped to L11
  xy.caribou.l11.0<-subset(xy[xy@data$QSID %in% caribou.l11.0$X, ])
  xy.caribou.l11.1<-subset(xy[xy@data$QSID %in% caribou.l11.1$X, ])
  xy.caribou.l11.2<-subset(xy[xy@data$QSID %in% caribou.l11.2$X, ])
  xy.caribou.l11.5<-subset(xy[xy@data$QSID %in% caribou.l11.5$X, ])
  
  xy.caribou.l11.0@data$Abund<-caribou.l11.0$Abund
  xy.caribou.l11.0@data$POINT_X<-coordinates(xy.caribou.l11.0)[,1]
  xy.caribou.l11.0@data$POINT_Y<-coordinates(xy.caribou.l11.0)[,2]
  
  xy.caribou.l11.1@data$Abund<-caribou.l11.1$Abund
  xy.caribou.l11.1@data$POINT_X<-coordinates(xy.caribou.l11.1)[,1]
  xy.caribou.l11.1@data$POINT_Y<-coordinates(xy.caribou.l11.1)[,2]
  
  xy.caribou.l11.2@data$Abund<-caribou.l11.2$Abund
  xy.caribou.l11.2@data$POINT_X<-coordinates(xy.caribou.l11.2)[,1]
  xy.caribou.l11.2@data$POINT_Y<-coordinates(xy.caribou.l11.2)[,2]
  
  xy.caribou.l11.5@data$Abund<-caribou.l11.5$Abund
  xy.caribou.l11.5@data$POINT_X<-coordinates(xy.caribou.l11.5)[,1]
  xy.caribou.l11.5@data$POINT_Y<-coordinates(xy.caribou.l11.5)[,2]
  
  #s11 - get the quarter-section centroid locations for mapping clipped to S11
  xy.caribou.s11.0<-subset(xy[xy@data$QSID %in% caribou.s11.0$X, ])
  xy.caribou.s11.1<-subset(xy[xy@data$QSID %in% caribou.s11.1$X, ])
  xy.caribou.s11.2<-subset(xy[xy@data$QSID %in% caribou.s11.2$X, ])
  xy.caribou.s11.5<-subset(xy[xy@data$QSID %in% caribou.s11.5$X, ])
  
  xy.caribou.s11.0@data$Abund<-caribou.s11.0$Abund
  xy.caribou.s11.0@data$POINT_X<-coordinates(xy.caribou.s11.0)[,1]
  xy.caribou.s11.0@data$POINT_Y<-coordinates(xy.caribou.s11.0)[,2]
  
  xy.caribou.s11.1@data$Abund<-caribou.s11.1$Abund
  xy.caribou.s11.1@data$POINT_X<-coordinates(xy.caribou.s11.1)[,1]
  xy.caribou.s11.1@data$POINT_Y<-coordinates(xy.caribou.s11.1)[,2]
  
  xy.caribou.s11.2@data$Abund<-caribou.s11.2$Abund
  xy.caribou.s11.2@data$POINT_X<-coordinates(xy.caribou.s11.2)[,1]
  xy.caribou.s11.2@data$POINT_Y<-coordinates(xy.caribou.s11.2)[,2]
  
  xy.caribou.s11.5@data$Abund<-caribou.s11.5$Abund
  xy.caribou.s11.5@data$POINT_X<-coordinates(xy.caribou.s11.5)[,1]
  xy.caribou.s11.5@data$POINT_Y<-coordinates(xy.caribou.s11.5)[,2]
  
  #s14 - get the quarter-section centroid locations for mapping clipped to s14
  xy.caribou.s14.0<-subset(xy[xy@data$QSID %in% caribou.s14.0$X, ])
  xy.caribou.s14.1<-subset(xy[xy@data$QSID %in% caribou.s14.1$X, ])
  xy.caribou.s14.2<-subset(xy[xy@data$QSID %in% caribou.s14.2$X, ])
  xy.caribou.s14.5<-subset(xy[xy@data$QSID %in% caribou.s14.5$X, ])
  
  xy.caribou.s14.0@data$Abund<-caribou.s14.0$Abund
  xy.caribou.s14.0@data$POINT_X<-coordinates(xy.caribou.s14.0)[,1]
  xy.caribou.s14.0@data$POINT_Y<-coordinates(xy.caribou.s14.0)[,2]
  
  xy.caribou.s14.1@data$Abund<-caribou.s14.1$Abund
  xy.caribou.s14.1@data$POINT_X<-coordinates(xy.caribou.s14.1)[,1]
  xy.caribou.s14.1@data$POINT_Y<-coordinates(xy.caribou.s14.1)[,2]
  
  xy.caribou.s14.2@data$Abund<-caribou.s14.2$Abund
  xy.caribou.s14.2@data$POINT_X<-coordinates(xy.caribou.s14.2)[,1]
  xy.caribou.s14.2@data$POINT_Y<-coordinates(xy.caribou.s14.2)[,2]
  
  xy.caribou.s14.5@data$Abund<-caribou.s14.5$Abund
  xy.caribou.s14.5@data$POINT_X<-coordinates(xy.caribou.s14.5)[,1]
  xy.caribou.s14.5@data$POINT_Y<-coordinates(xy.caribou.s14.5)[,2]
  
  #s18 - get the quarter-section centroid locations for mapping clipped to S18
  xy.caribou.s18.0<-subset(xy[xy@data$QSID %in% caribou.s18.0$X, ])
  xy.caribou.s18.1<-subset(xy[xy@data$QSID %in% caribou.s18.1$X, ])
  xy.caribou.s18.2<-subset(xy[xy@data$QSID %in% caribou.s18.2$X, ])
  xy.caribou.s18.5<-subset(xy[xy@data$QSID %in% caribou.s18.5$X, ])
  
  xy.caribou.s18.0@data$Abund<-caribou.s18.0$Abund
  xy.caribou.s18.0@data$POINT_X<-coordinates(xy.caribou.s18.0)[,1]
  xy.caribou.s18.0@data$POINT_Y<-coordinates(xy.caribou.s18.0)[,2]
  
  xy.caribou.s18.1@data$Abund<-caribou.s18.1$Abund
  xy.caribou.s18.1@data$POINT_X<-coordinates(xy.caribou.s18.1)[,1]
  xy.caribou.s18.1@data$POINT_Y<-coordinates(xy.caribou.s18.1)[,2]
  
  xy.caribou.s18.2@data$Abund<-caribou.s18.2$Abund
  xy.caribou.s18.2@data$POINT_X<-coordinates(xy.caribou.s18.2)[,1]
  xy.caribou.s18.2@data$POINT_Y<-coordinates(xy.caribou.s18.2)[,2]
  
  xy.caribou.s18.5@data$Abund<-caribou.s18.5$Abund
  xy.caribou.s18.5@data$POINT_X<-coordinates(xy.caribou.s18.5)[,1]
  xy.caribou.s18.5@data$POINT_Y<-coordinates(xy.caribou.s18.5)[,2]
  
  #s22 - get the quarter-section centroid locations for mapping clipped to S22
  xy.caribou.s22.0<-subset(xy[xy@data$QSID %in% caribou.s22.0$X, ])
  xy.caribou.s22.1<-subset(xy[xy@data$QSID %in% caribou.s22.1$X, ])
  xy.caribou.s22.2<-subset(xy[xy@data$QSID %in% caribou.s22.2$X, ])
  xy.caribou.s22.5<-subset(xy[xy@data$QSID %in% caribou.s22.5$X, ])
  
  xy.caribou.s22.0@data$Abund<-caribou.s22.0$Abund
  xy.caribou.s22.0@data$POINT_X<-coordinates(xy.caribou.s22.0)[,1]
  xy.caribou.s22.0@data$POINT_Y<-coordinates(xy.caribou.s22.0)[,2]
  
  xy.caribou.s22.1@data$Abund<-caribou.s22.1$Abund
  xy.caribou.s22.1@data$POINT_X<-coordinates(xy.caribou.s22.1)[,1]
  xy.caribou.s22.1@data$POINT_Y<-coordinates(xy.caribou.s22.1)[,2]
  
  xy.caribou.s22.2@data$Abund<-caribou.s22.2$Abund
  xy.caribou.s22.2@data$POINT_X<-coordinates(xy.caribou.s22.2)[,1]
  xy.caribou.s22.2@data$POINT_Y<-coordinates(xy.caribou.s22.2)[,2]
  
  xy.caribou.s22.5@data$Abund<-caribou.s22.5$Abund
  xy.caribou.s22.5@data$POINT_X<-coordinates(xy.caribou.s22.5)[,1]
  xy.caribou.s22.5@data$POINT_Y<-coordinates(xy.caribou.s22.5)[,2]
  
  #s23 - get the quarter-section centroid locations for mapping clipped to S23
  xy.caribou.s23.0<-subset(xy[xy@data$QSID %in% caribou.s23.0$X, ])
  xy.caribou.s23.1<-subset(xy[xy@data$QSID %in% caribou.s23.1$X, ])
  xy.caribou.s23.2<-subset(xy[xy@data$QSID %in% caribou.s23.2$X, ])
  xy.caribou.s23.5<-subset(xy[xy@data$QSID %in% caribou.s23.5$X, ])
  
  xy.caribou.s23.0@data$Abund<-caribou.s23.0$Abund
  xy.caribou.s23.0@data$POINT_X<-coordinates(xy.caribou.s23.0)[,1]
  xy.caribou.s23.0@data$POINT_Y<-coordinates(xy.caribou.s23.0)[,2]
  
  xy.caribou.s23.1@data$Abund<-caribou.s23.1$Abund
  xy.caribou.s23.1@data$POINT_X<-coordinates(xy.caribou.s23.1)[,1]
  xy.caribou.s23.1@data$POINT_Y<-coordinates(xy.caribou.s23.1)[,2]
  
  xy.caribou.s23.2@data$Abund<-caribou.s23.2$Abund
  xy.caribou.s23.2@data$POINT_X<-coordinates(xy.caribou.s23.2)[,1]
  xy.caribou.s23.2@data$POINT_Y<-coordinates(xy.caribou.s23.2)[,2]
  
  xy.caribou.s23.5@data$Abund<-caribou.s23.5$Abund
  xy.caribou.s23.5@data$POINT_X<-coordinates(xy.caribou.s23.5)[,1]
  xy.caribou.s23.5@data$POINT_Y<-coordinates(xy.caribou.s23.5)[,2]
  
  #Now read in and process Business-as-usual (EBM/NDM) Scenarios
  BAU.a14.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_ebm_0.csv"), header=TRUE)
  BAU.a14.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_ebm_1.csv"), header=TRUE)
  BAU.a14.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_ebm_2.csv"), header=TRUE)
  BAU.a14.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a14_ebm_5.csv"), header=TRUE)
  BAU.a15.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_ebm_0.csv"), header=TRUE)
  BAU.a15.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_ebm_1.csv"), header=TRUE)
  BAU.a15.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_ebm_2.csv"), header=TRUE)
  BAU.a15.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_a15_ebm_5.csv"), header=TRUE)
  BAU.l1.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_ebm_0.csv"), header=TRUE)
  BAU.l1.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_ebm_1.csv"), header=TRUE)
  BAU.l1.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_ebm_2.csv"), header=TRUE)
  BAU.l1.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l1_ebm_5.csv"), header=TRUE)
  BAU.l2.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_ebm_0.csv"), header=TRUE)
  BAU.l2.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_ebm_1.csv"), header=TRUE)
  BAU.l2.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_ebm_2.csv"), header=TRUE)
  BAU.l2.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l2_ebm_5.csv"), header=TRUE)
  BAU.l3.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_ebm_0.csv"), header=TRUE)
  BAU.l3.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_ebm_1.csv"), header=TRUE)
  BAU.l3.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_ebm_2.csv"), header=TRUE)
  BAU.l3.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l3_ebm_5.csv"), header=TRUE)
  BAU.l8.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_ebm_0.csv"), header=TRUE)
  BAU.l8.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_ebm_1.csv"), header=TRUE)
  BAU.l8.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_ebm_2.csv"), header=TRUE)
  BAU.l8.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l8_ebm_5.csv"), header=TRUE)
  BAU.l11.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_ebm_0.csv"), header=TRUE)
  BAU.l11.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_ebm_1.csv"), header=TRUE)
  BAU.l11.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_ebm_2.csv"), header=TRUE)
  BAU.l11.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_l11_ebm_5.csv"), header=TRUE)
  BAU.s11.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_ebm_0.csv"), header=TRUE)
  BAU.s11.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_ebm_1.csv"), header=TRUE)
  BAU.s11.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_ebm_2.csv"), header=TRUE)
  BAU.s11.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s11_ebm_5.csv"), header=TRUE)
  BAU.s14.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_ebm_0.csv"), header=TRUE)
  BAU.s14.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_ebm_1.csv"), header=TRUE)
  BAU.s14.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_ebm_2.csv"), header=TRUE)
  BAU.s14.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s14_ebm_5.csv"), header=TRUE)
  BAU.s18.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_ebm_0.csv"), header=TRUE)
  BAU.s18.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_ebm_1.csv"), header=TRUE)
  BAU.s18.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_ebm_2.csv"), header=TRUE)
  BAU.s18.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s18_ebm_5.csv"), header=TRUE)
  BAU.s22.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_ebm_0.csv"), header=TRUE)
  BAU.s22.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_ebm_1.csv"), header=TRUE)
  BAU.s22.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_ebm_2.csv"), header=TRUE)
  BAU.s22.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s22_ebm_5.csv"), header=TRUE)
  BAU.s23.0 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_ebm_0.csv"), header=TRUE)
  BAU.s23.1 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_ebm_1.csv"), header=TRUE)
  BAU.s23.2 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_ebm_2.csv"), header=TRUE)
  BAU.s23.5 <- read.csv(file=paste0("0_data/processed/patchworks/",SPP,"/Abundance_",SPP,"_bothhabitatHF_s23_ebm_5.csv"), header=TRUE)
  
  
  BAU.a14.0$Abund<-rowSums(BAU.a14.0[,c(2:ncol(BAU.a14.0))], na.rm = TRUE)
  BAU.a14.1$Abund<-rowSums(BAU.a14.1[,c(2:ncol(BAU.a14.1))], na.rm = TRUE)
  BAU.a14.2$Abund<-rowSums(BAU.a14.2[,c(2:ncol(BAU.a14.2))], na.rm = TRUE)
  BAU.a14.5$Abund<-rowSums(BAU.a14.5[,c(2:ncol(BAU.a14.5))], na.rm = TRUE)
  BAU.a15.0$Abund<-rowSums(BAU.a15.0[,c(2:ncol(BAU.a15.0))], na.rm = TRUE)
  BAU.a15.1$Abund<-rowSums(BAU.a15.1[,c(2:ncol(BAU.a15.1))], na.rm = TRUE)
  BAU.a15.2$Abund<-rowSums(BAU.a15.2[,c(2:ncol(BAU.a15.2))], na.rm = TRUE)
  BAU.a15.5$Abund<-rowSums(BAU.a15.5[,c(2:ncol(BAU.a15.5))], na.rm = TRUE)
  BAU.l1.0$Abund<-rowSums(BAU.l1.0[,c(2:ncol(BAU.l1.0))], na.rm = TRUE)
  BAU.l1.1$Abund<-rowSums(BAU.l1.1[,c(2:ncol(BAU.l1.1))], na.rm = TRUE)
  BAU.l1.2$Abund<-rowSums(BAU.l1.2[,c(2:ncol(BAU.l1.2))], na.rm = TRUE)
  BAU.l1.5$Abund<-rowSums(BAU.l1.5[,c(2:ncol(BAU.l1.5))], na.rm = TRUE)
  BAU.l2.0$Abund<-rowSums(BAU.l2.0[,c(2:ncol(BAU.l2.0))], na.rm = TRUE)
  BAU.l2.1$Abund<-rowSums(BAU.l2.1[,c(2:ncol(BAU.l2.1))], na.rm = TRUE)
  BAU.l2.2$Abund<-rowSums(BAU.l2.2[,c(2:ncol(BAU.l2.2))], na.rm = TRUE)
  BAU.l2.5$Abund<-rowSums(BAU.l2.5[,c(2:ncol(BAU.l2.5))], na.rm = TRUE)
  BAU.l3.0$Abund<-rowSums(BAU.l3.0[,c(2:ncol(BAU.l3.0))], na.rm = TRUE)
  BAU.l3.1$Abund<-rowSums(BAU.l3.1[,c(2:ncol(BAU.l3.1))], na.rm = TRUE)
  BAU.l3.2$Abund<-rowSums(BAU.l3.2[,c(2:ncol(BAU.l3.2))], na.rm = TRUE)
  BAU.l3.5$Abund<-rowSums(BAU.l3.5[,c(2:ncol(BAU.l3.5))], na.rm = TRUE)
  BAU.l8.0$Abund<-rowSums(BAU.l8.0[,c(2:ncol(BAU.l8.0))], na.rm = TRUE)
  BAU.l8.1$Abund<-rowSums(BAU.l8.1[,c(2:ncol(BAU.l8.1))], na.rm = TRUE)
  BAU.l8.2$Abund<-rowSums(BAU.l8.2[,c(2:ncol(BAU.l8.2))], na.rm = TRUE)
  BAU.l8.5$Abund<-rowSums(BAU.l8.5[,c(2:ncol(BAU.l8.5))], na.rm = TRUE)
  BAU.l11.0$Abund<-rowSums(BAU.l11.0[,c(2:ncol(BAU.l11.0))], na.rm = TRUE)
  BAU.l11.1$Abund<-rowSums(BAU.l11.1[,c(2:ncol(BAU.l11.1))], na.rm = TRUE)
  BAU.l11.2$Abund<-rowSums(BAU.l11.2[,c(2:ncol(BAU.l11.2))], na.rm = TRUE)
  BAU.l11.5$Abund<-rowSums(BAU.l11.5[,c(2:ncol(BAU.l11.5))], na.rm = TRUE)
  BAU.s11.0$Abund<-rowSums(BAU.s11.0[,c(2:ncol(BAU.s11.0))], na.rm = TRUE)
  BAU.s11.1$Abund<-rowSums(BAU.s11.1[,c(2:ncol(BAU.s11.1))], na.rm = TRUE)
  BAU.s11.2$Abund<-rowSums(BAU.s11.2[,c(2:ncol(BAU.s11.2))], na.rm = TRUE)
  BAU.s11.5$Abund<-rowSums(BAU.s11.5[,c(2:ncol(BAU.s11.5))], na.rm = TRUE)
  BAU.s14.0$Abund<-rowSums(BAU.s14.0[,c(2:ncol(BAU.s14.0))], na.rm = TRUE)
  BAU.s14.1$Abund<-rowSums(BAU.s14.1[,c(2:ncol(BAU.s14.1))], na.rm = TRUE)
  BAU.s14.2$Abund<-rowSums(BAU.s14.2[,c(2:ncol(BAU.s14.2))], na.rm = TRUE)
  BAU.s14.5$Abund<-rowSums(BAU.s14.5[,c(2:ncol(BAU.s14.5))], na.rm = TRUE)
  BAU.s18.0$Abund<-rowSums(BAU.s18.0[,c(2:ncol(BAU.s18.0))], na.rm = TRUE)
  BAU.s18.1$Abund<-rowSums(BAU.s18.1[,c(2:ncol(BAU.s18.1))], na.rm = TRUE)
  BAU.s18.2$Abund<-rowSums(BAU.s18.2[,c(2:ncol(BAU.s18.2))], na.rm = TRUE)
  BAU.s18.5$Abund<-rowSums(BAU.s18.5[,c(2:ncol(BAU.s18.5))], na.rm = TRUE)
  BAU.s22.0$Abund<-rowSums(BAU.s22.0[,c(2:ncol(BAU.s22.0))], na.rm = TRUE)
  BAU.s22.1$Abund<-rowSums(BAU.s22.1[,c(2:ncol(BAU.s22.1))], na.rm = TRUE)
  BAU.s22.2$Abund<-rowSums(BAU.s22.2[,c(2:ncol(BAU.s22.2))], na.rm = TRUE)
  BAU.s22.5$Abund<-rowSums(BAU.s22.5[,c(2:ncol(BAU.s22.5))], na.rm = TRUE)
  BAU.s23.0$Abund<-rowSums(BAU.s23.0[,c(2:ncol(BAU.s23.0))], na.rm = TRUE)
  BAU.s23.1$Abund<-rowSums(BAU.s23.1[,c(2:ncol(BAU.s23.1))], na.rm = TRUE)
  BAU.s23.2$Abund<-rowSums(BAU.s23.2[,c(2:ncol(BAU.s23.2))], na.rm = TRUE)
  BAU.s23.5$Abund<-rowSums(BAU.s23.5[,c(2:ncol(BAU.s23.5))], na.rm = TRUE)
  
  
  library(cure4insect)
  library(mefa4)
  library(rgdal)
  library(rgeos)
  library(sp)
  library(maps)
  library(maptools)
  library(mapproj)
  library(mapdata)
  library(raster)
  
  load_common_data()
  veg <- get_levels()$veg
  ## load this file from where you put it
  load("0_data/raw/hf-an-xy-for patchworks.RData")
  qsid <- rownames(coordinates(xy))
  xy@data$QSID<-paste0(xy@data$MER,"-",xy@data$RGE,"-",xy@data$TWP,"-",xy@data$SEC,"-",xy@data$QS)
  #create the variable that will be used to subset the spatial points data frame by matching to quarter sections
  #in each scenario file
  
  #a14
  xy.BAU.a14.0<-subset(xy[xy@data$QSID %in% BAU.a14.0$X, ])
  xy.BAU.a14.1<-subset(xy[xy@data$QSID %in% BAU.a14.1$X, ])
  xy.BAU.a14.2<-subset(xy[xy@data$QSID %in% BAU.a14.2$X, ])
  xy.BAU.a14.5<-subset(xy[xy@data$QSID %in% BAU.a14.5$X, ])
  
  xy.BAU.a14.0@data$Abund<-BAU.a14.0$Abund
  xy.BAU.a14.0@data$POINT_X<-coordinates(xy.BAU.a14.0)[,1]
  xy.BAU.a14.0@data$POINT_Y<-coordinates(xy.BAU.a14.0)[,2]
  
  xy.BAU.a14.1@data$Abund<-BAU.a14.1$Abund
  xy.BAU.a14.1@data$POINT_X<-coordinates(xy.BAU.a14.1)[,1]
  xy.BAU.a14.1@data$POINT_Y<-coordinates(xy.BAU.a14.1)[,2]
  
  xy.BAU.a14.2@data$Abund<-BAU.a14.2$Abund
  xy.BAU.a14.2@data$POINT_X<-coordinates(xy.BAU.a14.2)[,1]
  xy.BAU.a14.2@data$POINT_Y<-coordinates(xy.BAU.a14.2)[,2]
  
  xy.BAU.a14.5@data$Abund<-BAU.a14.5$Abund
  xy.BAU.a14.5@data$POINT_X<-coordinates(xy.BAU.a14.5)[,1]
  xy.BAU.a14.5@data$POINT_Y<-coordinates(xy.BAU.a14.5)[,2]
  
  #a15
  xy.BAU.a15.0<-subset(xy[xy@data$QSID %in% BAU.a15.0$X, ])
  xy.BAU.a15.1<-subset(xy[xy@data$QSID %in% BAU.a15.1$X, ])
  xy.BAU.a15.2<-subset(xy[xy@data$QSID %in% BAU.a15.2$X, ])
  xy.BAU.a15.5<-subset(xy[xy@data$QSID %in% BAU.a15.5$X, ])
  
  xy.BAU.a15.0@data$Abund<-BAU.a15.0$Abund
  xy.BAU.a15.0@data$POINT_X<-coordinates(xy.BAU.a15.0)[,1]
  xy.BAU.a15.0@data$POINT_Y<-coordinates(xy.BAU.a15.0)[,2]
  
  xy.BAU.a15.1@data$Abund<-BAU.a15.1$Abund
  xy.BAU.a15.1@data$POINT_X<-coordinates(xy.BAU.a15.1)[,1]
  xy.BAU.a15.1@data$POINT_Y<-coordinates(xy.BAU.a15.1)[,2]
  
  xy.BAU.a15.2@data$Abund<-BAU.a15.2$Abund
  xy.BAU.a15.2@data$POINT_X<-coordinates(xy.BAU.a15.2)[,1]
  xy.BAU.a15.2@data$POINT_Y<-coordinates(xy.BAU.a15.2)[,2]
  
  xy.BAU.a15.5@data$Abund<-BAU.a15.5$Abund
  xy.BAU.a15.5@data$POINT_X<-coordinates(xy.BAU.a15.5)[,1]
  xy.BAU.a15.5@data$POINT_Y<-coordinates(xy.BAU.a15.5)[,2]
  
  #l1
  xy.BAU.l1.0<-subset(xy[xy@data$QSID %in% BAU.l1.0$X, ])
  xy.BAU.l1.1<-subset(xy[xy@data$QSID %in% BAU.l1.1$X, ])
  xy.BAU.l1.2<-subset(xy[xy@data$QSID %in% BAU.l1.2$X, ])
  xy.BAU.l1.5<-subset(xy[xy@data$QSID %in% BAU.l1.5$X, ])
  
  xy.BAU.l1.0@data$Abund<-BAU.l1.0$Abund
  xy.BAU.l1.0@data$POINT_X<-coordinates(xy.BAU.l1.0)[,1]
  xy.BAU.l1.0@data$POINT_Y<-coordinates(xy.BAU.l1.0)[,2]
  
  xy.BAU.l1.1@data$Abund<-BAU.l1.1$Abund
  xy.BAU.l1.1@data$POINT_X<-coordinates(xy.BAU.l1.1)[,1]
  xy.BAU.l1.1@data$POINT_Y<-coordinates(xy.BAU.l1.1)[,2]
  
  xy.BAU.l1.2@data$Abund<-BAU.l1.2$Abund
  xy.BAU.l1.2@data$POINT_X<-coordinates(xy.BAU.l1.2)[,1]
  xy.BAU.l1.2@data$POINT_Y<-coordinates(xy.BAU.l1.2)[,2]
  
  xy.BAU.l1.5@data$Abund<-BAU.l1.5$Abund
  xy.BAU.l1.5@data$POINT_X<-coordinates(xy.BAU.l1.5)[,1]
  xy.BAU.l1.5@data$POINT_Y<-coordinates(xy.BAU.l1.5)[,2]
  
  #l2
  xy.BAU.l2.0<-subset(xy[xy@data$QSID %in% BAU.l2.0$X, ])
  xy.BAU.l2.1<-subset(xy[xy@data$QSID %in% BAU.l2.1$X, ])
  xy.BAU.l2.2<-subset(xy[xy@data$QSID %in% BAU.l2.2$X, ])
  xy.BAU.l2.5<-subset(xy[xy@data$QSID %in% BAU.l2.5$X, ])
  
  xy.BAU.l2.0@data$Abund<-BAU.l2.0$Abund
  xy.BAU.l2.0@data$POINT_X<-coordinates(xy.BAU.l2.0)[,1]
  xy.BAU.l2.0@data$POINT_Y<-coordinates(xy.BAU.l2.0)[,2]
  
  xy.BAU.l2.1@data$Abund<-BAU.l2.1$Abund
  xy.BAU.l2.1@data$POINT_X<-coordinates(xy.BAU.l2.1)[,1]
  xy.BAU.l2.1@data$POINT_Y<-coordinates(xy.BAU.l2.1)[,2]
  
  xy.BAU.l2.2@data$Abund<-BAU.l2.2$Abund
  xy.BAU.l2.2@data$POINT_X<-coordinates(xy.BAU.l2.2)[,1]
  xy.BAU.l2.2@data$POINT_Y<-coordinates(xy.BAU.l2.2)[,2]
  
  xy.BAU.l2.5@data$Abund<-BAU.l2.5$Abund
  xy.BAU.l2.5@data$POINT_X<-coordinates(xy.BAU.l2.5)[,1]
  xy.BAU.l2.5@data$POINT_Y<-coordinates(xy.BAU.l2.5)[,2]
  
  #l3
  xy.BAU.l3.0<-subset(xy[xy@data$QSID %in% BAU.l3.0$X, ])
  xy.BAU.l3.1<-subset(xy[xy@data$QSID %in% BAU.l3.1$X, ])
  xy.BAU.l3.2<-subset(xy[xy@data$QSID %in% BAU.l3.2$X, ])
  xy.BAU.l3.5<-subset(xy[xy@data$QSID %in% BAU.l3.5$X, ])
  
  xy.BAU.l3.0@data$Abund<-BAU.l3.0$Abund
  xy.BAU.l3.0@data$POINT_X<-coordinates(xy.BAU.l3.0)[,1]
  xy.BAU.l3.0@data$POINT_Y<-coordinates(xy.BAU.l3.0)[,2]
  
  xy.BAU.l3.1@data$Abund<-BAU.l3.1$Abund
  xy.BAU.l3.1@data$POINT_X<-coordinates(xy.BAU.l3.1)[,1]
  xy.BAU.l3.1@data$POINT_Y<-coordinates(xy.BAU.l3.1)[,2]
  
  xy.BAU.l3.2@data$Abund<-BAU.l3.2$Abund
  xy.BAU.l3.2@data$POINT_X<-coordinates(xy.BAU.l3.2)[,1]
  xy.BAU.l3.2@data$POINT_Y<-coordinates(xy.BAU.l3.2)[,2]
  
  xy.BAU.l3.5@data$Abund<-BAU.l3.5$Abund
  xy.BAU.l3.5@data$POINT_X<-coordinates(xy.BAU.l3.5)[,1]
  xy.BAU.l3.5@data$POINT_Y<-coordinates(xy.BAU.l3.5)[,2]
  
  #l8
  xy.BAU.l8.0<-subset(xy[xy@data$QSID %in% BAU.l8.0$X, ])
  xy.BAU.l8.1<-subset(xy[xy@data$QSID %in% BAU.l8.1$X, ])
  xy.BAU.l8.2<-subset(xy[xy@data$QSID %in% BAU.l8.2$X, ])
  xy.BAU.l8.5<-subset(xy[xy@data$QSID %in% BAU.l8.5$X, ])
  
  xy.BAU.l8.0@data$Abund<-BAU.l8.0$Abund
  xy.BAU.l8.0@data$POINT_X<-coordinates(xy.BAU.l8.0)[,1]
  xy.BAU.l8.0@data$POINT_Y<-coordinates(xy.BAU.l8.0)[,2]
  
  xy.BAU.l8.1@data$Abund<-BAU.l8.1$Abund
  xy.BAU.l8.1@data$POINT_X<-coordinates(xy.BAU.l8.1)[,1]
  xy.BAU.l8.1@data$POINT_Y<-coordinates(xy.BAU.l8.1)[,2]
  
  xy.BAU.l8.2@data$Abund<-BAU.l8.2$Abund
  xy.BAU.l8.2@data$POINT_X<-coordinates(xy.BAU.l8.2)[,1]
  xy.BAU.l8.2@data$POINT_Y<-coordinates(xy.BAU.l8.2)[,2]
  
  xy.BAU.l8.5@data$Abund<-BAU.l8.5$Abund
  xy.BAU.l8.5@data$POINT_X<-coordinates(xy.BAU.l8.5)[,1]
  xy.BAU.l8.5@data$POINT_Y<-coordinates(xy.BAU.l8.5)[,2]
  
  #l11
  xy.BAU.l11.0<-subset(xy[xy@data$QSID %in% BAU.l11.0$X, ])
  xy.BAU.l11.1<-subset(xy[xy@data$QSID %in% BAU.l11.1$X, ])
  xy.BAU.l11.2<-subset(xy[xy@data$QSID %in% BAU.l11.2$X, ])
  xy.BAU.l11.5<-subset(xy[xy@data$QSID %in% BAU.l11.5$X, ])
  
  xy.BAU.l11.0@data$Abund<-BAU.l11.0$Abund
  xy.BAU.l11.0@data$POINT_X<-coordinates(xy.BAU.l11.0)[,1]
  xy.BAU.l11.0@data$POINT_Y<-coordinates(xy.BAU.l11.0)[,2]
  
  xy.BAU.l11.1@data$Abund<-BAU.l11.1$Abund
  xy.BAU.l11.1@data$POINT_X<-coordinates(xy.BAU.l11.1)[,1]
  xy.BAU.l11.1@data$POINT_Y<-coordinates(xy.BAU.l11.1)[,2]
  
  xy.BAU.l11.2@data$Abund<-BAU.l11.2$Abund
  xy.BAU.l11.2@data$POINT_X<-coordinates(xy.BAU.l11.2)[,1]
  xy.BAU.l11.2@data$POINT_Y<-coordinates(xy.BAU.l11.2)[,2]
  
  xy.BAU.l11.5@data$Abund<-BAU.l11.5$Abund
  xy.BAU.l11.5@data$POINT_X<-coordinates(xy.BAU.l11.5)[,1]
  xy.BAU.l11.5@data$POINT_Y<-coordinates(xy.BAU.l11.5)[,2]
  
  #s11
  xy.BAU.s11.0<-subset(xy[xy@data$QSID %in% BAU.s11.0$X, ])
  xy.BAU.s11.1<-subset(xy[xy@data$QSID %in% BAU.s11.1$X, ])
  xy.BAU.s11.2<-subset(xy[xy@data$QSID %in% BAU.s11.2$X, ])
  xy.BAU.s11.5<-subset(xy[xy@data$QSID %in% BAU.s11.5$X, ])
  
  xy.BAU.s11.0@data$Abund<-BAU.s11.0$Abund
  xy.BAU.s11.0@data$POINT_X<-coordinates(xy.BAU.s11.0)[,1]
  xy.BAU.s11.0@data$POINT_Y<-coordinates(xy.BAU.s11.0)[,2]
  
  xy.BAU.s11.1@data$Abund<-BAU.s11.1$Abund
  xy.BAU.s11.1@data$POINT_X<-coordinates(xy.BAU.s11.1)[,1]
  xy.BAU.s11.1@data$POINT_Y<-coordinates(xy.BAU.s11.1)[,2]
  
  xy.BAU.s11.2@data$Abund<-BAU.s11.2$Abund
  xy.BAU.s11.2@data$POINT_X<-coordinates(xy.BAU.s11.2)[,1]
  xy.BAU.s11.2@data$POINT_Y<-coordinates(xy.BAU.s11.2)[,2]
  
  xy.BAU.s11.5@data$Abund<-BAU.s11.5$Abund
  xy.BAU.s11.5@data$POINT_X<-coordinates(xy.BAU.s11.5)[,1]
  xy.BAU.s11.5@data$POINT_Y<-coordinates(xy.BAU.s11.5)[,2]
  
  #s14
  xy.BAU.s14.0<-subset(xy[xy@data$QSID %in% BAU.s14.0$X, ])
  xy.BAU.s14.1<-subset(xy[xy@data$QSID %in% BAU.s14.1$X, ])
  xy.BAU.s14.2<-subset(xy[xy@data$QSID %in% BAU.s14.2$X, ])
  xy.BAU.s14.5<-subset(xy[xy@data$QSID %in% BAU.s14.5$X, ])
  
  xy.BAU.s14.0@data$Abund<-BAU.s14.0$Abund
  xy.BAU.s14.0@data$POINT_X<-coordinates(xy.BAU.s14.0)[,1]
  xy.BAU.s14.0@data$POINT_Y<-coordinates(xy.BAU.s14.0)[,2]
  
  xy.BAU.s14.1@data$Abund<-BAU.s14.1$Abund
  xy.BAU.s14.1@data$POINT_X<-coordinates(xy.BAU.s14.1)[,1]
  xy.BAU.s14.1@data$POINT_Y<-coordinates(xy.BAU.s14.1)[,2]
  
  xy.BAU.s14.2@data$Abund<-BAU.s14.2$Abund
  xy.BAU.s14.2@data$POINT_X<-coordinates(xy.BAU.s14.2)[,1]
  xy.BAU.s14.2@data$POINT_Y<-coordinates(xy.BAU.s14.2)[,2]
  
  xy.BAU.s14.5@data$Abund<-BAU.s14.5$Abund
  xy.BAU.s14.5@data$POINT_X<-coordinates(xy.BAU.s14.5)[,1]
  xy.BAU.s14.5@data$POINT_Y<-coordinates(xy.BAU.s14.5)[,2]
  
  #s18
  xy.BAU.s18.0<-subset(xy[xy@data$QSID %in% BAU.s18.0$X, ])
  xy.BAU.s18.1<-subset(xy[xy@data$QSID %in% BAU.s18.1$X, ])
  xy.BAU.s18.2<-subset(xy[xy@data$QSID %in% BAU.s18.2$X, ])
  xy.BAU.s18.5<-subset(xy[xy@data$QSID %in% BAU.s18.5$X, ])
  
  xy.BAU.s18.0@data$Abund<-BAU.s18.0$Abund
  xy.BAU.s18.0@data$POINT_X<-coordinates(xy.BAU.s18.0)[,1]
  xy.BAU.s18.0@data$POINT_Y<-coordinates(xy.BAU.s18.0)[,2]
  
  xy.BAU.s18.1@data$Abund<-BAU.s18.1$Abund
  xy.BAU.s18.1@data$POINT_X<-coordinates(xy.BAU.s18.1)[,1]
  xy.BAU.s18.1@data$POINT_Y<-coordinates(xy.BAU.s18.1)[,2]
  
  xy.BAU.s18.2@data$Abund<-BAU.s18.2$Abund
  xy.BAU.s18.2@data$POINT_X<-coordinates(xy.BAU.s18.2)[,1]
  xy.BAU.s18.2@data$POINT_Y<-coordinates(xy.BAU.s18.2)[,2]
  
  xy.BAU.s18.5@data$Abund<-BAU.s18.5$Abund
  xy.BAU.s18.5@data$POINT_X<-coordinates(xy.BAU.s18.5)[,1]
  xy.BAU.s18.5@data$POINT_Y<-coordinates(xy.BAU.s18.5)[,2]
  
  #s22
  xy.BAU.s22.0<-subset(xy[xy@data$QSID %in% BAU.s22.0$X, ])
  xy.BAU.s22.1<-subset(xy[xy@data$QSID %in% BAU.s22.1$X, ])
  xy.BAU.s22.2<-subset(xy[xy@data$QSID %in% BAU.s22.2$X, ])
  xy.BAU.s22.5<-subset(xy[xy@data$QSID %in% BAU.s22.5$X, ])
  
  xy.BAU.s22.0@data$Abund<-BAU.s22.0$Abund
  xy.BAU.s22.0@data$POINT_X<-coordinates(xy.BAU.s22.0)[,1]
  xy.BAU.s22.0@data$POINT_Y<-coordinates(xy.BAU.s22.0)[,2]
  
  xy.BAU.s22.1@data$Abund<-BAU.s22.1$Abund
  xy.BAU.s22.1@data$POINT_X<-coordinates(xy.BAU.s22.1)[,1]
  xy.BAU.s22.1@data$POINT_Y<-coordinates(xy.BAU.s22.1)[,2]
  
  xy.BAU.s22.2@data$Abund<-BAU.s22.2$Abund
  xy.BAU.s22.2@data$POINT_X<-coordinates(xy.BAU.s22.2)[,1]
  xy.BAU.s22.2@data$POINT_Y<-coordinates(xy.BAU.s22.2)[,2]
  
  xy.BAU.s22.5@data$Abund<-BAU.s22.5$Abund
  xy.BAU.s22.5@data$POINT_X<-coordinates(xy.BAU.s22.5)[,1]
  xy.BAU.s22.5@data$POINT_Y<-coordinates(xy.BAU.s22.5)[,2]
  
  #s23
  xy.BAU.s23.0<-subset(xy[xy@data$QSID %in% BAU.s23.0$X, ])
  xy.BAU.s23.1<-subset(xy[xy@data$QSID %in% BAU.s23.1$X, ])
  xy.BAU.s23.2<-subset(xy[xy@data$QSID %in% BAU.s23.2$X, ])
  xy.BAU.s23.5<-subset(xy[xy@data$QSID %in% BAU.s23.5$X, ])
  
  xy.BAU.s23.0@data$Abund<-BAU.s23.0$Abund
  xy.BAU.s23.0@data$POINT_X<-coordinates(xy.BAU.s23.0)[,1]
  xy.BAU.s23.0@data$POINT_Y<-coordinates(xy.BAU.s23.0)[,2]
  
  xy.BAU.s23.1@data$Abund<-BAU.s23.1$Abund
  xy.BAU.s23.1@data$POINT_X<-coordinates(xy.BAU.s23.1)[,1]
  xy.BAU.s23.1@data$POINT_Y<-coordinates(xy.BAU.s23.1)[,2]
  
  xy.BAU.s23.2@data$Abund<-BAU.s23.2$Abund
  xy.BAU.s23.2@data$POINT_X<-coordinates(xy.BAU.s23.2)[,1]
  xy.BAU.s23.2@data$POINT_Y<-coordinates(xy.BAU.s23.2)[,2]
  
  xy.BAU.s23.5@data$Abund<-BAU.s23.5$Abund
  xy.BAU.s23.5@data$POINT_X<-coordinates(xy.BAU.s23.5)[,1]
  xy.BAU.s23.5@data$POINT_Y<-coordinates(xy.BAU.s23.5)[,2]
  
  #a14
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU (EBM/NDM) Scenario
  mergeCarBAU.a14.0<-merge(xy.caribou.a14.0@data,xy.BAU.a14.0@data, by=c("QSID"))
  mergeCarBAU.a14.1<-merge(xy.caribou.a14.1@data,xy.BAU.a14.1@data, by=c("QSID"))
  mergeCarBAU.a14.2<-merge(xy.caribou.a14.2@data,xy.BAU.a14.2@data, by=c("QSID"))
  mergeCarBAU.a14.5<-merge(xy.caribou.a14.5@data,xy.BAU.a14.5@data, by=c("QSID"))
  
  #Now read in NRV file for the same species as the Caribou and BAU Scenarios
  NRVabund <- read.csv(file=paste0("0_data/processed/nrv/",SPP,"/NRVabund",SPP,".csv"), header=TRUE)
  str(NRVabund)
  #NRVabund$L50CI.CAWA<-NRVabund$Mean.CAWA-0.67*NRVabund$SE.CAWA
  #NRVabund$U50CI.CAWA<-NRVabund$Mean.CAWA+0.67*NRVabund$SE.CAWA
  
  NRVabund$QSID<-NRVabund$X#create linking variable
  mergeCarBAU.NRV.a14.0<-merge(mergeCarBAU.a14.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a14.0)#13164 obs
  str(mergeCarBAU.NRV.a14.0)
  mergeCarBAU.NRV.a14.0$Abund.caribou<-mergeCarBAU.NRV.a14.0$Abund.x
  mergeCarBAU.NRV.a14.0$Abund.BAU<-mergeCarBAU.NRV.a14.0$Abund.y
  
  mergeCarBAU.NRV.a14.1<-merge(mergeCarBAU.a14.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a14.1)#13164 obs
  str(mergeCarBAU.NRV.a14.1)
  mergeCarBAU.NRV.a14.1$Abund.caribou<-mergeCarBAU.NRV.a14.1$Abund.x
  mergeCarBAU.NRV.a14.1$Abund.BAU<-mergeCarBAU.NRV.a14.1$Abund.y
  
  mergeCarBAU.NRV.a14.2<-merge(mergeCarBAU.a14.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a14.2)#13164 obs
  str(mergeCarBAU.NRV.a14.2)
  mergeCarBAU.NRV.a14.2$Abund.caribou<-mergeCarBAU.NRV.a14.2$Abund.x
  mergeCarBAU.NRV.a14.2$Abund.BAU<-mergeCarBAU.NRV.a14.2$Abund.y
  
  mergeCarBAU.NRV.a14.5<-merge(mergeCarBAU.a14.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a14.5)#13164 obs
  str(mergeCarBAU.NRV.a14.5)
  mergeCarBAU.NRV.a14.5$Abund.caribou<-mergeCarBAU.NRV.a14.5$Abund.x
  mergeCarBAU.NRV.a14.5$Abund.BAU<-mergeCarBAU.NRV.a14.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.a14.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.a14.5.csv"))
  #data check
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.a14.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.a14.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.a14.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.a14.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.a14.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.a14.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.a14.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.a14.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.a14.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.a14.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.a14.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.a14.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.a14.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.a14.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.a14.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.a14.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.a14.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.a14.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.a14.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.a14.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.a14.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.a14.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.a14.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.a14.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.a14.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.a14.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.a14.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.a14.5$U95CI)
  
  a14comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(a14comparison,file=paste0("3_output/tables/",SPP,"a14comparison.csv"))
  
  
  a14 <- ggplot(data=a14comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (A14)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  a14
  #ggsave(filename=paste0("3_output/figures/",SPP,".a14.png"), plot=a14)
  
  #a15
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU (EBM/NDM) Scenario
  mergeCarBAU.a15.0<-merge(xy.caribou.a15.0@data,xy.BAU.a15.0@data, by=c("QSID"))
  mergeCarBAU.a15.1<-merge(xy.caribou.a15.1@data,xy.BAU.a15.1@data, by=c("QSID"))
  mergeCarBAU.a15.2<-merge(xy.caribou.a15.2@data,xy.BAU.a15.2@data, by=c("QSID"))
  mergeCarBAU.a15.5<-merge(xy.caribou.a15.5@data,xy.BAU.a15.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.a15.0<-merge(mergeCarBAU.a15.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a15.0)#13164 obs
  str(mergeCarBAU.NRV.a15.0)
  mergeCarBAU.NRV.a15.0$Abund.caribou<-mergeCarBAU.NRV.a15.0$Abund.x
  mergeCarBAU.NRV.a15.0$Abund.BAU<-mergeCarBAU.NRV.a15.0$Abund.y
  
  mergeCarBAU.NRV.a15.1<-merge(mergeCarBAU.a15.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a15.1)#13164 obs
  str(mergeCarBAU.NRV.a15.1)
  mergeCarBAU.NRV.a15.1$Abund.caribou<-mergeCarBAU.NRV.a15.1$Abund.x
  mergeCarBAU.NRV.a15.1$Abund.BAU<-mergeCarBAU.NRV.a15.1$Abund.y
  
  mergeCarBAU.NRV.a15.2<-merge(mergeCarBAU.a15.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a15.2)#13164 obs
  str(mergeCarBAU.NRV.a15.2)
  mergeCarBAU.NRV.a15.2$Abund.caribou<-mergeCarBAU.NRV.a15.2$Abund.x
  mergeCarBAU.NRV.a15.2$Abund.BAU<-mergeCarBAU.NRV.a15.2$Abund.y
  
  mergeCarBAU.NRV.a15.5<-merge(mergeCarBAU.a15.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.a15.5)#13164 obs
  str(mergeCarBAU.NRV.a15.5)
  mergeCarBAU.NRV.a15.5$Abund.caribou<-mergeCarBAU.NRV.a15.5$Abund.x
  mergeCarBAU.NRV.a15.5$Abund.BAU<-mergeCarBAU.NRV.a15.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.a15.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.a15.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.a15.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.a15.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.a15.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.a15.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.a15.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.a15.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.a15.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.a15.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.a15.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.a15.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.a15.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.a15.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.a15.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.a15.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.a15.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.a15.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.a15.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.a15.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.a15.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.a15.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.a15.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.a15.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.a15.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.a15.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.a15.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.a15.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.a15.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.a15.5$U95CI)
  
  a15comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(a15comparison,file=paste0("3_output/tables/",SPP,"a15comparison.csv"))
  #data check
  
  a15 <- ggplot(data=a15comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (A15)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  a15
  #ggsave(filename=paste0("3_output/figures/",SPP,".a15.png"), plot=a15)
  
  #l1
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.l1.0<-merge(xy.caribou.l1.0@data,xy.BAU.l1.0@data, by=c("QSID"))
  mergeCarBAU.l1.1<-merge(xy.caribou.l1.1@data,xy.BAU.l1.1@data, by=c("QSID"))
  mergeCarBAU.l1.2<-merge(xy.caribou.l1.2@data,xy.BAU.l1.2@data, by=c("QSID"))
  mergeCarBAU.l1.5<-merge(xy.caribou.l1.5@data,xy.BAU.l1.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.l1.0<-merge(mergeCarBAU.l1.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l1.0)#13164 obs
  str(mergeCarBAU.NRV.l1.0)
  mergeCarBAU.NRV.l1.0$Abund.caribou<-mergeCarBAU.NRV.l1.0$Abund.x
  mergeCarBAU.NRV.l1.0$Abund.BAU<-mergeCarBAU.NRV.l1.0$Abund.y
  
  mergeCarBAU.NRV.l1.1<-merge(mergeCarBAU.l1.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l1.1)#13164 obs
  str(mergeCarBAU.NRV.l1.1)
  mergeCarBAU.NRV.l1.1$Abund.caribou<-mergeCarBAU.NRV.l1.1$Abund.x
  mergeCarBAU.NRV.l1.1$Abund.BAU<-mergeCarBAU.NRV.l1.1$Abund.y
  
  mergeCarBAU.NRV.l1.2<-merge(mergeCarBAU.l1.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l1.2)#13164 obs
  str(mergeCarBAU.NRV.l1.2)
  mergeCarBAU.NRV.l1.2$Abund.caribou<-mergeCarBAU.NRV.l1.2$Abund.x
  mergeCarBAU.NRV.l1.2$Abund.BAU<-mergeCarBAU.NRV.l1.2$Abund.y
  
  mergeCarBAU.NRV.l1.5<-merge(mergeCarBAU.l1.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l1.5)#13164 obs
  str(mergeCarBAU.NRV.l1.5)
  mergeCarBAU.NRV.l1.5$Abund.caribou<-mergeCarBAU.NRV.l1.5$Abund.x
  mergeCarBAU.NRV.l1.5$Abund.BAU<-mergeCarBAU.NRV.l1.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.l1.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.l1.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.l1.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.l1.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.l1.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.l1.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.l1.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.l1.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.l1.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.l1.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.l1.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.l1.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.l1.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.l1.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.l1.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.l1.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.l1.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.l1.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.l1.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.l1.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.l1.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.l1.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.l1.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.l1.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.l1.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.l1.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.l1.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.l1.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.l1.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.l1.5$U95CI)
  
  l1comparison<-data.frame(Year=c(0,10,20,50),
                           Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                           Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                           Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                           L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                           U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                           L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                           U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(l1comparison,file=paste0("3_output/tables/",SPP,"l1comparison.csv"))
  
  
  l1 <- ggplot(data=l1comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (L1)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  l1
  #ggsave(filename=paste0("3_output/figures/",SPP,".l1.png"), plot=l1)
  
  #l2
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.l2.0<-merge(xy.caribou.l2.0@data,xy.BAU.l2.0@data, by=c("QSID"))
  mergeCarBAU.l2.1<-merge(xy.caribou.l2.1@data,xy.BAU.l2.1@data, by=c("QSID"))
  mergeCarBAU.l2.2<-merge(xy.caribou.l2.2@data,xy.BAU.l2.2@data, by=c("QSID"))
  mergeCarBAU.l2.5<-merge(xy.caribou.l2.5@data,xy.BAU.l2.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.l2.0<-merge(mergeCarBAU.l2.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l2.0)#13164 obs
  str(mergeCarBAU.NRV.l2.0)
  mergeCarBAU.NRV.l2.0$Abund.caribou<-mergeCarBAU.NRV.l2.0$Abund.x
  mergeCarBAU.NRV.l2.0$Abund.BAU<-mergeCarBAU.NRV.l2.0$Abund.y
  
  mergeCarBAU.NRV.l2.1<-merge(mergeCarBAU.l2.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l2.1)#13164 obs
  str(mergeCarBAU.NRV.l2.1)
  mergeCarBAU.NRV.l2.1$Abund.caribou<-mergeCarBAU.NRV.l2.1$Abund.x
  mergeCarBAU.NRV.l2.1$Abund.BAU<-mergeCarBAU.NRV.l2.1$Abund.y
  
  mergeCarBAU.NRV.l2.2<-merge(mergeCarBAU.l2.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l2.2)#13164 obs
  str(mergeCarBAU.NRV.l2.2)
  mergeCarBAU.NRV.l2.2$Abund.caribou<-mergeCarBAU.NRV.l2.2$Abund.x
  mergeCarBAU.NRV.l2.2$Abund.BAU<-mergeCarBAU.NRV.l2.2$Abund.y
  
  mergeCarBAU.NRV.l2.5<-merge(mergeCarBAU.l2.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l2.5)#13164 obs
  str(mergeCarBAU.NRV.l2.5)
  mergeCarBAU.NRV.l2.5$Abund.caribou<-mergeCarBAU.NRV.l2.5$Abund.x
  mergeCarBAU.NRV.l2.5$Abund.BAU<-mergeCarBAU.NRV.l2.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.l2.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.l2.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.l2.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.l2.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.l2.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.l2.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.l2.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.l2.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.l2.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.l2.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.l2.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.l2.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.l2.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.l2.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.l2.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.l2.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.l2.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.l2.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.l2.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.l2.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.l2.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.l2.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.l2.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.l2.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.l2.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.l2.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.l2.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.l2.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.l2.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.l2.5$U95CI)
  
  l2comparison<-data.frame(Year=c(0,10,20,50),
                           Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                           Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                           Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                           L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                           U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                           L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                           U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(l2comparison,file=paste0("3_output/tables/",SPP,"l2comparison.csv"))
  
  
  l2 <- ggplot(data=l2comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (L2)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  l2
  #ggsave(filename=paste0("3_output/figures/",SPP,".l2.png"), plot=l2)
  
  
  #l3
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.l3.0<-merge(xy.caribou.l3.0@data,xy.BAU.l3.0@data, by=c("QSID"))
  mergeCarBAU.l3.1<-merge(xy.caribou.l3.1@data,xy.BAU.l3.1@data, by=c("QSID"))
  mergeCarBAU.l3.2<-merge(xy.caribou.l3.2@data,xy.BAU.l3.2@data, by=c("QSID"))
  mergeCarBAU.l3.5<-merge(xy.caribou.l3.5@data,xy.BAU.l3.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.l3.0<-merge(mergeCarBAU.l3.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l3.0)#13164 obs
  str(mergeCarBAU.NRV.l3.0)
  mergeCarBAU.NRV.l3.0$Abund.caribou<-mergeCarBAU.NRV.l3.0$Abund.x
  mergeCarBAU.NRV.l3.0$Abund.BAU<-mergeCarBAU.NRV.l3.0$Abund.y
  
  mergeCarBAU.NRV.l3.1<-merge(mergeCarBAU.l3.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l3.1)#13164 obs
  str(mergeCarBAU.NRV.l3.1)
  mergeCarBAU.NRV.l3.1$Abund.caribou<-mergeCarBAU.NRV.l3.1$Abund.x
  mergeCarBAU.NRV.l3.1$Abund.BAU<-mergeCarBAU.NRV.l3.1$Abund.y
  
  mergeCarBAU.NRV.l3.2<-merge(mergeCarBAU.l3.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l3.2)#13164 obs
  str(mergeCarBAU.NRV.l3.2)
  mergeCarBAU.NRV.l3.2$Abund.caribou<-mergeCarBAU.NRV.l3.2$Abund.x
  mergeCarBAU.NRV.l3.2$Abund.BAU<-mergeCarBAU.NRV.l3.2$Abund.y
  
  mergeCarBAU.NRV.l3.5<-merge(mergeCarBAU.l3.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l3.5)#13164 obs
  str(mergeCarBAU.NRV.l3.5)
  mergeCarBAU.NRV.l3.5$Abund.caribou<-mergeCarBAU.NRV.l3.5$Abund.x
  mergeCarBAU.NRV.l3.5$Abund.BAU<-mergeCarBAU.NRV.l3.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.l3.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.l3.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.l3.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.l3.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.l3.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.l3.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.l3.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.l3.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.l3.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.l3.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.l3.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.l3.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.l3.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.l3.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.l3.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.l3.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.l3.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.l3.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.l3.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.l3.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.l3.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.l3.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.l3.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.l3.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.l3.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.l3.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.l3.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.l3.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.l3.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.l3.5$U95CI)
  
  l3comparison<-data.frame(Year=c(0,10,20,50),
                           Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                           Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                           Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                           L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                           U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                           L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                           U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(l3comparison,file=paste0("3_output/tables/",SPP,"l3comparison.csv"))
  
  
  l3 <- ggplot(data=l3comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (L3)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  l3
  #ggsave(filename=paste0("3_output/figures/",SPP,".l3.png"), plot=l3)
  
  #l8
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.l8.0<-merge(xy.caribou.l8.0@data,xy.BAU.l8.0@data, by=c("QSID"))
  mergeCarBAU.l8.1<-merge(xy.caribou.l8.1@data,xy.BAU.l8.1@data, by=c("QSID"))
  mergeCarBAU.l8.2<-merge(xy.caribou.l8.2@data,xy.BAU.l8.2@data, by=c("QSID"))
  mergeCarBAU.l8.5<-merge(xy.caribou.l8.5@data,xy.BAU.l8.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.l8.0<-merge(mergeCarBAU.l8.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l8.0)#13164 obs
  str(mergeCarBAU.NRV.l8.0)
  mergeCarBAU.NRV.l8.0$Abund.caribou<-mergeCarBAU.NRV.l8.0$Abund.x
  mergeCarBAU.NRV.l8.0$Abund.BAU<-mergeCarBAU.NRV.l8.0$Abund.y
  
  mergeCarBAU.NRV.l8.1<-merge(mergeCarBAU.l8.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l8.1)#13164 obs
  str(mergeCarBAU.NRV.l8.1)
  mergeCarBAU.NRV.l8.1$Abund.caribou<-mergeCarBAU.NRV.l8.1$Abund.x
  mergeCarBAU.NRV.l8.1$Abund.BAU<-mergeCarBAU.NRV.l8.1$Abund.y
  
  mergeCarBAU.NRV.l8.2<-merge(mergeCarBAU.l8.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l8.2)#13164 obs
  str(mergeCarBAU.NRV.l8.2)
  mergeCarBAU.NRV.l8.2$Abund.caribou<-mergeCarBAU.NRV.l8.2$Abund.x
  mergeCarBAU.NRV.l8.2$Abund.BAU<-mergeCarBAU.NRV.l8.2$Abund.y
  
  mergeCarBAU.NRV.l8.5<-merge(mergeCarBAU.l8.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l8.5)#13164 obs
  str(mergeCarBAU.NRV.l8.5)
  mergeCarBAU.NRV.l8.5$Abund.caribou<-mergeCarBAU.NRV.l8.5$Abund.x
  mergeCarBAU.NRV.l8.5$Abund.BAU<-mergeCarBAU.NRV.l8.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.l8.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.l8.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.l8.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.l8.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.l8.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.l8.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.l8.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.l8.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.l8.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.l8.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.l8.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.l8.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.l8.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.l8.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.l8.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.l8.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.l8.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.l8.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.l8.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.l8.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.l8.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.l8.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.l8.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.l8.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.l8.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.l8.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.l8.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.l8.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.l8.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.l8.5$U95CI)
  
  l8comparison<-data.frame(Year=c(0,10,20,50),
                           Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                           Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                           Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                           L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                           U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                           L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                           U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(l8comparison,file=paste0("3_output/tables/",SPP,"l8comparison.csv"))
  
  
  l8 <- ggplot(data=l8comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (L8)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  l8
  #ggsave(filename=paste0("3_output/figures/",SPP,".l8.png"), plot=l8)
  
  
  
  #l11
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.l11.0<-merge(xy.caribou.l11.0@data,xy.BAU.l11.0@data, by=c("QSID"))
  mergeCarBAU.l11.1<-merge(xy.caribou.l11.1@data,xy.BAU.l11.1@data, by=c("QSID"))
  mergeCarBAU.l11.2<-merge(xy.caribou.l11.2@data,xy.BAU.l11.2@data, by=c("QSID"))
  mergeCarBAU.l11.5<-merge(xy.caribou.l11.5@data,xy.BAU.l11.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.l11.0<-merge(mergeCarBAU.l11.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l11.0)#13164 obs
  str(mergeCarBAU.NRV.l11.0)
  mergeCarBAU.NRV.l11.0$Abund.caribou<-mergeCarBAU.NRV.l11.0$Abund.x
  mergeCarBAU.NRV.l11.0$Abund.BAU<-mergeCarBAU.NRV.l11.0$Abund.y
  
  mergeCarBAU.NRV.l11.1<-merge(mergeCarBAU.l11.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l11.1)#13164 obs
  str(mergeCarBAU.NRV.l11.1)
  mergeCarBAU.NRV.l11.1$Abund.caribou<-mergeCarBAU.NRV.l11.1$Abund.x
  mergeCarBAU.NRV.l11.1$Abund.BAU<-mergeCarBAU.NRV.l11.1$Abund.y
  
  mergeCarBAU.NRV.l11.2<-merge(mergeCarBAU.l11.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l11.2)#13164 obs
  str(mergeCarBAU.NRV.l11.2)
  mergeCarBAU.NRV.l11.2$Abund.caribou<-mergeCarBAU.NRV.l11.2$Abund.x
  mergeCarBAU.NRV.l11.2$Abund.BAU<-mergeCarBAU.NRV.l11.2$Abund.y
  
  mergeCarBAU.NRV.l11.5<-merge(mergeCarBAU.l11.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.l11.5)#13164 obs
  str(mergeCarBAU.NRV.l11.5)
  mergeCarBAU.NRV.l11.5$Abund.caribou<-mergeCarBAU.NRV.l11.5$Abund.x
  mergeCarBAU.NRV.l11.5$Abund.BAU<-mergeCarBAU.NRV.l11.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.l11.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.l11.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.l11.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.l11.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.l11.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.l11.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.l11.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.l11.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.l11.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.l11.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.l11.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.l11.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.l11.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.l11.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.l11.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.l11.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.l11.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.l11.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.l11.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.l11.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.l11.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.l11.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.l11.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.l11.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.l11.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.l11.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.l11.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.l11.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.l11.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.l11.5$U95CI)
  
  l11comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(l11comparison,file=paste0("3_output/tables/",SPP,"l11comparison.csv"))
  
  
  l11 <- ggplot(data=l11comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (L11)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  l11
  #ggsave(filename=paste0("3_output/figures/",SPP,".l11.png"), plot=l11)
  
  
  
  #s11
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.s11.0<-merge(xy.caribou.s11.0@data,xy.BAU.s11.0@data, by=c("QSID"))
  mergeCarBAU.s11.1<-merge(xy.caribou.s11.1@data,xy.BAU.s11.1@data, by=c("QSID"))
  mergeCarBAU.s11.2<-merge(xy.caribou.s11.2@data,xy.BAU.s11.2@data, by=c("QSID"))
  mergeCarBAU.s11.5<-merge(xy.caribou.s11.5@data,xy.BAU.s11.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.s11.0<-merge(mergeCarBAU.s11.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s11.0)#13164 obs
  str(mergeCarBAU.NRV.s11.0)
  mergeCarBAU.NRV.s11.0$Abund.caribou<-mergeCarBAU.NRV.s11.0$Abund.x
  mergeCarBAU.NRV.s11.0$Abund.BAU<-mergeCarBAU.NRV.s11.0$Abund.y
  
  mergeCarBAU.NRV.s11.1<-merge(mergeCarBAU.s11.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s11.1)#13164 obs
  str(mergeCarBAU.NRV.s11.1)
  mergeCarBAU.NRV.s11.1$Abund.caribou<-mergeCarBAU.NRV.s11.1$Abund.x
  mergeCarBAU.NRV.s11.1$Abund.BAU<-mergeCarBAU.NRV.s11.1$Abund.y
  
  mergeCarBAU.NRV.s11.2<-merge(mergeCarBAU.s11.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s11.2)#13164 obs
  str(mergeCarBAU.NRV.s11.2)
  mergeCarBAU.NRV.s11.2$Abund.caribou<-mergeCarBAU.NRV.s11.2$Abund.x
  mergeCarBAU.NRV.s11.2$Abund.BAU<-mergeCarBAU.NRV.s11.2$Abund.y
  
  mergeCarBAU.NRV.s11.5<-merge(mergeCarBAU.s11.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s11.5)#13164 obs
  str(mergeCarBAU.NRV.s11.5)
  mergeCarBAU.NRV.s11.5$Abund.caribou<-mergeCarBAU.NRV.s11.5$Abund.x
  mergeCarBAU.NRV.s11.5$Abund.BAU<-mergeCarBAU.NRV.s11.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.s11.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.s11.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.s11.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.s11.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.s11.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.s11.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.s11.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.s11.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.s11.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.s11.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.s11.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.s11.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.s11.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.s11.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.s11.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.s11.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.s11.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.s11.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.s11.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.s11.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.s11.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.s11.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.s11.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.s11.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.s11.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.s11.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.s11.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.s11.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.s11.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.s11.5$U95CI)
  
  s11comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(s11comparison,file=paste0("3_output/tables/",SPP,"s11comparison.csv"))
  
  
  s11 <- ggplot(data=s11comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (S11)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  s11
  #ggsave(filename=paste0("3_output/figures/",SPP,".s11.png"), plot=s11)
  
  
  
  
  #s14
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.s14.0<-merge(xy.caribou.s14.0@data,xy.BAU.s14.0@data, by=c("QSID"))
  mergeCarBAU.s14.1<-merge(xy.caribou.s14.1@data,xy.BAU.s14.1@data, by=c("QSID"))
  mergeCarBAU.s14.2<-merge(xy.caribou.s14.2@data,xy.BAU.s14.2@data, by=c("QSID"))
  mergeCarBAU.s14.5<-merge(xy.caribou.s14.5@data,xy.BAU.s14.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.s14.0<-merge(mergeCarBAU.s14.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s14.0)#13164 obs
  str(mergeCarBAU.NRV.s14.0)
  mergeCarBAU.NRV.s14.0$Abund.caribou<-mergeCarBAU.NRV.s14.0$Abund.x
  mergeCarBAU.NRV.s14.0$Abund.BAU<-mergeCarBAU.NRV.s14.0$Abund.y
  
  mergeCarBAU.NRV.s14.1<-merge(mergeCarBAU.s14.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s14.1)#13164 obs
  str(mergeCarBAU.NRV.s14.1)
  mergeCarBAU.NRV.s14.1$Abund.caribou<-mergeCarBAU.NRV.s14.1$Abund.x
  mergeCarBAU.NRV.s14.1$Abund.BAU<-mergeCarBAU.NRV.s14.1$Abund.y
  
  mergeCarBAU.NRV.s14.2<-merge(mergeCarBAU.s14.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s14.2)#13164 obs
  str(mergeCarBAU.NRV.s14.2)
  mergeCarBAU.NRV.s14.2$Abund.caribou<-mergeCarBAU.NRV.s14.2$Abund.x
  mergeCarBAU.NRV.s14.2$Abund.BAU<-mergeCarBAU.NRV.s14.2$Abund.y
  
  mergeCarBAU.NRV.s14.5<-merge(mergeCarBAU.s14.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s14.5)#13164 obs
  str(mergeCarBAU.NRV.s14.5)
  mergeCarBAU.NRV.s14.5$Abund.caribou<-mergeCarBAU.NRV.s14.5$Abund.x
  mergeCarBAU.NRV.s14.5$Abund.BAU<-mergeCarBAU.NRV.s14.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.s14.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.s14.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.s14.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.s14.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.s14.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.s14.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.s14.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.s14.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.s14.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.s14.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.s14.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.s14.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.s14.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.s14.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.s14.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.s14.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.s14.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.s14.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.s14.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.s14.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.s14.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.s14.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.s14.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.s14.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.s14.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.s14.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.s14.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.s14.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.s14.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.s14.5$U95CI)
  
  s14comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(s14comparison,file=paste0("3_output/tables/",SPP,"s14comparison.csv"))
  
  
  s14 <- ggplot(data=s14comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (S14)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  s14
  #ggsave(filename=paste0("3_output/figures/",SPP,".s14.png"), plot=s14)
  
  
  #s18
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.s18.0<-merge(xy.caribou.s18.0@data,xy.BAU.s18.0@data, by=c("QSID"))
  mergeCarBAU.s18.1<-merge(xy.caribou.s18.1@data,xy.BAU.s18.1@data, by=c("QSID"))
  mergeCarBAU.s18.2<-merge(xy.caribou.s18.2@data,xy.BAU.s18.2@data, by=c("QSID"))
  mergeCarBAU.s18.5<-merge(xy.caribou.s18.5@data,xy.BAU.s18.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.s18.0<-merge(mergeCarBAU.s18.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s18.0)#13164 obs
  str(mergeCarBAU.NRV.s18.0)
  mergeCarBAU.NRV.s18.0$Abund.caribou<-mergeCarBAU.NRV.s18.0$Abund.x
  mergeCarBAU.NRV.s18.0$Abund.BAU<-mergeCarBAU.NRV.s18.0$Abund.y
  
  mergeCarBAU.NRV.s18.1<-merge(mergeCarBAU.s18.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s18.1)#13164 obs
  str(mergeCarBAU.NRV.s18.1)
  mergeCarBAU.NRV.s18.1$Abund.caribou<-mergeCarBAU.NRV.s18.1$Abund.x
  mergeCarBAU.NRV.s18.1$Abund.BAU<-mergeCarBAU.NRV.s18.1$Abund.y
  
  mergeCarBAU.NRV.s18.2<-merge(mergeCarBAU.s18.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s18.2)#13164 obs
  str(mergeCarBAU.NRV.s18.2)
  mergeCarBAU.NRV.s18.2$Abund.caribou<-mergeCarBAU.NRV.s18.2$Abund.x
  mergeCarBAU.NRV.s18.2$Abund.BAU<-mergeCarBAU.NRV.s18.2$Abund.y
  
  mergeCarBAU.NRV.s18.5<-merge(mergeCarBAU.s18.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s18.5)#13164 obs
  str(mergeCarBAU.NRV.s18.5)
  mergeCarBAU.NRV.s18.5$Abund.caribou<-mergeCarBAU.NRV.s18.5$Abund.x
  mergeCarBAU.NRV.s18.5$Abund.BAU<-mergeCarBAU.NRV.s18.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.s18.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.s18.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.s18.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.s18.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.s18.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.s18.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.s18.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.s18.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.s18.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.s18.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.s18.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.s18.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.s18.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.s18.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.s18.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.s18.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.s18.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.s18.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.s18.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.s18.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.s18.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.s18.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.s18.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.s18.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.s18.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.s18.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.s18.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.s18.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.s18.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.s18.5$U95CI)
  
  s18comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(s18comparison,file=paste0("3_output/tables/",SPP,"s18comparison.csv"))
  
  
  s18 <- ggplot(data=s18comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (s18)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  s18
  #ggsave(filename=paste0("3_output/figures/",SPP,".s18.png"), plot=s18)
  
  
  
  #s22
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.s22.0<-merge(xy.caribou.s22.0@data,xy.BAU.s22.0@data, by=c("QSID"))
  mergeCarBAU.s22.1<-merge(xy.caribou.s22.1@data,xy.BAU.s22.1@data, by=c("QSID"))
  mergeCarBAU.s22.2<-merge(xy.caribou.s22.2@data,xy.BAU.s22.2@data, by=c("QSID"))
  mergeCarBAU.s22.5<-merge(xy.caribou.s22.5@data,xy.BAU.s22.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.s22.0<-merge(mergeCarBAU.s22.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s22.0)#13164 obs
  str(mergeCarBAU.NRV.s22.0)
  mergeCarBAU.NRV.s22.0$Abund.caribou<-mergeCarBAU.NRV.s22.0$Abund.x
  mergeCarBAU.NRV.s22.0$Abund.BAU<-mergeCarBAU.NRV.s22.0$Abund.y
  
  mergeCarBAU.NRV.s22.1<-merge(mergeCarBAU.s22.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s22.1)#13164 obs
  str(mergeCarBAU.NRV.s22.1)
  mergeCarBAU.NRV.s22.1$Abund.caribou<-mergeCarBAU.NRV.s22.1$Abund.x
  mergeCarBAU.NRV.s22.1$Abund.BAU<-mergeCarBAU.NRV.s22.1$Abund.y
  
  mergeCarBAU.NRV.s22.2<-merge(mergeCarBAU.s22.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s22.2)#13164 obs
  str(mergeCarBAU.NRV.s22.2)
  mergeCarBAU.NRV.s22.2$Abund.caribou<-mergeCarBAU.NRV.s22.2$Abund.x
  mergeCarBAU.NRV.s22.2$Abund.BAU<-mergeCarBAU.NRV.s22.2$Abund.y
  
  mergeCarBAU.NRV.s22.5<-merge(mergeCarBAU.s22.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s22.5)#13164 obs
  str(mergeCarBAU.NRV.s22.5)
  mergeCarBAU.NRV.s22.5$Abund.caribou<-mergeCarBAU.NRV.s22.5$Abund.x
  mergeCarBAU.NRV.s22.5$Abund.BAU<-mergeCarBAU.NRV.s22.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.s22.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.s22.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.s22.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.s22.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.s22.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.s22.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.s22.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.s22.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.s22.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.s22.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.s22.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.s22.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.s22.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.s22.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.s22.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.s22.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.s22.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.s22.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.s22.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.s22.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.s22.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.s22.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.s22.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.s22.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.s22.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.s22.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.s22.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.s22.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.s22.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.s22.5$U95CI)
  
  s22comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(s22comparison,file=paste0("3_output/tables/",SPP,"s22comparison.csv"))
  
  
  s22 <- ggplot(data=s22comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (S22)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  s22
  #ggsave(filename=paste0("3_output/figures/",SPP,".s22.png"), plot=s22)
  
  
  
  #s23
  #Now merge
  #First set of variables is for Caribou Scenario
  #Second set of variables is for BAU Scenario
  mergeCarBAU.s23.0<-merge(xy.caribou.s23.0@data,xy.BAU.s23.0@data, by=c("QSID"))
  mergeCarBAU.s23.1<-merge(xy.caribou.s23.1@data,xy.BAU.s23.1@data, by=c("QSID"))
  mergeCarBAU.s23.2<-merge(xy.caribou.s23.2@data,xy.BAU.s23.2@data, by=c("QSID"))
  mergeCarBAU.s23.5<-merge(xy.caribou.s23.5@data,xy.BAU.s23.5@data, by=c("QSID"))
  
  mergeCarBAU.NRV.s23.0<-merge(mergeCarBAU.s23.0, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s23.0)#13164 obs
  str(mergeCarBAU.NRV.s23.0)
  mergeCarBAU.NRV.s23.0$Abund.caribou<-mergeCarBAU.NRV.s23.0$Abund.x
  mergeCarBAU.NRV.s23.0$Abund.BAU<-mergeCarBAU.NRV.s23.0$Abund.y
  
  mergeCarBAU.NRV.s23.1<-merge(mergeCarBAU.s23.1, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s23.1)#13164 obs
  str(mergeCarBAU.NRV.s23.1)
  mergeCarBAU.NRV.s23.1$Abund.caribou<-mergeCarBAU.NRV.s23.1$Abund.x
  mergeCarBAU.NRV.s23.1$Abund.BAU<-mergeCarBAU.NRV.s23.1$Abund.y
  
  mergeCarBAU.NRV.s23.2<-merge(mergeCarBAU.s23.2, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s23.2)#13164 obs
  str(mergeCarBAU.NRV.s23.2)
  mergeCarBAU.NRV.s23.2$Abund.caribou<-mergeCarBAU.NRV.s23.2$Abund.x
  mergeCarBAU.NRV.s23.2$Abund.BAU<-mergeCarBAU.NRV.s23.2$Abund.y
  
  mergeCarBAU.NRV.s23.5<-merge(mergeCarBAU.s23.5, NRVabund, by=c("QSID"), na.rm=TRUE)
  nrow(mergeCarBAU.NRV.s23.5)#13164 obs
  str(mergeCarBAU.NRV.s23.5)
  mergeCarBAU.NRV.s23.5$Abund.caribou<-mergeCarBAU.NRV.s23.5$Abund.x
  mergeCarBAU.NRV.s23.5$Abund.BAU<-mergeCarBAU.NRV.s23.5$Abund.y
  
  #write.csv(mergeCarBAU.NRV.s23.5, file=paste0("3_output/tables/",SPP,"mergeCarBAU.NRV.s23.5.csv"))
  #this is a data check to confirm that there are distinct predicted 
  #abundances from different scenarios in each quarter-section,
  #in case there are any questions about it from the graphs
  
  Abund.caribou.0<-sum(mergeCarBAU.NRV.s23.0$Abund.caribou)
  Abund.caribou.1<-sum(mergeCarBAU.NRV.s23.1$Abund.caribou)
  Abund.caribou.2<-sum(mergeCarBAU.NRV.s23.2$Abund.caribou)
  Abund.caribou.5<-sum(mergeCarBAU.NRV.s23.5$Abund.caribou)
  
  Abund.BAU.0<-sum(mergeCarBAU.NRV.s23.0$Abund.BAU)
  Abund.BAU.1<-sum(mergeCarBAU.NRV.s23.1$Abund.BAU)
  Abund.BAU.2<-sum(mergeCarBAU.NRV.s23.2$Abund.BAU)
  Abund.BAU.5<-sum(mergeCarBAU.NRV.s23.5$Abund.BAU)
  
  Mean.0<-sum(mergeCarBAU.NRV.s23.0$Mean)
  Mean.1<-sum(mergeCarBAU.NRV.s23.1$Mean)
  Mean.2<-sum(mergeCarBAU.NRV.s23.2$Mean)
  Mean.5<-sum(mergeCarBAU.NRV.s23.5$Mean)
  
  L50CI.0<-sum(mergeCarBAU.NRV.s23.0$L50CI)
  L50CI.1<-sum(mergeCarBAU.NRV.s23.1$L50CI)
  L50CI.2<-sum(mergeCarBAU.NRV.s23.2$L50CI)
  L50CI.5<-sum(mergeCarBAU.NRV.s23.5$L50CI)
  
  U50CI.0<-sum(mergeCarBAU.NRV.s23.0$U50CI)
  U50CI.1<-sum(mergeCarBAU.NRV.s23.1$U50CI)
  U50CI.2<-sum(mergeCarBAU.NRV.s23.2$U50CI)
  U50CI.5<-sum(mergeCarBAU.NRV.s23.5$U50CI)
  
  L95CI.0<-sum(mergeCarBAU.NRV.s23.0$L95CI)
  L95CI.1<-sum(mergeCarBAU.NRV.s23.1$L95CI)
  L95CI.2<-sum(mergeCarBAU.NRV.s23.2$L95CI)
  L95CI.5<-sum(mergeCarBAU.NRV.s23.5$L95CI)
  
  U95CI.0<-sum(mergeCarBAU.NRV.s23.0$U95CI)
  U95CI.1<-sum(mergeCarBAU.NRV.s23.1$U95CI)
  U95CI.2<-sum(mergeCarBAU.NRV.s23.2$U95CI)
  U95CI.5<-sum(mergeCarBAU.NRV.s23.5$U95CI)
  
  s23comparison<-data.frame(Year=c(0,10,20,50),
                            Abund.caribou=c(Abund.caribou.0,Abund.caribou.1,Abund.caribou.2,Abund.caribou.5),
                            Abund.BAU=c(Abund.BAU.0,Abund.BAU.1,Abund.BAU.2,Abund.BAU.5),
                            Mean.NRV=c(Mean.0,Mean.1,Mean.2,Mean.5),
                            L50CI.NRV=c(L50CI.0,L50CI.1,L50CI.2,L50CI.5),
                            U50CI.NRV=c(U50CI.0,U50CI.1,U50CI.2,U50CI.5),
                            L95CI.NRV=c(L95CI.0,L95CI.1,L95CI.2,L95CI.5),
                            U95CI.NRV=c(U95CI.0,U95CI.1,U95CI.2,U95CI.5))
  write.csv(s23comparison,file=paste0("3_output/tables/",SPP,"s23comparison.csv"))
  
  
  s23 <- ggplot(data=s23comparison, aes(Year, Mean.NRV)) +
    geom_line(aes(x=Year, y=Mean.NRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
    geom_line(aes(x=Year, y=Abund.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
    geom_line(aes(x=Year, y=Abund.BAU), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
    ggtitle(paste0(SPP," (S23)"))+
    labs(x="Year", y="Predicted Abundance") +
    geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
    geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
    geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
    geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme
  s23
  #ggsave(filename=paste0("3_output/figures/",SPP,".s23.png"), plot=s23)
  
  
  library(grid)
  library(gridExtra)
  
  tiff(paste0("3_output/figures/bird species plots",SPP,"allFMUsthrutime.tiff"), units="in", width=16, height=8, res=300)
  grid.arrange(a14,
               a15,
               l1,
               l2,
               l3,
               l8,
               l11,
               s11,
               s14,
               s18,
               s22,
               s23,
               nrow=3, ncol=4)
  dev.off()  
  
}

#Compile results from individual species and FMUs into a single table at FMA scale
library(dplyr)
names<-c("AlderFlycatcher",
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
                "YellowbelliedSapsucker")#PalmWarbler already done


for (SPP in names){
  #list all files in folder with a certain name beginning 
  MASTERLIST =list.files("3_output/tables/",pattern=SPP)
  modelresults<-list()#empty list each time we draw a new number of samples
  
  for (j in MASTERLIST){
    spptdf<-read.csv(paste0("3_output/tables/",j), header=TRUE) #temporary data frame
    modelresults[[j]]<-spptdf#append temporary data frame at each loop iteration to the list
    
  }
  drawnresults = do.call(bind_rows, modelresults)#bind data frames together
  drawnresults$Species<-SPP
  write.csv(drawnresults, file=paste0("3_output/tables/bird species tables/",SPP,"comparisons_allFMU.csv"))
}

#list all files in folder with a certain name ending 
MASTERLIST =list.files("3_output/tables/bird species tables/",pattern="comparisons_allFMU.csv")
modelresults<-list()#empty list each time we draw a new number of samples

for (j in MASTERLIST){
  spptdf<-read.csv(paste0("3_output/tables/bird species tables/",j), header=TRUE) #temporary data frame
  modelresults[[j]]<-spptdf#append temporary data frame at each loop iteration to the list
  
}
drawnresults = do.call(bind_rows, modelresults)#bind data frames together
write.csv(drawnresults, file="3_output/tables/bird species tables/all_species_comparisons_allFMU.csv")

fmascale<-read.csv("3_output/tables/bird species tables/all_species_comparisons_allFMU.csv", header=TRUE)
fmascalefinal<-fmascale%>%
  group_by(Species,Year)%>%
  summarise(Abund.FMA.caribou=sum(Abund.caribou),
            Abund.FMA.ebm=sum(Abund.BAU),
            Abund.FMA.meanNRV=sum(Mean.NRV),
            L50CI.NRV=sum(L50CI.NRV),
            U50CI.NRV=sum(U50CI.NRV),
            L95CI.NRV=sum(L95CI.NRV),
            U95CI.NRV=sum(U95CI.NRV))
write.csv(fmascalefinal, file="3_output/tables/bird species tables/all_species_comparisons_FMAscale.csv")

library(jpeg)
img <- ("3_output/figures/CanadaWarblerallFMUsthrutime.jpeg")
# Set image size in inches (also need to set resolution in this case)
  jpeg("March2020CanadaWarbler.jpeg", width=8, height=4, unit="in", res=300)
  plot(as.raster(img))
  dev.off()

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")
library(BiocManager)
BiocManager::install(c("biocLite"), update = TRUE, ask = FALSE)
biocLite("EBImage")
library(EBImage)