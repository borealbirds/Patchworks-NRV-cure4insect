#For single file:
#Read into R
# TMP45<-read.csv("TMP45flip2shapeXqs.csv", header=TRUE)
# str(TMP45)
# #Strip all but the last 3 digits from the grid code
# library(stringr)
# #x <- "some text in a string"
# #str_sub(x,-3,-1)
# TMP45$gridcodeB<-str_sub(TMP45$gridcode,-3,-1)
# #Assign to forest stand based on the first number in the 3 digits
# attach(TMP45)
# TMP45$stand[str_sub(gridcodeB,1,1) == 1] <- "Pine"
# TMP45$stand[str_sub(gridcodeB,1,1) == 2] <- "BlackSpruce"
# TMP45$stand[str_sub(gridcodeB,1,1) == 4] <- "Deciduous"
# TMP45$stand[str_sub(gridcodeB,1,1) == 5] <- "Mixedwood"
# #Assign to forest age class based on the last 2 numbers in the 3 digits
# TMP45$age[str_sub(gridcodeB,-2,-1) == "00"] <- "R"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "01"] <- "1"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "02"] <- "2"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "03"] <- "2"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "04"] <- "3"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "05"] <- "3"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "06"] <- "4"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "07"] <- "4"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "08"] <- "5"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "09"] <- "5"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "10"] <- "6"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "11"] <- "6"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "12"] <- "7"
# TMP45$age[str_sub(gridcodeB,-2,-1) == "13"] <- "7"
# TMP45$age[str_sub(gridcodeB,-2,-1) > 14] <- "8"
# #Concatenate forest stand and age-class
# detach(TMP45)
# TMP45$forestageclass<-paste0(TMP45$stand,"_",TMP45$age)
# TMP45$QSLinkID<-paste0(TMP45$M,"-",TMP45$RGE,"-",TMP45$TWP,"-",TMP45$SEC,"-",TMP45$QS)
# 
# write.csv(TMP45, file="TMP45check.csv")
# #Recast by quarter section
# vegQS<-aggregate(TMP45$RECALCAREA, list(TMP45$QSLinkID, TMP45$forestageclass), sum)
# str(vegQS)
# library(reshape)
# vegQS.recast<-cast(vegQS, Group.1~Group.2, sum)
# str(vegQS.recast)
# vegQS.recast$QSLinkID<-vegQS.recast$Group.1
# vegQS.recast$Group.1<-NULL
# write.csv(vegQS.recast, file = "TMP45.recast.csv")

#now do a list
#file_list <- list.files()
ROOT<-getwd()
temp = list.files(path=paste0(ROOT,"/0_data/raw"), pattern="*.csv")
library(stringr)
library(reshape)

for (i in seq_along(temp)) {
  filename = temp[[i]]
  
  # Read data in
  df <- read.csv(paste0(ROOT,"/0_data/raw/", filename), header = TRUE)
  
  df$gridcodeB<-str_sub(df$gridcode,-3,-1)
  #Assign to forest stand based on the first number in the 3 digits
  attach(df)
  df$stand[str_sub(gridcodeB,1,1) == 1] <- "Pine"
  df$stand[str_sub(gridcodeB,1,1) == 2] <- "BlackSpruce"
  df$stand[str_sub(gridcodeB,1,1) == 4] <- "Deciduous"
  df$stand[str_sub(gridcodeB,1,1) == 5] <- "Mixedwood"
  #Assign to forest age class based on the last 2 numbers in the 3 digits
  df$age[str_sub(gridcodeB,-2,-1) == "00"] <- "0"
  df$age[str_sub(gridcodeB,-2,-1) == "01"] <- "10"
  df$age[str_sub(gridcodeB,-2,-1) == "02"] <- "20"
  df$age[str_sub(gridcodeB,-2,-1) == "03"] <- "20"
  df$age[str_sub(gridcodeB,-2,-1) == "04"] <- "40"
  df$age[str_sub(gridcodeB,-2,-1) == "05"] <- "40"
  df$age[str_sub(gridcodeB,-2,-1) == "06"] <- "60"
  df$age[str_sub(gridcodeB,-2,-1) == "07"] <- "60"
  df$age[str_sub(gridcodeB,-2,-1) == "08"] <- "80"
  df$age[str_sub(gridcodeB,-2,-1) == "09"] <- "80"
  df$age[str_sub(gridcodeB,-2,-1) == "10"] <- "100"
  df$age[str_sub(gridcodeB,-2,-1) == "11"] <- "100"
  df$age[str_sub(gridcodeB,-2,-1) == "12"] <- "120"
  df$age[str_sub(gridcodeB,-2,-1) == "13"] <- "120"
  df$age[str_sub(gridcodeB,-2,-1) > 14] <- "140"
  #Concatenate forest stand and age-class
  detach(df)
  df$forestageclass<-paste0(df$stand,df$age)
  #write.csv(TMP45, file="TMP45check.csv")
  #Recast by quarter section
  df$QSLinkID<-paste0(df$M,"-",df$RGE,"-",df$TWP,"-",df$SEC,"-",df$QS)
  vegQS<-aggregate(df$RECALCAREA, list(df$QSLinkID, df$forestageclass), sum)
  str(vegQS)
  #library(reshape)
  vegQS.recast<-cast(vegQS, Group.1~Group.2, sum)
  str(vegQS.recast)
  vegQS.recast$QSLinkID<-vegQS.recast$Group.1
  vegQS.recast$Group.1<-NULL
  vegQS.recast$Cutblock<-0
  vegQS.recast$Borrow.Pits.Dugouts.Sumps<-0
  vegQS.recast$Industrial.Site.Rural<-0
  vegQS.recast$Mine.Site<-0
  vegQS.recast$Municipal..Water.and.Sewage.<-0
  vegQS.recast$Other.Disturbed.Vegetation<-0
  vegQS.recast$Pipeline<-0
  vegQS.recast$Rail..Hard.Surface<-0
  vegQS.recast$Rail..Vegetated.Verge<-0
  vegQS.recast$Road..Hard.Surface<-0
  vegQS.recast$Road..Vegetated.Verge<-0
  vegQS.recast$Road.Trail..Vegetated.<-0
  vegQS.recast$Rural..Residential.Industrial.<-0
  vegQS.recast$Seismic.line<-0
  vegQS.recast$Transmission.Line<-0
  vegQS.recast$Urban<-0
  vegQS.recast$Well.Site<-0
  write.csv(vegQS.recast, file = paste0(ROOT,"/0_data/processed/recast/rec.",filename,".csv"))
  
}















