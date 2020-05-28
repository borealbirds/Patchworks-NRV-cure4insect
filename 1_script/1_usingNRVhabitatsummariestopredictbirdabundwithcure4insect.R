## this scripts does some preprocessing
## and explains how to calculate bird densities based on
## Patchworks output using ABMI predictions and the cure4insect R package

library(cure4insect)
library(mefa4)
library(rgdal)
library(rgeos)
library(sp)
ROOT<-getwd()
load_common_data()
veg <- get_levels()$veg
## load this file from where you put it
load(paste0(ROOT,"/0_data/raw/hf-an-xy-for patchworks.RData"))
qsid <- rownames(coordinates(xy))

# ignore this part
# if (FALSE) {
# ## coordinates for QSs
# load("e:/peter/AB_data_v2017/data/analysis/kgrid_table_qs.Rdata")
# xy <- kgrid[,c("POINT_X", "POINT_Y", "MER", "RGE", "TWP", "SEC", "QS", "TWNSHIP",
#      "SECTION", "Area_km2")]
# coordinates(xy) <- ~ POINT_X + POINT_Y
# proj4string(xy) <- proj4string(get_id_locations())
# head(xy@data)
# 
# hf <- read.csv("~/repos/abmianalytics/lookup/lookup-hf-class.csv")
# rownames(hf) <- tolower(hf$HF_GROUP)
# save(hf, xy, file="hf-an-xy-for patchworks.RData")
# }

####Dave Andison's NRV Snapshots
## define species and directory with the csv input

names<-c("BaybreastedWarbler","OlivesidedFlycatcher")
for (i in names){
    spp<-i
    fl <- list.files(path=paste0(ROOT,"/0_data/processed/recast"), pattern="rec") # filename must have habitat0HF
    ## load species data (spatial/climate part of predictions)
    y <- load_spclim_data(spp)
    
    for (i in 1:length(fl)) {
        fn <- fl[i]
        cat(fn, i, "/", length(fl), "\n")
        flush.console()
        d <- read.csv(paste0(ROOT,"/0_data/processed/recast/", fn))
        #d <- read.csv("C:/Users/llest/OneDrive/Documents/TimVingeNRVtocure4insect/0_data/processed/recast/rec.TMP100.csv.csv", header=TRUE)
        rownames(d) <- d$QSLinkID
        d$X <- NULL # rownames saved and included as 1st col
        d$QSLinkID <- NULL # tracking as rownames
        weird <- !(rownames(d) %in% qsid)
        if (any(weird)) {
            print(paste("found & excluded weird row(s):", sum(weird)))
            print(rownames(d)[weird])
            d <- d[!weird,,drop=FALSE]
        }
        colnames(d)[colnames(d) == "NA."] <- "Unknown" # some stuff was unknown
        colnames(d) <- gsub("\\.", "", colnames(d)) # remove dots from colnames
        colnames(d)[colnames(d)=="MunicipalWaterandSewage"] <- "MunicipalWaterSewage"
        ###NEW SECTION ADDED DEC. 14, 2019
        colnames(d) <- gsub("UP", "CC", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("CC80", "80", colnames(d)) # CC converged by 60
        colnames(d) <- gsub("CC100", "100", colnames(d)) # CC converged by 60
        colnames(d) <- gsub("CC120", "120", colnames(d)) # CC converged by 60
        colnames(d) <- gsub("CC140", "140", colnames(d)) # CC converged by 60
        colnames(d) <- gsub("DeciduousCC", "CCDeciduous", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("PineCC", "CCPine", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("MixedwoodCC", "CCMixedwood", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("WhiteSpruceCC", "CCWhiteSpruce", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        
        colnames(d)[colnames(d) == "Deciduous0"] <- "Deciduous_0-10" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous10"] <- "Deciduous_10-20" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous20"] <- "Deciduous_20-40" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous40"] <- "Deciduous_40-60" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous60"] <- "Deciduous_60-80" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous80"] <- "Deciduous_80-100" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous100"] <- "Deciduous_100-120" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous120"] <- "Deciduous_120-140" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Deciduous140"] <- "Deciduous_140+" # new descriptor for harvest class in v.2018 cure4insect
        
        colnames(d)[colnames(d) == "Mixedwood0"] <- "Mixedwood_0-10" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood10"] <- "Mixedwood_10-20" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood20"] <- "Mixedwood_20-40" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood40"] <- "Mixedwood_40-60" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood60"] <- "Mixedwood_60-80" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood80"] <- "Mixedwood_80-100" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood100"] <- "Mixedwood_100-120" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood120"] <- "Mixedwood_120-140" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Mixedwood140"] <- "Mixedwood_140+" # new descriptor for harvest class in v.2018 cure4insect
        
        colnames(d)[colnames(d) == "Pine0"] <- "Pine_0-10" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine10"] <- "Pine_10-20" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine20"] <- "Pine_20-40" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine40"] <- "Pine_40-60" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine60"] <- "Pine_60-80" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine80"] <- "Pine_80-100" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine100"] <- "Pine_100-120" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine120"] <- "Pine_120-140" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "Pine140"] <- "Pine_140+" # new descriptor for harvest class in v.2018 cure4insect
        
        colnames(d)[colnames(d) == "BlackSpruce0"] <- "BlackSpruce_0-10" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce10"] <- "BlackSpruce_10-20" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce20"] <- "BlackSpruce_20-40" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce40"] <- "BlackSpruce_40-60" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce60"] <- "BlackSpruce_60-80" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce80"] <- "BlackSpruce_80-100" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce100"] <- "BlackSpruce_100-120" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce120"] <- "BlackSpruce_120-140" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "BlackSpruce140"] <- "BlackSpruce_140+" # new descriptor for harvest class in v.2018 cure4insect
        
        colnames(d)[colnames(d) == "WhiteSpruce0"] <- "WhiteSpruce_0-10" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce10"] <- "WhiteSpruce_10-20" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce20"] <- "WhiteSpruce_20-40" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce40"] <- "WhiteSpruce_40-60" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce60"] <- "WhiteSpruce_60-80" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce80"] <- "WhiteSpruce_80-100" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce100"] <- "WhiteSpruce_100-120" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce120"] <- "WhiteSpruce_120-140" # new descriptor for harvest class in v.2018 cure4insect
        colnames(d)[colnames(d) == "WhiteSpruce140"] <- "WhiteSpruce_140+" # new descriptor for harvest class in v.2018 cure4insect
        
        colnames(d) <- gsub("Pipeline", "SoftLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("Seismicline", "SoftLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("TransmissionLine", "SoftLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("BorrowPitsDugoutsSumps", "UrbInd", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("Urban", "UrbInd", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("MineSite", "UrbInd", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("IndustrialSiteRural", "UrbInd", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("WellSite", "RoughP", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("OtherDisturbedVegetation", "RoughP", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("Cutblock", "RoughP", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("RoadHardSurface", "HardLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("RailHardSurface", "HardLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("RoadVegetatedVerge", "HardLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("RailVegetatedVerge", "HardLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("RoadTrailVegetated", "HardLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("RuralResidentialIndustrial", "HardLin", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        colnames(d) <- gsub("CultivationCropPastureBareGround", "TameP", colnames(d)) # new descriptor for harvest class in v.2018 cure4insect
        
        #get rid of any duplicated column names after adding their contents
        d<-t(rowsum(t(d), group = colnames(d), na.rm = T))
        ###END OF NEW SECTION
        
        ## lookup table
        tab <- data.frame(col_data=colnames(d))
        rownames(tab) <- tab$col_data
        tab$col_in <- as.character(tab$col_data)
        tab$col_in <- gsub("UP", "CC", tab$col_in)
        tab$col_coef <- as.character(veg[match(tab$col_in, veg)])
        hf2 <- hf[tolower(as.character(tab$col_in[is.na(tab$col_coef)])),]
        tab$col_coef[is.na(tab$col_coef)] <- as.character(hf2$UseInAnalysis)
        tab$col_coef[tab$col_in == "Unknown"] <- "Unknown"
        tab$col_coef <- gsub("CC80", "80", tab$col_coef) # CC converged by 60
        tab$col_coef <- gsub("CC100", "100", tab$col_coef) # CC converged by 60
        tab$col_coef <- gsub("CC120", "120", tab$col_coef) # CC converged by 60
        tab$col_coef <- gsub("CC140", "140", tab$col_coef) # CC converged by 60
        tab$col_zero <- tolower(tab$col_in) %in% tolower(c("Water",
                                                           "BorrowPitsDugoutsSumps","MunicipalWaterSewage","Reservoirs","Canals",
                                                           "RailHardSurface", "RoadHardSurface", "MineSite", "PeatMine",
                                                           "Unknown", "Cutblock"))
        veg2 <- intersect(tab$col_coef, veg)
        ## making inputs and predict
        X <- array(1, c(nrow(d), length(veg2)), list(rownames(d), veg2))
        XY <- xy[match(rownames(d), qsid),]
        prmat <- suppressWarnings(predict_mat(y, xy=XY, veg=X)$veg)
        ## recast predictions to fit input file
        d2 <- prmat[,match(tab$col_coef, colnames(prmat))]
        colnames(d2) <- colnames(d)
        d2[,tab$col_zero] <- 0
        ## this calculation assumes that input areas are in m^2
        d3 <- d2 * d #/ 10^4 #ignore the 10^4: densities are in ha but area is as well
        ## write results
        fn_out <- paste0("Density_", spp, "_", fn)
        if (!dir.exists(file.path(ROOT, paste0("0_data/processed/nrv"), spp)))
            dir.create(file.path(ROOT, paste0("0_data/processed/nrv"), spp))
        write.csv(d2, file=file.path(ROOT, paste0("0_data/processed/nrv"), spp, paste0("Density_", spp, "_", fn)))
        write.csv(d3, file=file.path(ROOT, paste0("0_data/processed/nrv"), spp, paste0("Abundance_", spp, "_", fn)))
    }   
    
}

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
for (i in names){
    spp<-i    
    DIR <- paste0(ROOT,"/0_data/processed/nrv/",spp)
    temp = list.files(DIR, pattern="Abundance") 
    
    df0<-read.csv(paste0(DIR,"/","Abundance_",spp,"_rec.TMP100.csv.csv"), header=TRUE)
    X<-df0$X
    new<-c()
    
    for (i in seq_along(temp)) {
        filename = temp[[i]]
        
        # Read data in
        df <- read.csv(paste0(DIR,"/",filename), header = TRUE)
        n<-rowSums(df[,c(2:ncol(df))], na.rm = TRUE)
        new<-cbind(new,n)
    }
    NRVabund<-data.frame(X,new)
    str(NRVabund)
    NRVabund$Mean<-rowSums(NRVabund[,c(2:ncol(NRVabund))])/97
    NRVabund$Std<-apply(NRVabund[,c(2:(ncol(NRVabund)-1))], 1, sd)
    NRVabund$SE<-NRVabund$Std/sqrt(97)
    NRVabund$L50CI<-NRVabund$Mean-0.67*NRVabund$SE
    NRVabund$U50CI<-NRVabund$Mean+0.67*NRVabund$SE
    NRVabund$L95CI<-NRVabund$Mean-1.96*NRVabund$SE
    NRVabund$U95CI<-NRVabund$Mean+1.96*NRVabund$SE
    write.csv(NRVabund, file=paste0(DIR,"/","NRVabund",spp,".csv"))
}




