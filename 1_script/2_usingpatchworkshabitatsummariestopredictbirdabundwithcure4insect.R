## this scripts does some preprocessing
## and explains how to calculate bird densities based on
## Patchworks output using ABMI predictions and the cure4insect R package
#devtools::install_github("ABbiodiversity/cure4insect", ref="v2018")
library(cure4insect)
library(mefa4)
library(rgdal)
library(rgeos)
library(sp)

load_common_data()
veg <- get_levels()$veg
## load this file from where you put it
load("0_data/raw/hf-an-xy-for patchworks.RData")
qsid <- rownames(coordinates(xy))

# ignore this part
if (FALSE) {
## coordinates for QSs
load("e:/peter/AB_data_v2017/data/analysis/kgrid_table_qs.Rdata")
xy <- kgrid[,c("POINT_X", "POINT_Y", "MER", "RGE", "TWP", "SEC", "QS", "TWNSHIP",
     "SECTION", "Area_km2")]
coordinates(xy) <- ~ POINT_X + POINT_Y
proj4string(xy) <- proj4string(get_id_locations())
head(xy@data)

hf <- read.csv("~/repos/abmianalytics/lookup/lookup-hf-class.csv")
rownames(hf) <- tolower(hf$HF_GROUP)
save(hf, xy, file="hf-an-xy-for patchworks.RData")
}

####Caribou Conservation Scenario
## define species and directory with the csv input

names<-c("AmericanThreetoedWoodpecker",
         "BarnSwallow",
         "BaltimoreOriole",
         "BlackbackedWoodpecker",
         "BlackpollWarbler",
         "BorealChickadee",
         "ClaycoloredSparrow",
         "CommonYellowthroat",
         "ConnecticutWarbler",
         "EveningGrosbeak",
         "EasternPhoebe",
         "LeastFlycatcher",
         "LeContesSparrow",
         "MourningWarbler",
         "NorthernFlicker",
         "PileatedWoodpecker",
         "RustyBlackbird",
         "SolitarySandpiper",
         "WesternWoodPewee",
         "WilsonsSnipe",
         "WhitethroatedSparrow",
         "YellowbelliedSapsucker")#PalmWarbler already done

names<-c("WhitewingedCrossbill")
for (i in names){
spp <- i
DIR <- "0_data/raw/patchworks"
fl <- list.files(DIR, pattern="caribou") # filename must have "caribou" in it
## load species data (spatial/climate part of predictions)
y <- load_spclim_data(spp)

for (i in 1:length(fl)) {
    #i<-"bothhabitatHF_s23_caribou_2.csv"
    fn <- fl[i]
    cat(fn, i, "/", length(fl), "\n")
    flush.console()
    d <- read.csv(file.path(DIR, fn))
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
    ###NEW SECTION ADDED DEC. 13, 2019
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

    colnames(d)[colnames(d) == "CCDeciduous0"] <- "CCDeciduous_0-10" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCDeciduous10"] <- "CCDeciduous_10-20" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCDeciduous20"] <- "CCDeciduous_20-40" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCDeciduous40"] <- "CCDeciduous_40-60" # new descriptor for harvest class in v.2018 cure4insect
    
    colnames(d)[colnames(d) == "Mixedwood0"] <- "Mixedwood_0-10" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood10"] <- "Mixedwood_10-20" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood20"] <- "Mixedwood_20-40" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood40"] <- "Mixedwood_40-60" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood60"] <- "Mixedwood_60-80" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood80"] <- "Mixedwood_80-100" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood100"] <- "Mixedwood_100-120" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood120"] <- "Mixedwood_120-140" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Mixedwood140"] <- "Mixedwood_140+" # new descriptor for harvest class in v.2018 cure4insect

    colnames(d)[colnames(d) == "CCMixedwood0"] <- "CCMixedwood_0-10" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCMixedwood10"] <- "CCMixedwood_10-20" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCMixedwood20"] <- "CCMixedwood_20-40" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCMixedwood40"] <- "CCMixedwood_40-60" # new descriptor for harvest class in v.2018 cure4insect
    
    colnames(d)[colnames(d) == "Pine0"] <- "Pine_0-10" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine10"] <- "Pine_10-20" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine20"] <- "Pine_20-40" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine40"] <- "Pine_40-60" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine60"] <- "Pine_60-80" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine80"] <- "Pine_80-100" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine100"] <- "Pine_100-120" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine120"] <- "Pine_120-140" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "Pine140"] <- "Pine_140+" # new descriptor for harvest class in v.2018 cure4insect

    colnames(d)[colnames(d) == "CCPine0"] <- "CCPine_0-10" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCPine10"] <- "CCPine_10-20" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCPine20"] <- "CCPine_20-40" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCPine40"] <- "CCPine_40-60" # new descriptor for harvest class in v.2018 cure4insect
    
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
    
    colnames(d)[colnames(d) == "CCWhiteSpruce0"] <- "CCWhiteSpruce_0-10" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCWhiteSpruce10"] <- "CCWhiteSpruce_10-20" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCWhiteSpruce20"] <- "CCWhiteSpruce_20-40" # new descriptor for harvest class in v.2018 cure4insect
    colnames(d)[colnames(d) == "CCWhiteSpruce40"] <- "CCWhiteSpruce_40-60" # new descriptor for harvest class in v.2018 cure4insect

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
    #tab$col_coef <- gsub("CC80", "80", tab$col_coef) # CC converged by 60
    #tab$col_coef <- gsub("CC100", "100", tab$col_coef) # CC converged by 60
    #tab$col_coef <- gsub("CC120", "120", tab$col_coef) # CC converged by 60
    #tab$col_coef <- gsub("CC140", "140", tab$col_coef) # CC converged by 60
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
    if (!dir.exists(file.path(paste0("0_data/processed/patchworks"), spp)))
        dir.create(file.path(paste0("0_data/processed/patchworks"), spp))
    write.csv(d2, file=file.path(paste0("0_data/processed/patchworks"), spp, paste0("Density_", spp, "_", fn)))
    write.csv(d3, file=file.path(paste0("0_data/processed/patchworks"), spp, paste0("Abundance_", spp, "_", fn)))
}

}

####Al-Pac Ecosystem-Based Management (EBM) Scenario
## define species and directory with the csv input
for (i in names){
spp <- i
DIR <- "0_data/raw/patchworks"
fl <- list.files(DIR, pattern="ebm") # filename must have ebm
## load species data (spatial/climate part of predictions)
y <- load_spclim_data(spp)

for (i in 1:length(fl)) {
  fn <- fl[i]
  cat(fn, i, "/", length(fl), "\n")
  flush.console()
  d <- read.csv(file.path(DIR, fn))
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
  
  colnames(d)[colnames(d) == "CCDeciduous0"] <- "CCDeciduous_0-10" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCDeciduous10"] <- "CCDeciduous_10-20" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCDeciduous20"] <- "CCDeciduous_20-40" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCDeciduous40"] <- "CCDeciduous_40-60" # new descriptor for harvest class in v.2018 cure4insect
  
  colnames(d)[colnames(d) == "Mixedwood0"] <- "Mixedwood_0-10" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood10"] <- "Mixedwood_10-20" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood20"] <- "Mixedwood_20-40" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood40"] <- "Mixedwood_40-60" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood60"] <- "Mixedwood_60-80" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood80"] <- "Mixedwood_80-100" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood100"] <- "Mixedwood_100-120" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood120"] <- "Mixedwood_120-140" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Mixedwood140"] <- "Mixedwood_140+" # new descriptor for harvest class in v.2018 cure4insect
  
  colnames(d)[colnames(d) == "CCMixedwood0"] <- "CCMixedwood_0-10" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCMixedwood10"] <- "CCMixedwood_10-20" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCMixedwood20"] <- "CCMixedwood_20-40" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCMixedwood40"] <- "CCMixedwood_40-60" # new descriptor for harvest class in v.2018 cure4insect
  
  colnames(d)[colnames(d) == "Pine0"] <- "Pine_0-10" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine10"] <- "Pine_10-20" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine20"] <- "Pine_20-40" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine40"] <- "Pine_40-60" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine60"] <- "Pine_60-80" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine80"] <- "Pine_80-100" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine100"] <- "Pine_100-120" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine120"] <- "Pine_120-140" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "Pine140"] <- "Pine_140+" # new descriptor for harvest class in v.2018 cure4insect
  
  colnames(d)[colnames(d) == "CCPine0"] <- "CCPine_0-10" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCPine10"] <- "CCPine_10-20" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCPine20"] <- "CCPine_20-40" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCPine40"] <- "CCPine_40-60" # new descriptor for harvest class in v.2018 cure4insect
  
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
  
  colnames(d)[colnames(d) == "CCWhiteSpruce0"] <- "CCWhiteSpruce_0-10" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCWhiteSpruce10"] <- "CCWhiteSpruce_10-20" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCWhiteSpruce20"] <- "CCWhiteSpruce_20-40" # new descriptor for harvest class in v.2018 cure4insect
  colnames(d)[colnames(d) == "CCWhiteSpruce40"] <- "CCWhiteSpruce_40-60" # new descriptor for harvest class in v.2018 cure4insect
  
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
  #tab$col_in <- gsub("UP", "CC", tab$col_in)
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
  if (!dir.exists(file.path(paste0("0_data/processed/patchworks"), spp)))
    dir.create(file.path(paste0("0_data/processed/patchworks"), spp))
  write.csv(d2, file=file.path(paste0("0_data/processed/patchworks"), spp, paste0("Density_", spp, "_", fn)))
  write.csv(d3, file=file.path(paste0("0_data/processed/patchworks"), spp, paste0("Abundance_", spp, "_", fn)))
}#works up to here as of Feb. 10, 2020

}

#Once densities and abundances are generated for a particular species,
#match the quarter-sections in these files with the quarter-sections in xy,
#then use these files to subset from the xy spatial points data frame

placeholder<-read.csv("C:/PATCHWORKS analysis/Sep 2017/CaribouScenarioHabitatHF/combined/Ovenbird/Abundance_Ovenbird_bothhabitatHF_l1_caribou_0.csv", header=TRUE)
str(placeholder)#82 variables, X (column 1) being QSID
#columns 2-82 being densities in the different habitats, or predicted abundance in the habitat area for that quarter section
#currently it looks like densities are being calculated per square metre

placeholder$Abund<-rowSums(placeholder[,c(2:82)])*10000
max(placeholder$Abund)

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
library(maps)
library(maptools)
library(mapproj)
library(mapdata)
library(sp)
library(rgdal)
library(sp)
library(raster)
xyreduced<-subset(xy[xy@data$QSID %in% placeholder$X, ])
#alternatively:
#xyreduced<-subset(xy[qsid %in% placeholder$X, ])  

nrow(xyreduced)#4570 obs instead of 1008568
nrow(placeholder)#4570 obs
xyreduced@data$Abund<-placeholder$Abund

nclr    <- 8
library(RColorBrewer)
plotclr <- brewer.pal(nclr,"BuGn")
xyreduced@data$colcode <- ifelse((xyreduced@data$Abund <=   0), plotclr[1],
                                 ifelse((xyreduced@data$Abund  <= max(xyreduced@data$Abund)*0.125), plotclr[2],
                                        ifelse((xyreduced@data$Abund <=max(xyreduced@data$Abund)*0.250), plotclr[3],
                                               ifelse((xyreduced@data$Abund <=max(xyreduced@data$Abund)*0.375), plotclr[4],
                                                      ifelse((xyreduced@data$Abund <=max(xyreduced@data$Abund)*0.5), plotclr[5],
                                                             ifelse((xyreduced@data$Abund <=max(xyreduced@data$Abund)*0.625), plotclr[6],
                                                                    ifelse((xyreduced@data$Abund <=max(xyreduced@data$Abund)*0.750), plotclr[7],
                                                                           plotclr[8])))))))


xyreduced@data$POINT_X<-coordinates(xyreduced)[,1]
xyreduced@data$POINT_Y<-coordinates(xyreduced)[,2]

plot(xyreduced, pch=15, col=xyreduced@data$colcode, cex=0.5, main="OVEN  L1 Caribou Scenario Year 0")
text(x=median(xyreduced@data$POINT_X), y = min(xyreduced@data$POINT_Y), labels = paste0("n= ",round(sum(xyreduced@data$Abund),digits=0)))#Numbers cells from 1 to 400 and assigns values from a 


###CREATE A GRID OF POINTS ON TOP OF POINTS

# Create a grid of points within the bbox of the SpatialPolygonsDataFrame 
# xyreduced with decimal degrees as map units
grid <- makegrid(xyreduced, cellsize = 0.005)

# grid is a data.frame. To change it to a spatial data set we have to
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(xyreduced)))

plot(xyreduced, pch=15, col=xyreduced@data$colcode, cex=0.5)

plot(xyreduced)
plot(grid, pch = ".", col=xyreduced@data$colcode, add = T)

#To extract only the points within the polygon, 
#use `[` to subset the points based on the location like this:
grid <- grid[xyreduced, ]
plot(xyreduced, col=xyreduced@data$colcode)
plot(grid, pch = ".", col=xyreduced@data$colcode, add=T)

###CREATE A POLYGON GRID ON TOP OF POINTS
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(classInt)
library(raster)


# create grid of polygons

grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as(grd, "SpatialPolygons")

row.names(polys)

#  [1] "g1"   "g2"   "g3"   "g4"   "g5"   "g6"   "g7"   "g8"   "g9"   "g10"  "g11"  "g12"  "g13"  "g14"  "g15"  "g16"  "g17"  "g18"  "g19" 
# [20] "g20"  "g21"  "g22"  "g23"  "g24"  "g25"  "g26"  "g27"  "g28"  "g29"  "g30"  "g31"  "g32"  "g33"  "g34"  "g35"  "g36"  "g37"  "g38" 
# [39] "g39"  "g40"  "g41"  "g42"  "g43"  "g44"  "g45"  "g46"  "g47"  "g48"  "g49"  "g50"  "g51"  "g52"  "g53"  "g54"  "g55"  "g56"  "g57" 
# [58] "g58"  "g59"  "g60"  "g61"  "g62"  "g63"  "g64"  "g65"  "g66"  "g67"  "g68"  "g69"  "g70"  "g71"  "g72"  "g73"  "g74"  "g75"  "g76" 
# [77] "g77"  "g78"  "g79"  "g80"  "g81"  "g82"  "g83"  "g84"  "g85"  "g86"  "g87"  "g88"  "g89"  "g90"  "g91"  "g92"  "g93"  "g94"  "g95" 
# [96] "g96"  "g97"  "g98"  "g99"  "g100"

length(row.names(polys))

class(polys)

# [1] "SpatialPolygons"
# attr(,"package")
# [1] "sp"

# assign projection to grid

proj4string(polys) = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

# create fake atttribute data for each grid cell

poly.data = data.frame(f=runif(length(row.names(polys)), 0, 14))
row.names(poly.data) <- paste0('g', 1:length(row.names(polys)))

# convert grid to a SpatialPolygonsDataFrame:
poly.df = SpatialPolygonsDataFrame(polys, poly.data)

# assign colors to grid cells

plotvar <- poly.df$f
nclr    <- 8
library(RColorBrewer)
plotclr <- brewer.pal(nclr,"BuPu")

colcode <- ifelse((                plotvar <=   2), plotclr[1],
                  ifelse((plotvar >   2 & plotvar <=   4), plotclr[2],
                         ifelse((plotvar >   4 & plotvar <=   6), plotclr[3],
                                ifelse((plotvar >   6 & plotvar <=   8), plotclr[4],
                                       ifelse((plotvar >   8 & plotvar <=  10), plotclr[5],
                                              ifelse((plotvar >  10 & plotvar <=  12), plotclr[6],
                                                     ifelse((plotvar >  12 & plotvar <=  14), plotclr[7],
                                                            plotclr[8])))))))

plot(poly.df, , col=colcode)
#text(coordinates(poly.df), labels=row.names(poly.df))




# library(rgeos)      ## for gBuffer()
# library(raster)     ## for bind()
# 
# ww <- sqrt(xyreduced@data$Area_km2)/2  ## Widths of buffers needed to produce desired areas    
# 
# pp <- list()
# for(i in seq_along(xyreduced)) {
#   pp[i] <- gBuffer(xyreduced[i], width=ww[i], quadsegs=1, capStyle="SQUARE")
# }
# PP <- do.call(bind, pp)
# 
# ## Check that it worked
# plot(PP)
# plot(B, add=TRUE)
# text(B, labels=1:4, adj=c(-1,0), col="red")
# 



# assign colors to grid cells

plotvar <- poly.df$f
nclr    <- 8
plotclr <- brewer.pal(nclr,"BuPu")

colcode <- ifelse((                plotvar <=   2), plotclr[1],
                  ifelse((plotvar >   2 & plotvar <=   4), plotclr[2],
                         ifelse((plotvar >   4 & plotvar <=   6), plotclr[3],
                                ifelse((plotvar >   6 & plotvar <=   8), plotclr[4],
                                       ifelse((plotvar >   8 & plotvar <=  10), plotclr[5],
                                              ifelse((plotvar >  10 & plotvar <=  12), plotclr[6],
                                                     ifelse((plotvar >  12 & plotvar <=  14), plotclr[7],
                                                            plotclr[8])))))))

jpeg(filename = "grid.plot.jpeg")

plot(poly.df, , col=colcode)
text(coordinates(poly.df), labels=row.names(poly.df))
dev.off()












#create raster from spatial points data frame
library(maptools)

# Load your point shapefile (with values in a field):

# Create a raster, give it the same extent as the points
# and define rows and columns:

rast <- raster()
extent(rast) <- extent(xyreduced) # this might be unnecessary
ncol(rast) <- 100 # this is one way of assigning cell size / resolution
nrow(rast) <- 80

# And then ... rasterize it! This creates a grid version 
# of your points using the cells of rast, values from the IP field:
rast2 <- rasterize(xyreduced, rast, xyreduced@data$Abund, fun=mean) 
plot(rast2)

install.packages("plotKML")
library(plotKML)
Rast <- vect2rast(xyreduced, FIELD=xyreduced@data$Abund)
plot(Rast, col="darkblue")

scatter_fill <- function (x, y, z,xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),zlim=c(min(z),max(z)),
                          nlevels = 20, plot.title, plot.axes, 
                          key.title, key.axes, asp = NA, xaxs = "i", 
                          yaxs = "i", las = 1, 
                          axes = TRUE, frame.plot = axes, ...) 
{
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  par(las = las)
  mar <- mar.orig
  mar[4L] <- mar[2L]
  mar[2L] <- 1
  par(mar = mar)
  
  # choose colors to interpolate
  levels <- seq(zlim[1],zlim[2],length.out = nlevels)
  col <- colorRampPalette(c("blue","yellow","red"))(nlevels)  
  colz <- col[cut(z,nlevels)]  
  #   
  plot.new()
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")
  
  rect(0, levels[-length(levels)], 1, levels[-1L],col=col,border=col) 
  if (missing(key.axes)) {if (axes){axis(4)}}
  else key.axes
  box()
  if (!missing(key.title)) 
    key.title
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)
  
  # points
  plot(x,y,type = "n",xaxt='n',yaxt='n',xlab="",ylab="",xlim=xlim,ylim=ylim,bty="n")
  points(x,y,col = colz,xaxt='n',yaxt='n',xlab="",ylab="",bty="n",...)
  
  ## options to make mapping more customizable
  
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
} 

library(jpeg)
mypath <- file.path("C:","Rplots","PatchworksJan2018", "L1caribou_Y0.jpg") 

jpeg(file=mypath) 
mytitle = "my title is" 

xyreduceddf<-as.data.frame(xyreduced)#convert spatial df to regular df to use this function
spp<-xyreduceddf$Abund  #Species as variable
scatter_fill(xyreduceddf$POINT_X, xyreduceddf$POINT_Y, spp, nlevels=15,xlim=c(min(xyreduceddf$POINT_X),max(xyreduceddf$POINT_X)),ylim=c(min(xyreduceddf$POINT_Y),max(xyreduceddf$POINT_Y)),zlim=c(0,max(spp)),main="Oven # per Q.S. (Year 0)",xlab="Longitude",ylab="Latitude",pch=".",cex=1)
map.scale(-118, 59, ratio=FALSE,relwidth=0.4,cex=1)

dev.off() 

new_rasterDF1 = raster(nrow = 20, ncol = 20, 
                       xmn = min(xyreduceddf$POINT_X), 
                       xmx = max(xyreduceddf$POINT_X), 
                       ymn = min(xyreduceddf$POINT_Y), 
                       ymx = max(xyreduceddf$POINT_Y),
                       vals =xyreduceddf$Abund)
plot(new_rasterDF1)
