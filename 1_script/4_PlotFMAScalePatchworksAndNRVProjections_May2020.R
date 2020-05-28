
library(ggplot2)
library(gridExtra)
library(grid)
my.theme <- theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))

fmascale<-read.csv("3_output/tables/bird species tables/all_species_comparisons_FMAscale.csv", header=TRUE)


attw<-fmascale[fmascale$Species=="AmericanThreetoedWoodpecker",]
str(attw)
gg.attw<-attw %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y=paste0("American Three-toed",'\n',"Woodpecker")) +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme


bbwa<-fmascale[fmascale$Species=="BaybreastedWarbler",]
gg.bbwa<-bbwa %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Bay-breasted Warbler") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme


bbwo<-fmascale[fmascale$Species=="BlackbackedWoodpecker",]
gg.bbwo<-bbwo %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y=paste0("Black-backed",'\n',"Woodpecker")) +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



btnw<-fmascale[fmascale$Species=="BlackthroatedGreenWarbler",]
gg.btnw<-btnw %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y=paste0("Black-throated Green ",'\n',"Warbler")) +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



blpw<-fmascale[fmascale$Species=="BlackpollWarbler",]
gg.blpw<-blpw %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Blackpoll Warbler") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



boch<-fmascale[fmascale$Species=="BorealChickadee",]
gg.boch<-boch %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Boreal Chickadee") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



brcr<-fmascale[fmascale$Species=="BrownCreeper",]
gg.brcr<-brcr %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Brown Creeper") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



cawa<-fmascale[fmascale$Species=="CanadaWarbler",]
gg.cawa<-cawa %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Canada Warbler") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme

cmwa<-fmascale[fmascale$Species=="CapeMayWarbler",]
gg.cmwa<-cmwa %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Cape May Warbler") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



evgr<-fmascale[fmascale$Species=="EveningGrosbeak",]
gg.evgr<-evgr %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Evening Grosbeak") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



nofl<-fmascale[fmascale$Species=="NorthernFlicker",]
gg.nofl<-nofl %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Northern Flicker") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



osfl<-fmascale[fmascale$Species=="OlivesidedFlycatcher",]
gg.osfl<-osfl %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Olive-sided Flycatcher") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



oven<-fmascale[fmascale$Species=="Ovenbird",]
gg.oven<-oven %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Ovenbird") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme


pawa<-fmascale[fmascale$Species=="PalmWarbler",]
gg.pawa<-pawa %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Palm Warbler") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme


piwo<-fmascale[fmascale$Species=="PileatedWoodpecker",]
gg.piwo<-piwo %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Pileated Woodpecker") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



rubl<-fmascale[fmascale$Species=="RustyBlackbird",]
gg.rubl<-rubl %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Rusty Blackbird") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme


weta<-fmascale[fmascale$Species=="WesternTanager",]
gg.weta<-weta %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Western Tanager") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



wewp<-fmascale[fmascale$Species=="WesternWoodPewee",]
gg.wewp<-wewp %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Western Wood-pewee") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



wwcr<-fmascale[fmascale$Species=="WhitewingedCrossbill",]
gg.wwcr<-wwcr %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="White-winged Crossbill") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



ybsa<-fmascale[fmascale$Species=="YellowbelliedSapsucker",]
gg.ybsa<-ybsa %>%
  ggplot(aes(Year, Abund.FMA.meanNRV)) +
  geom_line(aes(x=Year, y=Abund.FMA.meanNRV), colour="red", size=1, position=position_jitter(w=0, h=0))+
  geom_line(aes(x=Year, y=Abund.FMA.caribou), colour="darkgreen", size=2, position=position_jitter(w=0.4, h=0.4))+
  geom_line(aes(x=Year, y=Abund.FMA.ebm), colour="blue", size=2, position=position_jitter(w=0.4, h=0.4))+
  labs(x="Year", y="Yellow-bellied Sapsucker") +
  #geom_line(aes(x=Year, y=L50CI.NRV), size=1, colour="lightblue", linetype=2)+ 
  #geom_line(aes(x=Year, y=U50CI.NRV), size=1, colour="lightblue", linetype=2)+
  geom_line(aes(x=Year, y=L95CI.NRV), size=1, linetype=2)+ 
  geom_line(aes(x=Year, y=U95CI.NRV), size=1, linetype=2)+my.theme



# Plot

tiff('3_output/figures/bird species plots FMA scale/PatchworksFMAMay22_nolegend.tiff', units="in", width=18, height=15, res=300)
p3 <- grid.arrange(arrangeGrob(gg.attw + theme(legend.position="none"),
                               gg.bbwa + theme(legend.position="none"),
                               gg.bbwo + theme(legend.position="none"),
                               gg.btnw + theme(legend.position="none"),
                               gg.blpw + theme(legend.position="none"),
                               gg.boch + theme(legend.position="none"),
                               gg.brcr + theme(legend.position="none"),
                               gg.cawa + theme(legend.position="none"),
                               gg.cmwa + theme(legend.position="none"),
                               gg.evgr + theme(legend.position="none"),
                               gg.nofl + theme(legend.position="none"),
                               gg.osfl + theme(legend.position="none"),
                               gg.oven + theme(legend.position="none"),
                               gg.pawa + theme(legend.position="none"),
                               gg.piwo + theme(legend.position="none"),
                               gg.rubl + theme(legend.position="none"),
                               gg.weta + theme(legend.position="none"),
                               gg.wewp + theme(legend.position="none"),
                               gg.wwcr + theme(legend.position="none"),
                               gg.ybsa + theme(legend.position="none"),
                               nrow=5, ncol=4))
dev.off()
#


