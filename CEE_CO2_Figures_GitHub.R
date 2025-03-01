#Symptoms of a changing carbon cycle: a decadal-scale breakpoint in soil respiration in a Northeastern US forest 
#Figures 
#Updated 02-17-25

#Load packages##########

#Plotting 
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpubr)
library(pals)

#Data processing
library(dplyr)
library(Rmisc)

#Statistics
library(nlme)
library(EnvStats)

#Load processed data####
#Note: see data import and processing code for data citations and data processing steps
#Data are publicly available through Hubbard Brook Ecosystem Study Data Catalog EDI Repository, https://hubbardbrook.org/data-catalog/

breaks_annual <- readRDS(".../Processed Data/Processed R Data/breaks_annual.RData")
breaks_field <- readRDS(".../Processed Data/Processed R Data/breaks_field.RData") 
breaks_lab <- readRDS(".../Processed Data/Processed R Data/breaks_lab.RData") 
HBEF_clim <- readRDS(".../Processed Data/Processed R Data/HBEF_clim.RData")
sensor_dat <- readRDS(".../Processed Data/Processed R Data/sensor_dat.RData")
regional_atm <- readRDS(".../Processed Data/Processed R Data/regional_atm.RData")
precip_chem <- readRDS(".../Processed Data/Processed R Data/precip_chem.RData") 
lysim_data <- readRDS(".../Processed Data/Processed R Data/lysim_data.RData")
W6_2002 <- read.table(".../Manuscript Data/w6_2002veg.txt", sep=",", header=T)
W6_2007<- read.table(".../Manuscript Data/w62007veg.txt", sep=",", header=T)
W6_2012 <- read.table(".../Manuscript Data/w62012veg.txt", sep=",", header=T)
W6_2017 <- read.table(".../Manuscript Data/w6_2017_vegInventory.csv", sep=",", header=T)
ff_carbon <- read.csv(".../Manuscript Data/FF_C_pool.csv")

#Datasets new for R1 response####
#Note: same dataset as originally downloaded, but retaining hourly time periods
temp_hourly <- read.csv(".../Revision 1/R1 Data/hourly_soil_temperature_gradient_plots.csv")
#Groffman, P.M., J. Duran, J.L. Morse, G.F. Wilson, and M.B. Green. 2022. Soil temperature along an elevation gradient at the Hubbard Brook Experimental Forest, 2010 - present ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/85d9bc546191e92dad23077acc90a4bf (Accessed 2025-01-16).

#Fig. 1: Env Chg#### 
#Note: No change in R1

#Air temperature####

air_temp <- ggplot(subset(HBEF_clim, Year <= 2021)) + 
  geom_point(aes(x=Year, y=MAT.degreesC), shape=21, fill="darkslateblue", size=3, show.legend=FALSE) +
  stat_smooth(aes(x=Year, y=MAT.degreesC), method="lm", formula=y~x, color="black", se=FALSE) + 
  #geom_point(aes(x=Year, y=JulAug_MAT.degreesC, fill="July & August", shape="July & August"), size=3, show.legend=FALSE) +
  #stat_smooth(aes(x=Year, y=JulAug_MAT.degreesC), method="lm", formula=y~x, color="black", se=FALSE) + 
  #scale_fill_manual(values=c("orange","deepskyblue4")) + 
  scale_shape_manual(values=c(23,21)) + 
  scale_y_continuous(limits=c(4,7.5)) + 
  scale_x_continuous(limits=c(1956,2021), breaks=seq(1960,2020,10)) + 
  #geom_vline(xintercept=2015) + 
  #geom_vline(xintercept=2021) + 
  #scale_x_continuous(limits=c(2001,2022), breaks=seq(2001,2022,6)) + 
  #scale_y_continuous(limits=c(0,20)) + 
  labs(x="Year",y=expression(paste("Mean annual air temperature"," ","(",degree,"C)"))) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        aspect.ratio=3/3.5, 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=14, color="black"))

JulAug_air_temp_lm = lm(JulAug_MAT.degreesC~Year, data=HBEF_clim)
summary(JulAug_air_temp_lm)

air_temp_lm = lm(MAT.degreesC~Year, data=HBEF_clim)
summary(air_temp_lm)

# pdf(".../Figures/Component Figures/HBEF_airtemp.pdf", width=5.5, height=3.5) 
# print(air_temp)
# dev.off() 

#Mean air temp 2015
Dat_2015_temp <- subset(HBEF_clim, Year == 2015)
avg_temp_2015 <- summarySE(Dat_2015_temp, measurevar = "JulAug_MAT.degreesC")
#18.32124

#Mean CO2-rate pre-2015
Dat_2015 <- subset(breaks_field, Year_contin < 2015) 
JulAug_2015 <- subset(Dat_2015, Month == "07" | Month == "08")

Dat_2020 <- subset(breaks_field, Year == "2020")
JulAug_2020 <- subset(Dat_2020, Month == "07" | Month == "08")

avg_2015 = summarySE(JulAug_2015, measurevar = "CO2_rate", na.rm=TRUE)
#0.04346239

avg_2020 = summarySE(JulAug_2020, measurevar = "CO2_rate")
#0.09235417

#Precipitation####

precip <- ggplot(subset(HBEF_clim, Year <=2021)) + 
  geom_point(aes(x=Year, y=Total_Precip.mm/1000), fill="darkslateblue", pch=21, size=3) + 
  stat_smooth(aes(x=Year, y=Total_Precip.mm/1000), method="lm", formula=y~x, color="black", se=FALSE) + 
  scale_x_continuous(limits=c(1956,2021), breaks=seq(1960,2020,10)) + 
  scale_y_continuous(limits=c(0.75,2)) + 
  labs(x="Year",y=expression(paste("Total yearly precipitation (m)"))) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        aspect.ratio=3/3.5,
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=14, color="black"))

precip_lm_all <- lm(Total_Precip.mm~Year, data=subset(HBEF_clim, Year <=2021))
summary(precip_lm_all)
precip_lm_recent = lm(Total_Precip.mm~Year, data=subset(HBEF_clim, Year >=2002 & Year <=2021))
summary(precip_lm_recent) #-3.686

# pdf(".../Figures/Component Figures/HBEF_precip.pdf", width=5.5, height=3.5) 
# print(precip) 
# dev.off() 

#Acid precipitation####

sulfate_dat <- precip_chem

sulfate_dat$SO4 <- ifelse(sulfate_dat$SO4 == -3, NA, sulfate_dat$SO4)

sulfate_mean <- summarySE(sulfate_dat, measurevar = "SO4", groupvars = c("year"), na.rm=TRUE)

sulfate_mean <- sulfate_mean %>% filter(year > 1969)

sulfate <- ggplot(subset(sulfate_mean, year <= 2021)) + 
  geom_point(aes(x=year, y=SO4), fill="darkslateblue", pch=21, size=3) + 
  stat_smooth(aes(x=year, y=SO4), method="lm", formula=y~x, color="black", se=FALSE) + 
  scale_x_continuous(limits=c(1956,2021), breaks=seq(1960,2020,10)) + 
  #scale_y_continuous(limits=c(1200,1800)) + 
  labs(x="Year",y=expression(paste("Wet deposition sulfate (mg"," ",L^-1,")"))) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        aspect.ratio=3/3.5, 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=14, color="black"))

sulfate_lm <- lm(SO4~year, subset(sulfate_mean, year <= 2021))
summary(sulfate_lm)
#-0.061063

# pdf(".../Figures/Component Figures/HBEF_sulfate.pdf", width=5.5, height=3.5) 
# print(sulfate) 
# dev.off() 

#Harvard CO2####

CO2 <- ggplot(subset(regional_atm)) + 
  geom_point(aes(x=Year, y=HARV_CO2.ppm), pch=21, fill="darkslateblue", size=3.5) + 
  stat_smooth(aes(x=Year, y=HARV_CO2.ppm), method="lm", formula=y~x, color="black", se=FALSE) + 
  geom_errorbar(aes(x=Year, ymax=HARV_CO2.ppm+HARV_CO2_sd, ymin=HARV_CO2.ppm-HARV_CO2_sd)) + 
  #scale_x_continuous(breaks=seq(2000,2022,4)) + 
  scale_y_continuous(limits=c(350,440)) + 
  labs(x="Year",y=expression(paste("Atm."," ",CO[2]," ","concentration (ppm)"))) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        aspect.ratio=3/3.5, 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=14, color="black"))

CO2_lm = lm(HARV_CO2.ppm~Year, data=subset(regional_atm, Year >= 2002))
summary(CO2_lm) #2.4880

# pdf(".../Figures/Component Figures/CO2_HARV.pdf", width=5.5, height=3.5) 
# print(CO2)
# dev.off() 

#Above-ground biomass####

W6_2002$Year <- rep("2002", nrow(W6_2002))
W6_2002$bbd <- rep(NA, nrow(W6_2002))
W6_2007$Year <- rep("2007", nrow(W6_2007))
W6_2007$bbd <- rep(NA, nrow(W6_2007))
W6_2012$Year <- rep("2012", nrow(W6_2012))
W6_2017$Year <- rep("2017", nrow(W6_2017))

names(W6_2017)[names(W6_2017) == "Analysis.Code"] <- "AnalysisCode"

W6_inventory <- rbind(W6_2002, W6_2007, W6_2012, W6_2017)

W6_inventory$Plot <- as.factor(W6_inventory$Plot)

#Vigor codes 4 and 5 = standing dead trees = exclude 
#Greater than 10 cm DBH 
#Sum by plot, divide by plot area (X10Area)

W6_inventory_filtered <- W6_inventory %>% 
  dplyr::filter(Dbh >= 10 & !(Vigor == 4 | Vigor == 5))

W6_total <- W6_inventory_filtered %>% 
  dplyr::group_by(Year, Plot, X10Area) %>% 
  dplyr::summarize(AbvBio_plot.kg = sum(AbvBmss, na.rm=TRUE), BlwBio_plot.kg = sum(BlwBmss, na.rm=TRUE))

W6_total$AbvBio_plot.Mg = W6_total$AbvBio_plot.kg / 1000
W6_total$BlwBio_plot.Mg = W6_total$BlwBio_plot.kg / 1000 

W6_total$AbvBio_per_m2.Mgm2 = W6_total$AbvBio_plot.Mg / W6_total$X10Area
W6_total$BlwBio_per_m2.Mgm2 = W6_total$BlwBio_plot.Mg / W6_total$X10Area

W6_total$AbvBio_per_hectare.Mgha = W6_total$AbvBio_per_m2.Mgm2 * 10000
W6_total$BlwBio_per_hectare.Mgha = W6_total$BlwBio_per_m2.Mgm2 * 10000

W6_mean <- W6_total %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(AbvBio_mean.Mgha = mean(AbvBio_per_hectare.Mgha, na.rm=TRUE))

biomass <- ggplot(W6_total) + 
  geom_boxplot(aes(x=Year, y=AbvBio_per_hectare.Mgha), fill="darkslateblue") + 
  theme_bw() + 
  labs(x="Year", y=expression(paste("Above-ground biomass (Mg"," ",ha^-1,")"))) + 
  theme(panel.grid=element_blank(), 
        aspect.ratio=3/3.5, 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=14, color="black"))

anova_biomass <- aov(AbvBio_per_hectare.Mgha~Year, data=W6_total)
TukeyHSD(anova_biomass)

# pdf(".../Figures/Component Figures/HBEF_biomass.pdf", width=5.5, height=3.5) 
# print(biomass) 
# dev.off() 

#pH####

lab_july <- subset(breaks_lab, Month == "07")

#All pH measurements

lab_july$Elevation_group <- ifelse(lab_july$Elevation == "high" | lab_july$Elevation == "spruce_fir", "Higher elevation","Lower elevation")

lab_july$Site <- factor(lab_july$Site, levels=c("BearBrook","W1"), labels=c("Reference","Ca-treated")) 

min_pH <- subset(lab_july, Hor == "Min")

pH_combined <- ggplot() + 
  geom_point(aes(x=Year_contin, y=pH), data=min_pH, fill="darkslateblue", size=3.5, alpha=0.5, pch=21) + 
  stat_smooth(aes(x=Year_contin, y=pH), data=subset(min_pH, Year_contin < 2015), method="lm", color="red", size=1.5, se=FALSE) +
  stat_smooth(aes(x=Year_contin, y=pH), data=subset(min_pH, Year_contin >= 2015), method="lm", color="red", se=FALSE, size=1.5) +
  scale_x_continuous(limits=c(2000,2022)) + 
  scale_y_continuous(limits=c(3,5.5)) + 
  #facet_grid(factor(Hor, levels=c("Oi/Oe","Oa/A","Min"))~Site) + 
  labs(x="Year",y="Soil pH") + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        aspect.ratio=3/3.5, 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=14, color="black")) 

pH_pre2015 <- lm(pH~Year_contin, data=subset(min_pH, Year_contin <2015))
pH_post2015 <- lm(pH~Year_contin, data=subset(min_pH, Year_contin >= 2015))

# pdf(".../Figures/Component Figures/pH_combined.pdf", width=5.5, height=3.5)
# print(pH_combined)
# dev.off() 

#SI Fig. 1: Soil pH by watershed####
#No change in R1

pH_all <- ggplot() + 
  geom_point(aes(x=Year_contin, y=pH, fill=Elevation_group), data=lab_july, pch=21) + 
  scale_fill_manual(values=c("white","grey50")) + 
  stat_smooth(aes(x=Year_contin, y=pH, group=Elevation_group), data=subset(lab_july, Year_contin >= 2015), method="lm", color="blue") +
  scale_x_continuous(limits=c(2000,2022)) + 
  scale_y_continuous(limits=c(3,5.5)) + 
  facet_grid(factor(Hor, levels=c("Oi/Oe","Oa/A","Min"))~Site) + 
  labs(x="Year",y="Soil pH", fill="Elevation") + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"), 
        legend.title=element_text(size=12, color="black"))

pdf(".../Figures/Component Figures/pH_trend.pdf", width=7, height=6)
print(pH_all)
#dev.off 

pH_2015 <- subset(lab_july, Year == "2015")
mean(pH_2015$pH, na.rm=TRUE)

pH_2021 <- subset(lab_july, Year == "2021")
mean(pH_2021$pH, na.rm=TRUE)

pH_mean <- summarySE(lab_july, measurevar="pH", groupvars=c("Year", "Year_contin"), na.rm=TRUE)

#Fig. 2: Annual fluxes####
#Revised in R1 to show all points separately with one overall mean; resolves issue with overlapping breakpoint dates

annual_plot = breaks_annual
annual_plot$phase = factor(annual_plot$phase, levels=c("PRE","POST"), labels=c("Pre-breakpoint", "Post-breakpoint"))
annual_plot$HPU = factor(annual_plot$HPU, levels=c("Bhs","Bimodal"), labels=c("Higher elevation", "Lower elevation"))
annual_plot$Site = factor(annual_plot$Site, levels=c("W1","BearBrook"), labels=c("Ca-treated", "Reference"))

pdf(".../Revision 1/R1 Figures/Component Figures/annual_flux.pdf", width=8, height=5.5)
print(ggplot(annual_plot) + 
        geom_point(aes(x=Year_contin, y=CO2_annual_flux, fill=phase), pch=21, size=3, alpha=0.3) + 
        stat_summary(aes(x=Year_contin, y=CO2_annual_flux), pch=24, geom="point",fun="mean", size=3, fill="black") + 
        stat_summary(aes(x=Year_contin, y=CO2_annual_flux), geom="errorbar",fun.data="mean_se") + 
        stat_smooth(aes(x=Year_contin, y=CO2_annual_flux, color=phase), method="lm", formula=y~x) + 
        labs(x="Year", y=expression(paste("Growing season respiration flux (g"," ",CO[2]," ","-C"," ",m^-2," ",season^-1,")"))) + 
        scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        scale_color_manual(values=c("darkmagenta","darkcyan")) +
        theme_bw() + 
        facet_grid(Site~HPU) + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
#dev.off

#Check breakpoints at all landscape positions: 
# pdf(".../Figures/Component Figures/annual_flux_elevation.pdf", width=8, height=5)
# print(ggplot(annual_plot) + 
#         stat_summary(aes(x=Year_contin, y=CO2_annual_flux, fill=phase), pch=21, geom="point",fun="mean", size=3) + 
#         stat_summary(aes(x=Year_contin, y=CO2_annual_flux), geom="errorbar",fun.data="mean_se") + 
#         stat_smooth(aes(x=Year_contin, y=CO2_annual_flux, color=phase), method="lm", formula=y~x) + 
#         labs(x="Date (YYYY)", y=expression(paste("Growing season respiration flux (g"," ",CO[2]," ","-C"," ",m^-2," ",yr^-1,")"))) + 
#         scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
#         scale_color_manual(values=c("darkmagenta","darkcyan")) +
#         theme_bw() + 
#         facet_grid(Site~Elevation) + 
#         theme(panel.grid=element_blank(), 
#               legend.title=element_blank(), 
#               legend.text=element_text(size=14, color="black"), 
#               axis.text.y=element_text(size=14, color="black"),
#               axis.title=element_text(size=14, color="black"), 
#               axis.text.x=element_text(size=14, color="black"),
#               strip.text=element_text(size=14, color="black"), 
#               strip.background=element_rect(fill="white")))
# #dev.off

#Average flux pre-2015 
pre2015_flux <- annual_plot %>% 
  filter(Year_contin < 2015) 

#Average flux 2020
only2020_flux <- annual_plot %>% 
  filter(Year_contin == 2020) %>% 
  summarize(mean_2020 = mean(CO2_annual_flux, na.rm=TRUE))

#Average slope post-2015
post2015_flux <- annual_plot %>% 
  filter(Year_contin >= 2015)

increase <- lm(CO2_annual_flux~Year_contin, data=post2015_flux)
pre_change <- lm(CO2_annual_flux~Year_contin, data=pre2015_flux)

#Average pre-2015
pre2015_flux <- annual_plot %>% 
  filter(Year_contin < 2015) %>% 
  summarize(Mean_flux = mean(CO2_annual_flux, na.rm=TRUE))

#Relative to forest floor carbon####
str(ff_carbon)
ff_carbon$Horizon <- as.factor(ff_carbon$Horizon)

Oie <- ff_carbon %>% 
  filter(Horizon == "Oie")

mean_Oie <- summarySE(groupvars=c("Year"), measurevar="C_Pool", data=Oie)

Oa <- ff_carbon %>% 
  filter(Horizon == "Oa")

mean_Oa <- summarySE(groupvars=c("Year"), measurevar="C_Pool", data=Oa)

total_FF <- as.data.frame(cbind(mean_Oa$Year, mean_Oa$N))

total_FF$C_pool <- mean_Oa$C_Pool + mean_Oie$C_Pool

#SI Fig. 2: pH and Al correlation####
#No change in R1

#Average annual fluxes
avg_CO2 <- summarySE(breaks_annual, na.rm=TRUE, measurevar = "CO2_annual_flux", groupvars = c("Year_contin"))
avg_CO2$Year <- avg_CO2$Year_contin

#Reference WS
lab_july_BearBrook <- subset(lab_july, Site == "Reference")
pH_mean_BearBrook <- summarySE(lab_july_BearBrook, measurevar="pH", groupvars=c("Year", "Year_contin"), na.rm=TRUE)

#Monomeric Al
lysim_current <- filter(lysim_data, Year_contin >= 2002)
#Al_mean_BearBrook <- summarySE(lysim_current, measurevar="Alm", groupvars=c("year", "Year_contin"), na.rm=TRUE)

lysim_current$Ali_calculated <- lysim_current$Alm - lysim_current$Alo
lysim_data$Ali_calculated <- lysim_data$Alm - lysim_data$Alo

Ali_mean_BearBrook <- summarySE(lysim_current, measurevar="Ali_calculated", groupvars=c("year", "Year_contin"), na.rm=TRUE)

pH_CO2 <- avg_CO2 %>% right_join(pH_mean_BearBrook, by="Year_contin")

pH_CO2_Al <- pH_CO2 %>% right_join(Ali_mean_BearBrook, by="Year_contin")

pH_CO2_Al$phase <- ifelse(pH_CO2_Al$Year_contin < 2015, "Pre-2015", "2015-present")

pH_CO2_plot <- ggplot() + 
  geom_point(aes(x=pH, y=CO2_annual_flux), fill="black", pch=21, size=2, data=pH_CO2_Al) +
  stat_smooth(aes(x=pH, y=CO2_annual_flux),  color="black", method="lm", data=pH_CO2_Al) + 
  #scale_fill_manual(values=c("darkcyan","darkmagenta")) + 
  #scale_color_manual(values=c("darkcyan","darkmagenta")) + 
  theme_bw() + 
  labs(x="Soil pH", y=expression(paste("Growing season resp. (g"," ",CO[2]," ","-C"," ",m^-2," ",season^-1,")"))) + 
  theme(panel.grid=element_blank(), 
        legend.title=element_blank(), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, color="black"))

#Aluminum 

Ali_plot <- ggplot(lysim_current) + 
  stat_summary(aes(x=Year_contin, y=Ali_calculated), fun.data="mean_se", geom="point", size=2) + 
  stat_summary(aes(x=Year_contin, y=Ali_calculated), fun.data="mean_se", geom="errorbar") + 
  theme_bw() + 
  labs(x="Year", y=expression(paste("Total inorg. monomeric aluminum (",mu,"mol"," ",L^-1,")"))) + 
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, color="black"))

Al_combined <- ggplot(lysim_data) + 
  stat_summary(aes(x=Year_contin, y=Ali_calculated), fun.data="mean_se", geom="point", size=2) + 
  stat_summary(aes(x=Year_contin, y=Ali_calculated), fun.data="mean_se", geom="errorbar") + 
  stat_summary(aes(x=Year_contin, y=Alo), color="blue", fun.data="mean_se", geom="point", size=2) + 
  stat_summary(aes(x=Year_contin, y=Alo), color="blue", fun.data="mean_se", geom="errorbar") + 
  stat_summary(aes(x=Year_contin, y=Alm), color="red", fun.data="mean_se", geom="point", size=2) + 
  stat_summary(aes(x=Year_contin, y=Alm), color="red", fun.data="mean_se", geom="errorbar") + 
  theme_bw() + 
  labs(x="Year", y=expression(paste("Monomeric aluminum (",mu,"mol"," ",L^-1,")"))) + 
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black"))

Al_CO2_plot <- ggplot() + 
  geom_point(aes(x=Ali_calculated, y=CO2_annual_flux), fill="black", pch=21, size=2, data=pH_CO2_Al) +
  stat_smooth(aes(x=Ali_calculated, y=CO2_annual_flux),  color="black", method="lm", data=pH_CO2_Al) + 
  #scale_fill_manual(values=c("darkcyan","darkmagenta")) + 
  #scale_color_manual(values=c("darkcyan","darkmagenta")) + 
  theme_bw() + 
  labs(x=expression(paste("Total inorg. monomeric aluminum (",mu,"mol"," ",L^-1,")")), y=expression(paste("Growing season resp. (g"," ",CO[2]," ","-C"," ",m^-2," ",season^-1,")"))) + 
  theme(panel.grid=element_blank(), 
        legend.title=element_blank(), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, color="black"))

Al_CO2_plot_poly <- ggplot() + 
  geom_point(aes(x=Ali_calculated, y=CO2_annual_flux), fill="black", pch=21, size=2, data=pH_CO2_Al) +
  stat_smooth(aes(x=Ali_calculated, y=CO2_annual_flux),  color="black", method="lm", formula=y~poly(x,2), data=pH_CO2_Al) + 
  #scale_fill_manual(values=c("darkcyan","darkmagenta")) + 
  #scale_color_manual(values=c("darkcyan","darkmagenta")) + 
  theme_bw() + 
  labs(x=expression(paste("Total inorg. monomeric aluminum (",mu,"mol"," ",L^-1,")")), y=expression(paste("Growing season respiration (g"," ",CO[2]," ","-C"," ",m^-2," ",season^-1,")"))) + 
  theme(panel.grid=element_blank(), 
        legend.title=element_blank(), 
        axis.text=element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black"))

pdf(".../Figures/Component Figures/pH_CO2.pdf", width=5.2, height=5.4)
print(pH_CO2_plot)
#dev.off 

pdf(".../Figures/Component Figures/Al_CO2.pdf", width=5.2, height=5.4)
print(Al_CO2_plot)
#dev.off 

pdf(".../Figures/Component Figures/Al_time.pdf", width=5.2, height=5.4)
print(Ali_plot)
#dev.off 

pH_CO2_corr <- cor.test(pH_CO2$pH, pH_CO2$CO2_annual_flux, method="spearman")
pH_after_2015 <- subset(pH_CO2, Year_contin >= 2015)
pH_CO2_2015 <- cor.test(pH_after_2015$pH, pH_after_2015$CO2_annual_flux, method="spearman")

pH_CO2_lm <- lm(CO2_annual_flux~pH, data=pH_CO2) 

Al_CO2_corr <- cor.test(pH_CO2_Al$Ali_calculated, pH_CO2_Al$CO2_annual_flux, method="spearman")
Al_after_2015 <- subset(pH_CO2_Al, Year_contin >= 2015)
Al_CO2_2015 <- cor.test(Al_after_2015$pH, Al_after_2015$CO2_annual_flux, method="spearman")

Al_CO2_poly <- lm(CO2_annual_flux~poly(Ali_calculated,2), data=pH_CO2_Al)

#SI Fig. 3: Map####
#See mapping code 

#SI Fig. 4: DIC####
#No change in R1 

DIC_all <- ggplot(lysim_data) + 
  geom_point(aes(x=Date_posixct, y=DIC)) + 
  facet_grid(Site~Horizon)

#Group lysimeter data by pre- and post break years 

lysim_data$temp <- lysim_data$Date
lysim_data <- lysim_data %>% tidyr::separate(temp, into=c("year","month","day"), sep="-")
lysim_data$phase <- ifelse(lysim_data$year < 2015, "Pre-2015","2015+")
lysim_data$Year_contin <- as.numeric(lysim_data$year)

DIC_breaks <- ggplot(lysim_data) + 
  geom_point(aes(x=Date_posixct, y=DIC, fill=phase), pch=21) + 
  stat_smooth(aes(x=Date_posixct, y=DIC, group=phase), method="lm", formula=y~x) + 
  facet_grid(Site~Horizon)

DIC_breaks_2001 <- ggplot(subset(lysim_data, year > 2001)) + 
  geom_point(aes(x=Date_posixct, y=DIC, fill=phase), pch=21) + 
  stat_smooth(aes(x=Date_posixct, y=DIC, group=phase), method="lm", formula=y~x) + 
  facet_grid(Site~Horizon)

DIC_all_combined <- ggplot(subset(lysim_data, year > 2001)) + 
  geom_point(aes(x=Date_posixct, y=DIC, fill=phase), pch=21) + 
  scale_fill_manual(values=c("darkcyan","darkmagenta")) + 
  stat_smooth(aes(x=Date_posixct, y=DIC, group=phase), color="black", method="lm", formula=y~x) + 
  theme_bw() + 
  labs(x="Year", y=expression(paste("Dissolved inorganic carbon (",mu,"mol"," ",L^-1,")"))) + 
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=14, color="black"), 
        legend.text=element_text(size=14, color="black"), 
        legend.title=element_blank())

#DIC by phase
DIC_lm_sqrt <- lm(sqrt(DIC)~Year_contin*phase, data=subset(lysim_data, year > 2001))
qqPlot(DIC_lm_sqrt$residuals)

DIC_lm_nt <- lm(DIC~Year_contin*phase, data=subset(lysim_data, year > 2001))
qqPlot(DIC_lm_nt$residuals)

summary(DIC_lm_sqrt)
anova(DIC_lm_sqrt)

# Analysis of Variance Table

# Response: sqrt(DIC)
# Df Sum Sq Mean Sq F value    Pr(>F)    
# Year_contin          1   1114  1113.8  37.631 1.010e-09 ***
#   phase                1    357   357.4  12.075 0.0005208 ***
#   Year_contin:phase    1   1016  1015.9  34.324 5.367e-09 ***
#   Residuals         2206  65293    29.6                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Slope pre- and post-2015

DIC_pre2015 <- lm(DIC~Year_contin, data=subset(lysim_data, year > 2001 & year < 2015))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -12119.033   2372.456  -5.108 3.71e-07 ***
#   Year_contin      6.173      1.181   5.227 1.99e-07 ***

DIC_2015 <- lm(DIC~Year_contin, data=subset(lysim_data, year >= 2015))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -49039.850   8766.201  -5.594 3.01e-08 ***
#   Year_contin     24.464      4.345   5.630 2.46e-08 ***



#Fig. 3: Respiration rates####
#Updated to show all months and July/August only 

plot_field = breaks_field
plot_field$phase = factor(plot_field$phase, levels=c("PRE","POST"), labels=c("Pre-breakpoint", "Post-breakpoint"))
plot_field$HPU = factor(plot_field$HPU, levels=c("Bhs","Bimodal"), labels=c("Higher elevation", "Lower elevation"))
plot_field$Site = factor(plot_field$Site, levels=c("W1","BearBrook"), labels=c("Ca-treated", "Reference"))

# pdf(".../Revision 1/R1 Figures/Component Figures/resp_rate_all.pdf", width=6, height=4)
print(ggplot(plot_field) + 
        geom_point(aes(x=Date.posixct, y=CO2_rate, fill=phase), pch=21, alpha=0.5, size=3) + 
        # stat_summary(aes(x=Date.posixct, y=CO2_rate), geom="errorbar",fun.data="mean_se") + 
        stat_smooth(aes(x=Date.posixct, y=CO2_rate, group=phase), color="black", method="lm", formula=y~x) + 
        labs(x="Year", y=expression(paste("Soil respiration rate (g"," ",CO[2]," ","-C"," ",m^-2," ",hr^-1,")"))) + 
        scale_y_continuous(limits=c(-0.10, 0.25)) + 
        scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        # scale_color_manual(values=c("darkmagenta","darkcyan")) +
        theme_bw() + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
# dev.off()

#July and August only
# pdf(".../Revision 1/R1 Figures/Component Figures/resp_rate_jul_aug_new.pdf", width=6, height=4)
print(ggplot(subset(plot_field, Month == "07" | Month == "08")) + 
        geom_point(aes(x=Date.posixct, y=CO2_rate, fill=phase), pch=21, alpha=0.5, size=3) + 
        # stat_summary(aes(x=Date.posixct, y=CO2_rate), geom="errorbar",fun.data="mean_se") + 
        stat_smooth(aes(x=Date.posixct, y=CO2_rate, group=phase), color="black", method="lm", formula=y~x) + 
        labs(x="Year", y=expression(paste("Soil respiration rate (g"," ",CO[2]," ","-C"," ",m^-2," ",hr^-1,")"))) + 
        scale_y_continuous(limits=c(-0.10, 0.25)) + 
        scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        # scale_color_manual(values=c("darkmagenta","darkcyan")) +
        theme_bw() + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
# dev.off()

#Fig. 4 and new Fig. 5: Microbial parameters####
#Fig 4 and Fig. 5: Revised version with all points

plot_lab = breaks_lab
plot_lab$phase = factor(plot_lab$phase, levels=c("PRE","POST"), labels=c("Pre-breakpoint", "Post-breakpoint"))
plot_lab$HPU = factor(plot_lab$HPU, levels=c("Bhs","Bimodal"), labels=c("Higher elevation", "Lower elevation"))
plot_lab$Site = factor(plot_lab$Site, levels=c("W1","BearBrook"), labels=c("Ca-treated", "Reference"))
plot_lab$Hor = factor(plot_lab$Hor, levels=c("Oi/Oe", "Oa/A","Min"), labels=c("Oi/Oe", "Oa/A","Mineral soil"))

#1: BIOC##########

BIOC_JunJul <- subset(plot_lab, Month == "06" | Month == "07")

str(BIOC_JunJul)

# pdf(".../Revision 1/R1 Figures/Component Figures/BIOC_JunJul.pdf", width=5.25, height=7)
print(ggplot(BIOC_JunJul) +  
        geom_point(aes(x=Year_contin, y=BIOC/1000, fill=phase), pch=21, size=3, alpha=0.1, show.legend=TRUE) + 
        stat_summary(aes(x=Year_contin, y=BIOC/1000), pch=24, geom="point",fun="mean", size=2, fill="black") + 
        stat_summary(aes(x=Year_contin, y=BIOC/1000), geom="errorbar", fun.data="mean_se", color="black") + 
        stat_smooth(aes(x=Year_contin, y=BIOC/1000, group=phase), method="lm", size=0.5, formula=y~x, color="black") + 
        labs(x="Year", y=expression(paste("Microbial biomass carbon (g C kg"," ",soil^-1,")"))) + 
        scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        scale_color_manual(values=c("darkmagenta","darkcyan")) +
        theme_bw() + 
        facet_grid(Hor~., scales="free_y") + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
# dev.off() 

#Ln-transformed (SI Fig)
# pdf(".../Revision 1/R1 Figures/Component Figures/BIOC_ln_OiOe.pdf", width=5.75, height=4)
print(ggplot(subset(BIOC_JunJul, phase == "Post-breakpoint" & Hor == "Oi/Oe")) +  
        stat_summary(aes(x=Year_contin, y=log(BIOC/1000), fill=HPU, shape=HPU), geom="point",fun="mean", size=3) + 
        stat_summary(aes(x=Year_contin, y=log(BIOC/1000), group=HPU), color="black", geom="errorbar", fun.data="mean_se") + 
        stat_smooth(aes(x=Year_contin, y=log(BIOC/1000), color=HPU), method="lm", formula=y~x) + 
        labs(x="Year", y=expression(paste("ln Microbial biomass carbon (g C kg"," ",soil^-1,")"))) + 
        #scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        #scale_color_manual(values=c("darkmagenta","darkcyan")) +
        scale_fill_manual(values=c("darkorange","darkblue")) + 
        scale_color_manual(values=c("darkorange","darkblue")) + 
        scale_shape_manual(values=c(22,23)) + 
        #facet_grid(Hor~., scales="free_y") + 
        scale_x_continuous(breaks=seq(2012,2020,2)) + 
        theme_bw() + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.title.y=element_text(size=12, color="black"),  
              axis.text.y=element_text(size=14, color="black"),
              axis.title.x=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
# dev.off() 

#2: RESPC####

RESPC_JunJul <- subset(plot_lab, Month == "06" | Month == "07")

str(RESPC_JunJul)

# pdf(".../Revision 1/R1 Figures/Component Figures/RESPC_JunJul.pdf", width=5.25, height=7)
print(ggplot(RESPC_JunJul) +  
        geom_point(aes(x=Year_contin, y=RESPC, fill=phase), pch=21, size=3, alpha=0.1, show.legend=TRUE) + 
        stat_summary(aes(x=Year_contin, y=RESPC), pch=24, geom="point",fun="mean", size=2, fill="black") + 
        stat_summary(aes(x=Year_contin, y=RESPC), geom="errorbar", fun.data="mean_se", color="black") + 
        stat_smooth(aes(x=Year_contin, y=RESPC, group=phase), method="lm", size=0.5, formula=y~x, color="black") + 
        labs(x="Year", y=expression(paste("Microbial mineralization"," ","(mg C kg"," ",soil^-1," ",d^-1,")"))) + 
        scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        scale_color_manual(values=c("darkmagenta","darkcyan")) +
        theme_bw() + 
        facet_grid(Hor~., scales="free_y") + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
#dev.off

#pdf(".../Revision 1/R1 Figures/Component Figures/RESPC_ln_OiOe.pdf", width=5.75, height=4)
print(ggplot(subset(RESPC_JunJul, phase == "Post-breakpoint" & Hor == "Oi/Oe")) +
        stat_summary(aes(x=Year_contin, y=log(RESPC), fill=HPU, shape=HPU), geom="point",fun="mean", size=2) +
        stat_summary(aes(x=Year_contin, y=log(RESPC), group=HPU), color="black", geom="errorbar", fun.data="mean_se") +
        stat_smooth(aes(x=Year_contin, y=log(RESPC), color=HPU), method="lm", formula=y~x) +
        labs(x="Year", y=expression(paste("ln Microbial mineralization"," ","(mg C kg"," ",soil^-1," ",d^-1,")"))) +
        scale_fill_manual(values=c("darkorange","darkblue")) +
        scale_color_manual(values=c("darkorange","darkblue")) +
        scale_shape_manual(values=c(22,23)) +
        #facet_grid(Hor~., scales="free_y") +
        scale_x_continuous(breaks=seq(2012,2020,2)) +
        theme_bw() +
        theme(panel.grid=element_blank(),
              legend.title=element_blank(),
              legend.text=element_text(size=14, color="black"),
              axis.text.y=element_text(size=14, color="black"),
              axis.title.y=element_text(size=12, color="black"),
              axis.title.x=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"),
              strip.background=element_rect(fill="white")))
#dev.off

#Slope in post-breakpoint
RESPC_OiOe_lm_High = lm(log(RESPC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06") & HPU == "Bhs"))
RESPC_OiOe_lm_Low = lm(log(RESPC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06") & HPU == "Bimodal"))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -211.7516    97.0674  -2.181   0.0311 *
#   Year_contin    0.1077     0.0481   2.239   0.0270 *

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -105.95210   61.89701  -1.712   0.0888 .
# Year_contin    0.05516    0.03069   1.797   0.0740 .

#3: SRESPC##########

SRESPC_JunJul <- subset(plot_lab, Month == "06" | Month == "07")

str(SRESPC_JunJul)

#pdf(".../Revision 1/R1 Figures/Component Figures/SRESPC_JunJul.pdf", width=5, height=7)
print(ggplot(SRESPC_JunJul) +  
        geom_point(aes(x=Year_contin, y=SRESPC, fill=phase), pch=21, size=3, alpha=0.1, show.legend=TRUE) + 
        stat_summary(aes(x=Year_contin, y=SRESPC), pch=24, geom="point",fun="mean", size=2, fill="black") + 
        stat_summary(aes(x=Year_contin, y=SRESPC), geom="errorbar", fun.data="mean_se", color="black") + 
        stat_smooth(aes(x=Year_contin, y=SRESPC, group=phase), method="lm", size=0.5, formula=y~x, color="black") + 
        labs(x="Year", y=expression(paste("Specific respiration"," ","(mg C g"," ",MBC^-1," ",d^-1,")"))) + 
        scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        scale_color_manual(values=c("darkmagenta","darkcyan")) +
        theme_bw() + 
        facet_grid(Hor~., scales="free_y") + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
#dev.off

#pdf(".../Revision 1/R1 Figures/Component Figures/SRESPC_ln.pdf", width=4, height=6)
print(ggplot(subset(SRESPC_JunJul, phase == "Post-breakpoint")) + 
        stat_summary(aes(x=Year_contin, y=log(SRESPC)), fill="black", geom="point",fun="mean", size=2, pch=21) + 
        stat_summary(aes(x=Year_contin, y=log(SRESPC)), color="black", geom="errorbar", fun.data="mean_se") + 
        stat_smooth(aes(x=Year_contin, y=log(SRESPC)), method="lm", color="black", formula=y~x) + 
        labs(x="Year", y=expression(paste("ln Specific respiration"," ","(mg C g"," ",MBC^-1," ",d^-1,")"))) + 
        theme_bw() + 
        facet_grid(Hor~., scales="free_y") + 
        scale_x_continuous(breaks=seq(2012,2020,2)) + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
#dev.off 

#3: CN_ratio####

CN_ratio_JunJul <- subset(plot_lab, Month == "06" | Month == "07")

str(CN_ratio_JunJul)

#pdf(".../Revision 1/R1 Figures/Component Figures/CN_ratio_JunJul.pdf", width=5.25, height=7)
print(ggplot(CN_ratio_JunJul) +  
        geom_point(aes(x=Year_contin, y=CN_ratio, fill=phase), pch=21, size=3, alpha=0.1, show.legend=TRUE) + 
        stat_summary(aes(x=Year_contin, y=CN_ratio), pch=24, geom="point",fun="mean", size=2, fill="black") + 
        stat_summary(aes(x=Year_contin, y=CN_ratio), geom="errorbar", fun.data="mean_se", color="black") + 
        stat_smooth(aes(x=Year_contin, y=CN_ratio, group=phase), method="lm", size=0.5, formula=y~x, color="black") + 
        labs(x="Year", y="Microbial biomass C:N ratio") + 
        scale_fill_manual(values=c("darkmagenta","darkcyan")) + 
        scale_color_manual(values=c("darkmagenta","darkcyan")) +
        theme_bw() + 
        facet_grid(Hor~., scales="free_y") + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title=element_text(size=14, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
#dev.off

#pdf(".../Revision 1/R1 Figures/Component Figures/CNratio_ln_OiOe.pdf", width=5.75, height=4)
print(ggplot(subset(CN_ratio_JunJul, phase == "Post-breakpoint" & Hor == "Oi/Oe")) + 
        stat_summary(aes(x=Year_contin, y=log(CN_ratio), fill=HPU, shape=HPU), geom="point",fun="mean", size=3, show.legend=TRUE) + 
        stat_summary(aes(x=Year_contin, y=log(CN_ratio), group=HPU), geom="errorbar", fun.data="mean_se") + 
        stat_smooth(aes(x=Year_contin, y=log(CN_ratio), color=HPU, group=HPU), method="lm", formula=y~x) + 
        labs(x="Year", y="ln Microbial biomass C:N ratio") + 
        scale_fill_manual(values=c("darkorange","darkblue")) + 
        scale_color_manual(values=c("darkorange","darkblue")) + 
        scale_shape_manual(values=c(22,23)) + 
        theme_bw() + 
        #facet_grid(Hor~., scales="free_y") + 
        scale_x_continuous(breaks=seq(2012,2020,2)) + 
        theme(panel.grid=element_blank(), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              axis.text.y=element_text(size=14, color="black"),
              axis.title.x=element_text(size=14, color="black"), 
              axis.title.y=element_text(size=12, color="black"), 
              axis.text.x=element_text(size=14, color="black"),
              strip.text=element_text(size=14, color="black"), 
              strip.background=element_rect(fill="white")))
#dev.off 

plot_lab = breaks_lab
plot_lab$phase = factor(plot_lab$phase, levels=c("PRE","POST"), labels=c("Pre-breakpoint", "Post-breakpoint"))
plot_lab$HPU = factor(plot_lab$HPU, levels=c("Bhs","Bimodal"), labels=c("Higher elevation", "Lower elevation"))
plot_lab$Site = factor(plot_lab$Site, levels=c("W1","BearBrook"), labels=c("Ca-treated", "Reference"))
plot_lab$Hor = factor(plot_lab$Hor, levels=c("Oi/Oe", "Oa/A","Min"), labels=c("Oi/Oe", "Oa/A","Mineral soil"))

#Statistics for microbial parameters####
#1: BIOC####
#Test for differences in slope across elevation and treatment 
BIOC_OiOe_lme = lme(log(BIOC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_BIOC_OiOe_lme=as.data.frame(anova(BIOC_OiOe_lme))
anova_BIOC_OiOe_lme$variable = rep("BIOC", nrow(anova_BIOC_OiOe_lme))
anova_BIOC_OiOe_lme$horizon = rep("Oi/Oe", nrow(anova_BIOC_OiOe_lme))
# numDF denDF   F-value p-value
# (Intercept)          1   287 11479.952  <.0001
# Year_contin          1   287    12.937  0.0004
# Year_contin:HPU      1   287     3.902  0.0492
# Year_contin:Site     1   287     0.149  0.6999

BIOC_OaA_lme = lme(log(BIOC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_BIOC_OaA_lme=as.data.frame(anova(BIOC_OaA_lme))
anova_BIOC_OaA_lme$variable = rep("BIOC", nrow(anova_BIOC_OaA_lme))
anova_BIOC_OaA_lme$horizon = rep("Oa/A", nrow(anova_BIOC_OaA_lme))
# numDF denDF   F-value p-value
# (Intercept)          1   287 10329.739  <.0001
# Year_contin          1   287     3.354  0.0681
# Year_contin:HPU      1   287     0.184  0.6683
# Year_contin:Site     1   287     0.212  0.6456

BIOC_Min_lme = lme(log(BIOC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_BIOC_Min_lme=as.data.frame(anova(BIOC_Min_lme))
anova_BIOC_Min_lme$variable = rep("BIOC", nrow(anova_BIOC_Min_lme))
anova_BIOC_Min_lme$horizon = rep("Min", nrow(anova_BIOC_Min_lme))
# numDF denDF  F-value p-value
# (Intercept)          1   286 8949.762  <.0001
# Year_contin          1   286    2.533  0.1126
# Year_contin:HPU      1   286    5.348  0.0215
# Year_contin:Site     1   286    0.251  0.6164

BIOC_OiOe_lme_High = lm(log(BIOC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06") & HPU == "Bhs"))
BIOC_OiOe_lme_Low = lm(log(BIOC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06") & HPU == "Bimodal"))

BIOC_OaA_lme_High = lm(log(BIOC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06") & HPU == "Bhs"))
BIOC_OaA_lme_Low = lm(log(BIOC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06") & HPU == "Bimodal"))

BIOC_Min_lme_High = lm(log(BIOC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06") & HPU == "Bhs"))
BIOC_Min_lme_Low = lm(log(BIOC)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06") & HPU == "Bimodal"))

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/BIOC_OiOe_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(BIOC_OiOe_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/BIOC_OaA_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(BIOC_OaA_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/BIOC_Min_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(BIOC_Min_lme))))
#dev.off 

#2: RESPC##########
#Test for differences in slope across elevation and treatment 
RESPC_OiOe_lme = lme(log(RESPC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_RESPC_OiOe_lme=as.data.frame(anova(RESPC_OiOe_lme))
anova_RESPC_OiOe_lme$variable = rep("RESPC", nrow(anova_RESPC_OiOe_lme))
anova_RESPC_OiOe_lme$horizon = rep("Oi/Oe", nrow(anova_RESPC_OiOe_lme))
# numDF denDF  F-value p-value
# (Intercept)          1   287 5535.384  <.0001
# Year_contin          1   287    7.398  0.0069
# Year_contin:HPU      1   287    1.198  0.2746
# Year_contin:Site     1   287    0.197  0.6578

RESPC_OaA_lme = lme(log(RESPC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_RESPC_OaA_lme=as.data.frame(anova(RESPC_OaA_lme))
anova_RESPC_OaA_lme$variable = rep("RESPC", nrow(anova_RESPC_OaA_lme))
anova_RESPC_OaA_lme$horizon = rep("Oa/A", nrow(anova_RESPC_OaA_lme))
# numDF denDF  F-value p-value
# (Intercept)          1   288 340.0007  <.0001
# Year_contin          1   288   0.0021  0.9632
# Year_contin:HPU      1   288   1.4597  0.2280
# Year_contin:Site     1   288   0.5282  0.4679

RESPC_Min_lme = lme(log(RESPC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_RESPC_Min_lme=as.data.frame(anova(RESPC_Min_lme))
anova_RESPC_Min_lme$variable = rep("RESPC", nrow(anova_RESPC_Min_lme))
anova_RESPC_Min_lme$horizon = rep("Min", nrow(anova_RESPC_Min_lme))
# numDF denDF  F-value p-value
# (Intercept)          1   287 450.0949  <.0001
# Year_contin          1   287   1.9385  0.1649
# Year_contin:HPU      1   287   2.2127  0.1380
# Year_contin:Site     1   287   0.0098  0.9213

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/RESPC_OiOe_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(RESPC_OiOe_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/RESPC_OaA_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(RESPC_OaA_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/RESPC_Min_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(RESPC_Min_lme))))
#dev.off 

#3: SRESPC##########
#Test for differences in slope across elevation and treatment 
SRESPC_OiOe_lme = lme(log(SRESPC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_SRESPC_OiOe_lme=as.data.frame(anova(SRESPC_OiOe_lme))
anova_SRESPC_OiOe_lme$variable = rep("SRESPC", nrow(anova_SRESPC_OiOe_lme))
anova_SRESPC_OiOe_lme$horizon = rep("Oi/Oe", nrow(anova_SRESPC_OiOe_lme))
# numDF denDF   F-value p-value
# (Intercept)          1   287 2362.3352  <.0001
# Year_contin          1   287    2.3523  0.1262
# Year_contin:HPU      1   287    1.3871  0.2399
# Year_contin:Site     1   287    0.0169  0.8967
#-0.03957

SRESPC_OaA_lme = lme(log(SRESPC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_SRESPC_OaA_lme=as.data.frame(anova(SRESPC_OaA_lme))
anova_SRESPC_OaA_lme$variable = rep("SRESPC", nrow(anova_SRESPC_OaA_lme))
anova_SRESPC_OaA_lme$horizon = rep("Oa/A", nrow(anova_SRESPC_OaA_lme))
# numDF denDF  F-value p-value
# (Intercept)          1   287 794.0235  <.0001
# Year_contin          1   287   3.8399  0.0510
# Year_contin:HPU      1   287   2.2220  0.1372
# Year_contin:Site     1   287   0.0097  0.9217
#Slope: -0.05615

#Simple correlation 
post_dat <- subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06"))
SRESPC_time <- cor.test(post_dat$SRESPC, post_dat$Year_contin, method="spearman")
h = ggplot(post_dat)
h + geom_point(aes(x=Year_contin, y=SRESPC)) + 
  stat_smooth(aes(x=Year_contin, y=SRESPC), method="lm")
#Negative relationship

SRESPC_Min_lme = lme(log(SRESPC)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_SRESPC_Min_lme=as.data.frame(anova(SRESPC_Min_lme))
anova_SRESPC_Min_lme$variable = rep("SRESPC", nrow(anova_SRESPC_Min_lme))
anova_SRESPC_Min_lme$horizon = rep("Min", nrow(anova_SRESPC_Min_lme))
# numDF denDF  F-value p-value
# (Intercept)          1   286 797.1743  <.0001
# Year_contin          1   286   5.6526  0.0181
# Year_contin:HPU      1   286   0.0577  0.8104
# Year_contin:Site     1   286   0.2800  0.5971
#Slope: -0.06124

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/SRESPC_OiOe_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(SRESPC_OiOe_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/SRESPC_OaA_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(SRESPC_OaA_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/SRESPC_Min_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(SRESPC_Min_lme))))
#dev.off 

#3: CN_ratio##########
#Test for differences in slope across elevation and treatment 
CN_ratio_OiOe_lme = lme(log(CN_ratio)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_CN_ratio_OiOe_lme=as.data.frame(anova(CN_ratio_OiOe_lme))
anova_CN_ratio_OiOe_lme$variable = rep("CN_ratio", nrow(anova_CN_ratio_OiOe_lme))
anova_CN_ratio_OiOe_lme$horizon = rep("Oi/Oe", nrow(anova_CN_ratio_OiOe_lme))
# numDF denDF   F-value p-value
# (Intercept)          1   287 1072.1278  <.0001
# Year_contin          1   287   13.4310  0.0003
# Year_contin:HPU      1   287    3.1516  0.0769
# Year_contin:Site     1   287    0.0968  0.7560

CN_ratio_OaA_lme = lme(log(CN_ratio)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_CN_ratio_OaA_lme=as.data.frame(anova(CN_ratio_OaA_lme))
anova_CN_ratio_OaA_lme$variable = rep("CN_ratio", nrow(anova_CN_ratio_OaA_lme))
anova_CN_ratio_OaA_lme$horizon = rep("Oa/A", nrow(anova_CN_ratio_OaA_lme))
# numDF denDF  F-value p-value
# (Intercept)          1   286 864.6057  <.0001
# Year_contin          1   286   3.6458  0.0572
# Year_contin:HPU      1   286   0.5035  0.4785
# Year_contin:Site     1   286   0.9148  0.3397

CN_ratio_Min_lme = lme(log(CN_ratio)~Year_contin + Year_contin:HPU + Year_contin:Site, random=~1|Elevation, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06")), na.action=na.omit, corr = corCAR1())
anova_CN_ratio_Min_lme=as.data.frame(anova(CN_ratio_Min_lme))
anova_CN_ratio_Min_lme$variable = rep("CN_ratio", nrow(anova_CN_ratio_Min_lme))
anova_CN_ratio_Min_lme$horizon = rep("Min", nrow(anova_CN_ratio_Min_lme))
# numDF denDF   F-value p-value
# (Intercept)          1   286 1084.5005  <.0001
# Year_contin          1   286    2.4287  0.1202
# Year_contin:HPU      1   286    0.6866  0.4080
# Year_contin:Site     1   286    0.3544  0.5521

CN_ratio_OiOe_lme_High = lm(log(CN_ratio)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06") & HPU == "Bhs"))
CN_ratio_OiOe_lme_Low = lm(log(CN_ratio)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oi/Oe" & (Month == "07" | Month == "06") & HPU == "Bimodal"))

CN_ratio_OaA_lme_High = lm(log(CN_ratio)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06") & HPU == "Bhs"))
CN_ratio_OaA_lme_Low = lm(log(CN_ratio)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Oa/A" & (Month == "07" | Month == "06") & HPU == "Bimodal"))

CN_ratio_Min_lme_High = lm(log(CN_ratio)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06") & HPU == "Bhs"))
CN_ratio_Min_lme_Low = lm(log(CN_ratio)~Year_contin, data = subset(breaks_lab, phase == "POST" & Hor == "Min" & (Month == "07" | Month == "06") & HPU == "Bimodal"))

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/CN_ratio_OiOe_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(CN_ratio_OiOe_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/CN_ratio_OaA_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(CN_ratio_OaA_lme))))
#dev.off 

#pdf(paste0(".../Processed Data/Fits/LMM/Diagnostic plots/CN_ratio_Min_lme_qqplot.pdf"))
print(qqnorm(as.numeric(resid(CN_ratio_Min_lme))))
#dev.off 

#SI Fig. 6: MBC correlation####
#New for R1

#July measurements only for both MBC and soil respiration 

#breaks_field
Jul <- breaks_field %>% 
  filter(Month == "07")

#Mean by site, elevation, and chamber 
Jul_mean <- Jul %>% 
  group_by(HPU, Year, Site, Elevation) %>%
  summarize(Resp_summer_mean = mean(CO2_rate, na.rm=TRUE))

#Lab microbial biomass
Jul_MBC <- breaks_lab %>% 
  filter(Month == "07") 

#Mean by site, elevation, and chamber 

Jul_MBC_mean <- Jul_MBC %>% 
  filter(Year_contin > 2001 & Year_contin < 2021) %>% 
  group_by(HPU, Year, Site, Elevation, Hor) %>%
  summarize(BIOC_summer_mean = mean(BIOC, na.rm=TRUE), 
            RESPC_summer_mean = mean(RESPC, na.rm=TRUE), BION = mean(BION, na.rm=TRUE))

#Join 

Rs_mic_join <- Jul_MBC_mean %>% right_join(Jul_mean, by=c("HPU", "Year","Site","Elevation"))

Rs_mic_join$Year_contin <- as.character(Rs_mic_join$Year)

Rs_mic_join$Year_contin <- as.numeric(Rs_mic_join$Year_contin)

Rs_mic_join$phase <- factor(ifelse(Rs_mic_join$Year_contin < 2015, "Pre-2015", "2015-present"))

Rs_mic_join$phase <- factor(Rs_mic_join$phase, levels=c("Pre-2015","2015-present"))

#pdf(".../Revision 1/R1 Figures/Component Figures/BIOC_Rs_Jul.pdf", width=11.5, height=4.5)
print(ggplot(subset(Rs_mic_join, !(Year == "2010"))) + 
        geom_point(aes(x=BIOC_summer_mean/1000, y=Resp_summer_mean, fill=phase), size=3, pch=21) + 
        stat_smooth(aes(x=BIOC_summer_mean/1000, y=Resp_summer_mean, group=phase), method="lm", formula=y~x, color="black") + 
        facet_grid(.~Hor, scales="free_x") + 
        scale_fill_manual(values=c("darkmagenta", "darkcyan")) + 
        labs(x=expression(paste("Microbial biomass carbon (g C kg"," ",soil^-1,")")), 
             y=expression(paste("Soil respiration rate (g"," ",CO[2]," ","-C"," ",m^-2," ",hr^-1,")"))) + 
        theme_bw() + 
        theme(panel.grid=element_blank(), 
              axis.text=element_text(size=14, color="black"), 
              strip.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black"), 
              legend.title=element_blank(), 
              legend.text=element_text(size=14))) 
#dev.off 

#Spearman correlations (following soil pH and Al approach) 
#All horizons
MBC_CO2_corr <- cor.test(Rs_mic_join$BIOC_summer_mean, Rs_mic_join$RESPC_summer_mean, method="spearman")
# Spearman's rank correlation rho
# 
# data:  Rs_mic_join$BIOC_summer_mean and Rs_mic_join$RESPC_summer_mean
# S = 403142, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.9288823

#Separate by horizon and pre and post-2015 phases
Rs_mic_OiOe <- subset(Rs_mic_join, Hor == "Oi/Oe")
Rs_mic_OiOe_pre <- subset(Rs_mic_OiOe, Year_contin < 2015)
Rs_mic_OiOe_post <- subset(Rs_mic_OiOe, Year_contin >= 2015)
Rs_mic_OaA <- subset(Rs_mic_join, Hor == "Oa/A")
Rs_mic_OaA_pre <- subset(Rs_mic_OaA, Year_contin < 2015)
Rs_mic_OaA_post <- subset(Rs_mic_OaA, Year_contin >= 2015)
Rs_mic_Min <- subset(Rs_mic_join, Hor == "Min")
Rs_mic_Min_pre <- subset(Rs_mic_Min, Year_contin < 2015)
Rs_mic_Min_post <- subset(Rs_mic_Min, Year_contin >= 2015)

Rs_mic_OiOe_corr <- cor.test(Rs_mic_OiOe$BIOC_summer_mean, Rs_mic_OiOe$RESPC_summer_mean, method="spearman")
Rs_mic_OiOe_pre_corr <- cor.test(Rs_mic_OiOe_pre$BIOC_summer_mean, Rs_mic_OiOe_pre$RESPC_summer_mean, method="spearman")
Rs_mic_OiOe_post_corr <- cor.test(Rs_mic_OiOe_post$BIOC_summer_mean, Rs_mic_OiOe_post$RESPC_summer_mean, method="spearman")
Rs_mic_OaA_corr <- cor.test(Rs_mic_OaA$BIOC_summer_mean, Rs_mic_OaA$RESPC_summer_mean, method="spearman")
Rs_mic_OaA_pre_corr <- cor.test(Rs_mic_OaA_pre$BIOC_summer_mean, Rs_mic_OaA_pre$RESPC_summer_mean, method="spearman")
Rs_mic_OaA_post_corr <- cor.test(Rs_mic_OaA_post$BIOC_summer_mean, Rs_mic_OaA_post$RESPC_summer_mean, method="spearman")
Rs_mic_Min_corr <- cor.test(Rs_mic_Min$BIOC_summer_mean, Rs_mic_Min$RESPC_summer_mean, method="spearman")
Rs_mic_Min_pre_corr <- cor.test(Rs_mic_Min_pre$BIOC_summer_mean, Rs_mic_Min_pre$RESPC_summer_mean, method="spearman")
Rs_mic_Min_post_corr <- cor.test(Rs_mic_Min_post$BIOC_summer_mean, Rs_mic_Min_post$RESPC_summer_mean, method="spearman")

OiOe <- as.data.frame(cbind(Rs_mic_OiOe_corr$estimate, Rs_mic_OiOe_corr$p.value))
OiOe$hor <- rep("Oi/Oe", nrow(OiOe))
OiOe$phase <- rep("all", nrow(OiOe))
OiOe_pre <- as.data.frame(cbind(Rs_mic_OiOe_pre_corr$estimate, Rs_mic_OiOe_pre_corr$p.value))
OiOe_pre$hor <- rep("Oi/Oe", nrow(OiOe_pre))
OiOe_pre$phase <- rep("Pre-2015",nrow(OiOe_pre))
OiOe_post <- as.data.frame(cbind(Rs_mic_OiOe_post_corr$estimate, Rs_mic_OiOe_post_corr$p.value))
OiOe_post$hor <- rep("Oi/Oe", nrow(OiOe_post)) 
OiOe_post$phase <- rep("2015-present",nrow(OiOe_post))

OaA <- as.data.frame(cbind(Rs_mic_OaA_corr$estimate, Rs_mic_OaA_corr$p.value))
OaA$hor <- rep("Oa/A", nrow(OaA))
OaA$phase <- rep("all",nrow(OaA))
OaA_pre <- as.data.frame(cbind(Rs_mic_OaA_pre_corr$estimate, Rs_mic_OaA_pre_corr$p.value))
OaA_pre$hor <- rep("Oa/A", nrow(OaA_pre))
OaA_pre$phase <- rep("Pre-2015",nrow(OaA_pre))
OaA_post <- as.data.frame(cbind(Rs_mic_OaA_post_corr$estimate, Rs_mic_OaA_post_corr$p.value))
OaA_post$hor <- rep("Oa/A", nrow(OaA_post)) 
OaA_post$phase <- rep("2015-present",nrow(OaA_post))

Min <- as.data.frame(cbind(Rs_mic_Min_corr$estimate, Rs_mic_Min_corr$p.value))
Min$hor <- rep("Min", nrow(Min))
Min$phase <- rep("all",nrow(Min))
Min_pre <- as.data.frame(cbind(Rs_mic_Min_pre_corr$estimate, Rs_mic_Min_pre_corr$p.value))
Min_pre$hor <- rep("Min", nrow(Min_pre))
Min_pre$phase <- rep("Pre-2015",nrow(Min_pre))
Min_post <- as.data.frame(cbind(Rs_mic_Min_post_corr$estimate, Rs_mic_Min_post_corr$p.value))
Min_post$hor <- rep("Min", nrow(Min_post)) 
Min_post$phase <- rep("2015-present",nrow(Min_post))

MBC_Rs_corr <- rbind(OiOe, OiOe_pre, OiOe_post, OaA, OaA_pre, OaA_post, Min, Min_pre, Min_post)

#SI Fig. 7: Soil temperature####

sensor_dat_use <- sensor_dat %>% 
  dplyr::filter(Year > 2010 & Year < 2021)

sensor_dat_use$Phase <- factor(ifelse(sensor_dat_use$Year < 2015, "Pre-2015", "2015-present"), levels=c("Pre-2015","2015-present"))

#Sensor selection and plotting####

# #Daily mean across all sites 
# sensor_mean_all <- sensor_dat_use %>% 
#   group_by(Date, Year, Month, Day) %>% 
#   dplyr::summarize(All_soil_temp_mean.degreesC = mean(Soil_temp.degreesC, na.rm=TRUE))
# 
# #E10 and E12 only
# sensor_mean_E10_E12 <- sensor_dat_use %>% 
#   filter(Site == "E10" | Site == "E12") %>% 
#   group_by(Date, Year, Month, Day) %>% 
#   dplyr::summarize(E10_E12_soil_temp_mean.degreesC = mean(Soil_temp.degreesC, na.rm=TRUE))
# 
# #South-facing WS (excluding IL2 and IL1)
# sensor_mean_SWS <- sensor_dat_use %>% 
#   filter(Site == "IL3" | Site == "E07" | Site == "E08" | Site == "E09" | Site == "E10" | Site == "E12") %>% 
#   group_by(Date, Year, Month, Day) %>% 
#   dplyr::summarize(SWS_soil_temp_mean.degreesC = mean(Soil_temp.degreesC, na.rm=TRUE))
# 
# h = ggplot(sensor_mean_all) 
# h + geom_point(aes(x=Date, y=All_soil_temp_mean.degreesC))
# 
# h = ggplot(sensor_mean_E10_E12) 
# h + geom_point(aes(x=Date, y=E10_E12_soil_temp_mean.degreesC))
# 
# h = ggplot(sensor_mean_SWS) 
# h + geom_point(aes(x=Date, y=SWS_soil_temp_mean.degreesC))
# 
# h = ggplot() 
# h + geom_point(aes(x=sensor_mean_all$All_soil_temp_mean.degreesC, y=sensor_mean_SWS$SWS_soil_temp_mean.degreesC, fill=sensor_mean_all$Site), pch=21) + 
#   geom_abline(slope=1, color="red") + 
#   labs(x="Mean soil temp. all sensors (including N-facing) (degrees C)", y="Mean soil temp. S-facing except IL2 and IL3 (degrees C)")
# 
# h = ggplot() 
# h + geom_point(aes(x=sensor_mean_E10_E12$E10_E12_soil_temp_mean.degreesC, y=sensor_mean_SWS$SWS_soil_temp_mean.degreesC)) + 
#   geom_abline(slope=1, color="red") + 
#   labs(x="Mean soil temp. S-facing except IL2 and IL3 (degrees C)", y="Mean E10 and E12 only (degrees C)")

#Hourly soil temperature data####

temp_hourly$temp <- temp_hourly$date
temp_hourly <- temp_hourly %>% separate(temp, into=c("Cal_date","Time"), sep=" ")
temp_sampling <- temp_hourly %>% 
  filter(Time == "10:00:00" | Time == "11:00:00" | Time == "12:00:00" | Time == "13:00:00" |
           Time == "14:00:00" | Time == "15:00:00" | Time == "16:00:00")
temp_sampling_melt <- reshape2::melt(temp_sampling,  id.vars = c("date","Cal_date","Time"), variable.name = 'measurement')
temp_sampling_melt$temp2 <- temp_sampling_melt$measurement 
temp_sampling_melt <- temp_sampling_melt %>% separate(temp2, into=c("Site","Variable","Sensor"), sep="_")
temp_sampling_mean <- temp_sampling_melt %>% 
  dplyr::group_by(date, Cal_date, Time, Site) %>% 
  dplyr::summarize(Mean_temp = mean(value, na.rm=TRUE))

temp_sampling_mean$Cal_date <- as.POSIXct(temp_sampling_mean$Cal_date)
temp_sampling_mean$temp3 <- temp_sampling_mean$Cal_date
temp_sampling_mean <- temp_sampling_mean %>% 
  separate(temp3, into=c("Year","Month","Day"), sep="-")
temp_sampling_mean$Phase <- factor(ifelse(temp_sampling_mean$Year < 2015, "Pre-2015", "2015-present"), levels=c("Pre-2015","2015-present"))

#Use sensor data from S-facing watersheds, excluding IL1 and IL2 (sensor selection from prior)
sensor_dat_final <- temp_sampling_mean %>% 
  filter(Site == "IL3" | Site == "E07" | Site == "E08" | Site == "E09" | Site == "E10" | Site == "E12")

#Mean daily soil temperature across all sites 
soil_temp_stats <- sensor_dat_final %>% 
  group_by(Year, Month, Day, Cal_date, Phase) %>% 
  dplyr::summarize(Mean_daily_temp.degreesC = mean(Mean_temp, na.rm=TRUE), 
                   Min_daily_temp.degreesC = min(Mean_temp, na.rm=TRUE),
                   Max_daily_temp.degreesC = max(Mean_temp, na.rm=TRUE))

soil_temp_stats$Year <- as.factor(soil_temp_stats$Year)
soil_temp_stats$Month <- as.factor(soil_temp_stats$Month)
soil_temp_stats$Day <- as.factor(soil_temp_stats$Day)

CO2_rate_mean <- breaks_field %>% 
  group_by(Site, Elevation, Year, Month, Day, Date) %>% 
  dplyr::summarize(Mean_CO2_rate = mean(CO2_rate, na.rm=TRUE))

rate_temp <- soil_temp_stats %>% 
  right_join(CO2_rate_mean, by=c("Year","Month","Day"))

rate_temp$Year_contin <- as.character(rate_temp$Year)
rate_temp$Year_contin <- as.numeric(rate_temp$Year_contin)

#Diverging categorical palette
values = unname(watlington(7))

#SI Fig. 7a: Mean, all months####

#pdf(".../Revision 1/R1 Figures/Component Figures/daily_temp_all.pdf", width=6, height=4.5)
print(ggplot(rate_temp) + geom_point(aes(x=Cal_date, y=Mean_daily_temp.degreesC, fill=Month), pch=21, size=3) + 
        stat_smooth(aes(x=Cal_date, y=Mean_daily_temp.degreesC), method="lm", color="black") +
        theme_bw() + 
        labs(x="Year", y=expression(paste("Mean daily soil temperature (",degree,"C)"))) +  
        scale_fill_manual(values=unname(watlington())) + 
        #scale_y_continuous(limits=c(3.5, 19)) + 
        #scale_x_continuous(limits=c(2010, 2021)) + 
        theme(panel.grid=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              legend.title=element_text(size=14, color="black"), 
              axis.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black"))) 
#dev.off 

temp_all_lm <- lm(Mean_daily_temp.degreesC~Year_contin, data=rate_temp)
# -0.1358

#SI Fig. 7b: Mean for July/August####
#pdf(".../Revision 1/R1 Figures/Component Figures/daily_temp_jul_aug.pdf", width=6, height=4.5)
print(ggplot(subset(rate_temp, Month == "07" | Month == "08")) + 
        geom_point(aes(x=Cal_date, y=Mean_daily_temp.degreesC, fill=Month), pch=21, size=3) + 
        stat_smooth(aes(x=Cal_date, y=Mean_daily_temp.degreesC), method="lm", color="black") +
        theme_bw() + 
        labs(x="Year", y=expression(paste("Mean daily soil temperature (",degree,"C)"))) +  
        scale_fill_manual(values=c("#AD2323","#2A4BD7")) + 
        #scale_y_continuous(limits=c(3.5, 19)) + 
        #scale_x_continuous(limits=c(2010, 2021)) + 
        theme(panel.grid=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              legend.title=element_text(size=14, color="black"), 
              axis.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black")))
#dev.off 

temp_JulAug_lm <- lm(Mean_daily_temp.degreesC~Year_contin, data=subset(rate_temp, Month == "07" | Month == "08"))
#0.19496

#SI Fig. 7c: Min for July/August####
#pdf(".../Revision 1/R1 Figures/Component Figures/daily_temp_jul_aug_min.pdf", width=6, height=4.5)
print(ggplot(subset(rate_temp, Month == "07" | Month == "08")) +
        geom_point(aes(x=Cal_date, y=Min_daily_temp.degreesC, fill=Month), pch=21, size=3) + 
        stat_smooth(aes(x=Cal_date, y=Min_daily_temp.degreesC), method="lm", color="black") +
        theme_bw() + 
        labs(x="Year", y=expression(paste("Minimum soil temperature (",degree,"C)"))) +  
        scale_fill_manual(values=c("#AD2323","#2A4BD7")) + 
        #scale_y_continuous(limits=c(3.5, 19)) + 
        #scale_x_continuous(limits=c(2010, 2021)) + 
        theme(panel.grid=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              legend.title=element_text(size=14, color="black"), 
              axis.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black")))
#dev.off 

min_temp_JulAug_lm <- lm(Min_daily_temp.degreesC~Year_contin, data=subset(rate_temp, Month == "07" | Month == "08"))
#0.20902

#SI Fig. 7c: Max for July/August####
#pdf(".../Revision 1/R1 Figures/Component Figures/daily_temp_jul_aug_max.pdf", width=6, height=4.5)
print(ggplot(subset(rate_temp, Month == "07" | Month == "08")) + 
        geom_point(aes(x=Cal_date, y=Max_daily_temp.degreesC, fill=Month), pch=21, size=3) + 
        stat_smooth(aes(x=Cal_date, y=Max_daily_temp.degreesC), method="lm", color="black") +
        theme_bw() + 
        labs(x="Year", y=expression(paste("Maximum soil temperature (",degree,"C)"))) +  
        scale_fill_manual(values=c("#AD2323","#2A4BD7")) + 
        #scale_y_continuous(limits=c(3.5, 19)) + 
        #scale_x_continuous(limits=c(2010, 2021)) + 
        theme(panel.grid=element_blank(), 
              legend.text=element_text(size=14, color="black"), 
              legend.title=element_text(size=14, color="black"), 
              axis.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black")))
#dev.off 

max_temp_JulAug_lm <- lm(Max_daily_temp.degreesC~Year_contin, data=subset(rate_temp, Month == "07" | Month == "08"))
#0.14607

#Mean 2015 temp
Temp_2015 <- rate_temp %>% 
  filter(Year == "2015" & (Month == "07" | Month == "08")) %>% 
  summarize(Mean_temp_2015 = mean(Mean_soil_temp.degreesC, na.rm=TRUE))
#16.72153

#Estimates for Q10 calculations####

#Mean CO2-rate pre-2015
Dat_2015 <- subset(breaks_field, Year_contin < 2015) 
JulAug_2015 <- subset(Dat_2015, Month == "07" | Month == "08")

Dat_2020 <- subset(breaks_field, Year == "2020")
JulAug_2020 <- subset(Dat_2020, Month == "07" | Month == "08")

avg_2015 = summarySE(JulAug_2015, measurevar = "CO2_rate", na.rm=TRUE)
#0.04346239

avg_2020 = summarySE(JulAug_2020, measurevar = "CO2_rate")
#0.09235417

#SI Fig. 8: HPU mapping####
#See mapping code 

#SI Fig. 9: Breakpoint detection####
#See Data Import and Processing code 

#SI Methods: Methodological artifacts check####

#Is there a directional change in "measurement timing"?
#Metrics of "timing": months of sampling, day of month, time of day, "warmer daytime"? 

#SI Fig. 10: November measurements####
#November added: does adding November affect rates (Nov not included in "growing season" flux)? 
#NO: only 2 points, trend driven by July/August

Field_Nov_excluded <- breaks_field %>% filter(!(Month == "11"))

#pdf(".../Revision 1/R1 Figures/Component Figures/Rates_Nov_excluded.pdf", width=5, height=4.5)
print(ggplot(Field_Nov_excluded) + 
        geom_point(aes(x=Date.posixct, y=CO2_rate)) + 
        labs(x="Year", y=expression(paste("Soil respiration rate (g"," ",CO[2],"-C"," ",m^-2," ",hr^-1,")"))) + 
        theme_bw() + 
        theme(panel.grid=element_blank(), 
              axis.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black"))) 
#dev.off 

#pdf(".../Revision 1/R1 Figures/Component Figures/Rates_Nov_included.pdf", width=5, height=4.5)
print(ggplot(breaks_field) + 
        geom_point(aes(x=Date.posixct, y=CO2_rate)) + 
        labs(x="Year", y=expression(paste("Soil respiration rate (g"," ",CO[2],"-C"," ",m^-2," ",hr^-1,")"))) + 
        theme_bw() + 
        theme(panel.grid=element_blank(), 
              axis.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black")))
#dev.off 

#SI Fig. 11: Day of month####
#Day of month: did the day of month get consistently later/earlier over the long-term record? 
#pdf(".../Revision 1/R1 Figures/Component Figures/day_of_month.pdf", width=10, height=4.5)
print(ggplot(breaks_field) + 
        geom_boxplot(aes(x=Year, y=as.numeric(Day))) + 
        geom_point(aes(x=Year, y=as.numeric(Day), fill=Month), pch=21) + 
        labs(x="Year", y="Sampling day (day of the month)") + 
        theme_bw() + 
        theme(panel.grid=element_blank(), 
              axis.text=element_text(size=14, color="black"), 
              axis.title=element_text(size=14, color="black")))
#dev.off 


