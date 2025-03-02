#Symptoms of a changing carbon cycle: a decadal-scale breakpoint in soil respiration in a Northeastern US forest 
#Draft for Nature Climate Change
#Data import, processing, and breakpoint detection
#Updated 07-23-24

#Load packages#######################

library("strucchange")
library("ggplot2")
library("tibble")
library("nlme")
library("tidyr")
library("dplyr")
library("EnvStats")
library("Rmisc")
library("multcomp")
library("ggh4x")

#Data import####

#1: In-situ collar measurements (Groffman lab): Calcium amendment project#######
# Groffman, P.M., & Martel, L. D. (2021a). Hubbard Brook Experimental Forest: Soil-atmosphere fluxes of carbon dioxide, nitrous oxide and methane on Watershed 1 and Bear Brook ver 12. Environmental Data Initiative. https://doi.org/10.6073/pasta/140e9b608fe77efd595d1a243811d02b (Accessed 2022-05-23).

resp_field <- read.csv("HubbardBrook_TraceGas_Watershed1_BearBrook_2002-2020.csv")

dim(resp_field)
str(resp_field)

# 'data.frame':	1734 obs. of  7 variables:
#   $ Date     : chr  "2002-08-26" "2002-08-26" "2002-08-26" "2002-08-26" ...
# $ Site     : chr  "Bear Brook" "Bear Brook" "Bear Brook" "Bear Brook" ...
# $ Elevation: chr  "low" "low" "low" "mid" ...
# $ Chamber  : int  1 2 3 1 2 3 1 2 3 1 ...
# $ CO2_rate : num  0.063 0.045 0.018 0.03 0.046 0.059 0.062 0.057 0.073 0.071 ...
# $ N2O_rate : num  0.389 -0.5 -0.093 -0.845 0.584 0.368 0.048 0.74 0.046 0.511 ...
# $ CH4_rate : num  -1.562 -0.706 -3.18 -0.987 -2.729 ...

#CO2_rate = gramPerMeterSquaredPerHour #g C per m2 per hr

#2: Laboratory incubations (10-d aerobic mineralization, Groffman Lab): Calcium amendment project#########
#Groffman, P.M., & Martel, L. D. (2021b). Long-term measurements of microbial biomass and activity at the Hubbard Brook Experimental Forest 1994 – present ver 23. Environmental Data Initiative. https://doi.org/10.6073/pasta/87a42479bfcaaa2d3cea6dd087eba4a4 (Accessed 2022-07-06).

resp_lab <- read.csv("HubbardBrook_CalciumProject_MicrobialBiomass_1994-2021.csv")

dim(resp_lab)
str(resp_lab)

# 'data.frame':	4657 obs. of  18 variables:
#   $ Project  : chr  "Calcium" "Calcium" "Calcium" "Calcium" ...
# $ Date     : chr  "1994-05-19" "1994-05-19" "1994-05-19" "1994-05-19" ...
# $ Year     : int  1994 1994 1994 1994 1994 1994 1994 1994 1994 1994 ...
# $ Se       : chr  "SP" "SP" "SP" "SP" ...
# $ Treatment: chr  "BearBrook" "BearBrook" "BearBrook" "BearBrook" ...
# $ El       : chr  "L" "L" "U" "U" ...
# $ Plot     : chr  "LT162" "LT162" "LT121" "LT121" ...
# $ Hor      : chr  "Oi/Oe" "Oa/A" "Oi/Oe" "Oa/A" ...
# $ BIOC     : num  10713 3996 10564 5713 6942 ...
# $ RESPC    : num  396 141 278 163 188 ...
# $ BION     : num  530 206 335 225 320 ...
# $ NO3      : num  2.16 0.97 5.44 3.74 10.02 ...
# $ NH4      : num  37.82 5.19 20.87 4.11 31.32 ...
# $ NIT      : num  -0.22 -0.1 3.16 3.44 1.91 2.57 -0.46 9.32 -0.42 3.45 ...
# $ MIN      : num  9.05 3.31 7.65 3.23 5.44 ...
# $ DEA      : num  198.7 -10000 240.8 81.5 49.8 ...
# $ H2O      : num  0.76 0.54 0.81 0.71 0.66 0.41 0.79 0.69 0.78 0.54 ...
# $ pH       : num  -10000 -10000 -10000 -10000 -10000 ...

#RESPC - milligramPerKilogramPerDay - check whether CO2-C or CO2 
#BIOC - milligramPerKilogram

#3: Soil temperature####
# Groffman, P.M., J. Duran, J.L. Morse, G.F. Wilson, and M.B. Green. 2022. Soil temperature along an elevation gradient at the Hubbard Brook Experimental Forest, 2010 - present ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/85d9bc546191e92dad23077acc90a4bf (Accessed 2023-05-02).

soil_temp <- read.csv("hourly_soil_temperature_gradient_plots.csv")

#4: Air temperature and precipitation####
#USDA Forest Service, Northern Research Station. 2022. Hubbard Brook Experimental Forest: Daily Temperature Record, 1955 - present ver 11. Environmental Data Initiative. https://doi.org/10.6073/pasta/e51ee820bb04aace06fa35c00946b050 (Accessed 2023-05-06).
#USDA Forest Service, Northern Research Station. 2022. Hubbard Brook Experimental Forest: Daily Precipitation Rain Gage Measurements, 1956 - present ver 18. Environmental Data Initiative. https://doi.org/10.6073/pasta/aed7e68772106753f3c7deef4f75e09c (Accessed 2023-05-06).

air_temp <- read.csv("HBEF_air_temp_daily_1957-2022.csv")
precip <- read.csv("dailyGagePrecip1956-2022.csv")

#5: HARV Ameriflux Data (CO2)############
#J. William Munger (2022), AmeriFlux BASE US-Ha1 Harvard Forest EMS Tower (HFR1), Ver. 19-5, AmeriFlux AMP, (Dataset). https://doi.org/10.17190/AMF/1246059. 
#Downloaded 05-06-23. 
regional_CO2 <- read.csv("AMF_US-Ha1_BASE_HR_19-5_rowsremoved.csv")

#6: Regional ozone (US EPA)#################
regional_ozone <- read.csv("OzoneNortheast.csv")
#Downloaded from https://www.epa.gov/air-trends/ozone-trends on 05-06-23.
#Not used in manuscript

#7: Hubbard Brook precipitation chemistry##########
# Likens, G. 2019. Chemistry of Bulk Precipitation at Hubbard Brook Experimental Forest, 1969 - present, Robert S. Pierce Ecosystem Laboratory Facility ver 8. Environmental Data Initiative. https://doi.org/10.6073/pasta/49b5c122f2c94353e3271152c6e869c6 (Accessed 2023-05-29).
precip_chem <- read.table("rg22-pcp.txt", sep=",", header=T) 

#8: Above-ground biomass####
#Battles, J. et al. Forest Inventory of a Northern Hardwood Forest: Watershed 6 2002, Hubbard Brook Experimental Forest ver 8. Environmental Data Initiative https://doi.org/10.6073/pasta/bb0e2c20752780c97b63b5544c1aba60 (2019). 
#Battles, J. et al. Forest Inventory of a Northern Hardwood Forest: Watershed 6 2007, Hubbard Brook Experimental Forest ver 2. Environmental Data Initiative https://doi.org/10.6073/pasta/6a1cd73c4e196f930eda7d8e25bb69d8 (2019). 
#Battles, J. et al. Forest Inventory of a Northern Hardwood Forest: Watershed 6, 2012, Hubbard Brook Experimental Forest ver 2. Environmental Data Initiative https://doi.org/10.6073/pasta/b356ff76e03f661c581262658834a7c7 (2019). 
#Battles, J. et al. Forest Inventory of a Northern Hardwood Forest: Watershed 6, 2017, Hubbard Brook Experimental Forest ver 1. Environmental Data Initiative https://doi.org/10.6073/pasta/0593ba15fb76a4f085797126a1bea3a7 (2019). 
W6_2002 <- read.table("w6_2002veg.txt", sep=",", header=T)
W6_2007<- read.table("w62007veg.txt", sep=",", header=T)
W6_2012 <- read.table("w62012veg.txt", sep=",", header=T)
W6_2017 <- read.table("w6_2017_vegInventory.csv", sep=",", header=T)

#9: Forest floor carbon####
#Early data and methods from Johnson, C. E., Driscoll, C. T., Blum, J. D., Fahey, T. J. & Battles, J. J. Soil chemical dynamics after calcium silicate addition to a northern hardwood forest. Soil Sci. Soc. Am. J.  78, 1458–1468 (2014). 
#Updated data in FF_C_pool.csv file included with this repository
ff_carbon <- read.csv("FF_C_pool.csv")

#10: Lysimeter data####
#Driscoll, C. T. Chemistry of freely-draining soil solutions at the Hubbard Brook Experimental Forest, Watershed 6, 1982 - present ver 18. Environmental Data Initiative https://doi.org/10.6073/pasta/bd7ba65a4b703760c82c4e16fe494fb6 (2022). 
lysim_data <- read.csv("W6Lysim_HB1984-2020 Al Corrected MG240712.csv", na.strings=c("-1999.98","-1777.76","-999.99","-888.88","-874.71", "-773.38", "-757.67"))
lysim_data$Date_posixct <- as.POSIXct(lysim_data$Date, tz="EST")

#Soil resp. data processing##########
#1: Change -9999 coding to NA########
#In-situ measurements
resp_field$CO2_rate <- ifelse(resp_field$CO2_rate == -9999, NA, resp_field$CO2_rate)
min(resp_field$CO2_rate, na.rm=TRUE)
max(resp_field$CO2_rate, na.rm=TRUE)

#Lab measurements
resp_lab$BIOC <- ifelse(resp_lab$BIOC == -9999.99, NA, resp_lab$BIOC)
min(resp_lab$BIOC, na.rm=TRUE)
max(resp_lab$BIOC, na.rm=TRUE)
resp_lab$RESPC <- ifelse(resp_lab$RESPC == -9999.99, NA, resp_lab$RESPC)
min(resp_lab$RESPC, na.rm=TRUE)
max(resp_lab$RESPC, na.rm=TRUE)
resp_lab$BION <- ifelse(resp_lab$BION == -9999.99, NA, resp_lab$BION)
min(resp_lab$BION, na.rm=TRUE)
max(resp_lab$BION, na.rm=TRUE)
resp_lab$pH <- ifelse(resp_lab$pH == -9999.99, NA, resp_lab$pH)
min(resp_lab$pH, na.rm=TRUE)
max(resp_lab$pH, na.rm=TRUE)

#2: C:N ratio##########
resp_lab$CN_ratio <- resp_lab$BIOC / resp_lab$BION
min(resp_lab$CN_ratio, na.rm=TRUE)
max(resp_lab$CN_ratio, na.rm=TRUE)

#Filter C:N ratio by >0, not infinity
resp_lab$CN_ratio <- ifelse(resp_lab$CN_ratio < 0 | resp_lab$CN_ratio == Inf, NA, resp_lab$CN_ratio)

#3: Specific respiration#######
resp_lab$SRESPC <- resp_lab$RESPC / resp_lab$BIOC
min(resp_lab$SRESPC, na.rm=TRUE)
max(resp_lab$SRESPC, na.rm=TRUE)

#Filter SR ratio by >0, not infinity
resp_lab$SRESPC <- ifelse(resp_lab$SRESPC < 0 | resp_lab$SRESPC == Inf, NA, resp_lab$SRESPC)

#4: Posixct date format#########
resp_field$Date.posixct <- as.POSIXct(resp_field$Date)
resp_lab$Date.posixct <- as.POSIXct(resp_lab$Date)

#5: Convert Site, Elevation, Chamber, etc. to factors#########
resp_field$Site <- as.factor(resp_field$Site)
resp_field$Elevation <- factor(resp_field$Elevation, levels=c("high","low","mid","spruce/fir"), labels=c("high","low","mid","spruce_fir"))
resp_field$Chamber <- as.factor(resp_field$Chamber)

resp_lab$Treatment <- as.factor(resp_lab$Treatment)
resp_lab$El <- factor(resp_lab$El, levels=c("H","L","M","SF", "U"), labels=c("high","low","mid","spruce_fir", "upper"))
resp_lab$Hor <- factor(resp_lab$Hor)

#6: Add column with HPU/saturation frequency#######
resp_field$HPU <- as.factor(ifelse(resp_field$Elevation == "spruce_fir" | resp_field$Elevation == "high", "Bhs", "Bimodal"))
resp_lab$HPU <- as.factor(ifelse(resp_lab$El == "spruce_fir" | resp_lab$El == "high","Bhs","Bimodal"))

#7: New columns for year, month, day########
resp_field$temp <- resp_field$Date
resp_field <- resp_field %>% separate(temp, into=c("Year","Month","Day"), sep="-")
resp_field$Year <- as.factor(resp_field$Year)
resp_field$Month <- as.factor(resp_field$Month)
resp_field$Day <- as.factor(resp_field$Day)

resp_lab$temp <- resp_lab$Date
resp_lab <- resp_lab %>% separate(temp, into=c("Year","Month","Day"), sep="-")
resp_lab$Year <- as.factor(resp_lab$Year)
resp_lab$Month <- as.factor(resp_lab$Month)
resp_lab$Day <- as.factor(resp_lab$Day)

#8: Resp_lab field replicates: plots#######
#Reduce to one treatment, elevation, and horizon

plot_table <- resp_lab %>%
  filter(Year == "2005")

write.csv(plot_table, "core_plots.csv")

core_plots <- read.csv("core_plots_mod.csv")

core_plots_reduced <- core_plots[,c("Plot","Plot_number")]
core_plots_reduced <- core_plots_reduced %>% unique(.keep_all = TRUE)
resp_lab$Plot <- as.factor(resp_lab$Plot)

resp_lab <- resp_lab %>% right_join(core_plots_reduced, by="Plot")
resp_lab$Plot_number <- as.factor(resp_lab$Plot_number)

#9: Field cumulative fluxes###########
#Following Taylor et al. (2021): extrapolate to 30 d increments, sum by year
#CO2_rate = gramPerMeterSquaredPerHour #g C per m2 per hr

#Check for multiple observations per month
resp_check <- summarySE(resp_field, measurevar = "CO2_rate", groupvars = c("Site", "Elevation", "Chamber", "Year", "Month", "HPU"))
test <- ifelse(all(resp_check$N == 1), "TRUE", "FALSE") #TRUE
test

#Multiply monthly rate times 24 (d) x 30 (month)
resp_field$CO2_30d_flux <- resp_field$CO2_rate * 24 * 30 #g C per m2 per 30d

#Check chambers per month
chamber_check <- summarySE(resp_field, measurevar = "CO2_30d_flux", groupvars = c("Site", "Elevation", "Year", "Month"))
test <- ifelse(all(chamber_check$N == 3), "TRUE", "FALSE") #TRUE
test

#Exclude November
resp_red <- subset(resp_field, !(Month == "11"))

#How many months? 
length(levels(resp_red$Month)) #7 months

#Sum by year: does not include non-growing season fluxes
#Without November
resp_sum <- as.data.frame(aggregate(resp_red$CO2_30d_flux, by=list(Year = resp_red$Year, HPU = resp_red$HPU, Chamber = resp_red$Chamber, Site = resp_red$Site, Elevation = resp_red$Elevation), FUN=sum))
colnames(resp_sum) <- c("Year", "HPU", "Chamber", "Site", "Elevation", "CO2_annual_flux")

sum_check <- summarySE(resp_sum, measurevar = "CO2_annual_flux", groupvars = c("Site", "Elevation", "Year"))
test <- ifelse(all(sum_check$N == 3), "TRUE", "FALSE") #TRUE
test

#Convert Year to continuous
resp_sum$Year_contin <- as.character(resp_sum$Year)
resp_sum$Year_contin <- as.numeric(resp_sum$Year_contin)

resp_lab$Year_contin <- as.character(resp_lab$Year)
resp_lab$Year_contin <- as.numeric(resp_lab$Year_contin)

# #Check plot:
# h <- ggplot(resp_sum)
# h +
#   stat_summary(aes(x=Year_contin, y=CO2_annual_flux, group=Elevation, fill=Elevation), fun = "mean", geom="point", show.legend=TRUE, pch=21) +
#   facet_grid(.~Elevation) +
#   stat_summary(aes(x=Year_contin, y=CO2_annual_flux, group=Elevation, color=Elevation), fun.data = "mean_se", geom="errorbar", show.legend=TRUE) +
#   facet_grid(Site~Elevation)

#10: Find breakpoints##############
#Uses all field respiration rates

BearBrook <- subset(resp_field, Site == "Bear Brook")
W1 <- subset(resp_field, Site == "W1")

bearbrook_plot <- function(x){
  
  data <- subset(BearBrook, Elevation == x)
  
  #Use arbitrary numeric axis 
  data$Time <- as.numeric(data$Date.posixct)
  #NA rough fix - column median
  data$CO2_rate[is.na(data$CO2_rate)] <- median(data$CO2_rate, na.rm=TRUE)
  
  #Breakpoint model (1 break, defined a priori)
  bp <- breakpoints(data$CO2_rate ~ data$Time, h = 20, breaks = 1) 
  setwd("BIC") 
  pdf(paste0("BearBrook","-",x,"-BIC.pdf")) 
  print(plot(bp)) 
  dev.off()
  
  #Model fit 
  data$fit <- fitted(bp, breaks = 1)
  
  #Model coefficients 
  coef <- as.data.frame(coef(bp))
  coef <- tibble::rownames_to_column(coef, "interval")
  coef$change_date <- c(NA, data$Date[breakpoints(bp, breaks=1)$breakpoints])
  
  #coef$Site = rep(x,nrow(coef))
  
  coef$location <- rep(x,nrow(coef))
  coef$label <- rep(paste0("BearBrook","-",x),nrow(coef))
  
  #Data
  setwd("Breakpoints")
  write.csv(coef, paste0("BearBrook","-",x,".csv"))
  
  #Plot
  setwd("Plots")
  pdf(paste0("BearBrook","-",x,".pdf"), width=4, height=3)
  print(ggplot(data) + 
          geom_point(aes(x=Date.posixct, y=CO2_rate), pch=21, fill="grey75", size=3) + 
          geom_line(aes(x=Date.posixct, y=fit), size=1, color="red") + 
          theme_bw() + 
          labs(title=coef$label, x="Date", y=expression(paste(CO[2]," ","flux"," ","(g C"," ",m^-2," ",h^-1,")"))) + 
          theme(panel.grid=element_blank(), 
                axis.text=element_text(size=12, color="black"), 
                axis.title=element_text(size=12, color="black"))) 
  dev.off() 
  
  #Model interaction#########
  
  #Define as pre- and post 
  data$phase <- as.factor(ifelse(data$Date.posixct < coef[2,4], "PRE","POST"))
  
  #ggplot version 
  setwd("Plots")
  pdf(paste0("BearBrook-phase","-",x,".pdf"), width=4, height=3)
  print(ggplot(data) + 
          geom_point(aes(x=Date.posixct, y=CO2_rate, fill=phase), pch=21, size=3) + 
          scale_fill_viridis_d() + 
          geom_line(aes(x=Date.posixct, y=fit), size=1, color="black") + 
          theme_bw() + 
          labs(fill = "Phase", title=paste0("BearBrook","-",x), x="Date", y=expression(paste(CO[2]," ","flux"," ","(g C"," ",m^-2," ",h^-1,")"))) + 
          theme(panel.grid=element_blank(), 
                axis.text=element_text(size=12, color="black"), 
                axis.title=element_text(size=12, color="black"), 
                legend.text=element_text(size=12, color="black"),
                legend.title=element_text(size=12, color="black"))) 
  dev.off()
  
  #Slope pre- and post breakpoint
  modelMLRpre <- lm(CO2_rate ~ Date.posixct, data = subset(data, phase == "PRE"))
  summaryPRE <- summary(modelMLRpre)
  summaryPRE <- as.data.frame(summaryPRE$coefficients)
  summaryPRE$phase <- rep("PRE", nrow(summaryPRE))
  summaryPRE <- summaryPRE[2,]
  
  modelMLRpost <- lm(CO2_rate ~ Date.posixct, data = subset(data, phase == "POST"))
  summaryPOST <- summary(modelMLRpost)
  summaryPOST <- as.data.frame(summaryPOST$coefficients)
  summaryPOST$phase <- rep("POST", nrow(summaryPOST))
  summaryPOST <- summaryPOST[2,]
  
  summary <- rbind(summaryPRE, summaryPOST)
  summary$Site <- rep("BearBrook", nrow(summary))
  summary$Elevation <- rep(x, nrow(summary))
  
  setwd("Summary")
  write.csv(summary, paste0("BearBrook","-",x,"-summary.csv")) 
  
  
}

x = levels(BearBrook$Elevation)

for (i in 1:length(x)){
  bearbrook_plot(x=x[i])
}

W1_plot <- function(x){
  
  data <- subset(W1, Elevation == x)
  
  #Use arbitrary numeric axis 
  data$Time <- as.numeric(data$Date.posixct)
  #NA rough fix - column median
  data$CO2_rate[is.na(data$CO2_rate)] <- median(data$CO2_rate, na.rm=TRUE)
  
  #Breakpoint model (1 break, defined a priori)
  bp <- breakpoints(data$CO2_rate ~ data$Time, h = 20, breaks = 1) 
  setwd("BIC") 
  pdf(paste0("W1","-",x,"-BIC.pdf")) 
  print(plot(bp)) 
  dev.off()
  
  #Model fit 
  data$fit <- fitted(bp, breaks = 1)
  
  #Model coefficients 
  coef <- as.data.frame(coef(bp))
  coef <- tibble::rownames_to_column(coef, "interval")
  coef$change_date <- c(NA, data$Date[breakpoints(bp, breaks=1)$breakpoints])
  
  #coef$Site = rep(x,nrow(coef))
  
  coef$location <- rep(x,nrow(coef))
  coef$label <- rep(paste0("W1","-",x),nrow(coef))
  
  #Data
  setwd("Breakpoints")
  write.csv(coef, paste0("W1","-",x,".csv"))
  
  #Plot
  setwd("Plots")
  pdf(paste0("W1","-",x,".pdf"), width=4, height=3)
  print(ggplot(data) + 
          geom_point(aes(x=Date.posixct, y=CO2_rate), pch=21, fill="grey75", size=3) + 
          geom_line(aes(x=Date.posixct, y=fit), size=1, color="red") + 
          theme_bw() + 
          labs(title=coef$label, x="Date", y=expression(paste(CO[2]," ","flux"," ","(g C"," ",m^-2," ",h^-1,")"))) + 
          theme(panel.grid=element_blank(), 
                axis.text=element_text(size=12, color="black"), 
                axis.title=element_text(size=12, color="black"))) 
  dev.off() 
  
  #Model interaction#########
  
  #Define as pre- and post 
  data$phase <- as.factor(ifelse(data$Date.posixct < coef[2,4], "PRE","POST"))
  
  #ggplot version 
  setwd("Plots")
  pdf(paste0("W1-phase","-",x,".pdf"), width=4, height=3)
  print(ggplot(data) + 
          geom_point(aes(x=Date.posixct, y=CO2_rate, fill=phase), pch=21, size=3) + 
          scale_fill_viridis_d() + 
          geom_line(aes(x=Date.posixct, y=fit), size=1, color="black") + 
          theme_bw() + 
          labs(fill = "Phase", title=paste0("W1","-",x), x="Date", y=expression(paste(CO[2]," ","flux"," ","(g C"," ",m^-2," ",h^-1,")"))) + 
          theme(panel.grid=element_blank(), 
                axis.text=element_text(size=12, color="black"), 
                axis.title=element_text(size=12, color="black"), 
                legend.text=element_text(size=12, color="black"),
                legend.title=element_text(size=12, color="black"))) 
  dev.off()
  
  #Slope pre- and post-breakpoint
  modelMLRpre <- lm(CO2_rate ~ Date.posixct, data = subset(data, phase == "PRE"))
  summaryPRE <- summary(modelMLRpre)
  summaryPRE <- as.data.frame(summaryPRE$coefficients)
  summaryPRE$phase <- rep("PRE", nrow(summaryPRE))
  summaryPRE <- summaryPRE[2,]
  
  modelMLRpost <- lm(CO2_rate ~ Date.posixct, data = subset(data, phase == "POST"))
  summaryPOST <- summary(modelMLRpost)
  summaryPOST <- as.data.frame(summaryPOST$coefficients)
  summaryPOST$phase <- rep("POST", nrow(summaryPOST))
  summaryPOST <- summaryPOST[2,]
  
  summary <- rbind(summaryPRE, summaryPOST)
  
  summary$Site <- rep("W1", nrow(summary))
  summary$Elevation <- rep(x, nrow(summary))
  
  setwd("Summary")
  write.csv(summary, paste0("W1","-",x,"-summary.csv")) 
  
  
}

x = levels(W1$Elevation)

for (i in 1:length(x)){
  W1_plot(x=x[i])
}

#Merge summary files#######
setwd("Summary")
list = list.files(pattern="*.csv")
myfiles = lapply(list, read.csv)

combined_summary=do.call(rbind, myfiles)

#Import breakpoints#####################
setwd("Breakpoints")
list = list.files(pattern="*.csv")
myfiles = lapply(list, read.csv)

combined_breaks=do.call(rbind, myfiles)

#Rows with dates only 
combined_breaks <- combined_breaks[complete.cases(combined_breaks), ]
combined_breaks$temp <- combined_breaks$label
combined_breaks <- combined_breaks %>% separate(temp, into=c("Site","Elevation"), sep="-")
breaks <- combined_breaks[,c("change_date","Site","Elevation")]
breaks$Site <- factor(breaks$Site, levels=c("BearBrook","W1"), labels=c("BearBrook", "W1"))

#Separate by break month, day, and year
breaks$temp <- breaks$change_date
breaks <- breaks %>% separate(temp, into=c("break_year","break_month","break_day"), sep="-")

#Merge with resp_field, resp_sum, and resp_lab
#Rename and name factors for Site and Elevation 
resp_field$Site <- factor(resp_field$Site, levels=c("Bear Brook","W1"), labels=c("BearBrook","W1"))
resp_field$Elevation <- factor(resp_field$Elevation, levels=c("high","low","mid","spruce_fir"))

resp_sum$Site <- factor(resp_sum$Site, levels=c("Bear Brook","W1"), labels=c("BearBrook","W1"))
resp_sum$Elevation <- factor(resp_sum$Elevation, levels=c("high","low","mid","spruce_fir"))

names(resp_lab)[names(resp_lab) == "Treatment"] = "Site"
names(resp_lab)[names(resp_lab) == "El"] = "Elevation"

#Filter out "upper"
resp_lab <- resp_lab %>% filter(!(Elevation == "upper"))

#New dataframes
breaks_annual <- breaks %>% right_join(resp_sum, by=c("Site","Elevation"))
breaks_field <- breaks %>% right_join(resp_field, by=c("Site","Elevation"))
breaks_lab <- breaks %>% right_join(resp_lab, by=c("Site","Elevation"))

#Year_contin 
breaks_field$Year_contin <- as.character(breaks_field$Year)
breaks_field$Year_contin <- as.numeric(breaks_field$Year_contin)

breaks_lab$Year_contin <- as.character(breaks_lab$Year)
breaks_lab$Year_contin <- as.numeric(breaks_lab$Year_contin)

#Name phase PRE and POST
breaks_annual$phase <- ifelse(breaks_annual$Year_contin < breaks_annual$break_year, "PRE", "POST")
breaks_field$phase <- ifelse(breaks_field$Year_contin < breaks_field$break_year, "PRE", "POST")
breaks_lab$phase <- ifelse(breaks_lab$Year_contin < breaks_lab$break_year, "PRE", "POST")

#Environmental covariate data processing#####

#1: pH: environmental covariates########
average_pH <- summarySE(breaks_lab, measurevar = "pH", groupvars = c("Hor","Year","Month","HPU","phase","Site"))

average_pH <- average_pH %>% 
  filter(Month == "07" | Month == "06")

average_pH$Year_contin <- as.character(average_pH$Year)
average_pH$Year_contin <- as.numeric(average_pH$Year_contin)

#Check plot
h <- ggplot(subset(average_pH, Hor == "Min"))
h + geom_point(aes(x=Year_contin, y=pH)) + 
  stat_smooth(aes(x=Year_contin, y=pH), method="lm", formula=y~x) + 
  facet_grid(.~Site)

#2: Soil temperature: environmental covariates########

temp_melt <- reshape2::melt(soil_temp,  id.vars = 'date', variable.name = 'measurement')

temp_melt$temp <- temp_melt$measurement 
temp_melt <- temp_melt %>% separate(temp, into=c("Site","Variable","Sensor"), sep="_")

temp_melt$temp2 <- temp_melt$date 
temp_melt <- temp_melt %>% separate(temp2, into=c("Date","Time"), sep=" ")

temp_melt$temp3 <- temp_melt$Date
temp_melt <- temp_melt %>% separate(temp3, into=c("Year","Month","Day"), sep="-")

temp_avg <- summarySE(temp_melt, measurevar = "value", groupvars = c("Year","Month","Day","Site"))

temp_avg$Date = paste(temp_avg$Month, temp_avg$Day, temp_avg$Year, sep="/")

temp_avg$Date = as.POSIXct(temp_avg$Date, format = "%m/%d/%Y")

#Exclude 2010 and 2021 (only November/February)
temp_avg <- subset(temp_avg, !(Year == "2010" | Year == "2021"))

h = ggplot(temp_avg)
h + geom_point(aes(x=Date, y=value, fill=Site), pch=21) + 
  geom_hline(yintercept=mean(temp_avg$value, na.rm=TRUE), linetype="longdash")

colnames(temp_avg) = c("Year","Month","Day","Site","Soil_temp_N","Soil_temp.degreesC","Soil_temp_sd","Soil_temp_se","Soil_temp_ci","Date")

#3: Soil moisture: Environmnetal covariates ############

moisture_melt <- reshape2::melt(soil_moisture,  id.vars = 'date', variable.name = 'measurement')

moisture_melt$temp <- moisture_melt$measurement 
moisture_melt <- moisture_melt %>% separate(temp, into=c("Site","Variable","Sensor"), sep="_")

moisture_melt$temp2 <- moisture_melt$date 
moisture_melt <- moisture_melt %>% separate(temp2, into=c("Date","Time"), sep=" ")

moisture_melt$temp3 <- moisture_melt$Date
moisture_melt <- moisture_melt %>% separate(temp3, into=c("Year","Month","Day"), sep="-")

moisture_avg <- summarySE(moisture_melt, measurevar = "value", groupvars = c("Year","Month","Day","Site"))

moisture_avg$Date = paste(moisture_avg$Month, moisture_avg$Day, moisture_avg$Year, sep="/")

moisture_avg$Date = as.POSIXct(moisture_avg$Date, format = "%m/%d/%Y")

h <- ggplot(moisture_avg)
h + geom_point(aes(x=Date, y=value, fill=Site), pch=21) + 
  labs(x="Date",y="VWC") + 
  geom_hline(yintercept=mean(moisture_avg$value, na.rm=TRUE), linetype="longdash")

colnames(moisture_avg) <- c("Year","Month","Day","Site","Soil_moisture_N","Soil_moisture.VWC","Soil_moisture_sd","Soil_moisture_se","Soil_moisture_ci","Date")

#Combine temp and moisture 

sensor_dat_combined <- temp_avg %>% right_join(moisture_avg, by=c("Year","Month","Day","Date","Site"))

#4: Air temperature#######

#Daily average over stations

air_temp$temp <- air_temp$date
air_temp <- air_temp %>% separate(temp, into=c("Year","Month","Day"), sep="-")
air_temp_JulAug <- air_temp %>% filter(Month == "07" | Month == "08")
air_temp_avg <- summarySE(air_temp, measurevar = "AVE", groupvars = c("Year"))

air_temp_avg <- air_temp_avg[,c("Year","AVE")]
colnames(air_temp_avg) = c("Year","MAT.degreesC")
air_temp_avg$Year <- as.numeric(air_temp_avg$Year)

air_temp_JulAug_avg <- summarySE(air_temp_JulAug, measurevar = "AVE", groupvars = c("Year"))
air_temp_JulAug_avg <- air_temp_JulAug_avg[,c("Year","AVE")]
colnames(air_temp_JulAug_avg) <- c("Year","JulAug_MAT.degreesC")
air_temp_JulAug_avg$Year <- as.numeric(air_temp_JulAug_avg$Year)

#1955 and 2022 are missing most values
air_temp_avg <- air_temp_avg %>% filter(!(Year == 1955 & Year <= 2021))
air_temp_JulAug_avg <- air_temp_JulAug_avg %>% filter(!(Year == 1955 & Year <= 2021))

#Combine 
air_temp <- air_temp_avg %>% right_join(air_temp_JulAug_avg, by="Year")

h <- ggplot(air_temp_avg)
h + geom_point(aes(x=Year, y=MAT.degreesC)) + 
  stat_smooth(aes(x=Year, y=MAT.degreesC), method="lm", formula=y~x)

#5: Precipitation#########

#Daily average over stations

precip$temp <- precip$DATE
precip <- precip %>% separate(temp, into=c("Year","Month","Day"), sep="-")
precip_avg <- summarySE(precip, measurevar = "Precip", groupvars = c("Year","Month","Day"))
precip_sum <- aggregate(precip_avg$Precip, list(precip_avg$Year), FUN=sum)
colnames(precip_sum) = c("Year","Total_Precip.mm")
precip_sum$Year <- as.numeric(precip_sum$Year)

#1955 is missing most values
precip_sum = precip_sum %>% filter(!(Year == 1955))

h <- ggplot(precip_sum)
h + geom_point(aes(x=Year, y=Total_Precip.mm)) + 
  stat_smooth(aes(x=Year, y=Total_Precip.mm), method="lm", formula=y~x)

#Combined air temp and precipitation###########

HBEF_clim <- air_temp %>% right_join(precip_sum, by="Year")

#6: Regional CO2##########

#Reduce columns 

HARV_CO2 <- regional_CO2[,c("TIMESTAMP_START","TIMESTAMP_END","CO2_1_1_1","CO2_1_1_2")]

#Convert -9999 to NA 
HARV_CO2$CO2_1_1_1 <- ifelse(HARV_CO2$CO2_1_1_1 == -9999, NA, HARV_CO2$CO2_1_1_1)
HARV_CO2$CO2_1_1_2 <- ifelse(HARV_CO2$CO2_1_1_2 == -9999, NA, HARV_CO2$CO2_1_1_2)

#Remove scientific notation 
HARV_CO2$TIMESTAMP_END <- format(HARV_CO2$TIMESTAMP_END, scientific=F)

#Extract year from timestamp 
HARV_CO2$Year <- as.numeric(substr(HARV_CO2$TIMESTAMP_END, 0, 4))

#Mean across two sensors 
HARV_CO2$Mean_CO2.ppm <- rowMeans(HARV_CO2[,3:4], na.rm=TRUE)

#Annual mean 
HARV_CO2_mean <- summarySE(HARV_CO2, na.rm=TRUE, measurevar = "Mean_CO2.ppm", groupvars = c("Year"))

h <- ggplot(HARV_CO2_mean)
h + geom_point(aes(x=Year, y=Mean_CO2.ppm))

colnames(HARV_CO2_mean) <- c("Year","HARV_CO2_N","HARV_CO2.ppm","HARV_CO2_sd","HARV_CO2_se","HARV_CO2_ci") 

#7: Regional ozone########

regional_ozone$Year <- as.numeric(regional_ozone$Year)
colnames(regional_ozone) <- c("Year","EPA_ozone.ppm","EPA_ozone_N","EPA_ozone_perct10","EPA_ozone_perct90")

#Combine regional datasets#########

regional_atm <- HARV_CO2_mean %>% right_join(regional_ozone, by="Year")

#Save datasets########
saveRDS(breaks_annual, "/.../breaks_annual.RData")

saveRDS(breaks_field, "/.../breaks_field.RData") 

saveRDS(breaks_lab, "/.../breaks_lab.RData") 

saveRDS(HBEF_clim,"/.../HBEF_clim.RData")

saveRDS(sensor_dat_combined,"/.../sensor_dat.RData")

saveRDS(regional_atm,"/.../regional_atm.RData")

saveRDS(precip_chem, "/.../precip_chem.RData") 

saveRDS(lysim_data, "/.../lysim_data.RData")
