#CEE Soil Respiration: map of collar, sensor, and lysimeter locations 
#Updated 06-24-24

library(sf) #Package "sf" - vector data 
library(raster) #Package "raster" - raster data
library(Rcpp) #Depedencies on Rcpp
library(tmap) #Package tmap (plots)
library(leafem) #Dependences on "leafem"
library(dplyr) #Working with sf object as data frame
library(magrittr) #Special pipe
library(purrr)
library(rnaturalearth) #Downloading data
library(rnaturalearthdata) #Downloading data
#library(rgdal)
library(viridis)
library(RColorBrewer)

#Hubbard Brook contours
# USDA Forest Service, Northern Research Station. 2022. Hubbard Brook Experimental Forest USGS 40ft Contours: GIS Shapefile ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/6b5a895756973d23e40881f8459d23f0 (Accessed 2022-06-20).

hbef_base <- st_read(".../hbef_contusgs.dbf")
#NAD83 / UTM zone 19N

hbef_base_cropped <- st_crop(hbef_base, xmin=279500, ymin=4869500, xmax=282500, ymax=4872000)

#Hubbard Brook Hydrogeography
# USDA Forest Service. 2022. Hubbard Brook Experimental Forest Hydrography: GIS Shapefile ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/c62e92e0eada569e8580f5541b064dac (Accessed 2022-06-20).

hbef_hydro <- st_read("/.../hbef_hydro.dbf", crs=26919)

#Hubbard Brook Watershed Boundaries
# USDA Forest Service, Northern Research Station. 2022. Hubbard Brook Experimental Forest Watershed Boundaries: GIS Shapefile ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/10f8b45b1f689c4667db6a6be04c4de6 (Accessed 2022-06-20).

hbef_wsheds <- st_read("/.../hbef_wsheds.dbf")
# NAD83 / UTM zone 19N

#Collar positions 
# collars = read.csv("/.../trimble_collars.csv")
# 
# str(collars)
# 
# points <- SpatialPointsDataFrame(collars[,5:6], collars)
# 
# crs(points) <- "+proj=utm +zone=19N +datum=NAD83"
# 
# writeOGR(points, "/.../", "collars_shape", driver="ESRI Shapefile")

#Import as shapefile
hbef_collars <- st_read("/.../collars_shape.dbf")

#Lysimeter HPUs
# HPU = read.csv("/.../Lysimeter_HPU.csv")
# 
# str(HPU)
# 
# HPU_points <- SpatialPointsDataFrame(HPU[,6:7], HPU)
# 
# crs(HPU_points) <- "+proj=utm +zone=19N +datum=NAD83"
# 
# writeOGR(HPU_points, "/.../", "HPU_shape", driver="ESRI Shapefile")

#Import as shapefile
hbef_HPU <- st_read("/.../HPU_shape.dbf")

# #Sensor positions
#Write OGR function is deprecated.... need to just work with the SpatialPointsDataFrame directly 
sensor_locations <- read.csv("/.../sensor_plot_coordinates_rev.csv")
# 
str(sensor_locations)

sensor_points <- SpatialPointsDataFrame(sensor_locations[,2:3], sensor_locations)

crs(sensor_points) <- "+proj=longlat"
# 
# writeOGR(sensor_points, "/.../", "sensors_shape_latlong", driver="ESRI Shapefile")

# # #Import as shapefile
# hbef_sensors <- st_read("/.../sensors_shape_latlong.dbf")

# #South-facing experimental watersheds: IL1, E07, E08, E09, E10 and E12 only####
sensor_SWS <- subset(sensor_locations, Site == "IL3" | Site == "E07" | Site == "E08" | Site == "E09" | Site == "E10" | Site == "E12") 
sensor_SWS_points <- SpatialPointsDataFrame(sensor_SWS[,2:3], sensor_SWS)
crs(sensor_SWS_points) <- "+proj=longlat"

#Lysimeter locations####
lysim_locations <- read.csv("/.../lysim_plot_coordinates.csv")

lysim_points <- SpatialPointsDataFrame(lysim_locations[,2:3], lysim_locations)
# # 
crs(lysim_points) <- "+proj=longlat"

#Supplementary Figure 1: map####

#Elevation palette: R color brewer "dark" palette
palette <- brewer.pal(4, "Dark2") 

#Overview
#Sensors from south-facing experimental watersheds####
#Lysimeter locations####
#pdf("/.../Overview_Map_Lysim.pdf")
print(
  #tm_shape(hbef_base) + 
  #tm_lines(col="grey50", alpha=0.5) + 
  tm_shape(hbef_wsheds) + 
    tm_borders(col = "black", lwd = 1.5, lty = "solid") + 
    tm_compass() + 
    tm_scale_bar() + 
    tm_shape(hbef_collars) + 
    tm_bubbles(col="Elevatn", size=0.1, alpha=1, palette=c("#D95F02", "#E7298A","#7570B3","#1B9E77"), legend.col.show=FALSE) + 
    tm_shape(sensor_SWS_points) + 
    tm_bubbles(col="black", size=0.1, shape=22) + 
    tm_shape(lysim_points) + 
    tm_bubbles(col="grey50", size=0.1, shape=22) + 
    tm_shape(hbef_hydro) + 
    tm_lines())
#dev.off() 

#Detail map of collars, sensor, and lysimeter locations####
#pdf("/.../Detail_Map.pdf")
print(tm_shape(hbef_base_cropped) + 
        tm_lines(col="grey50", alpha=0.5) + 
        tm_compass() + 
        tm_scale_bar() + 
        tm_shape(hbef_collars) + 
        tm_bubbles(col="Elevatn", shape="HPU", size=0.5, alpha=1, palette=c("#D95F02", "#E7298A","#7570B3","#1B9E77"), shapes=c(21,24), legend.col.show=FALSE, legend.shape.show=FALSE) + 
        tm_shape(sensor_SWS_points) +
        tm_bubbles(col="black", size=0.5, shape=22) +
        tm_shape(lysim_points) + 
        tm_bubbles(col="grey50", size=0.5, shape=22) + 
        tm_shape(hbef_wsheds) + 
        tm_borders(col = "black", lwd = 1.5, lty = "solid") + 
        tm_shape(hbef_hydro) + 
        tm_lines()) 
#dev.off() 
