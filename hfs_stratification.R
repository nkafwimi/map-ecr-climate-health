# load libraries

library(sf)          # for reading and working with shapefiles (vector data)
library(tidyverse)   # for data manipulation (dplyr), plotting (ggplot2), tidyr, etc.
library(ggplot2)
library(lubridate)   # for date/time functions
library(terra)       # for raster data
library(tmap)        # for interactive or thematic mapping
library(ggspatial)   # for scale bars, north arrows in ggplot
library(dplyr)
library(tidyr)
library(geodata)     # for reading climate from WorldClim:https://worldclim.org/


# read shapefile
tz_adm2 <- st_read("data/shapefiles/District_TZ_wgs84.shp")      # Tanzania administrative locations (adm 2) spatial vector data

# inspect the shapefile data
print(tz_adm2)       # prints the sf object, showing attributes + geometry
names(tz_adm2)       # column names
st_crs(tz_adm2)      # check the coordinate reference system (CRS)

# rename columns
tz_adm2 <- tz_adm2 %>% rename(region = Region_Nam, district_name = District_N, district = DHIS2_Dist)


# plot the sf object
plot(tz_adm2, main = "Tanzania")      # plots the map of Tanzania divided by districts

# ggpplot for the tz_adm2 spatial vector data
ggplot(data=tz_adm2)+
  geom_sf(aes(fill=region))+
  geom_sf_text(aes(label=district))+
  scale_fill_viridis_d()+
  labs(
    title="Map of Tanzania",                               # adds the title 
    subtitle="Coloured by Region",                         # adds the subtitle 
    fill="Region"                                          # adds the title of the legend
  )+
  theme_minimal()+                                         # removes the grey tint in the background
  theme(
    plot.title = element_text(face = "bold", size = 16),   # customizes the title size
    legend.position = "bottom"                             # sets the position of the legend 
  )


# layering health facility location data points onto the shapefile
# read Tanzania health facilities' dataset containing  sets of coordinate pairs

# tz_hfs <- read_csv("data/csvs/health_facilities_list.csv")
tz_hfs <- read_csv("data/csvs/hf_final.csv")

### EDA
spec(tz_hfs)
head(tz_hfs)
names(tz_hfs)

nrow(tz_hfs)                      # number of HFs
tz_hfs %>% count(ownershipgroup)  # private vs. public HFs
tz_hfs %>% count(region)          # number of HFs per region
tz_hfs %>% count(ward)            # number of HFs per ward


# clean the tz_hfs dataset
tz_hfs$latitude <- as.numeric(tz_hfs$latitude)          # change coordinates values data type to numeric
tz_hfs$longitude <- as.numeric(tz_hfs$longitude)


tz_hfs$longitude[tz_hfs["longitude"] == "null"] <- NA   # replace null coordinates values with NA
tz_hfs$latitude[tz_hfs["latitude"] == "null"] <- NA
tz_hfs$longitude[grep('_', tz_hfs$longitude)] <- NA     # replace incorrect coordinates values with NA
tz_hfs$latitude[grep('_', tz_hfs$latitude)] <- NA


tz_hfs <- subset(tz_hfs, select = -c(date, year))       # remove columns not needed for analysis

sapply(tz_hfs, function(x) sum(is.na(x)))               # number variables with missing values
tz_hfs <- drop_na(tz_hfs)                               # remove records with incorrect or missing geolocation


# converting the tz_hfs dataset into sf object
tz_hfs <- subset(tz_hfs, select=c(longitude, latitude))
tz_hfs_sf <- st_as_sf(tz_hfs, coords = c("longitude", "latitude"), crs=4326 )

ggplot() +
  geom_sf(data = tz_hfs_sf) 

plot(tz_hfs_sf)
plot(tz_hfs)


# adding health facility location data points on the Tanzania map
ggplot() + 
  geom_sf(data = tz_adm2, aes(fill = region), colour = "grey50", size = 0.2) + # now we need to tell ggplot an attribute of the data to use as an aesthetic
  scale_fill_viridis_d()+
  geom_sf(data = tz_hfs_sf, size = 2, colour = "grey20") 
# labs(title = "API, Fakeland 20XX", fill = "API ",
#      shape = "Health Facility") +
# coord_sf() + # deals with map projection
# guides() +
# 

# reading the climate data from WorldClim:https://worldclim.org/

rasters_filepath = "data/rasters"    # file location for storing rasters


raster_stack_prec <- worldclim_country(country = "TZ", 
                                       path = rasters_filepath, 
                                       version = "2.1", res=0.5,var="prec")         # loading mean monthly precitation data  

raster_stack_temp <- worldclim_country(country = "TZA",
                                       path = rasters_filepath, 
                                       version = "2.1", res=0.5,var="tavg")    # loading mean monthly precitation data  

# converting the crs of the tz_hfs to be the of the same type as the rasters
tz_hfs_sf <- st_transform(tz_hfs_sf, st_crs(raster_stack_temp))


# plotting the mean monthly rainfall data across Tanzania
# set up the plot area
par(mfrow=c(4,3))
n=length(raster_stack_prec[1])

for (i in 1:n){
  plot(raster_stack_prec[[i]],main=names(raster_stack_prec[[i]]))
  plot(st_geometry(tz_adm2), bg='transparent', border='lightgreen', add=T)
}

# plotting the average monthly temperature data across Tanzania
# set up the plot area
par(mfrow=c(4,3))
n=length(raster_stack_temp[1])

for (i in 1:n){
  plot(raster_stack_temp[[i]],main=names(raster_stack_temp[[i]]))
  plot(st_geometry(tz_adm2), bg='transparent', border='grey', add=T)
}
