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

#########################################################################################################################

# load data

tz_districts <- st_read("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/data/shapefiles/District_TZ_wgs84.shp")
tz_lakes <- st_read("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/data/shapefiles/Lakes/Lakes_wgs84.shp")


hf_final <- read_csv("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/data/csvs/tz_health_facilities.csv")


######### EDA ###########

# HFs ownership
hf_own <- hf_final  %>% group_by(ownershipgroup) %>%
  summarise(count = n())

ggplot(hf_own, aes(x=reorder(ownershipgroup, -count), y=count))+
  geom_bar(stat="identity", width = 0.5, color='skyblue',fill='skyblue')+
  theme(axis.text.x=element_text(angle=45, hjust=0.9))+
  labs(x="Ownership Group")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Health Facilities Ownership: Private vs Public")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  

# HFs distribution across regions
hf_dist <- hf_final  %>% group_by(region) %>% 
  summarise(count = n()) 

ggplot(hf_dist, aes(x=reorder(region, -count), y=count))+
  geom_bar(stat="identity", color='skyblue',fill='skyblue')+
  theme(axis.text.x=element_text(angle=45, hjust=0.9))+
  labs(x="Region")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Health Facilities Distribution")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

# HFs type
hf_type <- hf_final  %>% group_by(facilitytypegroup) %>% 
  summarise(count = n())

ggplot(hf_type, aes(x=reorder(facilitytypegroup, -count), y=count))+
  geom_bar(stat="identity", color='skyblue',fill='skyblue')+
  theme(axis.text.x=element_text(angle=45, hjust=0.9))+
  labs(x="Type")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Health Facilities Type")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())


########################################################################################################################

# Data Processing

hf_final <- hf_final %>%
  mutate(
    Longitude = as.numeric(longitude),
    Latitude = as.numeric(latitude)
  )

summary(hf_final$longitude)
summary(hf_final$latitude)

nrow(hf_final %>% filter(!is.na(longitude), !is.na(latitude))) #to check number of missing values 12527

hf_final_sf<- hf_final %>%
  filter(
    #year >= 2010,
    !is.na(Longitude),
    !is.na(Latitude),
    Longitude >= -180 & Longitude <= 180,
    Latitude >= -90 & Latitude <= 90
  )%>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)



nrow(hf_final_sf)  # to check number of missing values after


hf_final_sf <- hf_final_sf %>%
  mutate(inside_tanzania = st_within(hf_final_sf, tz_districts, sparse = FALSE) %>% rowSums() > 0)

hf_final_sf <- hf_final_sf %>% filter(inside_tanzania)


ggplot() + 
  geom_sf(data = tz_districts , aes(fill = Region_Nam), colour = "grey50", size = 0.2) + # now we need to tell ggplot an attribute of the data to use as an aesthetic
  scale_fill_viridis_d()+
  geom_sf(data = tz_lakes, fill= "lightblue", colour = "lightblue", legend=F) +
  geom_sf(data = hf_final_sf, size = 0.75, colour = "coral1") +
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Tanzania Health Facilities' Locations", fill = "Region",
       shape = "Health Facility") +
  theme(legend.position = "right") 

# Filter for selected years and facility type
hf_final_sf <- hf_final_sf %>%
  filter(year < 2025 & inside_tanzania,
         facilitytypegroup %in% c("Dispensary", "Clinic", "Hospital", "Maternity and Nursing Home",
                                  "Maternity Home", "Health Center", "Blood Transfusion Centre at Zonal Level",
                                  "Mobile Radiology and Imaging Centre"))


######################################################################################################
####### Load Raster flood data for 100 yr period ######################################################
 

fld <- rast("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/floodMap/floodMapGL_rp100y.tif") # raster file from DanWeis

tz_extent <- ext(29.5901945315773, 40.4456408407231, -11.7640079901029, -0.98578751029319)
fld_tz <- crop(fld,tz_extent)
plot(fld_tz)

hist(fld_tz, main="Flood Value Distribution", col="lightblue")

#Extract flood values for each facility

extracted_values <- extract(fld_tz,hf_final_sf,bind=TRUE)
extracted_df <- as.data.frame(extracted_values)
View(extracted_df)

# add geometric object to the flood values
hf_floodvalues_df <- merge(hf_final_sf, extracted_df[, c("facility_code", "floodMapGL_rp100y")], by.x = "facility_code", by.y = "facility_code") 
hf_floodvalues_df <- hf_floodvalues_df %>% 
  filter(floodMapGL_rp100y > 0)             # filter locations with inundation values > 0

# plot HFs located at forecasted flood areas
plot(fld_tz, main = "Health Facilities at Flood Risk")
plot(st_geometry(tz_districts), border='grey', add=TRUE)
plot(st_geometry(hf_floodvalues_df), add=TRUE, pch=19, cex = 0.75, col="blue1")


### Identify facilities located 5 km from forecasted flooded areas by calculating distance between points
# For each hf at risk, calculate the distance between that point and all the hfs if distance is < 5 km then add the point to the empty sf dataframe

# transform crs to UTM for calculations 
hf_fldv_geom <- st_transform(hf_floodvalues_df[,c("geometry")], crs=32736)
hf_final_geom <- st_transform(hf_final_sf[,c("geometry")], crs=32736)

n <- dim(hf_fldv_geom)[1]
m <- dim(hf_final_geom)[1]
hf_5km <- st_sf(st_sfc(), crs=32736)   # initialize an empty sf dataframe
hf_1km <- st_sf(st_sfc(), crs=32736)   # initialize an empty sf dataframe

#################### HFs within 5 km ################################################################

for (i in 1:n){
  for (j in 1:m){
    dist <- as.numeric(st_distance(hf_fldv_geom[i,], hf_final_geom[j,]))
    if (dist > 0 & dist <= 5000){
      hf_5km <- rbind(hf_5km, hf_final_geom[j,])
    }
  }
}

################### HFs within 1 km ####################################################################

for (i in 1:n){
  for (j in 1:m){
    dist <- as.numeric(st_distance(hf_fldv_geom[i,], hf_final_geom[j,]))
    if (dist > 0 & dist <= 1000){
      hf_1km <- rbind(hf_1km, hf_final_geom[j,])
    }
  }
}

######################################################################################################


hf_5km_flt <- subset(hf_5km, !(geometry %in% hf_fldv_geom$geometry))
hf_5km_unq <- distinct(hf_5km_flt)

hf_5km_risk <- st_transform(hf_5km_unq, crs=4326)
# hf_5_risk <- st_join(hf_5_risk, hf_final_sf, join = st_nearest_feature)


# plot HFs on forcested flood zone and HFs within 5 km 

plot(fld_tz, main = "Health Facilities at Flood Risk", legend=F)
plot(st_geometry(tz_districts), add = TRUE, border='grey', main = "Health Facilities at Flood Risk")
plot(st_geometry(hf_floodvalues_df), add=TRUE, pch=19, cex = 0.75, col="blue1")
plot(st_geometry(hf_5km_risk), add=TRUE, pch=19, cex = 0.75, col="coral1")
legend(30,-9.5,legend=c("Flood zone", "≤ 5 km"), 
       fill = c("blue1","coral1"), box.lwd=1,cex = 0.75)
