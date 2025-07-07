# load libraries

library(sf)          # for reading and working with shapefiles (vector data)
library(tidyverse)   # for data manipulation (dplyr), plotting (ggplot2), tidyr, etc.
library(ggplot2)
library(lubridate)   # for date/time functions
library(terra)       # for raster data
library(ggspatial)   # for scale bars, north arrows in ggplot
library(dplyr)
library(tidyr)

#########################################################################################################################

# load data

tz_districts <- st_read("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/data/shapefiles/District_TZ_wgs84.shp")
tz_lakes <- st_read("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/data/shapefiles/Lakes/Lakes_wgs84.shp")


hf_final <- read_csv("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/data/csvs/tz_health_facilities.csv")

#######################################################################################################

# Data Processing

######### Data Processing Part. 1 #################



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
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) #removing incorrect and missing coordinates



nrow(hf_final_sf)  # to check number of missing values after


hf_final_sf <- hf_final_sf %>%
  mutate(inside_tanzania = st_within(hf_final_sf, tz_districts, sparse = FALSE) %>% rowSums() > 0) # adds column to check if coordinates fit within the Tanzania geographical boundaries

hf_final_sf <- hf_final_sf %>% filter(inside_tanzania) # filters coordinates outside the Tanzania geographic boundaries



ggplot() + 
  geom_sf(data = tz_districts , aes(fill = Region_Nam), colour = "grey50", size = 0.2) + # now we need to tell ggplot an attribute of the data to use as an aesthetic
  scale_fill_viridis_d()+
  geom_sf(data = tz_lakes, fill= "lightblue", colour = "lightblue", legend=F) +
  geom_sf(data = hf_final_sf, size = 0.75, colour = "coral1") +
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Tanzania Health Facilities' Locations", fill = "Region",
       shape = "Health Facility") +
  theme(legend.position = "right") 


######### EDA ######################################

# HFs ownership
hf_own <- hf_final  %>% group_by(ownershipgroup) %>%
  summarise(count = n())

ggplot(hf_own, aes(x=reorder(ownershipgroup, -count), y=count))+
  geom_bar(stat="identity", width = 0.5, color='skyblue',fill='skyblue')+
  theme(axis.text.x=element_text(angle=45, hjust=0.9), 
        plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank())+
  labs(title = "Health Facilities Ownership: Private vs Public", x="Ownership Group")
  

# HFs distribution across regions
hf_dist <- hf_final  %>% group_by(region) %>% 
  summarise(count = n()) 

ggplot(hf_dist, aes(x=reorder(region, -count), y=count))+
  geom_bar(stat="identity", color='skyblue',fill='skyblue')+
  theme(axis.text.x=element_text(angle=45, hjust=0.9),
        plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank())+
  labs(x="Region", title = "Health Facilities Distribution")

hf_dist_zone <- hf_final  %>% group_by(zone) %>% 
  summarise(count = n())  

# HFs type
hf_type <- hf_final  %>% group_by(facilitytypegroup) %>% 
  summarise(count = n())

ggplot(hf_type, aes(x=reorder(facilitytypegroup, -count), y=count))+
  geom_bar(stat="identity", color='skyblue',fill='skyblue')+
  theme(axis.text.x=element_text(angle=45, hjust=0.9),
        plot.title = element_text(hjust=0.5),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank())+
  labs(x="Type", title = "Health Facilities Type")

########################################################################################################################


######### Data Processing Part. 2 #################

# Filter for selected years and facility type
hf_final_sf <- hf_final_sf %>%
  filter(year < 2025 & inside_tanzania,
         facilitytypegroup %in% c("Dispensary", "Clinic", "Hospital", "Maternity and Nursing Home",
                                  "Maternity Home", "Health Center", "Blood Transfusion Centre at Zonal Level",
                                  "Mobile Radiology and Imaging Centre"))


######################################################################################################

# Load Raster flood data for 100 yr period 
 

fld <- rast("C:/Users/nkafwimi/Desktop/Stuff/ECR/ECR Project/health-facilities-mapping/flood_100yr_period/floodMapGL_rp100y.tif") # raster file from DanWeis

tz_extent <- ext(29.5901945315773, 40.4456408407231, -11.7640079901029, -0.98578751029319) # define geographic boundary of Tanzania interms of coordinates
fld_tz <- crop(fld,tz_extent) # crop flood raster data based in Tanzania map
plot(fld_tz)

hist(fld_tz, main="Flood Value Distribution", col="lightblue")

#Extract flood values for each facility

extracted_values <- extract(fld_tz,hf_final_sf,bind=TRUE)
extracted_df <- as.data.frame(extracted_values)
View(extracted_df)

# add geometric object to the flood values
# hf_floodvalues_df <- merge(hf_final_sf, extracted_df[, c("facility_code", "floodMapGL_rp100y")], by.x = "facility_code", by.y = "facility_code") 
hf_floodvalues_df <- extracted_df %>% 
  filter(floodMapGL_rp100y > 0) %>% # filter locations with inundation values > 0
  st_as_sf(coords = c( "longitude","latitude"), crs = 4326) %>% 
  mutate(risklevel = case_when(
    floodMapGL_rp100y >= 0 & floodMapGL_rp100y < 0.3 ~ "Low <0.3m",
    floodMapGL_rp100y >= 0.3 & floodMapGL_rp100y < 0.9 ~ "Medium 0.3m - 0.9m",
    floodMapGL_rp100y >= 0.9 ~ "High >0.9m",
    TRUE ~ NA_character_
    
  )
  )

# plot HFs located at the forecasted flood areas (as layered on the flood raster image)
plot(fld_tz, main = "Health Facilities at Flood Risk")
plot(st_geometry(tz_districts), border='grey', add=TRUE)
plot(st_geometry(hf_floodvalues_df), add=TRUE, pch=19, cex = 0.75, col="blue1")

# plot HFs located at the forecasted flood areas (without the as layered on the flood raster image)
ggplot() +
  # Tanzania district borders
  geom_sf(data = tz_districts, fill = "white", color = "black", size = 0.3) +
  
  # Lakes
  geom_sf(data = tz_lakes, fill = "lightblue", color = NA) +
  
  # Health facilities colored by flood risk and shaped by ownership
  geom_sf(data = hf_floodvalues_df, aes(color = risklevel, shape = ownershipgroup), size = 3, fill = "darkgrey") +
  
  # Color palette for flood risk categories
  scale_color_manual(
    values = c("Low <0.3m" = "green", "Medium 0.3m - 0.9m" = "orange", "High >0.9m" = "red"),
    na.value = "grey"
  ) +
  
  # Shape palette for ownership types 
  scale_shape_manual(
    values = c("Public" = 21, "Private" = 24)  # 21: circle, 24: triangle
  ) +
  
  # Labels and theme
  labs(
    title = "Flood Risk Categories for Health Facilities (100-Year Flood)",
    color = "Flood Risk Category",
    shape = "Ownership"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )# Health facilities colored by flood risk and shaped by ownership
  geom_sf(data = hf_floodvalues_df, color = "blue", size = 3, fill = "darkgrey") 


########### Analyzing HFs located on the flood zone by type, ownership, zone and region
# HFs ownership
hf_risk_own_1 <- hf_floodvalues_df %>% group_by(ownershipgroup) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs type
hf_risk_type_1 <- hf_floodvalues_df  %>% group_by(facilitytypegroup) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs zone
hf_risk_zone_1 <- hf_floodvalues_df  %>% group_by(zone) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs region
hf_risk_reg_1 <- hf_floodvalues_df  %>% group_by(region) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs ownership and type
hf_risk_type_own_1 <- hf_floodvalues_df  %>% group_by(facilitytypegroup, ownershipgroup) %>%
  summarise(count = n(), .groups="keep") %>% 
  mutate(proportion = count / sum(count))

# HFs risk level
hf_risklevel_1 <- hf_floodvalues_df  %>% group_by(risklevel) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs risk level and type
hf_risklevel_type_1 <- hf_floodvalues_df  %>% group_by(risklevel, facilitytypegroup) %>%
  summarise(count = n(), .groups="keep") %>% 
  mutate(proportion = count / sum(count))

# HFs risk level, type, and ownership
hf_risklevel_type_own_1 <- hf_floodvalues_df  %>% group_by(risklevel, facilitytypegroup, ownershipgroup) %>%
  summarise(count = n(), .groups="keep") %>% 
  mutate(proportion = count / sum(count))



#################### HFs within 5 km ################################################################
### Identify facilities located 5 km from forecasted flooded areas by
### calculating distance between points

###################### Approach 1 #############################

# # transform crs to UTM for calculations 
# hf_fldv_geom <- st_transform(hf_floodvalues_df[,c("geometry")], crs=32736)
# hf_final_geom <- st_transform(hf_final_sf[,c("geometry")], crs=32736)
# 
# n <- dim(hf_fldv_geom)[1]
# m <- dim(hf_final_geom)[1]
# hf_5km <- st_sf(st_sfc(), crs=32736)   # initialize an empty sf dataframe
# hf_1km <- st_sf(st_sfc(), crs=32736)   # initialize an empty sf dataframe
# 
# 
# # For each hf at risk, calculate the distance between that point and all the hfs
# # if distance is < 5 km then add the point to the empty sf dataframe
# 
# for (i in 1:n){
#   for (j in 1:m){
#     dist <- as.numeric(st_distance(hf_fldv_geom[i,], hf_final_geom[j,]))
#     if (dist > 0 & dist <= 5000){
#       hf_5km <- rbind(hf_5km, hf_final_geom[j,])
#     }
#   }
# }
# 
# 
# hf_5km_flt <- subset(hf_5km, !(geometry %in% hf_fldv_geom$geometry))  # remove hfs located in flood areas 
# hf_5km_unq <- distinct(hf_5km_flt)                                    # remove duplicate values
# 
# hf_5km_risk <- st_transform(hf_5km_unq, crs=4326)                     # transform crs back to WGS 84
# # hf_5_risk <- st_join(hf_5_risk, hf_final_sf, join = st_nearest_feature)
# 
# 
# # plot HFs on forcested flood zone and HFs within 5 km 
# 
# plot(fld_tz, main = "Health Facilities at Flood Risk", legend=F)
# plot(st_geometry(tz_districts), add = TRUE, border='grey', main = "Health Facilities at Flood Risk")
# plot(st_geometry(hf_floodvalues_df), add=TRUE, pch=19, cex = 0.75, col="blue1")
# plot(st_geometry(hf_5km_risk), add=TRUE, pch=19, cex = 0.75, col="coral1")
# legend(30,-9.5,legend=c("Flood zone", "≤ 5 km"), 
#        fill = c("blue1","coral1"), box.lwd=1,cex = 0.75)
# 
# 

###################### Approach 2 #############################

# Create flood mask (flooded = 1, others = NA)
mask_fld <- fld_tz > 0

# Mask original raster to keep only flooded cells
fld_tz_val <- mask(fld_tz, mask_fld)

# Convert to points (will only convert cells with non-NA values)
fld_tz_pts <- as.points(fld_tz_val, values = TRUE)

fld_tz_sf <- st_as_sf(fld_tz_pts)                      # convert SpatVector to an sf object
fld_tz_sf <- st_transform(fld_tz_sf, crs=32736)        # transform crs to UTM::32736 in order to calculate the distance


# fld_tz100_utm <- project(fld_tz, "EPSG:32736")
# 
# plot(fld_tz100_utm)
# plot(st_geometry(fld_tz_sf), add=TRUE, col="blue", pch=20)


# creating a 5 km buffer around flood zone
buffer_5k <- st_buffer(fld_tz_sf, dist = 5000)

fld_tz_sf_5k <- st_union(buffer_5k)

# transform CRS
hf_final_sf_UTM <- st_transform(hf_final_sf, crs=32736)
hf_at_risk_5k <- hf_final_sf_UTM[st_intersects(hf_final_sf_UTM,fld_tz_sf_5k, sparse=FALSE),]

# join hf_at_risk_5k and fld_tz_sf to pull information about inundation
hf_at_risk_5k <- st_join(hf_at_risk_5k, fld_tz_sf, join=st_nearest_feature)
hf_at_risk_5k <- hf_at_risk_5k %>%
  mutate(risklevel = case_when(
    floodMapGL_rp100y >= 0 & floodMapGL_rp100y < 0.3 ~ "Low <0.3m",
    floodMapGL_rp100y >= 0.3 & floodMapGL_rp100y < 0.9 ~ "Medium 0.3m - 0.9m",
    floodMapGL_rp100y >= 0.9 ~ "High >0.9m",
    TRUE ~ NA_character_
    
  )
  )

# hf_at_risk_5k <- st_transform(hf_at_risk_5k, crs=4326)

# plot HFs within 5 km of flood zone
# plot(fld_tz, main = "Health Facilities at Flood Risk")
# plot(st_geometry(tz_districts), border='grey', add=TRUE)
# plot(st_geometry(hf_at_risk_5k), add=TRUE, pch=19, cex = 0.75, col="blue1")

# Plot map
ggplot() +
  # Tanzania border
  geom_sf(data = tz_districts, fill = "grey95", color = "black", size = 0.5) +

  # Flooded points
  geom_sf(data = fld_tz_sf, aes(color = floodMapGL_rp100y), size = 0.6, show.legend = TRUE) +

  # Health facilities at risk
  geom_sf(data = hf_at_risk_5k, color = "red", size = 2) +

  # Title and labels
  labs(title = "Health Facilities at Risk of 100-year Flood Inundation",
       #subtitle = "Flood depths from floodMapGL_rp100y.tif",
       caption = "Source: GLOFAS") +

  # Legend and theme tweaks
  scale_color_viridis_c(name = "Flood Depth (m)", option = "D") +

  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_text(size = 8)
  )

########### Analyzing HFs located within 5 km of flood zone by type, ownership, zone and region
# HFs ownership
hf_risk_own_5 <- hf_at_risk_5k %>% group_by(ownershipgroup) %>%
  summarise(count = n())  %>% 
  mutate(proportion = count / sum(count))

# HFs type
hf_risk_type_5 <- hf_at_risk_5k  %>% group_by(facilitytypegroup) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs zone
hf_risk_zone_5 <- hf_at_risk_5k  %>% group_by(zone) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs region
hf_risk_reg_5 <- hf_at_risk_5k  %>% group_by(region) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count))

# HFs type, ownership
hf_risk_reg_5 <- hf_at_risk_5k  %>% group_by(facilitytypegroup, ownershipgroup) %>%
  summarise(count = n(), .groups="keep") %>% 
  mutate(proportion = count / sum(count))







######################################################################################################

