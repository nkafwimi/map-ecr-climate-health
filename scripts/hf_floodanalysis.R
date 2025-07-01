library(terra)
library(ggplot2)
library(sf)
library(dplyr)


tz_districts <- st_read("C:/Users/Admin/OneDrive/MAP EA/Tanzania_SMPS/Tanzania/Tanzania_shapefile_STPH/District_TZ_wgs84.shp")
tz_lakes <- st_read("C:/Users/Admin/OneDrive/MAP EA/Tanzania shapefile/Lakes/Lakes_wgs84.shp")
tz_natural <- st_read("C:/Users/Admin/OneDrive/MAP EA/Tanzania shapefile/Other Shapefiles/Natural/natural.shp")

hf_final <- read_csv("C:/Users/Admin/OneDrive/MAP EA/health facility extraction/data2use/hf_final.csv")


########################################################################################################################
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



nrow(hf_final_sf) #to check number of missing values after


hf_final_sf <- hf_final_sf %>%
  mutate(inside_tanzania = st_within(hf_final_sf, tz_districts, sparse = FALSE) %>% rowSums() > 0)

# Filter for selected years
hf_final_sf <- hf_final_sf %>%
  filter(year < 2025 & inside_tanzania)
######################################################################################
#######Load Raster flood data######################################
fld <- rast("C:/Users/Admin/OneDrive/MAP EA/ECR project/floodMapGL_rp100y (1)/floodMapGL_rp100y.tif") # raster file from DanWeis
tz_extent <- ext(29.5901945315773, 40.4456408407231, -11.7640079901029, -0.98578751029319)
fld_tz <- crop(fld,tz_extent)
plot(fld_tz)

hist(fld_tz, main="Flood Value Distribution", col="lightblue")


#Extract flood values for each facility
extracted_values <- extract(fld_tz,hf_final_sf,bind=TRUE)
extracted_df <- as.data.frame(extracted_values)
View(extracted_df)

#drop the NAs unflooded areas
extracted_df <- extracted_df %>% 
  drop_na(floodMapGL_rp100y)

write.csv(extracted_df, "extracted_values2.csv", row.names = FALSE)

extracted_sf<- extracted_df %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

##############################################################################################
###########################################################################################
# 1️ Categorize flood risk levels based on extracted flood values

extracted_sf <- extracted_sf %>%
  mutate(flood_category = case_when(
    floodMapGL_rp100y >= 0 & floodMapGL_rp100y < 0.3 ~ "Low <0.3m",
    floodMapGL_rp100y >= 0.3 & floodMapGL_rp100y <= 0.9 ~ "Medium 0.3m - 0.9m ",
    floodMapGL_rp100y > 0.9 ~ "High >0.9m",
    TRUE ~ NA_character_
  ))
ggplot() +
  # Tanzania district borders
  geom_sf(data = tz_districts, fill = "white", color = "black", size = 0.3) +
  
  # Lakes
  geom_sf(data = tz_lakes, fill = "lightblue", color = NA) +
  
  # Health facilities colored by flood risk and shaped by ownership
  geom_sf(data = extracted_sf, aes(color = flood_category, shape = ownershipgroup), size = 3, fill = "darkgrey") +
  
  # Color palette for flood risk categories
  scale_color_manual(
    values = c("Low <0.3m" = "green", "Medium 0.3m - 0.9m " = "orange", "High >0.9m" = "red"),
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
  )

write.csv(extracted_sf, "extracted_sf100yr.csv", row.names = FALSE)

#########################################################################################
#########Loading flood data per 10 year##################
fld_10 <- rast("C:/Users/Admin/OneDrive/MAP EA/ECR project/floodMapGL_rp10y/floodMapGL_rp10y.tif") 
fld_tz10 <- crop(fld_10,tz_extent)


plot(fld_tz10)

#Extract flood values for each facility
extracted_values10 <- extract(fld_tz10,hf_final_sf,bind=TRUE)
extracted_df10 <- as.data.frame(extracted_values10)
View(extracted_df10)

#drop the NAs unflooded areas
extracted_df10 <- extracted_df10 %>% 
  drop_na(floodMapGL_rp10y)


#write.csv(extracted_df10, "extracted_values1.csv", row.names = FALSE)

extracted_sf10<- extracted_df10 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 1️⃣ Categorize flood risk levels based on extracted flood values
extracted_sf10 <- extracted_sf10 %>%
  mutate(flood_category = case_when(
    floodMapGL_rp10y >= 0 & floodMapGL_rp10y < 0.3 ~ "Low <0.3m",
    floodMapGL_rp10y >= 0.3 & floodMapGL_rp10y <= 0.9 ~ "Medium <=0.9m",
    floodMapGL_rp10y > 0.9 ~ "High >0.9m",
    TRUE ~ NA_character_
  ))

# 2️⃣ Plot everything together
ggplot() +
  # Plot Tanzania district borders
  geom_sf(data = tz_districts, fill = "white", color = "black", size = 0.3) +
  
  # Plot lakes
  geom_sf(data = tz_lakes, fill = "lightblue", color = NA) +
  
  # Plot health facilities colored by flood risk category
  geom_sf(data = extracted_sf10, aes(color = flood_category), size = 2, shape=21, fill="white") +
  
  # Color palette for risk categories
  scale_color_manual(
    values = c("Low <0.3m" = "green", "Medium <=0.9m" = "orange", "High >0.9m" = "red"),
    na.value = "grey"
  ) +
  
  # Labels and theme
  labs(
    title = "Flood Risk Categories for Health Facilities (10-Year Flood)",
    color = "Flood Risk Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size=14, face="bold")
  )


write.csv(extracted_df, "extracted_values2.csv", row.names = FALSE)


###############################################################################################
###################################################################################################
####Calculating 5 km distance from flood zone###################################################
##############################################################################################3


# Load raster and crop
fld_100 <- rast("C:/Users/Admin/OneDrive/MAP EA/ECR project/floodMapGL_rp100y (1)/floodMapGL_rp100y.tif") 
fld_tz100 <- crop(fld_100, tz_extent)

# Create flood mask (flooded = 1, others = NA)
flood_mask100 <- fld_tz100 > 0

# Mask original raster to keep only flooded cells
flooded_depth100 <- mask(fld_tz100, fld_tz100 > 0)

# Convert to points (will only convert cells with non-NA values)
flood_points100 <- as.points(flooded_depth100, values=TRUE)

# Check number of flooded points
nrow(flood_points100)

flood_points_sf100 <- st_as_sf(flood_points100)
flood_points_sf100 <- st_transform(flood_points_sf100, crs=32736)

# #check CRS alignment
# st_crs(flood_points_sf) EPSG 32736 UTM - good for buffering in meters
# crs(fld_tz10) EPSG 4326 (long/lat)

#project raster to match
fld_tz100_utm <- project(fld_tz100, "EPSG:32736")



# plot(fld_tz100_utm)
# plot(st_geometry(flood_points_sf100), add=TRUE, col="blue", pch=20)
# plot(st_geometry(hf_final_sf_), add=TRUE, col="red", pch=20)

# Create 1km buffer around flooded points
flood_buffers100 <- st_buffer(flood_points_sf100, dist=5000)

# Merge overlapping buffers into one flood risk zone
flood_zone100 <- st_union(flood_buffers100)


# Identify health facilities within flood risk zone
hf_at_risk100 <- hf_final_sf_[st_intersects(hf_final_sf_, flood_zone100, sparse=FALSE), ]

# Check results
nrow(hf_at_risk100)



plot(st_geometry(hf_at_risk100))


# Plot map
ggplot() +
  # Tanzania border
  geom_sf(data = tz_districts, fill = "grey95", color = "black", size = 0.5) +
  
  # Flooded points
  geom_sf(data = flood_points_sf100, aes(color = floodMapGL_rp100y), size = 0.6, show.legend = TRUE) +
  
  # Health facilities at risk
  geom_sf(data = hf_at_risk100, color = "red", size = 2) +
  
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


write.csv(hf_at_risk100, "hf_at_risk100.csv", row.names = FALSE)






































