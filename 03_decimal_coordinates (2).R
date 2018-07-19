library(tidyverse)
library(sp)
library(leaflet)


# Retrieve utm coordinates and create zone --------------------------------

latlong <- modelizar[,c(3,6, 14, 19,20)] %>% 
  mutate(zone = if_else(Comunidad_autonoma == "GALICIA", 29, 30)) %>%
  select(-c(target, Comunidad_autonoma)) %>% 
  na.omit %>% 
  unique
  


# filter by zone
latlong29 <- latlong %>% 
  filter(zone == 29)

latlong30 <- latlong %>% 
  filter(zone == 30)


# Create SpatialPoints for UTM coordinates --------------------------------

# UTM Zone 29
latlong29_points <- SpatialPoints(latlong29[,c(3, 4, 5, 1)],
                         proj4string=CRS("+proj=utm +zone=29 +datum=WGS84"))

latlong_dec29 <- spTransform(latlong29_points, CRS('+proj=longlat +datum=WGS84'))


# UTM Zone 30
latlong30_points <- SpatialPoints(latlong30[,c(3, 4, 5, 1)],
                           proj4string=CRS("+proj=utm +zone=30 +datum=WGS84"))

latlong_dec30 <- spTransform(latlong30_points, CRS('+proj=longlat +datum=WGS84'))




# Create leaflet Map ------------------------------------------------------

Ubicacion_tecnica_de_aero <- bind_rows(latlong29[,2], latlong30[,2])

coord_iberdrola <- tbl_df(rbind(latlong_dec29@coords, latlong_dec30@coords))
colnames(coord_iberdrola)[1:2] <- c("long", "lat")
coord_iberdrola <- bind_cols(coord_iberdrola, Ubicacion_tecnica_de_aero)


# custom popup
aero_popup <- paste0("<strong>Aerogenerador: </strong>", 
                      as.character(coord_iberdrola$Ubicacion_tecnica_de_aero))

col <- if_else(coord_iberdrola$target == 0, "steelblue", "firebrick")

# map
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = coord_iberdrola$long, lat = coord_iberdrola$lat,
                   color = col,
                   radius = 8,
                   opacity = 1, 
                   popup = aero_popup)
  
