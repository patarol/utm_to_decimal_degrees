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

Ubicacion <- bind_rows(latlong29[,2], latlong30[,2])

coord_place <- tbl_df(rbind(latlong_dec29@coords, latlong_dec30@coords))
colnames(coord_place)[1:2] <- c("long", "lat")
coord_place <- bind_cols(coord_place, Ubicacion)


# custom popup
aero_popup <- paste0("<strong>Aerogenerador: </strong>",
                      as.character(coord_place$Ubicacion))

col <- if_else(coord_place$target == 0, "steelblue", "firebrick")

# map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(lng = coord_place$long, lat = coord_place$lat,
                   color = col,
                   radius = 8,
                   opacity = 1,
                   popup = aero_popup)
