# library(sf)
# library(stars)
# library(raster)
# library(tidyverse)
# library(meteoland)
#
#
# # (1) Load data from meteospain and reshape as SpatialPointsMeteorology
# meteo_data <- readRDS("meteo_data.rds")
# spm <- reshapemeteospain(meteo_data)
# saveRDS(spm, "meteo_data_spm.rds")
#
#
# # (2) Build interpolator
# spm <- readRDS("meteo_data_spm.rds")
# # Elevacion
# elevation <- meteo_data %>% group_by(station_id) %>% summarize(elevation = mean(altitude))
# sum(names(spm@data)!= elevation$station_id)
# # Encontré un error en el constructor, que no permitia construirlo a partir de SpatialPointsMeteorology
# # Esto me pasa por no tener tests :-)
# # Hay que actualizar la libreria desde GitHub para que funcione el siguiente paso:
# interpolator <- MeteorologyInterpolationData(points = spm, elevation = as.numeric(elevation$elevation))
# saveRDS(interpolator, "interpolator.rds")
#
# # (3) Interpolate on points
# # Load sf spatial points and reshape them as 'SpatialPointTopography'
# interpolator <- readRDS("interpolator.rds")
# pts_sf <- readRDS("points_data.rds")
# pts_spdf <- as(pts_sf, "Spatial")
# pts_spt <- SpatialPointsTopography(as(pts_spdf,"SpatialPoints"), elevation = pts_spdf$elevation,
#                         slope = pts_spdf$slope, aspect = pts_spdf$aspect)
# # Interpolation
# spm_out <- interpolationpoints(interpolator, pts_spt)
#
# # (4) Interpolate on grid
# raster_data <-readRDS("raster_data.rds")
# # Conversion to spatialGridDataFrame
# sgdf <- as(as(raster_data,"Raster"),"SpatialGridDataFrame")
# sgt <- SpatialGridTopography(as(sgdf,"SpatialGrid"), elevation = sgdf$elevation,
#                              slope = sgdf$slope, aspect = sgdf$aspect)
# # Interpolation
# interpolator <- readRDS("interpolator.rds")
# sgm_out <- interpolationgrid(interpolator, sgt)
