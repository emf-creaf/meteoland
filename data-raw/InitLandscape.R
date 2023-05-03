# library(meteoland)
#
# ## INIT LANDSCAPE TOPOGRAPHY
# load("/home/miquel/OneDrive/Datasets/MDT/Sources/Catalunya/CMDE30m_rev2.rdata")
# gt=GridTopology(c(360000,4631000), cellsize=c(100,100), cells.dim=c(100,100))
# indices = getGridIndex(coordinates(gt), mdt@grid, all.inside = FALSE)
# elevation = mdt@data[indices,]
# crs = CRS(SRS_string="EPSG:32631") # +proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"
# comment(crs)<-gsub("°", "º", comment(crs)) # Replace non-ASCII character
# examplegridtopography = SpatialGridTopography(gt, elevation, proj4string = crs)
# rm(mdt)
# spplot(examplegridtopography, variable="elevation")
# spplot(examplegridtopography, variable="slope")
# spplot(examplegridtopography, variable="aspect")
# usethis::use_data(examplegridtopography, overwrite = TRUE)
#
