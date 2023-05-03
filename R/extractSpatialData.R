# extractSpatialData<-function(x, y) {
#   if(!(inherits(x, "GridTopology") || inherits(x, "SpatialGrid") || inherits(x, "SpatialPoints") ))
#   stop("x must be an object 'GridTopology', 'SpatialGrid' or 'SpatialPoints'")
#   if(!(inherits(y, "SpatialGridDataFrame") || inherits(y, "SpatialPixelsDataFrame") ))
#     stop("y must be a SpatialGridDataFrame or SpatialPixelsDataFrame")
#
#   if(inherits(y, "SpatialPixelsDataFrame")) y = as(y, "SpatialGridDataFrame")
#   gridIndices = getGridIndex(coordinates(x), y@grid)
#   return(y@data[gridIndices,])
# }
