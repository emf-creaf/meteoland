SpatialPixelsTopography<-function(points, elevation, slope, aspect, tolerance = sqrt(.Machine$double.eps), 
                                  proj4string = CRS(as.character(NA)), round = NULL, grid = NULL) {
  if(!(inherits(points,"SpatialPoints")|| inherits(points,"matrix"))) stop("'points' has to be of class 'matrix' or 'SpatialPoints'.")
  if(inherits(points,"SpatialPoints")) {
    npoints = nrow(points@coords)
  } else {
    npoints = nrow(as.matrix(points))
  }
  if(length(elevation)!=npoints) stop("'elevation' has to be of length equal to the number of points")
  if(length(slope)!=npoints) stop("'slope' has to be of length equal to the number of points")
  if(length(aspect)!=npoints) stop("'aspect' has to be of length equal to the number of points")
  data = data.frame(elevation = elevation, slope = slope, aspect = aspect)
  spdf = SpatialPixelsDataFrame(points, data, tolerance, proj4string, round, grid)
  lt = new("SpatialPixelsTopography",
          coords = spdf@coords,
          coords.nrs = spdf@coords.nrs,
          bbox = spdf@bbox,
          grid = spdf@grid,
          grid.index = spdf@grid.index,
          proj4string = spdf@proj4string,
          data = spdf@data)
  return(lt)
}
