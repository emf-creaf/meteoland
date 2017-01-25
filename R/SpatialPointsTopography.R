SpatialPointsTopography<-function(points, elevation, slope, aspect) {
  if(!inherits(points,"SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'.")
  npoints = nrow(points@coords)
  if(length(elevation)!=npoints) stop("'elevationp' has to be of length equal to the number of points")
  if(length(slope)!=npoints) stop("'slopep' has to be of length equal to the number of points")
  if(length(aspect)!=npoints) stop("'aspectp' has to be of length equal to the number of points")
  data = data.frame(elevation = elevation, slope = slope, aspect = aspect)
  lt = new("SpatialPointsTopography",
          coords = points@coords,
          bbox = points@bbox,
          proj4string =points@ proj4string,
          data = data)
  return(lt)
}
