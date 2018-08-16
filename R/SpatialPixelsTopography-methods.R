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

setMethod("spplot", signature("SpatialPixelsTopography"), definition =
            function(obj, variable="elevation",...) {
              if(variable=="elevation") {
                spplot(as(obj,"SpatialPixelsDataFrame"), zcol = "elevation",
                       col.regions = topo.colors,...)
              } else if(variable=="slope") {
                spplot(as(obj,"SpatialPixelsDataFrame"), zcol = "slope",
                       ...)
              } else if(variable=="aspect") {
                spplot(as(obj,"SpatialPixelsDataFrame"), zcol = "aspect",
                       col.regions = colorRampPalette(c("black","green","red","blue", "black")),
                       at = seq(0,360, by=5),...)
              } else if(variable=="N-S") {
                spplot(SpatialPixelsDataFrame(obj@coords, grid=obj@grid, data=data.frame(NS = cos(pi*obj@data$aspect/180))),...)
              } else if(variable=="E-W") {
                spplot(SpatialPixelsDataFrame(obj@coords, grid=obj@grid, data=data.frame(EW = sin(pi*obj@data$aspect/180))),...)
              }
            }
)

print.SpatialPixelsTopography = function(x, ...) {
  cat("Object of class SpatialPixelsTopography\n")
  print(summary(x@grid))
  cat("SpatialPoints:\n")
  print(coordinates(x))
  pst <- paste(strwrap(paste(
    "Coordinate Reference System (CRS) arguments:", 
    proj4string(x))), collapse="\n")
  cat(pst, "\n")
  if (length(x) > 0) {
    cat("\n")
    cat("Topography summary:\n")
    if (ncol(x@data) > 0)
      print(summary(x@data))
  }
  invisible(x)
}
setMethod("show", "SpatialPixelsTopography", 
          function(object) print.SpatialPixelsTopography(object))
