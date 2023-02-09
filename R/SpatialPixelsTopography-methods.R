#' Creates a 'SpatialPixelsTopography'
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Function \code{SpatialPixelsTopography} creates an object of class
#' \code{\link{SpatialPixelsTopography-class}} containing topographic variables
#' for a set of points.
#' 
#' 
#' @param points An object of class \code{\link{SpatialPoints-class}} or a
#' numeric matrix of coordinates.
#' @param elevation Elevation values (in m) of the points.
#' @param slope Slope values (in degrees) of the points.
#' @param aspect Aspect values (in degrees from North) of the points.
#' @param tolerance Precision up to which extent points should be exactly on a
#' grid.
#' @param proj4string Object of class \code{\linkS4class{CRS}} in the first
#' form only used when points does not inherit from
#' \code{\linkS4class{Spatial}}.
#' @param round default \code{NULL}, otherwise a value passed to as the digits
#' argument to \code{\link{round}} for setting cell size.
#' @param grid Grid topology using an object of class
#' \code{\linkS4class{GridTopology}}; a value of \code{NULL} implies that this
#' will be derived from the point coordinates.
#' @return Function \code{SpatialPixelsTopography} returns an object
#' '\code{\link{SpatialPixelsTopography-class}}'.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsTopography-class}}
#' @examples
#' 
#' data(examplegridtopography)
#' 
#' #Creates spatial topography pixels as a subset of points in the grid
#' spt = as(examplegridtopography,"SpatialPointsTopography")
#' cc = spt@coords
#' center = 5160
#' d = sqrt((cc[,1]-cc[center,1])^2+(cc[,2]-cc[center,2])^2)
#' p = which(d<3000) #Select points at maximum distance of 3km from center
#' spxt = SpatialPixelsTopography(spt[p], spt$elevation[p],
#'                               spt$slope[p],
#'                               spt$aspect[p])
#' 
#' #Alternatively, use coercing and subsetting (drop = TRUE causes grid to be recalculated) 
#' spxt = as(examplegridtopography, "SpatialPixelsTopography")[p, drop=TRUE]          
#'                               
#' #Display data
#' spplot(spxt, variable="elevation", scales=list(draw=TRUE))
#' spplot(spxt, variable="slope", scales=list(draw=TRUE))
#' spplot(spxt, variable="aspect", scales=list(draw=TRUE))
#' 
#' @export
SpatialPixelsTopography<-function(points, elevation, slope, aspect, tolerance = sqrt(.Machine$double.eps), 
                                  proj4string = CRS(as.character(NA)), round = NULL, grid = NULL) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "SpatialPixelsTopography()", with = NULL,
    details = "Spatial_*_Topography classes are soft deprecated.
    User topography now can be provided as sf or stars objects"
  )
  
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

#' @export
setMethod("[", "SpatialPixelsTopography",
          function(x, i, j, ..., drop = FALSE) {
            if (!missing(j))
              stop("can only select pixels with a single index")
            if (missing(i))
              return(x)
            if (is(i, "Spatial"))
              i = !is.na(over(x, geometry(i)))
            if(length(i)==1) {
              return(new("SpatialPointsTopography",
                  coords = x@coords[i,,drop=FALSE],
                  bbox = x@bbox,
                  proj4string = x@proj4string,
                  data = x@data[i, , drop = FALSE]))
            }
            if (drop) { # if FALSE: adjust bbox and grid
              res = as(x, "SpatialPoints")[i]
              tolerance = list(...)$tolerance
              if (!is.null(tolerance))
                spdf = SpatialPixelsDataFrame(res, x@data[i,,FALSE], tolerance)
              else
                spdf = SpatialPixelsDataFrame(res, x@data[i,,FALSE])
              new("SpatialPixelsTopography",
                       coords = spdf@coords,
                       coords.nrs = spdf@coords.nrs,
                       bbox = spdf@bbox,
                       grid = spdf@grid,
                       grid.index = spdf@grid.index,
                       proj4string = spdf@proj4string,
                       data = spdf@data)
            } else # default: don't adjust bbox and grid
              new("SpatialPixelsTopography",
                  coords = x@coords[i, , drop = FALSE],
                  coords.nrs = x@coords.nrs,
                  bbox = x@bbox,
                  grid = x@grid,
                  grid.index = x@grid.index[i],
                  proj4string = x@proj4string,
                  data = x@data[i,,drop=FALSE])
          }
)

as.SpPixTop.SpPoiTop = function(from) {
  
  sp <- as(from, "SpatialPoints")
  new("SpatialPointsTopography",
      coords = sp@coords,
      bbox = sp@bbox,
      proj4string = sp@proj4string,
      data = from@data)
}
setAs("SpatialPixelsTopography", "SpatialPointsTopography", as.SpPixTop.SpPoiTop)

as.SpPixTop.SpGrdTop = function(from) { 
  sgdf = as(from, "SpatialGridDataFrame")
  new("SpatialGridTopography",
      grid = sgdf@grid,
      bbox = sgdf@bbox,
      proj4string = sgdf@proj4string,
      data = sgdf@data)
  
}
setAs("SpatialPixelsTopography", "SpatialGridTopography", as.SpPixTop.SpGrdTop)


#' @export
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

#' @export
setMethod("show", "SpatialPixelsTopography", 
          function(object) print.SpatialPixelsTopography(object))
