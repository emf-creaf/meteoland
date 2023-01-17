#' Creates a 'SpatialGridTopography'
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Function \code{SpatialGridTopography} creates an object of class
#' \code{\link{SpatialGridTopography-class}} containing topographic variables
#' over a landscape.
#' 
#' @details
#' Slope and aspect calculations were adapted from functions in package
#' 'SDMTools', which used the approach described in Burrough & McDonell (1998).
#' 
#' The rate of change (delta) of the surface in the horizontal \code{(dz/dx)}
#' and vertical \code{(dz/dy)} directions from the center cell determines the
#' slope and aspect. The values of the center cell and its eight neighbors
#' determine the horizontal and vertical deltas. The neighbors are identified
#' as letters from 'a' to 'i', with 'e' representing the cell for which the
#' aspect is being calculated. The rate of change in the x direction for cell
#' 'e' is calculated with the algorithm:
#' 
#' \code{[dz/dx] = ((c + 2f + i) - (a + 2d + g) / (8 * x_cell_size)}
#' 
#' The rate of change in the y direction for cell 'e' is calculated with the
#' following algorithm:
#' 
#' \code{[dz/dy] = ((g + 2h + i) - (a + 2b + c)) / (8 * y_cell_size)}
#' 
#' The algorithm calculates slope as: \code{rise_run = sqrt ( [dz/dx]2 +
#' [dz/dy]2 ])}.
#' 
#' From this value , one can calculate the slope in degrees or radians as:
#' 
#' \code{slope_degrees = ATAN (rise_run) * 57.29578}
#' 
#' \code{slope_radians = ATAN (rise_run)}
#' 
#' Taking the rate of change in both the x and y direction for cell 'e', aspect
#' is calculated using:
#' 
#' \code{aspect = 57.29578 * atan2 ([dz/dy], -[dz/dx])}
#' 
#' The aspect value is then converted to compass direction values (0-360
#' degrees).
#' 
#' @param grid An object of class \code{\link{GridTopology-class}} or
#' \code{\link{SpatialGrid-class}}.
#' @param elevation A vector of elevation values for all cells of the grid (in
#' m.a.s.l.).
#' @param slope A vector of slope angles for all cells of the grid (in
#' degrees). If \code{slope=NULL}, slope is calculated as indicated in details.
#' @param aspect A vector of aspect angles for all cells of the grid (in
#' degrees from North clockwise ). \code{aspect=NULL}, aspect values are
#' calculated as indicated in details.
#' @param proj4string An object of class \code{\link{CRS-class}}.
#' @return Function \code{SpatialGridTopography} returns an object
#' '\code{\link{SpatialGridTopography-class}}'.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialGridTopography-class}}
#' @references Burrough, P. A. and McDonell, R.A., 1998. Principles of
#' Geographical Information Systems (Oxford University Press, New York), p.
#' 190.
#' @examples
#' 
#' data(examplegridtopography)
#' 
#' #Display data
#' spplot(examplegridtopography, variable="elevation", scales=list(draw=TRUE))
#' 
#' #Grids can be subsetted
#' sgt = examplegridtopography[1:50, 1:50]
#' spplot(sgt, variable="elevation", scales=list(draw=TRUE))
#' 
#' @export
SpatialGridTopography<-function(grid, elevation, slope = NULL, aspect = NULL, proj4string = CRS(as.character(NA))) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "SpatialGridTopography()", with = NULL,
    details = "Spatial_*_Topography classes are soft deprecated.
    User topography now can be provided as sf or stars objects"
  )
  
  if (!is(grid, "SpatialGrid")) sg = SpatialGrid(grid, proj4string)
  else sg = grid
  nrows = sg@grid@cells.dim[1]
  ncols = sg@grid@cells.dim[2]
  cellWidth = sg@grid@cellsize[1]
  cellHeight = sg@grid@cellsize[2]
  if(is.null(slope)) slope = .slope(elevation, nrows, ncols, cellWidth, cellHeight)
  if(is.null(aspect)) aspect = .aspect(elevation, nrows, ncols, cellWidth, cellHeight)
  data = data.frame(elevation = elevation, slope = slope, aspect = aspect)
  lt = new("SpatialGridTopography",
          grid = sg@grid,
          bbox = sg@bbox,
          proj4string = sg@proj4string,
          data = data)
  return(lt)
}

#' @export
setMethod("spplot", signature("SpatialGridTopography"), definition =
            function(obj, variable="elevation",...) {
              if(variable=="elevation") {
                spplot(as(obj,"SpatialGridDataFrame"), zcol = "elevation",
                       col.regions = topo.colors,...)
              } else if(variable=="slope") {
                spplot(as(obj,"SpatialGridDataFrame"), zcol = "slope",
                       ...)
              } else if(variable=="aspect") {
                spplot(as(obj,"SpatialGridDataFrame"), zcol = "aspect",
                       col.regions = colorRampPalette(c("black","green","red","blue", "black")),
                       at = seq(0,360, by=5),...)
              } else if(variable=="N-S") {
                spplot(SpatialGridDataFrame(obj@grid, data.frame(NS = cos(pi*obj@data$aspect/180))),...)
              } else if(variable=="E-W") {
                spplot(SpatialGridDataFrame(obj@grid, data.frame(EW = sin(pi*obj@data$aspect/180))),...)
              }
            }
)

print.SpatialGridTopography = function(x, ...) {
  cat("Object of class SpatialGridTopography\n")
  print(summary(x@grid))
  pst <- paste(strwrap(paste(
    "Coordinate Reference System (CRS) arguments:", 
    proj4string(x))), collapse="\n")
  cat(pst, "\n")
  if (length(x) > 0) {
    cat("\n")
    cat("Topography summary:\n")
    if (ncol(x@data) > 1)
      sobj = summary(x@data)
    else sobj = summary(x@data[[1]])
    print(sobj)
  }
  invisible(x)
}

#' @export
setMethod("print", "SpatialGridTopography", function(x, ...) print.SpatialGridTopography(x, ...))

#' @export
setMethod("show", "SpatialGridTopography", 
          function(object) print.SpatialGridTopography(object))

subs.SpatialGridTopography <- function(x, i, j, ..., drop = FALSE) {
  drop <- FALSE
  #		if (!missing(drop))
  #			stop("don't supply drop: it needs to be FALSE anyway")
  grd = x@grid
  if (missing(i))
    rows = 1:grd@cells.dim[2]
  else {
    if (is(i, "Spatial"))
      stop("area selection only makes sense for objects of class SpatialPixels or SpatialGridDataFrame; for object of class SpatialGrid you can only select x[rows,cols]")
    rows = i
  }
  if (missing(j))
    cols = 1:grd@cells.dim[1]
  else
    cols = j
  idx = 1:prod(grd@cells.dim[1:2])
  m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[rows,cols]
  idx = as.vector(t(m)) # t(m)?
  # print(idx)
  if (any(is.na(idx)))
    stop("NAs not permitted in index")
  if (length(idx) == 0) {
    return(x)
  } 
  pts = SpatialPoints(coordinates(x)[idx,,drop=FALSE], CRS(proj4string(x)))
  if (length(idx) == 1) {
    new("SpatialPointsTopography",
        coords = pts@coords,
        bbox = pts@bbox,
        proj4string = pts@proj4string,
        data = x@data[idx, , drop = FALSE])
  } else {
    sg = as(SpatialPixels(pts), "SpatialGrid")
    new("SpatialGridTopography",
             grid = sg@grid,
             bbox = sg@bbox,
             proj4string = sg@proj4string,
             data = x@data[idx, , drop = FALSE])
  }
}

#' @export
setMethod("[", "SpatialGridTopography", subs.SpatialGridTopography)

as.SpGrdTop.SpPoiTop = function(from) {
  sp <- as(from, "SpatialPoints")
  new("SpatialPointsTopography",
      coords = sp@coords,
      bbox = sp@bbox,
      proj4string = sp@proj4string,
      data = from@data)
}
setAs("SpatialGridTopography", "SpatialPointsTopography", as.SpGrdTop.SpPoiTop)


as.SpGrdTop.SpPixTop = function(from) { 
  spdf = as(from, "SpatialPixelsDataFrame")
  new("SpatialPixelsTopography",
           coords = spdf@coords,
           coords.nrs = spdf@coords.nrs,
           bbox = spdf@bbox,
           grid = spdf@grid,
           grid.index = spdf@grid.index,
           proj4string = spdf@proj4string,
           data = spdf@data)

}
setAs("SpatialGridTopography", "SpatialPixelsTopography", as.SpGrdTop.SpPixTop)
