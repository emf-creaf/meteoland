SpatialGridTopography<-function(grid, elevation, slope = NULL, aspect = NULL, proj4string = CRS(as.character(NA))) {
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
setMethod("print", "SpatialGridTopography", function(x, ...) print.SpatialGridTopography(x, ...))
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