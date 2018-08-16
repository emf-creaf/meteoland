SpatialGridTopography<-function(grid, elevation, proj4string = CRS(as.character(NA))) {
  if (!is(grid, "SpatialGrid")) sg = SpatialGrid(grid, proj4string)
  else sg = grid
  nrows = sg@grid@cells.dim[1]
  ncols = sg@grid@cells.dim[2]
  cellWidth = sg@grid@cellsize[1]
  cellHeight = sg@grid@cellsize[2]
  slopeVec = .slope(elevation, nrows, ncols, cellWidth, cellHeight)
  aspectVec = .aspect(elevation, nrows, ncols, cellWidth, cellHeight)
  data = data.frame(elevation = elevation, slope = slopeVec, aspect = aspectVec)
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
setMethod("show", "SpatialGridTopography", 
          function(object) print.SpatialGridTopography(object))

