setClass("SpatialPointsMeteorology", slots = list(dates = "Date", data="vector"), contains="SpatialPoints")
setClass("SpatialPointsTopography", contains="SpatialPointsDataFrame")
setClass("SpatialGridMeteorology", slots = list(dates = "Date", data="vector"), contains="SpatialGrid")
setClass("SpatialGridTopography", contains="SpatialGridDataFrame")
setClass("SpatialPixelsMeteorology", slots = list(dates = "Date", data="vector"), contains="SpatialPixels")
setClass("SpatialPixelsTopography", contains="SpatialPixelsDataFrame")

setMethod("spplot", signature("SpatialPixelsMeteorology"), definition=
            function(obj, date, variable="MeanTemperature", ...) {
              sgd = SpatialPixelsDataFrame(obj@grid, obj@data[[date]], obj@proj4string)
              spplot(sgd, variable, ...)
            }
)
setMethod("spplot", signature("SpatialGridMeteorology"), definition=
            function(obj, date, variable="MeanTemperature", ...) {
              sgd = SpatialGridDataFrame(obj@coords, obj@data[[date]], obj@proj4string, grid = obj@grid)
              spplot(sgd, variable, ...)
            }
)
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

setMethod("[", signature("SpatialPointsMeteorology"),definition =
  function (x, i, j, ..., drop = TRUE) 
  {
    if (!missing(j)) 
      warning("j index ignored")
    if (is.character(i)) 
      i <- match(i, row.names(x))
    else if (is(i, "Spatial")) 
      i = !is.na(over(x, geometry(i)))
    if (any(is.na(i))) 
      stop("NAs not permitted in row index")
    sp = as(x,"SpatialPoints")[i, , drop=drop]
    SpatialPointsMeteorology(sp, x@data[i], x@dates)
  }
)

setMethod("[", signature("SpatialPointsTopography"),definition =
    function (x, i, j, ..., drop = TRUE) 
    {
      missing.i = missing(i)
      if (!missing(j)) 
        warning("j index ignored")
      if (missing.i) i = TRUE
      if (is.matrix(i)) 
        stop("matrix argument not supported in SpatialPointsDataFrame selection")
      if (is(i, "Spatial")) 
        i = !is.na(over(x, geometry(i)))
      if (is.character(i)) 
        i <- match(i, row.names(x))
      if (any(is.na(i))) 
        stop("NAs not permitted in row index")
      sp = as(x,"SpatialPoints")[i, , drop=drop]
      x@coords = sp@coords
      x@bbox = sp@bbox
      x@data = x@data[i, , ..., drop = FALSE]
      x
    }
)