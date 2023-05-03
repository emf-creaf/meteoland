#' Creates a 'SpatialPointsTopography'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Function \code{SpatialPointsTopography} creates an object of class
#' \code{\link{SpatialPointsTopography-class}} containing topographic variables
#' for a set of points.
#'
#' @details
#' If either \code{slope = NULL} or \code{aspect = NULL} then when estimating
#' weather on the object locations radiation will be calculated assuming a flat
#' surface.
#'
#' @param points An object of class \code{SpatialPoints}.
#' @param elevation Elevation values (in m) of the points.
#' @param slope Slope values (in degrees) of the points.
#' @param aspect Aspect values (in degrees from North) of the points.
#' @param proj4string Object of class \code{CRS} in the first
#' form only used when points does not inherit from
#' \code{Spatial}.
#' @return Function \code{SpatialPointsTopography} returns an object
#' '\code{\link{SpatialPointsTopography-class}}'.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsTopography-class}}
#'
#' @export
SpatialPointsTopography<-function(points, elevation, slope = NULL, aspect = NULL, proj4string = as.character(NA)) {

  # deprecation notice
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "SpatialPointsTopography()", with = NULL,
    details = "Spatial_*_Topography classes are deprecated.
    User topography now can be provided as sf or stars objects"
  )

  # if(!(inherits(points,"SpatialPoints")|| inherits(points,"matrix"))) stop("'points' has to be of class 'matrix' or 'SpatialPoints'.")
  # if(inherits(points,"SpatialPoints")) {
  #   npoints = nrow(points@coords)
  #   proj4string =points@proj4string
  #   coords = points@coords
  #   bbox = points@bbox
  # } else {
  #   coords  = as.matrix(points)
  #   npoints = nrow(coords)
  #   bbox = bbox(SpatialPoints(coords))
  # }
  # if(length(elevation)!=npoints) stop("'elevation' has to be of length equal to the number of points")
  # if(is.null(slope))
  #   slope = as.numeric(rep(NA, length(elevation)))
  # else if(length(slope)!=npoints)
  #   stop("'slope' has to be of length equal to the number of points")
  # if(is.null(aspect))
  #   aspect = as.numeric(rep(NA, length(elevation)))
  # else if(length(aspect)!=npoints)
  #   stop("'aspect' has to be of length equal to the number of points")
  # data = data.frame(elevation = elevation, slope = slope, aspect = aspect,
  #                   row.names = row.names(coords))
  # lt = new("SpatialPointsTopography",
  #         coords = coords,
  #         bbox = bbox,
  #         proj4string = proj4string,
  #         data = data)
  # return(lt)
}

# #' @export
# setMethod("[", signature("SpatialPointsTopography"),definition =
#             function (x, i, j, ..., drop = TRUE)
#             {
#               missing.i = missing(i)
#               if (!missing(j))
#                 warning("j index ignored")
#               if (missing.i) i = TRUE
#               if (is.matrix(i))
#                 stop("matrix argument not supported in SpatialPointsTopography selection")
#               if (is(i, "Spatial"))
#                 i = !is.na(over(x, geometry(i)))
#               if (is.character(i))
#                 i <- match(i, row.names(x))
#               if (any(is.na(i)))
#                 stop("NAs not permitted in row index")
#               sp = as(x,"SpatialPoints")[i, , drop=drop]
#               x@coords = sp@coords
#               x@bbox = sp@bbox
#               x@data = x@data[i, , ..., drop = FALSE]
#               x
#             }
# )
#
# print.SpatialPointsTopography = function(x, ..., digits = getOption("digits")) {
#   cat("Object of class SpatialPointsTopography\n")
#   cc = substring(paste(as.data.frame(
#     t(signif(coordinates(x), digits)))),2,999)
#   df = data.frame("coordinates" = cc, x@data)
#   row.names(df) = row.names(x@data)
#   print(df, ..., digits = digits)
# }

# #' @export
# setMethod("print", "SpatialPointsTopography", function(x, ..., digits = getOption("digits")) print.SpatialPointsTopography(x, ..., digits))

# #' @export
# setMethod("show", "SpatialPointsTopography", function(object) print.SpatialPointsTopography(object))

# head.SpatialPointsTopography <- function(x, n=6L, ...) {
#   n <- min(n, length(x))
#   ix <- sign(n)*seq(abs(n))
#   x[ ix , , drop=FALSE]
# }
# setMethod("head", "SpatialPointsTopography", function(x, n=6L, ...) head.SpatialPointsTopography(x,n,...))
#
# tail.SpatialPointsTopography <- function(x, n=6L, ...) {
#   n <- min(n, length(x))
#   ix <- sign(n)*rev(seq(nrow(x), by=-1L, len=abs(n)))
#   x[ ix , , drop=FALSE]
# }

# #' @export
# setMethod("tail", "SpatialPointsTopography", function(x, n=6L, ...) tail.SpatialPointsTopography(x,n,...))


# #' @export
# setMethod("spTransform", signature("SpatialPointsTopography", "CRS"),
#       function(x, CRSobj, ...) {
#             sp = spTransform(as(x,"SpatialPoints"), CRSobj) # calls the rgdal methods
#             new("SpatialPointsTopography",
#                 coords = sp@coords,
#                 bbox = sp@bbox,
#                 proj4string = sp@proj4string,
#                 data = x@data)
#       }
# )
#
# as.SpPtsTop.SpPixTop = function(from) {
#   spdf = as(from, "SpatialPixelsDataFrame")
#   new("SpatialPixelsTopography",
#       coords = spdf@coords,
#       coords.nrs = spdf@coords.nrs,
#       bbox = spdf@bbox,
#       grid = spdf@grid,
#       grid.index = spdf@grid.index,
#       proj4string = spdf@proj4string,
#       data = spdf@data)
#
# }
# setAs("SpatialPointsTopography", "SpatialPixelsTopography", as.SpPtsTop.SpPixTop)
#
# as.SpPtsTop.SpGrdTop = function(from) {
#   as.SpPixTop.SpGrdTop(as.SpPtsTop.SpPixTop(from))
# }
# setAs("SpatialPointsTopography", "SpatialGridTopography", as.SpPtsTop.SpGrdTop)
