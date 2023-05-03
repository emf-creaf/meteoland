#' Class \code{"SpatialPointsMeteorology"}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An S4 class that represents a set of points with meteorology data series.
#'
#'
#' @name SpatialPointsMeteorology-class
# #' @aliases SpatialPointsMeteorology-class
# #' [,SpatialPointsMeteorology-method
# #' [,SpatialPointsMeteorology,ANY,ANY-method
# #' [,SpatialPointsMeteorology,ANY,ANY,ANY-method
# #' show,SpatialPointsMeteorology-method
# #' head,SpatialPointsMeteorology-method
# #' print,SpatialPointsMeteorology-method
# #' tail,SpatialPointsMeteorology-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPointsMeteorology", ...)}, or by calls to the function
#' \code{\link{SpatialPointsMeteorology}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsTopography-class}},
#' \code{SpatialPoints}
#' @keywords classes
#' @examples
#'
#' #Structure of the S4 object
#' showClass("SpatialPointsMeteorology")
#'
#' @export
setClass("SpatialPointsMeteorology", slots = list(dates = "Date", data="vector"), contains= character())

#' Class \code{"SpatialPointsTopography"}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An S4 class that represents topography over a grid of coordinates.
#'
#'
#' @name SpatialPointsTopography-class
# #' @aliases SpatialPointsTopography-class
# #' [,SpatialPointsTopography-method
# #' [,SpatialPointsTopography,ANY,ANY-method
# #' [,SpatialPointsTopography,ANY,ANY,ANY-method
# #' show,SpatialPointsTopography-method
# #' head,SpatialPointsTopography-method
# #' print,SpatialPointsTopography-method
# #' spTransform,SpatialPointsTopography,CRS-method
# #' tail,SpatialPointsTopography-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPointsTopography", ...)}, or by calls to the function
#' \code{\link{SpatialPointsTopography}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsTopography}},
#' \code{SpatialPointsDataFrame}
#' @keywords classes
#' @examples
#'
#' #Structure of the S4 object
#' showClass("SpatialPointsTopography")
#'
#' @export
setClass("SpatialPointsTopography", contains=character())

#' Class \code{"SpatialGridMeteorology"}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An S4 class that represents a spatial grid with meteorology daily data.
#'
#'
#' @name SpatialGridMeteorology-class
# #' @aliases SpatialGridMeteorology-class
# #' [,SpatialGridMeteorology-method
# # # #' [,SpatialGridMeteorology,ANY,ANY-method
# #' [,SpatialGridMeteorology,ANY,ANY,ANY-method
# #' show,SpatialGridMeteorology-method
# #' print,SpatialGridMeteorology-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialGridMeteorology", ...)}, or by calls to the function
#' \code{\link{SpatialGridMeteorology}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialGridTopography}},
#' \code{SpatialGridDataFrame}
#' @keywords classes
#' @examples
#'
#' #Structure of the S4 object
#' showClass("SpatialGridMeteorology")
#'
#' @export
setClass("SpatialGridMeteorology", slots = list(dates = "Date", data="vector"), contains=character())

#' Class \code{"SpatialGridTopography"}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An S4 class that represents topography over a grid of coordinates.
#'
#'
#' @name SpatialGridTopography-class
# #' @aliases SpatialGridTopography-class
# # # #' [,SpatialGridTopography-method
# #' [,SpatialGridTopography,ANY,ANY-method
# #' [,SpatialGridTopography,ANY,ANY,ANY-method
# #' show,SpatialGridTopography-method
# #' print,SpatialGridTopography-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialGridTopography", ...)}, or by calls to the function
#' \code{\link{SpatialGridTopography}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialGridTopography}},
#' \code{SpatialGridDataFrame}
#' @keywords classes
#' @examples
#'
#' #Structure of the S4 object
#' showClass("SpatialGridTopography")
#'
#' @export
setClass("SpatialGridTopography", contains=character())

#' Class \code{"SpatialPixelsMeteorology"}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An S4 class that represents meteorology data that has locations on a regular
#' grid.
#'
#'
#' @name SpatialPixelsMeteorology-class
# #' @aliases SpatialPixelsMeteorology-class
# #' [,SpatialPixelsMeteorology-method
# #' [,SpatialPixelsMeteorology,ANY,ANY-method
# #' [,SpatialPixelsMeteorology,ANY,ANY,ANY-method
# #' show,SpatialPixelsMeteorology-method
# #' print,SpatialPixelsMeteorology-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPixelsMeteorology", ...)}, or by calls to the function
#' \code{\link{SpatialPixelsMeteorology}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsTopography}},
#' \code{SpatialPixelsDataFrame}
#' @keywords classes
#' @examples
#'
#' #Structure of the S4 object
#' showClass("SpatialPixelsMeteorology")
#'
#' @export
setClass("SpatialPixelsMeteorology", slots = list(dates = "Date", data="vector"), contains=character())

#' Class \code{"SpatialPixelsTopography"}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An S4 class that represents topography that has locations on a regular grid.
#'
#'
#' @name SpatialPixelsTopography-class
# #' @aliases SpatialPixelsTopography-class
# #' [,SpatialPixelsTopography-method
# #' [,SpatialPixelsTopography,ANY,ANY-method
# #' [,SpatialPixelsTopography,ANY,ANY,ANY-method
# #' show,SpatialPixelsTopography-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPixelsTopography", ...)}, or by calls to the function
#' \code{\link{SpatialPixelsTopography}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsTopography}},
#' \code{SpatialPixelsDataFrame}
#' @keywords classes
#' @examples
#'
#' #Structure of the S4 object
#' showClass("SpatialPixelsTopography")
#'
#' @export
setClass("SpatialPixelsTopography", contains=character())
