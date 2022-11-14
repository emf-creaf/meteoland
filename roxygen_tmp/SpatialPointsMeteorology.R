#' Creates a 'SpatialPointsMeteorology'
#' 
#' Initializes an object of class \code{SpatialPointsMeteorology-class}
#' 
#' There are two ways of building an object of of class
#' \code{\link{SpatialPointsMeteorology-class}}. The first way
#' (\code{dataByDate = FALSE}) is to supply as value for \code{data} a vector
#' of data frames with one data frame per spatial point, with dates as rows and
#' meteorological variables as columns. In this case all data frames must have
#' the same number of rows (dates) and columns (variables). The second way (if
#' \code{dataByDate = TRUE}) is to supply as value for \code{data} a vector of
#' data frames with one data frame per date, with points as rows and
#' meteorological variables as columns. In this case, the data frames may have
#' different rows and different columns. Only the information corresponding to
#' \code{points} will be taken and some variables may be missing.
#' 
#' @param points An object of class \code{\link{SpatialPoints-class}}. Row
#' names of point coordinates are used to identify points.
#' @param data A list of data frames. If \code{dataByDate = FALSE} the elements
#' of \code{data} are assumed to correspond to points. If \code{dataByDate =
#' TRUE} the elements of \code{data} are assumed to correspond to dates (see
#' 'Details').
#' @param dates Object of class \code{"Date"} describing the time period of
#' meteorological estimates.
#' @param dataByDate A flag to indicate that elements of \code{data} correspond
#' to dates, as opposed to the default (\code{dataByDate = FALSE}) which
#' assumes that elements correspond to points (see 'Details').
#' @return An object of class \code{\link{SpatialPointsMeteorology-class}}
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
