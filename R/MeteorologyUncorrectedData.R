#' Creates an object of class 'MeteorologyUncorrectedData'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Initializes an object for statistical correction of meteorological data over
#' landscapes.
#'
#' @details
#' See correction details in vignettes or in \code{\link{correctionpoints}}.
#'
#' @param points An object of class \code{\link{SpatialPoints}}.
#' @param reference_data Reference (historic) meteorological data used to
#' calibrate correction factors when compared with observations. A vector of
#' data frames (one per point) or a single data frame containing the meta data
#' (columns \code{dir} and \code{filename}) of meteorological files that will
#' be read from the disk. Alternatively, a NetCDF file name where points should
#' be read.
#' @param projection_data Projected meteorological data to be corrected. A
#' vector of data frames (one per point) or a single data frame containing the
#' meta data (columns \code{dir} and \code{filename}) of meteorological files
#' that will be read from the disk. Alternatively, a NetCDF file name where
#' points should be read.
#' @param dates Object of class \code{"Date"} describing the time period for
#' which meteorological correction is possible (corresponding to
#' \code{projection_data}).
#' @param params A \code{"list"} containing correction parameters.
#' @return An object of class \code{\linkS4class{MeteorologyUncorrectedData}.}
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\linkS4class{MeteorologyUncorrectedData}},
#' \code{\link{examplecorrectiondata}}, \code{\link{defaultCorrectionParams}}.
#' @export
MeteorologyUncorrectedData<-function(points, reference_data, projection_data, dates,
                                     params = defaultCorrectionParams()) {

  lifecycle::deprecate_warn(
    when = "2.0.0", what = "MeteorologyUncorrectedData()", with = NULL,
    details = "Correction methods and data classes are soft deprecated.
    Better bias correction methods are provided by other packages (see package `MBC` for example)"
  )

  if(!inherits(points, "SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'")
  if(!inherits(reference_data,"data.frame") && !inherits(reference_data,"list") && !inherits(reference_data,"character"))
    stop("'reference_data' has to be of class 'data.frame, 'character' or 'list'.")
  if(!inherits(projection_data,"data.frame") && !inherits(projection_data,"list") && !inherits(projection_data,"character"))
    stop("'projection_data' has to be of class 'data.frame', 'character' or 'list'.")
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  ndates = length(dates)
  spm = new("MeteorologyUncorrectedData",
            coords = points@coords,
            bbox = points@bbox,
            proj4string = points@proj4string,
            reference_data = reference_data,
            projection_data = projection_data,
            dates = dates,
            params = params)
  return(spm)
}
