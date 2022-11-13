#' Creates an object of class 'MeteorologyUncorrectedData'
#' 
#' Initializes an object for statistical correction of meteorological data over
#' landscapes.
#' 
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
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\linkS4class{MeteorologyUncorrectedData}},
#' \code{\link{examplecorrectiondata}}, \code{\link{defaultCorrectionParams}}.