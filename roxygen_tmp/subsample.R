#' Sub-sampling procedure data
#' 
#' Generates a spatial and/or temporal subset of procedure data
#' 
#' 
#' @name subsample
#' @aliases subsample subsample-methods
#' subsample,MeteorologyUncorrectedData-method
#' subsample,MeteorologyInterpolationData-method
#' @docType methods
#' @param object An object of a sub-class
#' \code{\link{MeteorologyProcedureData-class}}.
#' @param bbox A 2x2 numeric matrix with the boundaries of the target area. If
#' \code{NULL}, the original boundary box is kept (except if \code{stations} is
#' specified).
#' @param stations A numeric, character or logical vector specifying a subset
#' of weather stations. If \code{NULL} all original weather stations are kept
#' (except if \code{bbox} is specified).
#' @param dates A vector of \code{\link{Date}} with the subset of dates of
#' interest. If \code{NULL}, all dates are kept.
#' @param buffer A buffer to put around bbox for the spatial selection of data.
#' @return An object of the same class as \code{object}.
#' @section Methods: \describe{ \item{subsample}{\code{signature(object =
#' "MeteorologyUncorrectedData")}: Generates a
#' \code{\link{MeteorologyUncorrectedData}} object for a smaller area and a
#' subset of dates. }
#' 
#' \item{subsample}{\code{signature(object = "MeteorologyInterpolationData")}:
#' Generates a \code{\link{MeteorologyInterpolationData}} object for a smaller
#' area and a subset of dates. } }
#' @keywords methods
#' @examples
#' 
#' data(exampleinterpolationdata)
#' 
#' oridates = exampleinterpolationdata@dates
#' 
#' #Interpolation data using the first ten dates (same boundary box)
#' subdata = subsample(exampleinterpolationdata, dates = oridates[1:10])
#' 