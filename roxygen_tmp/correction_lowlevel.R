#' Low-level correction functions
#' 
#' Low-level function to perform bias correction.
#' 
#' 
#' @aliases correction_series
#' @param obs Observed series for the reference (historical) period.
#' @param mod Modelled series for the reference (historical) period.
#' @param proj Modelled series for the projected period. If missing, the
#' reference (historical) period is corrected.
#' @param method Correction method, either \code{"unbias"}, \code{"scaling"},
#' \code{"quantmap"}
#' @param isPrec A flag to indicate that variable is precipitation (only
#' relevant for quantile mapping).
#' @param qstep Probability step for quantile mapping (see
#' \code{\link{defaultCorrectionParams}}).
#' @return Returns a vector with corrected values.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{correctionpoints}},
#' \code{\link{defaultCorrectionParams}}
#' @references De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018)
#' Estimating daily meteorological data and downscaling climate models over
#' landscapes. Environmental Modelling and Software 108: 186-196.
