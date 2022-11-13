#' Determine Markov State Thresholds from Precipitation
#' 
#' Determine Markov State Thresholds from Precipitation
#' 
#' 
#' @param x vector of daily precipitation
#' @param months vector of months corresponding to daily precipitation vector
#' \code{x}
#' @param dry_wet_threshold threshold precipitation amount for dry/wet states
#' @param wet_extreme_quantile_threshold threshold quantile for wet/extreme
#' states
#' @return monthly list of precipitation thresholds as vectors of length 2
#' (first element is threshold amount between dry/wet, second element is
#' threshold amount between wet/extreme)