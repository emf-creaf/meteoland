#' Assign Markov States from Precipitation and State Thresholds
#' 
#' Assign Markov States from Precipitation and State Thresholds
#' 
#' 
#' @param x vector of daily precipitation
#' @param months vector of months corresponding to daily precipitation
#' @param thresholds list of monthly transition matrices generated from
#' mc_fit()
#' @param states character list of markov states