#' Humidity conversion tools
#' 
#' Functions to transform relative humidity to specific humidity or dew point
#' temperature and viceversa.
#' 
#' 
#' @aliases humidity_relative2specific humidity_specific2relative
#' humidity_relative2dewtemperature humidity_dewtemperature2relative
#' @param Tc A numeric vector of temperature in degrees Celsius.
#' @param HS A numeric vector of specific humidity (unitless).
#' @param HR A numeric vector of relative Humidity (in \%).
#' @param Td A numeric vector of dew temperature in degrees Celsius.
#' @param allowSaturated Logical flag to allow values over 100\%
#' @return A numeric vector with specific or relative humidity.
#' @author Nicholas Martin-StPaul, INRA
#' 
#' Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{meteocomplete}}