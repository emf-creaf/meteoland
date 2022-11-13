#' Spatial grid plots
#' 
#' Function \code{spplot} for \code{\link{SpatialGridTopography-class}} and
#' \code{\link{SpatialPixelsTopography-class}} objects allows drawing maps of
#' topographic attributes. Function \code{spplot} for
#' \code{\link{SpatialGridMeteorology-class}} and
#' \code{\link{SpatialPixelsMeteorology-class}} objects allows drawing maps of
#' meteorological variables corresponding to specific dates.
#' 
#' 
#' @aliases spplot,SpatialGridTopography-method
#' spplot,SpatialGridMeteorology-method spplot,SpatialPixelsTopography-method
#' spplot,SpatialPixelsMeteorology-method
#' @param obj An object of class \code{SpatialGridTopography}.
#' @param variable A string of the variable to be plotted (only
#' \code{type="elevation"}, \code{type="slope"}, \code{type="aspect"} are
#' allowed).
#' @param ... Additional parameters to function \code{\link{spplot}}.
#' @param date A string or an integer for the date to be plotted.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{meteoplot}}
#' @examples
#' 
#' data(examplegridtopography)
#' 
#' #Display data
#' spplot(examplegridtopography, type="elevation", scales=list(draw=TRUE))
#' spplot(examplegridtopography, type="slope", scales=list(draw=TRUE))
#' spplot(examplegridtopography, type="aspect", scales=list(draw=TRUE))
#' 