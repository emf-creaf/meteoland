#' Merges meteorological data
#' 
#' Merges point or gridded meteorological data into a single object
#' 
#' Function \code{mergepoints} requires all coordinate reference systems to the
#' same. The function allows merging data from the same points (i.e. if they
#' have the same coordinates). Function \code{mergegrids} pools dates and
#' variables, but spatial structures (i.e. grid topology, pixel indices,
#' reference system, ...) should be the same for all objects to be merged.
#' 
#' @aliases mergegrids mergepoints
#' @param ... Objects to be merged, either of class
#' \code{SpatialPointsMeteorology}, \code{SpatialGridMeteorology} or
#' \code{SpatialPixelsMeteorology}. All objects of the same class.
#' @param verbose A logical flag to indicate console output.
#' @return Function codemergepoints returns an object
#' \code{\link{SpatialPointsMeteorology-class}}. Function \code{mergegrids}
#' returns an object \code{\link{SpatialGridMeteorology-class}} or an object
#' \code{\link{SpatialPixelsMeteorology-class}}, depending on the input.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}},
#' \code{\link{SpatialGridMeteorology-class}},
#' \code{\link{SpatialPixelsMeteorology-class}}