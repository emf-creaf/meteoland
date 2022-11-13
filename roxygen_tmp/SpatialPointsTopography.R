#' Creates a 'SpatialPointsTopography'
#' 
#' Function \code{SpatialPointsTopography} creates an object of class
#' \code{\link{SpatialPointsTopography-class}} containing topographic variables
#' for a set of points.
#' 
#' If either \code{slope = NULL} or \code{aspect = NULL} then when estimating
#' weather on the object locations radiation will be calculated assuming a flat
#' surface.
#' 
#' @param points An object of class \code{\link{SpatialPoints-class}}.
#' @param elevation Elevation values (in m) of the points.
#' @param slope Slope values (in degrees) of the points.
#' @param aspect Aspect values (in degrees from North) of the points.
#' @param proj4string Object of class \code{\linkS4class{CRS}} in the first
#' form only used when points does not inherit from
#' \code{\linkS4class{Spatial}}.
#' @return Function \code{SpatialPointsTopography} returns an object
#' '\code{\link{SpatialPointsTopography-class}}'.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsTopography-class}}
#' @examples
#' 
#' data(examplegridtopography)
#' 
#' #Creates spatial topography points from the grid
#' p = 1:2
#' points = as(examplegridtopography,"SpatialPoints")[p]
#' spt = SpatialPointsTopography(points, examplegridtopography$elevation[p],
#'                               examplegridtopography$slope[p],
#'                               examplegridtopography$aspect[p])
#' spt
#' 
#' #Alternatively, use coercing and subsetting
#' spt = as(examplegridtopography, "SpatialPointsTopography")[p]
#' spt
#' 