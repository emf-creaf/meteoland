#' Creates a 'SpatialPixelsTopography'
#' 
#' Function \code{SpatialPixelsTopography} creates an object of class
#' \code{\link{SpatialPixelsTopography-class}} containing topographic variables
#' for a set of points.
#' 
#' 
#' @param points An object of class \code{\link{SpatialPoints-class}} or a
#' numeric matrix of coordinates.
#' @param elevation Elevation values (in m) of the points.
#' @param slope Slope values (in degrees) of the points.
#' @param aspect Aspect values (in degrees from North) of the points.
#' @param tolerance Precision up to which extent points should be exactly on a
#' grid.
#' @param proj4string Object of class \code{\linkS4class{CRS}} in the first
#' form only used when points does not inherit from
#' \code{\linkS4class{Spatial}}.
#' @param round default \code{NULL}, otherwise a value passed to as the digits
#' argument to \code{\link{round}} for setting cell size.
#' @param grid Grid topology using an object of class
#' \code{\linkS4class{GridTopology}}; a value of \code{NULL} implies that this
#' will be derived from the point coordinates.
#' @return Function \code{SpatialPixelsTopography} returns an object
#' '\code{\link{SpatialPixelsTopography-class}}'.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsTopography-class}}
#' @examples
#' 
#' data(examplegridtopography)
#' 
#' #Creates spatial topography pixels as a subset of points in the grid
#' spt = as(examplegridtopography,"SpatialPointsTopography")
#' cc = spt@coords
#' center = 5160
#' d = sqrt((cc[,1]-cc[center,1])^2+(cc[,2]-cc[center,2])^2)
#' p = which(d<3000) #Select points at maximum distance of 3km from center
#' spxt = SpatialPixelsTopography(spt[p], spt$elevation[p],
#'                               spt$slope[p],
#'                               spt$aspect[p])
#' 
#' #Alternatively, use coercing and subsetting (drop = TRUE causes grid to be recalculated) 
#' spxt = as(examplegridtopography, "SpatialPixelsTopography")[p, drop=TRUE]          
#'                               
#' #Display data
#' spplot(spxt, variable="elevation", scales=list(draw=TRUE))
#' spplot(spxt, variable="slope", scales=list(draw=TRUE))
#' spplot(spxt, variable="aspect", scales=list(draw=TRUE))
#' 