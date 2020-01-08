\encoding{UTF-8}
\name{interpolationpoints}
\alias{interpolationpoints}
\alias{interpolationpixels}
\alias{interpolationgrid}

\title{
Interpolate daily meteorology over a landscape
}
\description{
Functions to interpolate meteorological data for spatial locations (at points, grid pixels or full grids) using an object of class \code{\link{MeteorologyInterpolationData-class}}.
}
\usage{
interpolationpoints(object, points, dates = NULL, export = FALSE,
                    exportDir = getwd(), exportFile = NULL, 
                    exportFormat = "meteoland/txt",
                    metadataFile = "MP.txt", verbose=TRUE)
interpolationpixels(object, pixels, dates = NULL, 
                    exportFile = NULL, exportFormat = "netCDF", 
                    add = FALSE, overwrite = FALSE, verbose=TRUE)
interpolationgrid(object, grid, dates = NULL, 
                  exportFile = NULL, exportFormat = "netCDF",
                  add = FALSE, overwrite = FALSE, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{MeteorologyInterpolationData-class}}.}
  \item{points}{An object of class \code{\link{SpatialPointsTopography-class}}.}
  \item{pixels}{An object of class \code{\link{SpatialPixelsTopography-class}} representing the target landscape.}
  \item{grid}{An object of class \code{\link{SpatialGridTopography-class}} representing the target landscape.}
  \item{dates}{An object of class \code{\link{Date}}. If this is \code{NULL} then all dates in \code{object} are processed.}
  \item{export}{
    If \code{export = FALSE} the result of interpolation is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).
  }
  \item{exportDir}{Output directory for interpolated meteorology data files (txt/rds format).}
  \item{exportFile}{Output file for interpolated meteorology data (netCDF format).}
  \item{exportFormat}{Export format for meteorological data (see \code{\link{writemeteorologypoint}}).  If format is \code{"meteoland/txt"},  \code{"meteoland/rds"}, \code{"castanea/txt"} or \code{"castanea/rds"} the function tries to write one file per point in \code{exportDir}. If format is \code{"netCDF"} the function will write data to a single file specified by \code{exportFile}.}
  \item{metadataFile}{The name of the ascii text file that will store the meta data describing all written files.}
  \item{verbose}{Boolean flag to print process information.}
  \item{add}{Boolean flag to indicate that NetCDF exists and data should be added/replaced.}
  \item{overwrite}{Boolean flag to force overwriting an existing NetCDF.}
}
\value{
If \code{export = FALSE}, function \code{interpolationpoints} returns an object of \code{\link{SpatialPointsMeteorology-class}}. If \code{export = TRUE} files and written in the disk. For text/rds format the function returns an object of class \code{\link{SpatialPointsDataFrame-class}} containing point meta data.

If \code{export = FALSE}, function \code{interpolationpixels} returns an object of \code{\link{SpatialPixelsMeteorology-class}}, or an object of \code{\link{SpatialPixelsDataFrame-class}} if a single date is interpolated. If \code{export = TRUE}, the function writes the results in a NetCDF.

If \code{export = FALSE}, function \code{interpolationgrid} returns an object of \code{\link{SpatialGridMeteorology-class}}, or an object of \code{\link{SpatialGridDataFrame-class}} if a single date is interpolated. If \code{export = TRUE}, the function writes the results in files and a \code{data.frame} with columns 'dir' and 'filename' is returned.
}
\details{
If CRS projection is different between \code{object} and \code{points}/\code{pixels}/\code{grid}, the function transforms the coordinates of \code{points}/\code{pixels}/\code{grid} to adapt them to the CRS of \code{object}.
}
\references{
Thornton, P.E., Running, S.W., White, M. A., 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.

De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa,  CTFC
}
\seealso{\code{\link{penman}}, \code{\link{SpatialPointsTopography-class}}, \code{\link{SpatialGridTopography}}, \code{\link{SpatialPixelsTopography}}, \code{\link{MeteorologyInterpolationData}}}

\examples{
data(examplegridtopography)
data(exampleinterpolationdata)

####### INTERPOLATION on particular POINTS 

#Creates spatial topography points from the grid
p = 1:2
spt = as(examplegridtopography, "SpatialPointsTopography")[p]

#Interpolation of two points for the whole time period (2000-2003)
mp = interpolationpoints(exampleinterpolationdata, spt)

#Plot interpolated meteorological series
meteoplot(mp,1, ylab="Mean temperature")

####### INTERPOLATION on PIXELS 
# Creates spatial topography pixels as a subset of grid pixels
# and select pixels at maximum distance of 2km from center
spt = as(examplegridtopography,"SpatialPointsTopography")
cc = spt@coords
center = 5160
d = sqrt((cc[,1]-cc[center,1])^2+(cc[,2]-cc[center,2])^2)
spxt = as(spt[which(d<2000)], "SpatialPixelsTopography") 

# Interpolation of meteorology over pixels for two days
ml = interpolationpixels(exampleinterpolationdata, spxt,
                       as.Date(c("2001-02-03", "2001-06-03")))
                       
#Plot PET corresponding to 2001-06-03
spplot(ml,2,"PET")

####### INTERPOLATION over a complete GRID 
#Interpolation of meteorology over a grid for two days
ml = interpolationgrid(exampleinterpolationdata, examplegridtopography,
                       as.Date(c("2001-02-03", "2001-06-03")))
#Plot PET corresponding to 2001-06-03
spplot(ml,2,"PET")
}
