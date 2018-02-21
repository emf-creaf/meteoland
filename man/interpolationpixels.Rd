\encoding{UTF-8}
\name{interpolationpixels}
\alias{interpolationpixels}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolates meteorological variables over a set of grid pixels.
}
\description{
Interpolates meteorological data over a set of grid pixels using objects of class \code{\link{MeteorologyInterpolationData-class}}.
}
\usage{
interpolationpixels(object, pixels, dates, export = FALSE,
                  exportDir=getwd(), exportFormat = "netCDF",
                  metadatafile = "MG.txt", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{MeteorologyInterpolationData-class}}.}
  \item{pixels}{An object of class \code{\link{SpatialPixelsTopography-class}} representing the target landscape.}
  \item{dates}{An object of class \code{\link{Date}}.}
  \item{export}{
    If \code{export = FALSE} the result of interpolation is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).
  }
  \item{exportDir}{Output directory for interpolated meteorology.}
  \item{exportFormat}{Format of meteorological data. Current accepted format is \code{"netCDF"} only.}
  \item{metadatafile}{The name of the file that will store the meta data describing all written files.}
  \item{verbose}{Boolean flag to print process information.}
}
\value{
If \code{export = FALSE}, function \code{interpolationpixels} returns an object of \code{\link{SpatialPixelsMeteorology-class}}. If \code{export = TRUE}, the function writes the results in files and a \code{data.frame} with columns 'dir' and 'filename' is returned.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{penman}}, \code{\link{SpatialPixelsTopography}}, \code{\link{MeteorologyInterpolationData}}}

\examples{
data(examplegridtopography)
data(exampleinterpolationdata)

#Creates spatial topography pixels as a subset of points in the grid
points = as(examplegridtopography,"SpatialPoints")
cc = points@coords
center = 5160
d = sqrt((cc[,1]-cc[center,1])^2+(cc[,2]-cc[center,2])^2)
p = which(d<3000) #Take points at maximum distance of 4km from center
spt = SpatialPixelsTopography(points[p], examplegridtopography$elevation[p],
                              examplegridtopography$slope[p],
                              examplegridtopography$aspect[p],
                              grid = examplegridtopography@grid)

#Interpolation of meteorology over a pixels for two days
spt@proj4string = exampleinterpolationdata@proj4string
ml = interpolationpixels(exampleinterpolationdata, spt,
                       as.Date(c("2001-02-03", "2001-06-03")))
                       
#Plot PET corresponding to 2001-06-03
spplot(ml,2,"PET")
}
