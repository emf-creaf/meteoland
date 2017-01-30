\encoding{UTF-8}
\name{correctiongrid}
\alias{correctiongrid}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistical correction of meteorological variables over a grid
}
\description{
Performs statistical correction of meteorological data over grids using objects of class \code{\link{MeteorologyUncorrectedData-class}}, by applying bias correction to all meteorological variables excepting precipitation, which is  corrected using quantile mapping.
}
\usage{
correctiongrid(object, gridfiles, topodata = NULL, dates = NULL,
                maxreadcells = 50, export = FALSE,
                exportDir=getwd(), exportFormat = "netCDF",
                metadatafile = "MG.txt", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{MeteorologyUncorrectedData-class}}.}
  \item{gridfiles}{Either a vector of filename strings or a data frame with two columns: 'dir' and 'filename, to indicate the path to the meteorological data over a grid that will be used to calibrate corrections.}
  \item{topodata}{A data frame with topographic data for each grid cell (i.e. three columns named \code{elevation}, \code{slope} and \code{aspect}). If \code{topodata = NULL} then Penman's potential evapotranspiration is not calculated.}
  \item{dates}{An object of class \code{\link{Date}}. If \code{dates = NULL} then all dates in \code{object} are processed.}
  \item{maxreadcells}{Maximum number of cells to be read simultaneously when calculating cell biases for the reference period. Reading historical meteorology for the whole grid simultaneously can cause memory problems.}
  \item{export}{
    If \code{export = FALSE} the result of interpolation is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).
  }
  \item{exportDir}{Output directory for downscaled meteorology data.}
  \item{exportFormat}{Format of meteorological data. Current accepted format is \code{"netCDF"} only.}
  \item{metadatafile}{The name of the file that will store the meta data describing all written files.}
  \item{verbose}{Boolean flag to print process information.}
}
\details{
The function first determines the predicted coarse-scale cell where each target grid cell falls. Then, the function calculates the bias for each cell between the meteorological data in \code{gridfiles} and the historic (reference) meteorological data in the corresponding coarse-scale cell of \code{object}. Finally, it uses this biases for the correction of future (climatic) data in \code{object}. For temperature, wind speed and radiation, the mean bias calculated for each of the twelve months in the historical period is used to correct the corresponding month in the future period. Relative humidity is first transformed to specific humidity, then it is corrected, and finally back-transformed to relative humidity. In the case of precipitation correction is performed by applying a quantile mapping between observed and modelled data (Gudmundsson et al. 2012), also on a monthly basis.
}
\value{
If \code{export = FALSE}, function \code{correctiongrid} returns an object of \code{\link{SpatialGridMeteorology-class}}. If \code{export = TRUE}, the function writes the results in files and a \code{data.frame} with columns 'dir' and 'filename' is returned.
}
\references{
Gudmundsson L, Bremnes JB, Haugen JE, Engen-Skaugen T (2012) Technical Note: Downscaling predicted climatic precipitation to the station scale using statistical transformations - A comparison of methods. Hydrology and Earth System Sciences 16, 3383–3390. doi:10.5194/hess-16-3383-2012.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{penman}}, \code{\link{SpatialGridMeteorology-class}}, \code{\link{MeteorologyUncorrectedData}}}

\examples{
## TO BE DONE ##
}
