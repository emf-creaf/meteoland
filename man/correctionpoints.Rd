\encoding{UTF-8}
\name{correctionpoints}
\alias{correctionpoints}
\title{
Statistical correction of meteorological variables for a set of points
}
\description{
This function performs correction of predicted climatic data by applying bias correction to all meteorological variables excepting precipitation, which is corrected using quantile mapping.
}
\usage{
correctionpoints(object, points, topodata = NULL, dates = NULL,
                  export = FALSE, exportDir = getwd(), exportFormat = "meteoland",
                  metadatafile = "MP.txt", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{MeteorologyUncorrectedData-class}}.}
  \item{points}{
    An object of class \code{\link{SpatialPointsMeteorology-class}} with the coordinates and historical meteorological data of the locations for which correction of predicted climatic data has to be done. Alternatively, an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk.
  }
  \item{topodata}{A data frame with topographic data for each point (i.e. three columns named \code{elevation}, \code{slope} and \code{aspect}). If \code{topodata = NULL} then Penman's potential evapotranspiration is not calculated.}
  \item{dates}{An object of class \code{\link{Date}}. If \code{dates = NULL} then all dates in \code{object} are processed.}
  \item{export}{
    If \code{export = FALSE} the result of correction is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).
  }
  \item{exportDir}{Output directory for downscaled meteorology.}
  \item{metadatafile}{The name of the file that will store the meta data describing all written files.}
  \item{exportFormat}{Format of meteorological data. Current accepted formats are \code{"castanea"} and \code{"meteoland"}.}
  \item{verbose}{Boolean flag to print process information.}
}
\details{
Function \code{correctionpoints} performs statistical correction of predicted climatic data for all points supplied in \code{points}. Observed meteorological data for each point typically comes from a nearby meteorological station, but they can be the result of interpolating the meteorology of several stations (see \code{\link{MeteorologyInterpolationData}}) or they can be extracted from reanalyzed meteorology (e.g. EU-WATCH) (see \code{\link{extractNetCDF}}).

For each target point, the function first determines the predicted cell where the point falls. Then it determines the dates that are shared in observed and predicted data for the historical period. These meteorological data of dates are used to conduct the correction of predicted climatic data for the future period. For temperature, wind speed and radiation, the mean bias calculated for each of the twelve months in the historical period is used to correct the corresponding month in the future period. Relative humidity is first transformed to specific humidity, then it is corrected, and finally back-transformed to relative humidity. In the case of precipitation correction is performed by applying a quantile mapping between observed and modelled data (Gudmundsson et al. 2012), also on a monthly basis.
}
\value{
If \code{export = FALSE}, the function returns an object of class \code{\link{SpatialPointsMeteorology-class}} with the downscaled meteorology for each point. Otherwise the function returns an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data of the files written in the disk.
}
\references{
Gudmundsson L, Bremnes JB, Haugen JE, Engen-Skaugen T (2012) Technical Note: Downscaling predicted climatic precipitation to the station scale using statistical transformations - A comparison of methods. Hydrology and Earth System Sciences 16, 3383–3390. doi:10.5194/hess-16-3383-2012.
}
\author{
Nicolas Martin, INRA-Avignon

Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}
\seealso{
\code{\link{penman}}, \code{\link{extractNetCDF}}, \code{\link{SpatialPointsMeteorology-class}}, \code{\link{writemeteorologypointfiles}}, \code{\link{MeteorologyUncorrectedData}}, \code{\link{MeteorologyInterpolationData}}
}
\examples{
data(examplegridtopography)
data(exampleinterpolationdata)
data(examplecorrectiondata)

#Creates spatial topography points from the grid
p = 1:2
points = as(examplegridtopography,"SpatialPoints")[p]
points = spTransform(points, exampleinterpolationdata@proj4string)
spt = SpatialPointsTopography(points, examplegridtopography$elevation[p],
                              examplegridtopography$slope[p],
                              examplegridtopography$aspect[p])

#Interpolation of two points for the whole time period (2000-2003)
historical = interpolationpoints(exampleinterpolationdata, spt)

#Downscaling of future predictions (RCM models, year 2023)
predicted = correctionpoints(examplecorrectiondata, historical, spt@data)

#Plot predicted mean temperature for point 1
meteoplot(predicted, 1, "MeanTemperature", ylab="Mean temperature (Celsius)", ylim=c(-5,40))
#Add uncorrected mean temperature data (cell #3)
lines(examplecorrectiondata@dates,
      examplecorrectiondata@projection_data[[3]]$MeanTemperature,
      col="red")
legend("topright", legend=c("corrected","uncorrected"), col=c("black","red"), lty=c(1,1), bty="n")

#Scatter plot
plot(examplecorrectiondata@projection_data[[3]]$MeanTemperature,
     predicted@data[[1]]$MeanTemperature, cex=0.1, asp=1,
     ylab="Corrected mean temperature", xlab="Uncorrected mean temperature")
abline(a=0,b=1,col="gray")

#Plot predicted precipitation for point 1
meteoplot(predicted, 1, "Precipitation", ylab="Precipitation (mm)", ylim=c(0,120))
#Add uncorrected mean temperature data (cell #3)
lines(examplecorrectiondata@dates,
      examplecorrectiondata@projection_data[[3]]$Precipitation,
      col="red", lty=3)
legend("topleft", legend=c("corrected","uncorrected"), col=c("black","red"), lty=c(1,3), bty="n")

#Scatter plot
plot(examplecorrectiondata@projection_data[[3]]$Precipitation,
     predicted@data[[1]]$Precipitation, cex=0.1, asp=1,
     ylab="Corrected precipitation (mm)", xlab="Uncorrected precipitation (mm)")
abline(a=0,b=1,col="gray")
}
