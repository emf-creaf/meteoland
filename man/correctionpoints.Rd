\encoding{UTF-8}
\name{correctionpoints}
\alias{correctionpoint}
\alias{correctionpoints}
\alias{correctionpoints.errors}
\title{
Statistical correction of meteorological variables for a set of points
}
\description{
Functions \code{correctionpoint} and \code{correctionpoints} perform correction of predicted climatic data by applying statistical correction methods (unbiasing, scaling, or quantile mapping) to meteorological variables. Function \code{correctionpoints.errors} allows evaluating, for each point, the bias and mean absolute error (MAE) obtained before and after correcting the climate model for the historical period. 
}
\usage{
correctionpoint(obs, mod, proj, dates = NULL, 
                params = defaultCorrectionParams(), verbose=TRUE)
correctionpoints(object, points, topodata = NULL, dates = NULL,
                 export = FALSE, exportDir = getwd(), exportFile = NULL,
                 exportFormat = "meteoland/txt", metadataFile = "MP.txt", 
                 corrOut = FALSE, verbose = TRUE)
correctionpoints.errors(object, points, topodata = NULL, 
                  error.type="residuals.cv",keep.data = FALSE, verbose = FALSE)
  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A data frame with observed meteorology.}
  \item{mod, proj}{Data frame with predicted meteorology for the reference and projection periods, respectively.}
  \item{params}{A list with correction params (see \code{\link{defaultCorrectionParams}}).}
  \item{object}{An object of class \code{\link{MeteorologyUncorrectedData-class}} containing the meteorology of more than one point.}
  \item{points}{
    An object of class \code{\link{SpatialPointsMeteorology-class}} with the coordinates and historical meteorological data of the locations for which correction of predicted climatic data has to be done. Alternatively, an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data (columns \code{dir},  \code{filename} and possibly \code{format}) of meteorological files that will be read from the disk.
  }
  \item{topodata}{A data frame with topographic data for each point (i.e. three columns named \code{elevation}, \code{slope} and \code{aspect}). If \code{topodata = NULL} then Penman's potential evapotranspiration is not calculated.}
  \item{dates}{An object of class \code{\link{Date}} with a subset of dates of the projection period to be corrected. If \code{dates = NULL} then all dates in \code{proj} or the projection data of \code{object} are processed.}
  \item{export}{
    If \code{export = FALSE} the result of correction is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).
  }
  \item{exportDir}{Output directory for corrected meteorology data (txt/rds format).}
  \item{exportFile}{Output file for corrected meteorology data (netCDF format).}
  \item{metadataFile}{The name of the file that will store the meta data describing all written files.}
  \item{exportFormat}{Export format for meteorological data (see \code{\link{writemeteorologypoint}}).  If format is \code{"meteoland/txt"},  \code{"meteoland/rds"}, \code{"castanea/txt"} or \code{"castanea/rds"} the function tries to write one file per point in \code{exportDir}. If format is \code{"netCDF"} the function will write data to a single file specified by \code{exportFile}.}
  \item{corrOut}{Boolean flag to indicate that correction parameters (i.e. calculated biases) should be included with the output. Setting \code{corrOut = TRUE} changes the returned value.}
  \item{verbose}{Boolean flag to print process information.}
  \item{error.type}{String to specify the error to be evaluated, either \code{"before"} (before correction), \code{"residual"} (after correction) or \code{"residual.cv"} (after correction, but using cross-validation).}
  \item{keep.data}{Boolean flag to return the uncorrected/corrected data for the historical period.}
}
\details{
Function \code{correctionpoints} performs statistical correction of predicted climatic data for all points supplied in \code{points} whereas \code{correctionpoint} performs statistical correction of one single point. Observed meteorological data for each point typically comes from a nearby meteorological station, but they can be the result of interpolating the meteorology of several stations (see \code{\link{MeteorologyInterpolationData}}) or they can be extracted from reanalyzed meteorology (e.g. EU-WATCH) (see \code{\link{extractNetCDF}}).

For each target point, \code{correctionpoints} function first determines the predicted cell where the point falls according to the euclidean distance in the geographic space of \code{object}. Then it calls \code{correctionpoint}. In turn, \code{correctionpoint} determines the dates that are shared in observed and predicted data for the historical period. These meteorological data of dates are used to conduct the correction of predicted climatic data for the future period. Corrections biases are calculated and applied for the twelve months separately. The user can control the methods used for correction of each meteorological variable by changing the slot \code{params} in \code{object} (see class \code{\link{MeteorologyUncorrectedData-class}}) or the parameter \code{params} to \code{correctionpoint}. Three options are allowed (see \code{\link{defaultCorrectionParams}}): (a) 'unbias' for shifting the mean; (b) 'scaling' for multiplication by a factor; and (c) 'quantmap' for empirical quantile mapping between observed and modelled data (\enc{Déqué}{Deque} 2007).

A difficulty arises for quantile mapping when the variables bounded by zero, such as precipitation. As the models tend to drizzle (or may have lower frequency of precipitation events), the probability of precipitation in the model may be greater or lower than that observed. To correct this, when model precipitation is zero an observed value is randomly chosen in the interval where the observed cumulative frequency is less than or equal to the probability of no precipitation in the model. This procedure ensures that the probability of precipitation after correction is equal to that observed (\enc{Boé}{Boe} 2007).
}
\value{
\itemize{
\item{Function \code{correctionpoint} returns a data frame.}
\item{If \code{export = FALSE}, the function \code{correctionpoints} returns an object of class \code{\link{SpatialPointsMeteorology-class}} with the bias-corrected meteorology for each point. If \code{export=TRUE} then bias-corrected data is written into the disk. For txt/rds export formats, the function returns an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data of the files written in the disk. For netCDF export format the function returns \code{NULL}. If \code{corrOut = TRUE} the function returns a list which contains any previous output and an object with the calculated correction factors (biases, mappings) for each point and month.}
\item{Function \code{correctionpoints.errors} (\code{keep.data = FALSE}) returns a data frame with the mean absolute error (MAE) and bias for each variable and point. If \code{keep.data = TRUE} then the function also returns a list of data frames with the uncorrected/corrected series used in the comparisons with observations.}
}
}
\references{
\enc{Boé}{Boe} J, Terray L, Habets F, Martin E (2007) Statistical and dynamical downscaling of the Seine basin climate for hydro-meteorological studies. Int J Climatol 27:1643–1655. doi: 10.1002/joc.1602

De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.

\enc{Déqué}{Deque} M (2007) Frequency of precipitation and temperature extremes over France in an anthropogenic scenario: Model results and statistical correction according to observed values. Glob Planet Change 57:16–26. doi: 10.1016/j.gloplacha.2006.11.030
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC

Nicolas Martin, INRA-Avignon
}
\seealso{
\code{\link{penman}}, \code{\link{SpatialPointsMeteorology-class}}, \code{\link{writemeteorologypointfiles}}, \code{\link{MeteorologyUncorrectedData}}, \code{\link{MeteorologyInterpolationData}}
}
\examples{
data(examplegridtopography)
data(exampleinterpolationdata)
data(examplecorrectiondata)

#Creates spatial topography points from the grid
p = 1:2
spt = as(examplegridtopography, "SpatialPointsTopography")[p]


#Interpolation of two points for the whole time period (2000-2003)
historical = interpolationpoints(exampleinterpolationdata, spt)

#Downscaling of future predictions (RCM models, year 2023)
predicted = correctionpoints(examplecorrectiondata, historical, spt@data)

#Plot predicted mean temperature for point 1
meteoplot(predicted, 1, "MeanTemperature", ylab="Temperature (Celsius)", ylim=c(-5,40))
meteoplot(predicted, 1, "MinTemperature", add=TRUE, col="blue")
meteoplot(predicted, 1, "MaxTemperature", add=TRUE, col="red")
#Add uncorrected mean temperature data (cell #3)
lines(examplecorrectiondata@dates,
      examplecorrectiondata@projection_data[[3]]$MeanTemperature,
      lty=3)
lines(examplecorrectiondata@dates,
      examplecorrectiondata@projection_data[[3]]$MinTemperature,
      col="blue", lty=3)
lines(examplecorrectiondata@dates,
      examplecorrectiondata@projection_data[[3]]$MaxTemperature,
      col="red", lty=3)
legend("topright", legend=c("corrected","uncorrected", "Maximum", "Mean", "Minimum"), 
       col=c("black","black", "red","black","blue"), lty=c(1,3,1,1,1), bty="n")

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
