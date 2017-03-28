\name{subsample}
\docType{methods}
\alias{subsample}
\alias{subsample-methods}
\alias{subsample,MeteorologyUncorrectedData-method}
\alias{subsample,MeteorologyInterpolationData-method}
\title{Sub-sampling procedure data}
\description{
 Generates a spatial and/or temporal subset of procedure data
}
\usage{
\S4method{subsample}{MeteorologyUncorrectedData}(object, bbox = NULL, stations = NULL, dates = NULL, buffer = 0)
\S4method{subsample}{MeteorologyInterpolationData}(object, bbox = NULL, stations = NULL, dates = NULL, buffer = 0)
}
\arguments{
\item{object}{An object of a sub-class \code{\link{MeteorologyProcedureData-class}}.}
\item{bbox}{A 2x2 numeric matrix with the boundaries of the target area. If \code{NULL}, the original boundary box is kept (except if \code{stations} is specified).}
\item{stations}{A numeric, character or logical vector specifying a subset of weather stations. If \code{NULL} all original weather stations are kept (except if \code{bbox} is specified).}
\item{dates}{A vector of \code{\link{Date}} with the subset of dates of interest. If \code{NULL}, all dates are kept.}
\item{buffer}{A buffer to put around bbox for the spatial selection of data.}
}
\value{An object of the same class as \code{object}.}
\section{Methods}{
\describe{
    \item{subsample}{\code{signature(object = "MeteorologyUncorrectedData")}: Generates a \code{\link{MeteorologyUncorrectedData}} object for a smaller area and a subset of dates. }

    \item{subsample}{\code{signature(object = "MeteorologyInterpolationData")}: Generates a \code{\link{MeteorologyInterpolationData}} object for a smaller area and a subset of dates. }
	 }
}
\examples{
data(exampleinterpolationdata)

oridates = exampleinterpolationdata@dates

#Interpolation data using the first ten dates (same boundary box)
subdata = subsample(exampleinterpolationdata, dates = oridates[1:10])
}
\keyword{methods}
