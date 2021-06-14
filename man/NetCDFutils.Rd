\encoding{UTF-8}
\name{NetCDFutils}
\alias{readNetCDFdates}
\alias{readNetCDFpoints}
\alias{readNetCDFgridtopology}
\alias{readNetCDFproj4string}
\title{
Utility functions for NetCDFs
}
\description{
Functions to read spatial and temporal coordinates from NetCDFs.
}
\usage{
readNetCDFdates(file)
readNetCDFpoints(file)
readNetCDFgridtopology(file)
readNetCDFproj4string(file)
}
\arguments{
  \item{file}{String of the NetCDF whose spatial/temporal coordinates are desired.}
}

\value{
\itemize{
\item{Function \code{readNetCDFdates} returns a \code{\link{Date}} vector.}
\item{Function \code{readNetCDFpoints} returns an object \code{\link{SpatialPoints-class}}.}
\item{Function \code{readNetCDFgridtopology} returns an object \code{\link{GridTopology-class}}.}
\item{Function \code{readNetCDFproj4string} returns an object \code{\link{CRS-class}}.}
}
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{readmeteorologypoints}}, \code{\link{readmeteorologygrid}}
}
