\encoding{UTF-8}
\name{humidity}
\alias{humidity_relative2specific}
\alias{humidity_specific2relative}
\alias{humidity_relative2dewtemperature}
\alias{humidity_dewtemperature2relative}

\title{
Humidity conversion tools
}
\description{
Functions to transform relative humidity to specific humidity or dew point temperature and viceversa.
}
\usage{
humidity_specific2relative(Tc, HS, allowSaturated = FALSE)
humidity_relative2specific(Tc, HR)
humidity_dewtemperature2relative(Tc, Td, allowSaturated = FALSE)
humidity_relative2dewtemperature(Tc, HR)
}
\arguments{
  \item{Tc}{A numeric vector of temperature in degrees Celsius.}
  \item{HS}{A numeric vector of specific humidity (unitless).}
  \item{HR}{A numeric vector of relative Humidity (in \%).}
  \item{Td}{A numeric vector of dew temperature in degrees Celsius.}
  \item{allowSaturated}{Logical flag to allow values over 100\%}
}
\value{
A numeric vector with specific or relative humidity.
}
\author{
Nicholas Martin-StPaul, INRA

Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{\code{\link{meteocomplete}}}
