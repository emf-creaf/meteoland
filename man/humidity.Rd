\encoding{UTF-8}
\name{humidity}
\alias{humidity_relative2specific}
\alias{humidity_specific2relative}

\title{
Humidity conversion tools
}
\description{
Functions to transform specific humidity to relative humidity and viceversa.
}
\usage{
humidity_specific2relative(Tc, HS, allowSaturated = FALSE)
humidity_relative2specific(Tc, HR)
}
\arguments{
  \item{Tc}{A numeric vector of temperature in degrees Celsius.}
  \item{HS}{A numeric vector of specific humidity (unitless).}
  \item{HR}{A numeric vector of relative Humidity (in \%).}
  \item{allowSaturated}{Logical flag to allow values over 100\%}
}
\value{
A numeric vector with specific or relative humidity.
}
\author{
Nicholas Martin-StPaul, INRA

Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}
\seealso{\code{\link{meteocomplete}}}
