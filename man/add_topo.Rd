% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/with_meteo.R
\name{add_topo}
\alias{add_topo}
\title{Add topology data to meteo object}
\usage{
add_topo(meteo, topo)
}
\arguments{
\item{meteo}{meteo object}

\item{topo}{topo object}
}
\value{
meteo with the topology info added
}
\description{
Add topology data to meteo object
}
\details{
When using meteo data without topology info to create an inteprolator,
topology must be addded
}
\seealso{
Other interpolator functions: 
\code{\link{create_meteo_interpolator}()},
\code{\link{read_interpolator}()},
\code{\link{with_meteo}()},
\code{\link{write_interpolator}()}
}
\concept{interpolator functions}