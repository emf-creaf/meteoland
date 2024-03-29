% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_interpolator.R
\name{get_interpolation_params}
\alias{get_interpolation_params}
\title{Retrieving interpolation parameters from interpolator object}
\usage{
get_interpolation_params(interpolator)
}
\arguments{
\item{interpolator}{interpolator object as returned by
\code{\link{create_meteo_interpolator}}}
}
\value{
The complete parameter list from the interpolator object
}
\description{
Retrieve the parameter list from and interpolator object
}
\examples{
# example interpolator
data(meteoland_interpolator_example)
# get the params from the interpolator
get_interpolation_params(meteoland_interpolator_example)

}
\seealso{
Other interpolator functions: \code{\link{add_topo}()},
\code{\link{create_meteo_interpolator}()},
\code{\link{read_interpolator}()}, \code{\link{set_interpolation_params}()},
\code{\link{with_meteo}()}, \code{\link{write_interpolator}()}
}
\author{
Victor Granda \enc{García}{Garcia}, EMF-CREAF
}
