% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_interpolator.R
\name{write_interpolator}
\alias{write_interpolator}
\title{Write the interpolator object}
\usage{
write_interpolator(interpolator, filename, .overwrite = FALSE)
}
\arguments{
\item{interpolator}{meteoland interpolator object, as created by
\code{\link{create_meteo_interpolator}}}

\item{filename}{file name for the interpolator nc file}

\item{.overwrite}{logical indicating if the file should be overwritten if it
already exists}
}
\value{
invisible interpolator object, to allow using this function as a
step in a pipe
}
\description{
Write the interpolator object to a file
}
\details{
This function writes the interpolator object created with
\code{\link{create_meteo_interpolator}} in a NetCDF-CF standard
compliant format, as specified in
https://cfconventions.org/cf-conventions/cf-conventions.html
}
\examples{

\donttest{
# example interpolator
data(meteoland_interpolator_example)

# temporal folder
tmp_dir <- tempdir()

# write interpolator
write_interpolator(
  meteoland_interpolator_example,
  file.path(tmp_dir, "meteoland_interpolator_example.nc"),
  .overwrite = TRUE
)

# check file exists
file.exists(file.path(tmp_dir, "meteoland_interpolator_example.nc"))

# read it again
read_interpolator(file.path(tmp_dir, "meteoland_interpolator_example.nc"))
}

}
\seealso{
Other interpolator functions: \code{\link{add_topo}()},
\code{\link{create_meteo_interpolator}()},
\code{\link{get_interpolation_params}()}, \code{\link{read_interpolator}()},
\code{\link{set_interpolation_params}()}, \code{\link{with_meteo}()}
}
\author{
Victor Granda \enc{García}{Garcia}, EMF-CREAF
}
