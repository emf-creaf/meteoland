\encoding{UTF-8}
\name{penman}
\alias{penman}
\alias{penmanmonteith}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Potential evapotranspiration
}
\description{
Functions to calculate potential evapotranspiration using Penman or Penman-Monteith.
}
\usage{
penman(latrad, elevation, slorad, asprad, J,
            Tmin, Tmax, RHmin, RHmax, R_s,  u,
            z=2.0, z0 = 0.001, alpha = 0.08, windfun="1956")
penmanmonteith(rc, elevation, Tmin, Tmax, RHmin, RHmax,
               Rn, u = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{latrad}{Latitude in radians.}
  \item{elevation}{Elevation (in m).}
  \item{slorad}{Slope (in radians).}
  \item{asprad}{Aspect (in radians from North).}
  \item{J}{Julian day, number of days since January 1, 4713 BCE at noon UTC.}
  \item{Tmax}{Maximum temperature (degrees Celsius).}
  \item{Tmin}{Minimum temperature (degrees Celsius).}
  \item{RHmin}{Minimum relative humidity (percent).}
  \item{RHmax}{Maximum relative humidity (percent).}
  \item{R_s}{Solar radiation (MJ/m2).}
  \item{u}{With wind speed (m/s).}
  \item{z}{Wind measuring height (m).}
  \item{z0}{Roughness height (m).}
  \item{alpha}{Albedo.}
  \item{windfun}{Wind speed function version, either "1948" or "1956".}
  \item{rc}{Canopy vapour flux (stomatal) resistance (s·m-1).}
  \item{Rn}{Daily net radiation (MJ·m-2·day-1).}
}
\details{
The code  was adapted from package `Evapotranspiration', which follows McMahon et al. (2013). If wind speed is not available, an alternative formulation for potential evapotranspiration is used as an approximation (Valiantzas 2006)
}
\references{
Penman, H. L. 1948. Natural evaporation from open water, bare soil and grass. Proceedings of the Royal Society of London. Series A. Mathematical and Physical Sciences, 193, 120-145.

Penman, H. L. 1956. Evaporation: An introductory survey. Netherlands Journal of Agricultural Science, 4, 9-29.

McMahon, T.A., Peel, M.C., Lowe, L., Srikanthan, R., McVicar, T.R. 2013. Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis. Hydrology \& Earth System Sciences 17, 1331–1363. doi:10.5194/hess-17-1331-2013.
}
\value{
Potential evapotranspiration (in mm of water).
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{interpolationpoints}}}
