#' Example data set for statistical correction of RCM predictions
#' 
#' Example data set including the predictions of Regional Climate Model
#' (CCLM4-8-17; driving global model CNRM-CERFACS-CNRM-CM5) for 3 model cells
#' in a small area in Catalonia (NE Spain). Meteorological data covers an
#' historical (reference) period (2000-2003) and a future (projection) period
#' (2020-2023), the latter simulated under rcp4.5 scenario.
#' 
#' 
#' @name examplecorrectiondata
#' @docType data
#' @format Formal class '\code{\link{MeteorologyUncorrectedData-class}}'
#' @source ESFG web site (http://esgf.llnl.gov/) that centralizes climate data
#' from GCM and RCM uploaded in the frame of different international
#' consortium, including the EURO-CORDEX regionalisation project.
#' @keywords datasets
#' @examples
#' 
#' data(examplecorrectiondata)
#' 
NULL





#' Example spatial grid topography
#' 
#' 'SpatialGridTopography' object describing topographic features for a grid of
#' 5 km x 5 km and cell size of 100 m in Catalonia (NE Spain).
#' 
#' 
#' @name examplegridtopography
#' @docType data
#' @format Formal class 'SpatialGridTopography'
#' @source 'Institut Cartogràfic de Catalunya' (ICC)
#' @keywords datasets
#' @examples
#' 
#' data(examplegridtopography)
#' 
NULL





#' Example data set for interpolation from weather stations
#' 
#' Example data set of spatial location, topography and daily meteorological
#' records from 38 weather stations in Catalonia (NE Spain) corresponding to
#' years 2000-2003.
#' 
#' 
#' @name exampleinterpolationdata
#' @docType data
#' @format Formal class '\code{\link{MeteorologyInterpolationData-class}}'
#' @source 'Servei Meteorològic de Catalunya' (SMC) and 'Agencia Española de
#' Meteorología' (AEMET)
#' @keywords datasets
#' @examples
#' 
#' data(exampleinterpolationdata)
#' 
NULL





#' Humidity conversion tools
#' 
#' Functions to transform relative humidity to specific humidity or dew point
#' temperature and viceversa.
#' 
#' 
#' @aliases humidity_relative2specific humidity_specific2relative
#' humidity_relative2dewtemperature humidity_dewtemperature2relative
#' @param Tc A numeric vector of temperature in degrees Celsius.
#' @param HS A numeric vector of specific humidity (unitless).
#' @param HR A numeric vector of relative Humidity (in \%).
#' @param Td A numeric vector of dew temperature in degrees Celsius.
#' @param allowSaturated Logical flag to allow values over 100\%
#' @return A numeric vector with specific or relative humidity.
#' @author Nicholas Martin-StPaul, INRA
#' 
#' Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{meteocomplete}}
NULL





#' Low-level interpolation functions
#' 
#' Low-level functions to interpolate meteorology (one day) on a set of points.
#' 
#' 
#' @aliases interpolation_dewtemperature interpolation_temperature
#' interpolation_precipitation interpolation_wind
#' @param Xp,Yp,Zp Spatial coordinates and elevation (Zp; in m.a.s.l) of target
#' points.
#' @param X,Y,Z Spatial coordinates and elevation (Zp; in m.a.s.l) of reference
#' locations (e.g. meteorological stations).
#' @param T Temperature (e.g., minimum, maximum or dew temperature) at the
#' reference locations (in degrees).
#' @param P Precipitation at the reference locations (in mm).
#' @param Psmooth Temporally-smoothed precipitation at the reference locations
#' (in mm).
#' @param WS,WD Wind speed (in m/s) and wind direction (in degrees from north
#' clock-wise) at the reference locations.
#' @param iniRp Initial truncation radius.
#' @param iterations Number of station density iterations.
#' @param debug Boolean flag to show extra console output.
#' @param alpha,alpha_amount,alpha_event Gaussian shape parameter.
#' @param N,N_event,N_amount Average number of stations with non-zero weights.
#' @param popcrit Critical precipitation occurrence parameter.
#' @param fmax Maximum value for precipitation regression extrapolations (0.6
#' equals to a maximum of 4 times extrapolation).
#' @param directionsAvailable A flag to indicate that wind directions are
#' available (i.e. non-missing) at the reference locations.
#' @return All functions return a vector with interpolated values for the
#' target points.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{defaultInterpolationParams}}
#' @references Thornton, P.E., Running, S.W., White, M. A., 1997. Generating
#' surfaces of daily meteorological variables over large regions of complex
#' terrain. J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.
#' 
#' De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating
#' daily meteorological data and downscaling climate models over landscapes.
#' Environmental Modelling and Software 108: 186-196.
#' @examples
#' 
#' data("exampleinterpolationdata")
#' mxt100 = exampleinterpolationdata@MaxTemperature[,100]
#' Psmooth100 = exampleinterpolationdata@SmoothedPrecipitation[,100]
#' P100 = exampleinterpolationdata@Precipitation[,100]
#' mismxt = is.na(mxt100)
#' misP = is.na(P100)
#' Z = exampleinterpolationdata@elevation
#' X = exampleinterpolationdata@coords[,1]
#' Y = exampleinterpolationdata@coords[,2]
#' Zpv = seq(0,1000, by=100)
#' xp = 360000
#' yp = 4640000
#' xpv = rep(xp, 11)
#' ypv = rep(yp, 11)
#' 
#' interpolation_temperature(xpv, ypv, Zpv, 
#'                           X[!mismxt], Y[!mismxt], Z[!mismxt], 
#'                           mxt100[!mismxt])
#' interpolation_precipitation(xpv, ypv, Zpv, 
#'                            X[!misP], Y[!misP], Z[!misP], 
#'                            P100[!misP], Psmooth100[!misP])
#' 
NULL





#' meteoland: Landscape Meteorology Tools
#' 
#' Functions to estimate weather variables at any position of a landscape [De
#' Caceres et al. (2018) c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1016/j.envsoft.2018.08.003")\Sexpr{tools:::Rd_expr_doi("10.1016/j.envsoft.2018.08.003")}].
#' 
#' 
#' @name meteoland-package
#' @aliases meteoland meteoland-package
#' @docType package
#' @author \strong{Maintainer}: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' (\href{https://orcid.org/0000-0001-7132-2080ORCID})
#' 
#' Authors: \itemize{ \item Nicolas Martin
#' (\href{https://orcid.org/0000-0001-7574-0108ORCID}) \item Víctor Granda
#' (\href{https://orcid.org/0000-0002-0469-1991ORCID}) \item Antoine Cabon
#' (\href{https://orcid.org/0000-0001-6426-1726ORCID}) }
#' @seealso Useful links: \itemize{ \item
#' \url{https://emf-creaf.github.io/meteoland/index.html} }
#' @keywords internal
NULL





#' Class \code{"MeteorologyInterpolationData"}
#' 
#' An S4 class to interpolate meteorology over a landscape.
#' 
#' 
#' @name MeteorologyInterpolationData-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("MeteorologyInterpolationData", ...)}, or by calls to the function
#' \code{\link{MeteorologyInterpolationData}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyInterpolationData}},
#' \code{\link{MeteorologyProcedureData-class}}, \code{\link{subsample}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("MeteorologyInterpolationData")
#' 
NULL





#' Class \code{"MeteorologyProcedureData"}
#' 
#' A virtual class for estimating meteorology over landscapes
#' 
#' 
#' @name MeteorologyProcedureData-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyInterpolationData-class}},
#' \code{\link{MeteorologyUncorrectedData-class}}
#' @keywords classes
#' @examples
#' 
#' showClass("MeteorologyProcedureData")
#' 
NULL





#' Class \code{"MeteorologyUncorrectedData"}
#' 
#' An S4 class to conduct statistical correction of meteorology over a
#' landscape.
#' 
#' 
#' @name MeteorologyUncorrectedData-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("MeteorologyUncorrectedData", ...)}, or by calls to the function
#' \code{\link{MeteorologyUncorrectedData}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyUncorrectedData}},
#' \code{\link{MeteorologyProcedureData-class}},
#' \code{\link{examplecorrectiondata}}, \code{\link{subsample}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("MeteorologyUncorrectedData")
#' 
NULL





#' Utility functions for NetCDFs
#' 
#' Functions to read spatial and temporal coordinates from NetCDFs.
#' 
#' 
#' @aliases readNetCDFdates readNetCDFpoints readNetCDFgridtopology
#' readNetCDFproj4string
#' @param file String of the NetCDF whose spatial/temporal coordinates are
#' desired.
#' @return \itemize{ \itemFunction \code{readNetCDFdates} returns a
#' \code{\link{Date}} vector. \itemFunction \code{readNetCDFpoints} returns an
#' object \code{\link{SpatialPoints-class}}. \itemFunction
#' \code{readNetCDFgridtopology} returns an object
#' \code{\link{GridTopology-class}}. \itemFunction \code{readNetCDFproj4string}
#' returns an object \code{\link{CRS-class}}. }
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{readmeteorologypoints}},
#' \code{\link{readmeteorologygrid}}
NULL





#' Solar radiation utility functions
#' 
#' Set of functions used in the calculation of incoming solar radiation and net
#' radiation.
#' 
#' 
#' @aliases radiation_dateStringToJulianDays radiation_daylength
#' radiation_daylengthseconds radiation_directDiffuseInstant
#' radiation_directDiffuseDay radiation_potentialRadiation radiation_julianDay
#' radiation_skyLongwaveRadiation radiation_outgoingLongwaveRadiation
#' radiation_netRadiation radiation_solarRadiation radiation_solarConstant
#' radiation_solarElevation radiation_solarDeclination radiation_sunRiseSet
#' @param dateStrings A character vector with dates in format "YYYY-MM-DD".
#' @param latrad Latitude (in radians North).
#' @param slorad Slope (in radians).
#' @param asprad Aspect (in radians from North).
#' @param delta Solar declination (in radians).
#' @param solarConstant Solar constant (in kW·m-2).
#' @param hrad Solar hour (in radians).
#' @param R_s Daily incident solar radiation (MJ·m-2).
#' @param clearday Boolean flag to indicate a clearsky day (vs. overcast).
#' @param nsteps Number of daily substeps.
#' @param J Julian day (integer), number of days since January 1, 4713 BCE at
#' noon UTC.
#' @param year,month,day Year, month and day as integers.
#' @param alpha Surface albedo (from 0 to 1).
#' @param Tair Air temperature (in degrees Celsius).
#' @param vpa Average daily vapor pressure (kPa).
#' @param c Proportion of sky covered by clouds [0-1].
#' @param tmin,tmax Minimum and maximum daily temperature (ºC).
#' @param elevation Elevation above sea level (in m).
#' @param precipitation Precipitation (in mm).
#' @param diffTemp Difference between maximum and minimum temperature (ºC).
#' @param diffTempMonth Difference between maximum and minimum temperature,
#' averaged over 30 days (ºC).
#' @return Values returned for each function are: \itemize{
#' \item\code{radiation_dateStringToJulianDays}: A vector of Julian days (i.e.
#' number of days since January 1, 4713 BCE at noon UTC).
#' \item\code{radiation_daylength}: Day length (in hours).
#' \item\code{radiation_daylengthseconds}: Day length (in seconds).
#' \item\code{radiation_directDiffuseInstant}: A vector with instantaneous
#' direct and diffusive radiation rates (for both SWR and PAR).
#' \item\code{radiation_directDiffuseDay}: A data frame with instantaneous
#' direct and diffusive radiation rates (for both SWR and PAR) for each
#' subdaily time step. \item\code{radiation_potentialRadiation}: Daily
#' (potential) solar radiation (in MJ·m-2). \item\code{radiation_julianDay}:
#' Number of days since January 1, 4713 BCE at noon UTC.
#' \item\code{radiation_skyLongwaveRadiation}: Instantaneous incoming (sky)
#' longwave radiation (W·m-2).
#' \item\code{radiation_outgoingLongwaveRadiation}: Daily outgoing longwave
#' radiation (MJ·m-2·day-1).  \item\code{radiation_netRadiation}: Daily net
#' solar radiation (MJ·m-2·day-1).  \item\code{radiation_solarConstant}: Solar
#' constant (in kW·m-2). \item\code{radiation_solarDeclination}: Solar
#' declination (in radians). \item\code{radiation_solarElevation}: Angle of
#' elevation of the sun with respect to the horizon (in radians).
#' \item\code{radiation_solarRadiation}: Daily incident solar radiation
#' (MJ·m-2·day-1). \item\code{radiation_sunRiseSet}: Sunrise and sunset hours
#' in hour angle (radians). }
#' @note Code for \code{radiation_julianDay()},
#' \code{radiation_solarConstant()} and \code{radiation_solarDeclination()} was
#' translated to C++ from R code in package 'insol' (by J. G. Corripio).
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{interpolationpoints}}
#' @references Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics,
#' 2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.
#' 
#' Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct
#' shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
#' 
#' McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013.
#' Estimating actual, potential, reference crop and pan evaporation using
#' standard meteorological data: a pragmatic synthesis. Hydrology & Earth
#' System Sciences 17:1331–1363. See also:
#' http://www.fao.org/docrep/x0490e/x0490e06.htm.
#' 
#' Reda, I. and Andreas, A. 2003. Solar Position Algorithm for Solar Radiation
#' Applications. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008.
#' http://www.nrel.gov/docs/fy08osti/34302.pdf
#' 
#' Spitters, C.J.T., Toussaint, H.A.J.M. and Goudriaan, J. (1986). Separating
#' the diffuse and direct components of global radiation and its implications
#' for modeling canopy photosynthesis. I. Components of incoming radiation.
#' Agricultural and Forest Meteorology, 38, 231–242.
NULL





#' Reshapes weather data from 'meteospain', 'worldmet' or 'weathercan'
#' 
#' Reshapes weather station data acquired using the 'meteospain', 'worldmet' or
#' 'weathercan' R packages into formats useful for meteoland
#' 
#' Note that to have precipitation included in downloads from 'worldmet' you
#' should set 'precip = TRUE' when calling function 'importNOAA'. In the case
#' of weathercan, precipitation is only provided for daily data (i.e. setting
#' 'interval="day"' when calling 'weather_dl'), whereas wind speed and relative
#' humidity are only availave for hourly data (i.e., setting 'interval="hour"'
#' when calling 'weather_dl'). Hence, in \code{meteoland} we recommend
#' downloading both daily and hourly data and then calling function
#' \code{reshapeweathercan} to merge the two sources.
#' 
#' @aliases reshapemeteospain reshapeweathercan reshapeworldmet
#' @param weather_data Hourly or daily weather data, in form of an sf (spatial)
#' object, obtained using function 'get_meteo_from()' in package 'meteospain'.
#' @param hourly_data Hourly weather data. In the case of
#' \code{reshapeworldmet}, a tibble or data frame returned by function
#' 'importNOAA'. In the case of \code{reshapeweathercan}, a tibble or data
#' frame returned by function 'weather_dl' with 'interval="hour"'.
#' @param daily_data Daily weather data (only for \code{reshapeweathercan}), a
#' tibble or data frame returned by function 'weather_dl' with
#' 'interval="day"'.
#' @param output Kind of output desired. Either
#' '\code{\link{SpatialPointsTopography}}',
#' '\code{\link{SpatialPointsMeteorology}}' or
#' '\code{\link{MeteorologyInterpolationData}}'.
#' @param proj4string A string or CRS to change the coordinate reference system
#' of the output. If NULL the spatial reference will be geographic coordinates
#' (i.e. \code{CRS("+proj=longlat")}). When reshaping to
#' \code{MeteorologyInterpolationData} it is recommended to use a reference
#' system with meters in units, such as UTM.
#' @param complete A flag to indicate that missing variables should be
#' completed using function \code{\link{meteocomplete}}
#' @param verbose A flag to show information of the reshape process in the
#' console output.
#' @return An object of the class indicated in \code{output}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{meteocomplete}}
NULL





#' Class \code{"SpatialGridMeteorology"}
#' 
#' An S4 class that represents a spatial grid with meteorology daily data.
#' 
#' 
#' @name SpatialGridMeteorology-class
#' @aliases SpatialGridMeteorology-class
#' [,SpatialGridMeteorology,ANY,ANY-method
#' [,SpatialGridMeteorology,ANY,ANY,ANY-method
#' show,SpatialGridMeteorology-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialGridMeteorology", ...)}, or by calls to the function
#' \code{\link{SpatialGridMeteorology}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialGridTopography}},
#' \code{\link{SpatialGridDataFrame-class}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("SpatialGridMeteorology")
#' 
NULL





#' Class \code{"SpatialGridTopography"}
#' 
#' An S4 class that represents topography over a grid of coordinates.
#' 
#' 
#' @name SpatialGridTopography-class
#' @aliases SpatialGridTopography-class [,SpatialGridTopography,ANY,ANY-method
#' [,SpatialGridTopography,ANY,ANY,ANY-method show,SpatialGridTopography-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialGridTopography", ...)}, or by calls to the function
#' \code{\link{SpatialGridTopography}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialGridTopography}},
#' \code{\link{SpatialGridDataFrame-class}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("SpatialGridTopography")
#' 
NULL





#' Class \code{"SpatialPixelsMeteorology"}
#' 
#' An S4 class that represents meteorology data that has locations on a regular
#' grid.
#' 
#' 
#' @name SpatialPixelsMeteorology-class
#' @aliases SpatialPixelsMeteorology-class
#' [,SpatialPixelsMeteorology,ANY,ANY-method
#' [,SpatialPixelsMeteorology,ANY,ANY,ANY-method
#' show,SpatialPixelsMeteorology-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPixelsMeteorology", ...)}, or by calls to the function
#' \code{\link{SpatialPixelsMeteorology}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsTopography}},
#' \code{\link{SpatialPixelsDataFrame-class}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("SpatialPixelsMeteorology")
#' 
NULL





#' Class \code{"SpatialPixelsTopography"}
#' 
#' An S4 class that represents topography that has locations on a regular grid.
#' 
#' 
#' @name SpatialPixelsTopography-class
#' @aliases SpatialPixelsTopography-class
#' [,SpatialPixelsTopography,ANY,ANY-method
#' [,SpatialPixelsTopography,ANY,ANY,ANY-method
#' show,SpatialPixelsTopography-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPixelsTopography", ...)}, or by calls to the function
#' \code{\link{SpatialPixelsTopography}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsTopography}},
#' \code{\link{SpatialPixelsDataFrame-class}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("SpatialPixelsTopography")
#' 
NULL





#' Class \code{"SpatialPointsMeteorology"}
#' 
#' An S4 class that represents a set of points with meteorology data series.
#' 
#' 
#' @name SpatialPointsMeteorology-class
#' @aliases SpatialPointsMeteorology-class
#' [,SpatialPointsMeteorology,ANY,ANY-method
#' [,SpatialPointsMeteorology,ANY,ANY,ANY-method
#' show,SpatialPointsMeteorology-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPointsMeteorology", ...)}, or by calls to the function
#' \code{\link{SpatialPointsMeteorology}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsTopography-class}},
#' \code{\link{SpatialPoints-class}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("SpatialPointsMeteorology")
#' 
NULL





#' Class \code{"SpatialPointsTopography"}
#' 
#' An S4 class that represents topography over a grid of coordinates.
#' 
#' 
#' @name SpatialPointsTopography-class
#' @aliases SpatialPointsTopography-class
#' [,SpatialPointsTopography,ANY,ANY-method
#' [,SpatialPointsTopography,ANY,ANY,ANY-method
#' show,SpatialPointsTopography-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SpatialPointsTopography", ...)}, or by calls to the function
#' \code{\link{SpatialPointsTopography}}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsTopography}},
#' \code{\link{SpatialPointsDataFrame-class}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("SpatialPointsTopography")
#' 
NULL





#' Spatial grid plots
#' 
#' Function \code{spplot} for \code{\link{SpatialGridTopography-class}} and
#' \code{\link{SpatialPixelsTopography-class}} objects allows drawing maps of
#' topographic attributes. Function \code{spplot} for
#' \code{\link{SpatialGridMeteorology-class}} and
#' \code{\link{SpatialPixelsMeteorology-class}} objects allows drawing maps of
#' meteorological variables corresponding to specific dates.
#' 
#' 
#' @aliases spplot,SpatialGridTopography-method
#' spplot,SpatialGridMeteorology-method spplot,SpatialPixelsTopography-method
#' spplot,SpatialPixelsMeteorology-method
#' @param obj An object of class \code{SpatialGridTopography}.
#' @param variable A string of the variable to be plotted (only
#' \code{type="elevation"}, \code{type="slope"}, \code{type="aspect"} are
#' allowed).
#' @param ... Additional parameters to function \code{\link{spplot}}.
#' @param date A string or an integer for the date to be plotted.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{meteoplot}}
#' @examples
#' 
#' data(examplegridtopography)
#' 
#' #Display data
#' spplot(examplegridtopography, type="elevation", scales=list(draw=TRUE))
#' spplot(examplegridtopography, type="slope", scales=list(draw=TRUE))
#' spplot(examplegridtopography, type="aspect", scales=list(draw=TRUE))
#' 
NULL





#' Sub-sampling procedure data
#' 
#' Generates a spatial and/or temporal subset of procedure data
#' 
#' 
#' @name subsample
#' @aliases subsample subsample-methods
#' subsample,MeteorologyUncorrectedData-method
#' subsample,MeteorologyInterpolationData-method
#' @docType methods
#' @param object An object of a sub-class
#' \code{\link{MeteorologyProcedureData-class}}.
#' @param bbox A 2x2 numeric matrix with the boundaries of the target area. If
#' \code{NULL}, the original boundary box is kept (except if \code{stations} is
#' specified).
#' @param stations A numeric, character or logical vector specifying a subset
#' of weather stations. If \code{NULL} all original weather stations are kept
#' (except if \code{bbox} is specified).
#' @param dates A vector of \code{\link{Date}} with the subset of dates of
#' interest. If \code{NULL}, all dates are kept.
#' @param buffer A buffer to put around bbox for the spatial selection of data.
#' @return An object of the same class as \code{object}.
#' @section Methods: \describe{ \item{subsample}{\code{signature(object =
#' "MeteorologyUncorrectedData")}: Generates a
#' \code{\link{MeteorologyUncorrectedData}} object for a smaller area and a
#' subset of dates. }
#' 
#' \item{subsample}{\code{signature(object = "MeteorologyInterpolationData")}:
#' Generates a \code{\link{MeteorologyInterpolationData}} object for a smaller
#' area and a subset of dates. } }
#' @keywords methods
#' @examples
#' 
#' data(exampleinterpolationdata)
#' 
#' oridates = exampleinterpolationdata@dates
#' 
#' #Interpolation data using the first ten dates (same boundary box)
#' subdata = subsample(exampleinterpolationdata, dates = oridates[1:10])
#' 
NULL





#' Physical utility functions
#' 
#' Set of functions used in the calculation of physical variables.
#' 
#' 
#' @aliases utils_airDensity utils_atmosphericPressure utils_averageDailyVP
#' utils_averageDaylightTemperature utils_latentHeatVaporisation
#' utils_latentHeatVaporisationMol utils_psychrometricConstant
#' utils_saturationVP utils_saturationVaporPressureCurveSlope
#' @param temperature Air temperature (ºC).
#' @param Tmin,Tmax Minimum and maximum daily temperature (ºC).
#' @param RHmin,RHmax Minimum and maximum relative humidity (\%).
#' @param Patm Atmospheric air pressure (in kPa).
#' @param elevation Elevation above sea level (in m).
#' @return Values returned for each function are: \itemize{
#' \item\code{utils_airDensity}: air density (in kg·m-3).
#' \item\code{utils_atmosphericPressure}: Air atmospheric pressure (in kPa).
#' \item\code{utils_averageDailyVP}: average (actual) vapour pressure (in kPa).
#' \item\code{utils_averageDaylightTemperature}: average daylight air
#' temperature (in ºC). \item\code{utils_latentHeatVaporisation}: Latent heat
#' of vaporisation (MJ·kg-1).  \item\code{utils_latentHeatVaporisationMol}:
#' Latent heat of vaporisation (J·mol-1).
#' \item\code{utils_psychrometricConstant}: Psychrometric constant (kPa·ºC-1).
#' \item\code{utils_saturationVP}: saturation vapour pressure (in kPa).
#' \item\code{utils_saturationVaporPressureCurveSlope}: Slope of the saturation
#' vapor pressure curve (kPa·ºC-1).  }
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @references McMurtrie, R. E., D. A. Rook, and F. M. Kelliher. 1990.
#' Modelling the yield of Pinus radiata on a site limited by water and
#' nitrogen. Forest Ecology and Management 30:381–413.
#' 
#' McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013.
#' Estimating actual, potential, reference crop and pan evaporation using
#' standard meteorological data: a pragmatic synthesis. Hydrology & Earth
#' System Sciences 17:1331–1363. See also:
#' http://www.fao.org/docrep/x0490e/x0490e06.htm
NULL



