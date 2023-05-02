#' Complete daily meteorological variables
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Fills missing values of relative humidity, radiation and potential
#' evapotranspiration from a data frame with daily values of
#' minimum/maximum/mean temperature and precipitation.
#'
#' @details
#' The function fills values for humidity, radiation and PET only if they are
#' missing in the input data frame. If a column 'SpecificHumidity' is present
#' in the input data, relative humidity is calculated from it. Otherwise,
#' relative humidity is calculated assuming that dew point temperature equals
#' the minimum temperature. Potential solar radiation is calculated from
#' latitude, slope and aspect. Incoming solar radiation is then corrected
#' following Thornton & Running (1999) and potential evapotranspiration
#' following Penman (1948).
#'
#' @param x A data frame with dates as row names and columns named
#' 'MeanTemperature', 'MaxTemperature', 'MinTemperature' and 'Precipitation'
#' @param latitude Latitude in degrees North.
#' @param elevation Elevation in m.a.s.l.
#' @param slope Slope in degrees.
#' @param aspect Aspect in degrees from North.
#' @return A data frame copied from \code{x} but with filled values for
#' variables: \itemize{ \item\code{MeanRelativeHumidity}: Mean daily relative
#' humidity (in percent).  \item\code{MinRelativeHumidity}: Minimum daily
#' relative humidity (in percent).  \item\code{MaxRelativeHumidity}: Maximum
#' daily relative humidity (in percent).  \item\code{Radiation}: Incoming solar
#' radiation (MJ/m2).  \item\code{PET}: Potential evapotranspiration (in mm of
#' water).  }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{penman}}, \code{\link{radiation_solarRadiation}}
#' @references Thornton, P.E., Running, S.W., 1999. An improved algorithm for
#' estimating incident daily solar radiation from measurements of temperature,
#' humidity, and precipitation. Agric. For. Meteorol. 93, 211-228.
#'
#' Penman, H. L. 1948. Natural evaporation from open water, bare soil and
#' grass. Proceedings of the Royal Society of London. Series A. Mathematical
#' and Physical Sciences, 193, 120-145.
#' @export
meteocomplete<-function(x, latitude, elevation, slope, aspect) {
  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "meteocomplete()", with = "complete_meteo()",
    details = "Meteo objects complying with the new standard (see with_meteo()) can be completed
    with complete_meteo()"
  )

  # patm = utils_atmosphericPressure(elevation)
  # latrad = latitude*(pi/180)
  # slorad = slope*(pi/180)
  # asprad = aspect*(pi/180)
  # if(is.na(slorad)) slorad = 0
  # if(is.na(asprad)) asprad = 0
  # if(!("MeanRelativeHumidity" %in% names(x))) x$MeanRelativeHumidity = NA
  # if(!("MinRelativeHumidity" %in% names(x))) x$MinRelativeHumidity = NA
  # if(!("MaxRelativeHumidity" %in% names(x))) x$MaxRelativeHumidity = NA
  # if(!("Radiation" %in% names(x))) x$Radiation = NA
  # if(!("PET" %in% names(x))) x$PET = NA
  # for(j in 1:nrow(x)) {
  #   date = as.Date(row.names(x)[j])
  #   year = as.numeric(format(date,"%Y"))
  #   month = as.numeric(format(date,"%m"))
  #   day = as.numeric(format(date,"%d"))
  #   J = radiation_julianDay(year, month, day)
  #   tmin = x$MinTemperature[j]
  #   tmax = x$MaxTemperature[j]
  #   tmean = x$MeanTemperature[j]
  #   diffTemp = tmax-tmin
  #   precipitation = x$Precipitation[j]
  #   if(("SpecificHumidity" %in% names(x))) {
  #     hs = x$SpecificHumidity[j]
  #     #Mean relative humidity
  #     if(is.na(x$MeanRelativeHumidity[j])) {
  #       rhmean = humidity_specific2relative(tmean, hs)
  #       x$MeanRelativeHumidity[j] = rhmean
  #     } else {
  #       rhmean = x$MeanRelativeHumidity[j]
  #     }
  #     #Minimum relative humidity
  #     if(is.na(x$MinRelativeHumidity[j])) {
  #       rhmin = humidity_specific2relative(tmax, hs)
  #       x$MinRelativeHumidity[j] = rhmin
  #     } else {
  #       rhmin = x$MinRelativeHumidity[j]
  #     }
  #     #Maximum relative humidity
  #     if(is.na(x$MaxRelativeHumidity[j])) {
  #       rhmax = humidity_specific2relative(tmin, hs)
  #       x$MaxRelativeHumidity[j] = rhmax
  #     } else {
  #       rhmax = x$MaxRelativeHumidity[j]
  #     }
  #
  #   } else {
  #     #Mean relative humidity
  #     if(is.na(x$MeanRelativeHumidity[j])) {
  #       rhmean = 100*utils_saturationVP(tmin)/utils_saturationVP(tmean)
  #       x$MeanRelativeHumidity[j] = max(min(rhmean,100),0)
  #     } else {
  #       rhmean = x$MeanRelativeHumidity[j]
  #     }
  #     #Minimum relative humidity
  #     if(is.na(x$MinRelativeHumidity[j])) {
  #       rhmin = 100*utils_saturationVP(tmin)/utils_saturationVP(tmax)
  #       x$MinRelativeHumidity[j] = max(min(rhmin,100),0)
  #     } else {
  #       rhmin = x$MinRelativeHumidity[j]
  #     }
  #     #Maximum relative humidity
  #     if(is.na(x$MaxRelativeHumidity[j])) {
  #       rhmax = 100
  #       x$MaxRelativeHumidity[j] = max(min(rhmax,100),0)
  #     } else {
  #       rhmax = x$MaxRelativeHumidity[j]
  #     }
  #   }
  #   #Radiation
  #   if(is.na(x$Radiation[j])) {
  #     sc = radiation_solarConstant(J)
  #     delta = radiation_solarDeclination(J)
  #     vpa = utils_averageDailyVP(tmin, tmax, rhmin, rhmax)
  #     r_s = radiation_solarRadiation(sc,latrad = latrad, elevation, slorad, asprad, delta,
  #                                    diffTemp,diffTemp, vpa, precipitation)
  #     x$Radiation[j] = r_s
  #   } else {
  #     r_s = x$Radiation[j]
  #   }
  #   if(is.na(x$PET[j])) {
  #     x$PET[j] = penman(latrad, elevation, slorad, asprad, J, tmin, tmax, rhmin, rhmax, r_s, NA)
  #   }
  # }
  # return(x)
}
