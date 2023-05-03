# .reshapemeteospain_current<-function(data_ms, daily = TRUE, service = "aemet",
#                                      verbose = TRUE) {
#   spdf_data <-sf::as_Spatial(data_ms)
#   data_df <- spdf_data@data
#   # Isolate coordinates and elevation
#   coords_data <- as.data.frame(coordinates(spdf_data))
#   data_df$lon = coords_data$coords.x1
#   data_df$lat = coords_data$coords.x2
#
#   if(verbose)cat("\nFormating data")
#   if(service=="meteocat") {
#     varnames <-c("station_id", "lon", "lat", "station_name", "altitude", "timestamp", "temperature",  "precipitation",
#                  "relative_humidity", "wind_direction", "wind_speed")
#     numvar <- c("lon", "lat","altitude","temperature", "precipitation", "relative_humidity", "wind_direction", "wind_speed")
#   } else {
#     varnames <-c("station_id", "lon", "lat", "station_name", "altitude", "timestamp", "temperature", "min_temperature", "max_temperature",  "precipitation",
#                  "relative_humidity", "wind_direction", "wind_speed")
#     numvar <- c("lon", "lat","altitude","temperature", "min_temperature", "max_temperature",  "precipitation", "relative_humidity", "wind_direction", "wind_speed")
#   }
#   data_df <- data_df[,varnames]
#   data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
#   data_df$timestamp <- as.POSIXlt(sub("T", " ",data_df$timestamp), format = "%Y-%m-%d %H:%M:%S")
#
#   if(daily){
#     if(verbose)cat("\nAggregating hourly data to 24h-scale\n")
#     options(warn=-1)
#     data_agg <- aggregate(data_df[,numvar],list(station_id = data_df$station_id, station_name = data_df$station_name),
#                           function(x){mean<-mean(x,na.rm=T);min<-min(x,na.rm=T);max<-max(x,na.rm=T);sum<-sum(x,na.rm=T)
#                           return(c(mean=mean,min=min,max=max,sum=sum))})
#     # wind direction
#     dv_agg <- aggregate(list(dv = data_df$wind_direction),list(station_id = data_df$station_id, station_name = data_df$station_name),
#                         function(dvvec){
#                           y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
#                           x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
#                           dv = (180/pi)*atan(y/x)
#                           dv[dv<0] <- dv[dv<0]+360
#                           return(dv)
#                         })
#     options(warn=0)
#     if(service=="meteocat") {
#       data_df <- data.frame(ID = as.character(data_agg$station_id), name = data_agg$station_name,
#                             long = data_agg$lon[,"mean"],lat = data_agg$lat[,"mean"], elevation = data_agg$altitude[,"mean"],
#                             MeanTemperature = data_agg$temperature[,"mean"], MinTemperature = data_agg$temperature[,"min"], MaxTemperature = data_agg$temperature[,"max"],
#                             Precipitation = data_agg$precipitation[,"sum"], WindSpeed = data_agg$wind_speed[,"mean"], WindDirection = dv_agg$dv,
#                             MeanRelativeHumidity = data_agg$relative_humidity[,"mean"], MinRelativeHumidity = data_agg$relative_humidity[,"min"], MaxRelativeHumidity = data_agg$relative_humidity[,"max"])
#     } else {
#       data_df <- data.frame(ID = as.character(data_agg$station_id), name = data_agg$station_name,
#                             long = data_agg$lon[,"mean"],lat = data_agg$lat[,"mean"], elevation = data_agg$altitude[,"mean"],
#                             MeanTemperature = data_agg$temperature[,"mean"], MinTemperature = data_agg$min_temperature[,"min"], MaxTemperature = data_agg$max_temperature[,"max"],
#                             Precipitation = data_agg$precipitation[,"sum"], WindSpeed = data_agg$wind_speed[,"mean"], WindDirection = dv_agg$dv,
#                             MeanRelativeHumidity = data_agg$relative_humidity[,"mean"], MinRelativeHumidity = data_agg$relative_humidity[,"min"], MaxRelativeHumidity = data_agg$relative_humidity[,"max"])
#     }
#
#     data_df <- as.data.frame(lapply(data_df,function(x){
#       x. <- x
#       if(is.numeric(x.))x.[is.nan(x.)|is.infinite(x.)] <- NA
#       return(x.)
#     }))
#
#     data_sp <- SpatialPointsDataFrame(coords = data_df[,c("long", "lat")],
#                                       data = data_df[,which(!colnames(data_df) %in% c("long", "lat", "name", "ID"))],
#                                       proj4string = CRS(SRS_string = "EPSG:4326"))
#     row.names(data_sp) <- data_df$ID
#     return(data_sp)
#   }else{
#     if(verbose)cat("\nHourly results are returned\n")
#     if(service=="meteocat") {
#       data_out <-data_df[,c("station_id", "lon", "lat", "station_name", "altitude", "timestamp",
#                             "temperature",
#                             "precipitation", "relative_humidity", "wind_direction", "wind_speed")]
#       colnames(data_out) <- c("ID", "long", "lat", "name", "elevation", "timestamp",
#                               "MeanTemperature",
#                               "Precipitation", "MeanRelativeHumidity", "WindDirection", "WindSpeed")
#     } else {
#       data_out <-data_df[,c("station_id", "lon", "lat", "station_name", "altitude", "timestamp",
#                             "temperature", "min_temperature", "max_temperature",
#                             "precipitation", "relative_humidity", "wind_direction", "wind_speed")]
#       colnames(data_out) <- c("ID", "long", "lat", "name", "elevation", "timestamp",
#                               "MeanTemperature", "MinTemperature", "MaxTemperature",
#                               "Precipitation", "MeanRelativeHumidity", "WindDirection", "WindSpeed")
#     }
#     return(data_out)
#   }
# }

#### AEMET
#' Download data from AEMET
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Download data from the Spanish National Meterology Agency
#' (AEMET)
#'
#' @details
#' API key needs to be acquired from AEMET (https://opendata.aemet.es/)
#'
#' @aliases downloadAEMETstationlist downloadAEMEThistorical
#' downloadAEMETcurrentday
#' @param api String with the AEMET API key (see https://opendata.aemet.es/).
#' @param dates An object of class \code{\link{Date}}.
#' @param station_id A string vector containing station ids (the list of
#' stations for which historical climatic series are available is given by
#' \code{downloadAEMETstationlist}).
#' @param export If \code{export = FALSE} the downloaded data is stored in
#' memory. Otherwise the result is written on the disk (using the format
#' specified in \code{exportFormat}).
#' @param exportDir Output directory for downloaded meteorology.
#' @param exportFormat Format of meteorological data. Current accepted formats
#' are \code{"castanea"} and \code{"meteoland"}.
#' @param metadataFile The name of the file that will store the meta data
#' describing all written files.
#' @param verbose Boolean flag to print process information.
#' @param daily Boolean flag. Are data to be returned at a daily or hourly
#' scale?
#' @return Function \code{downloadAEMETstationlist} returns a
#' \code{SpatialPointsDataFrame} object containing the list of
#' AEMET weather stations for which historical climatic series are available
#' and can be retrieved using \code{downloadAEMEThistorical}.
#'
#' Function \code{downloadAEMEThistorical} downloads data for the specified
#' AEMET stations and dates. Data is available for dates up to 4 days before
#' current date. If \code{export = FALSE}, function
#' \code{downloadAEMEThistorical} returns a
#' \code{\link{SpatialPointsMeteorology-class}} object with the downloaded
#' meteorology for each station (point). Otherwise the function writes on the
#' disk at the location specified by \code{exportDir} and solely returns a
#' \code{SpatialPointsDataFrame} object containing the files
#' metadata.
#'
#' Function \code{downloadAEMETcurrentday} downloads recent weather (the last
#' 24h) from all currently available stations and returns data frame if
#' \code{daily = FALSE} or a \code{SpatialPointsDataFrame} object
#' with observations aggregated at the daily scale if else.
#' @note Since ver. 1.0.1, weather data download functions included in
#' \code{meteoland} make internal calls to functions in package
#' \code{meteospain}. For an enhanced flexibility, users are recommended to
#' call functions in \code{meteospain} themselves, and then to use function
#' \code{\link{reshapemeteospain}} to generate data suitable for
#' \code{meteoland}.
#'
#' The list of stations available in \code{downloadAEMETcurrentday} (current
#' observations) may be different from the list given by
#' \code{downloadAEMETstationlist} and available in
#' \code{downloadAEMEThistorical} (stations with historical climate series).
#' @author Antoine Cabon, CTFC
#'
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
#' @references AEMET should be acknowledged as author of information when using
#' this data.
#' @export
downloadAEMETcurrentday <- function(api, daily = TRUE, verbose=TRUE){

  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "downloadAEMETcurrentday()", with = "meteospain::get_meteo_from()",
    details = "Weather download functions are now provided by `meteospain` package"
  )

  # if(verbose) cat("Downloading hourly data from all available stations")
  # api_options <- meteospain::aemet_options(resolution = 'current_day', api_key = api)
  # data_ms <- meteospain::get_meteo_from("aemet", api_options)
  # return(.reshapemeteospain_current(data_ms, daily = daily, service = "aemet", verbose = verbose))
}



#### SMC
#' Download data from SMC
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Download data from the Catalan automatic weather station network
#' (XEMA from Servei \enc{Meteorològic}{Meteorologic} de Catalunya)
#'
#' @details
#' API key needs to be requested from SMC
#' (https://apidocs.meteocat.gencat.cat/).
#'
#' @aliases downloadSMCcurrentday downloadSMCstationlist downloadSMChistorical
#' @param api String with the SMC API key (the procedure to apply for an api
#' key is explained in https://apidocs.meteocat.gencat.cat/ ).
#' @param daily Boolean flag. Are data to be returned at a daily or hourly
#' scale?
#' @param date An object of class \code{\link{Date}}. By default the current
#' day in the case of \code{downloadSMCcurrentday()}. In the case of
#' \code{downloadSMCstationlist()} a date for which operational stations are
#' queried. In the case of \code{downloadSMChistorical}, the function returns
#' the whole month the first date selected is in (implementation in package
#' \code{meteospain}).
#' @param station_id A string vector containing station ids (the list of
#' stations for which climatic series are available is given by
#' \code{downloadSMCstationlist()}). If \code{NULL}, all available stations are
#' queried. Otherwise, only the data corresponding to the specified stations
#' will be returned.
#' @param export If \code{export = FALSE} the downloaded data is stored in
#' memory. Otherwise the result is written on the disk (using the format
#' specified in \code{exportFormat}).
#' @param exportDir Output directory for downloaded meteorology.
#' @param exportFormat Format of meteorological data. Current accepted formats
#' are \code{"castanea"} and \code{"meteoland"}.
#' @param metadataFile The name of the file that will store the meta data
#' describing all written files.
#' @param verbose Boolean flag to print process information.
#' @return Function \code{downloadSMCstationlist} returns a
#' \code{SpatialPointsDataFrame} object containing the list of SMC
#' operational weather stations for the date given.
#'
#' Function \code{downloadSMCcurrentday} downloads recent weather (the last 24h
#' or the weather for a given date) from all currently available stations and
#' returns data frame if \code{daily_meteoland = FALSE} or a
#' \code{SpatialPointsDataFrame} object with observations
#' aggregated at the daily scale otherwise.
#'
#' Function \code{downloadSMChistorical} downloads historical daily weather
#' corresponding to a given time period from a set (or all currently available)
#' stations. Results are returned (or exported) after formatting data as a
#' \code{\link{SpatialPointsMeteorology-class}} if \code{variable_code = NULL},
#' or as a data frame otherwise.
#' @note Since ver. 1.0.1, weather data download functions included in
#' \code{meteoland} make internal calls to functions in package
#' \code{meteospain}. For an enhanced flexibility, users are recommended to
#' call functions in \code{meteospain} themselves, and then to use function
#' \code{\link{reshapemeteospain}} to generate data suitable for
#' \code{meteoland}.
#' @author Antoine Cabon, CTFC
#'
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
#' @references Servei \enc{Meteorològic}{Meteorologic} de Catalunya (SMC) should
#' be acknowledged as author of information when accessing weather data with
#' these functions.
#' @export
downloadSMCcurrentday <- function(api, daily=TRUE, station_id=NULL,
                                  date = Sys.Date(), verbose=TRUE){

  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "downloadSMCcurrentday()", with = "meteospain::get_meteo_from()",
    details = "Weather download functions are now provided by `meteospain` package"
  )
  # api_options <- meteospain::meteocat_options(resolution = 'hourly', api_key = api,
  #                                 start_date = date, stations = station_id)
  # if(verbose)cat("Downloading hourly data from all available stations\n")
  # data_ms <- meteospain::get_meteo_from("meteocat", api_options)
  # return(.reshapemeteospain_current(data_ms, daily = daily,  service = "meteocat",verbose = verbose))
}


#### MeteoGalicia
#' Download data from MeteoGalicia
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Download data from the Galician Meterology Agency (MeteoGalicia)
#' @details
#' See available data services of MeteoGalicia at
#' https://www.meteogalicia.gal/web/RSS/rssIndex.action?request_locale=es.
#'
#' @aliases downloadMGstationlist downloadMGhistorical downloadMGcurrentday
#' @param date_from,date_to Strings or objects of class \code{\link{Date}}
#' specifying first and last date of the desired period.
#' @param station_id A string vector containing station ids (the list of
#' stations presently operative is given by \code{downloadMGstationlist}). If
#' NULL all stations with available data are returned.
#' @param verbose Boolean flag to print process information.
#' @param daily Boolean flag. Are data to be returned at a daily or hourly
#' scale?
#' @return Function \code{downloadMGstationlist} returns a
#' \code{SpatialPointsDataFrame} object containing the list of
#' MeteoGalicia weather stations currently operative.  Function
#' \code{downloadMGhistorical} downloads data for the specified MG weather
#' stations (or all) and dates and returns a
#' \code{\link{SpatialPointsMeteorology-class}} object with the downloaded
#' meteorology for each station (point).
#'
#' Function \code{downloadMGcurrentday} downloads recent weather data (the last
#' 24h) from all currently available stations and returns data frame if
#' \code{daily = FALSE} or a \code{SpatialPointsDataFrame} object
#' with observations aggregated at the daily scale otherwise.
#' @note Since ver. 1.0.1, weather data download functions included in
#' \code{meteoland} make internal calls to functions in package
#' \code{meteospain}. For an enhanced flexibility, users are recommended to
#' call functions in \code{meteospain} themselves, and then to use function
#' \code{\link{reshapemeteospain}} to generate data suitable for
#' \code{meteoland}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
#' @references MeteoGalicia (from the Conselleria de Medio Ambiente, Territorio
#' e Vivenda of Xunta de Galicia) should be acknowledged as source of
#' information when using this data.
#' @export
downloadMGcurrentday <- function(station_id=NULL, daily = TRUE, verbose = TRUE) {
  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "downloadMGcurrentday()", with = "meteospain::get_meteo_from()",
    details = "Weather download functions are now provided by `meteospain` package"
  )

  # if(verbose) cat("Downloading hourly data from all available stations")
  # api_options <- meteospain::meteogalicia_options(resolution = 'current_day', stations = station_id)
  # data_ms <- meteospain::get_meteo_from("meteogalicia", api_options)
  # return(.reshapemeteospain_current(data_ms, daily = daily,  service = "meteogalicia", verbose = verbose))
}

#### Meteoclimatic
#' Download data from Meteoclimatic network
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Download data from the Spanish Automatic Stations Network
#' (non-professional)
#'
#' @details
#' Meteoclimatic is a non-professional automatic stations network, maintained
#' by volunteers that share the data from their climatic stations. Data offered
#' by these stations has not passed any quality control.
#'
#' @aliases downloadMETEOCLIMATICstationlist downloadMETEOCLIMATICcurrentday
#' @param station_id A string vector containing station ids (the list of
#' stations for which current day climatic data is available is given by
#' \code{downloadMETEOCLIMATICstationlist}). By default, returns the list of
#' stations in Catalonia.
#' @return Function \code{downloadMETEOCLIMATICstationlist} returns a
#' \code{SpatialPointsDataFrame} object containing the list of
#' Meteoclimatic weather stations for which data is available based on the
#' station_id codes provided.
#'
#' Function \code{downloadMETEOCLIMATICcurrentday} downloads recent weather
#' (for the current day) from all currently available stations and returns a
#' \code{SpatialPointsDataFrame} object with observations. Only
#' accumulated precipitation, maximum and minimum temperature and relative
#' humidity are returned.
#' @note Since ver. 1.0.1, weather data download functions included in
#' \code{meteoland} make internal calls to functions in package
#' \code{meteospain}. For an enhanced flexibility, users are recommended to
#' call functions in \code{meteospain} themselves, and then to use function
#' \code{\link{reshapemeteospain}} to generate data suitable for
#' \code{meteoland}.
#' @author Víctor Granda, EMF-CREAF
#'
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
#' @references Meteoclimatic should be acknowledged as author of information
#' when using this data.
#' @export
downloadMETEOCLIMATICcurrentday <- function(station_id = "ESCAT") {
  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "downloadMETEOCLIMATICcurrentday()", with = "meteospain::get_meteo_from()",
    details = "Weather download functions are now provided by `meteospain` package"
  )
  # api_options <- meteospain::meteoclimatic_options(stations = station_id, resolution = "current_day")
  # data_ms <- meteospain::get_meteo_from("meteoclimatic", api_options)
  # data_sp <- as(data_ms, "Spatial")
  # data_sp@data <- data_sp@data[,c("station_id", "station_name", "min_temperature", "max_temperature", "min_relative_humidity",
  #                                 "max_relative_humidity", "precipitation")]
  # names(data_sp@data)<-c("ID", "name", "MinimumTemperature", "MaximumTemperature", "MinimumRelativeHumidity",
  #                        "MaximumRelativeHumidity", "Precipitation")
  # return(data_sp)
}
