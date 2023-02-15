.aggregate_coords_meteospain<-function(coords_df, data_df) {
  coords_agg <- aggregate(coords_df, list(date = as.Date(data_df$date)), 
                          function(x){
                            if(sum(is.na(x))<length(x)) {
                              return(c(mean=mean(x,na.rm=T)))
                            } 
                            return(c(mean=NA))})
  return(coords_agg)
}
.aggregate_daily_meteospain<-function(data_df) {
  weathervarnames <-c("temperature", "mean_temperature", "min_temperature", "max_temperature",  
                      "relative_humidity","mean_relative_humidity","min_relative_humidity","max_relative_humidity", 
                      "precipitation", "wind_direction", "mean_wind_direction","wind_speed","mean_wind_speed")
  weathervarnames <- weathervarnames[weathervarnames %in% names(data_df)]
  varnames <-c("timestamp","date" , "station_id", weathervarnames)
  
  data_df <- data_df[,varnames]
  df_dates = levels(as.factor(data_df$date))
  
  data_agg <- aggregate(data_df[,weathervarnames],list(date = as.Date(data_df$date)), 
                        function(x){
                          if(sum(is.na(x))<length(x)) {
                            mean<-mean(x,na.rm=T)
                            min<-min(x,na.rm=T)
                            max<-max(x,na.rm=T)
                            sum<-sum(x,na.rm=T)
                            return(c(mean=mean,min=min,max=max,sum=sum))
                          } 
                          return(c(mean=NA, min=NA, max=NA, sum=NA))})

  data_out <- data.frame(row.names = as.character(df_dates))
  data_out$MeanTemperature = NA
  data_out$MinTemperature = NA
  data_out$MaxTemperature = NA
  data_out$Precipitation = NA
  data_out$MeanRelativeHumidity = NA
  data_out$MinRelativeHumidity = NA
  data_out$MaxRelativeHumidity = NA
  data_out$WindSpeed = NA
  data_out$WindDirection = NA
  if("temperature" %in% varnames) data_out$MeanTemperature = data_agg$temperature[,"mean"]
  if("mean_temperature" %in% varnames) data_out$MeanTemperature = data_agg$mean_temperature[,"mean"]
  if("min_temperature" %in% varnames) data_out$MinTemperature = data_agg$min_temperature[,"min"]
  if("max_temperature" %in% varnames) data_out$MaxTemperature = data_agg$max_temperature[,"max"]
  if("precipitation" %in% varnames) data_out$Precipitation = data_agg$precipitation[,"sum"] 
  if("relative_humidity" %in% varnames) {
    data_out$MeanRelativeHumidity = data_agg$relative_humidity[,"mean"]
    data_out$MinRelativeHumidity = data_agg$relative_humidity[,"min"]
    data_out$MaxRelativeHumidity = data_agg$relative_humidity[,"max"]
  }
  if("mean_relative_humidity" %in% varnames) data_out$MeanRelativeHumidity = data_agg$mean_relative_humidity[,"mean"]
  if("min_relative_humidity" %in% varnames) data_out$MinRelativeHumidity = data_agg$min_relative_humidity[,"min"]
  if("max_relative_humidity" %in% varnames) data_out$MaxRelativeHumidity = data_agg$max_relative_humidity[,"max"]
  if("wind_speed" %in% varnames) data_out$WindSpeed = data_agg$wind_speed[,"mean"]
  if("mean_wind_speed" %in% varnames) data_out$WindSpeed = data_agg$mean_wind_speed[,"mean"]
  if("wind_direction" %in% varnames) {
    # wind direction
    wd_agg <- aggregate(list(wind_direction = units::drop_units(data_df$wind_direction)),
                        list(date = as.Date(data_df$date)),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })
    data_out$WindDirection = wd_agg$wind_direction
  }
  if("mean_wind_direction" %in% varnames) {
    # wind direction
    wd_agg <- aggregate(list(wind_direction = units::drop_units(data_df$mean_wind_direction)),
                        list(date = as.Date(data_df$date)),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })
    data_out$WindDirection = wd_agg$wind_direction
  }
  return(data_out)
}

#' Reshapes weather data from 'meteospain', 'worldmet' or 'weathercan'
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Reshapes weather station data acquired using the 'meteospain', 'worldmet' or
#' 'weathercan' R packages into formats useful for meteoland
#' 
#' @details
#' Note that to have precipitation included in downloads from 'worldmet' you
#' should set 'precip = TRUE' when calling function 'importNOAA'. In the case
#' of weathercan, precipitation is only provided for daily data (i.e. setting
#' 'interval="day"' when calling 'weather_dl'), whereas wind speed and relative
#' humidity are only available for hourly data (i.e., setting 'interval="hour"'
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
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{meteocomplete}}
#' @export
reshapemeteospain<-function(weather_data, output="SpatialPointsMeteorology", 
                            proj4string = NULL, complete=TRUE, verbose = TRUE) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "reshapemeteospain()", with = "meteospain2meteoland()",
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Adapting meteospain meteo output to meteoland can be done with meteospain2meteoland()"
  )
  
  output <- match.arg(output, c("SpatialPointsMeteorology", "SpatialPointsTopography", "MeteorologyInterpolationData"))

  # Filter missing coordinates  
  weather_data = weather_data[!is.na(sf::st_coordinates(weather_data$geometry)[,1]),]
  weather_data = weather_data[!is.na(sf::st_coordinates(weather_data$geometry)[,2]),]
  # Reshape as SpatialPointsDataFrame
  spdf_data <-sf::as_Spatial(weather_data)
  x <- spdf_data@data
  x$date = as.Date(x$timestamp)
  # Isolate coordinates and elevation
  coords_data <- as.data.frame(coordinates(spdf_data))
  coords_data$elev <- x$altitude
  # Split following station id
  s <- split(x, x$station_id)
  s_coords = split(coords_data, x$station_id)
  
  codes = names(s)
  nstations = length(s)
  elevation = rep(NA, nstations)
  l = vector("list", nstations)
  names(l) <- codes
  coords = data.frame(lon = rep(NA, nstations), lat = rep(NA, nstations),
                      row.names = codes)
  dates = character(0)
  
  if(verbose) {
    cat("\nParsing meteospain data...\n")
    pb = txtProgressBar(0, nstations, style = 3)
  } 
  for(i in 1:nstations) {
    if(verbose) setTxtProgressBar(pb, i)
    data_df = s[[i]]
    coords_df = s_coords[[i]]
    df_dates = levels(as.factor(data_df$date))
    dates = sort(unique(c(dates,as.character(df_dates)))) # Adds new dates if necessary
    
    coords_agg<- .aggregate_coords_meteospain(coords_df, data_df)
    coords$lon[i] = coords_agg$coords.x1[1]
    coords$lat[i] = coords_agg$coords.x2[1]
    elevation[i] = coords_agg$elev[1]
    
    data_out <-.aggregate_daily_meteospain(data_df)
    
    if(complete) data_out<-meteocomplete(data_out, 
                                         latitude = coords$lat[i],
                                         elevation = elevation[i],
                                         aspect= NA,
                                         slope = NA)
     l[[i]] <-data_out
  }
  # Complete dates with missing
  for(i in 1:nstations) {
    df <- l[[i]]
    df<-df[dates,]
    rownames(df) <- dates
    l[[i]] <- df
  }
  
  sp <- SpatialPoints(coords = coords,
                      proj4string = spdf_data@proj4string)
  if(!is.null(proj4string)) {
    if(inherits(proj4string,"character")) proj4string = CRS(proj4string)
    sp = spTransform(sp, proj4string)
    colnames(sp@coords)<-c("x","y")
    rownames(sp@bbox)<-c("x","y")
  }
  spt <- SpatialPointsTopography(sp, elevation)
  spm <- SpatialPointsMeteorology(sp, l, dates = as.Date(dates))
  if(output=="SpatialPointsMeteorology") return(spm)
  else if(output=="SpatialPointsTopography") return(spt)
  else if(output=="MeteorologyInterpolationData") {
    mid = MeteorologyInterpolationData(spm, elevation = elevation)
    return(mid)
  }
}
