.reshapemeteospain_current<-function(data_ms, daily = TRUE, service = "aemet",
                                     verbose = TRUE) {
  spdf_data <-sf::as_Spatial(data_ms)
  data_df <- spdf_data@data
  # Isolate coordinates and elevation
  coords_data <- as.data.frame(coordinates(spdf_data))
  data_df$lon = coords_data$coords.x1
  data_df$lat = coords_data$coords.x2
  
  if(verbose)cat("\nFormating data")
  if(service=="meteocat") {
    varnames <-c("station_id", "lon", "lat", "station_name", "altitude", "timestamp", "temperature",  "precipitation",
                 "relative_humidity", "wind_direction", "wind_speed")
    numvar <- c("lon", "lat","altitude","temperature", "precipitation", "relative_humidity", "wind_direction", "wind_speed")
  } else {
    varnames <-c("station_id", "lon", "lat", "station_name", "altitude", "timestamp", "temperature", "min_temperature", "max_temperature",  "precipitation",
                 "relative_humidity", "wind_direction", "wind_speed")
    numvar <- c("lon", "lat","altitude","temperature", "min_temperature", "max_temperature",  "precipitation", "relative_humidity", "wind_direction", "wind_speed")
  }
  data_df <- data_df[,varnames]
  data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
  data_df$timestamp <- as.POSIXlt(sub("T", " ",data_df$timestamp), format = "%Y-%m-%d %H:%M:%S")
  
  if(daily){
    if(verbose)cat("\nAggregating hourly data to 24h-scale\n")
    options(warn=-1)
    data_agg <- aggregate(data_df[,numvar],list(station_id = data_df$station_id, station_name = data_df$station_name), 
                          function(x){mean<-mean(x,na.rm=T);min<-min(x,na.rm=T);max<-max(x,na.rm=T);sum<-sum(x,na.rm=T)
                          return(c(mean=mean,min=min,max=max,sum=sum))})
    # wind direction
    dv_agg <- aggregate(list(dv = data_df$wind_direction),list(station_id = data_df$station_id, station_name = data_df$station_name),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })
    options(warn=0)
    if(service=="meteocat") {
      data_df <- data.frame(ID = as.character(data_agg$station_id), name = data_agg$station_name, 
                            long = data_agg$lon[,"mean"],lat = data_agg$lat[,"mean"], elevation = data_agg$altitude[,"mean"],
                            MeanTemperature = data_agg$temperature[,"mean"], MinTemperature = data_agg$temperature[,"min"], MaxTemperature = data_agg$temperature[,"max"],
                            Precipitation = data_agg$precipitation[,"sum"], WindSpeed = data_agg$wind_speed[,"mean"], WindDirection = dv_agg$dv,
                            MeanRelativeHumidity = data_agg$relative_humidity[,"mean"], MinRelativeHumidity = data_agg$relative_humidity[,"min"], MaxRelativeHumidity = data_agg$relative_humidity[,"max"])
    } else {
      data_df <- data.frame(ID = as.character(data_agg$station_id), name = data_agg$station_name, 
                            long = data_agg$lon[,"mean"],lat = data_agg$lat[,"mean"], elevation = data_agg$altitude[,"mean"],
                            MeanTemperature = data_agg$temperature[,"mean"], MinTemperature = data_agg$min_temperature[,"min"], MaxTemperature = data_agg$max_temperature[,"max"],
                            Precipitation = data_agg$precipitation[,"sum"], WindSpeed = data_agg$wind_speed[,"mean"], WindDirection = dv_agg$dv,
                            MeanRelativeHumidity = data_agg$relative_humidity[,"mean"], MinRelativeHumidity = data_agg$relative_humidity[,"min"], MaxRelativeHumidity = data_agg$relative_humidity[,"max"])
    }
    
    data_df <- as.data.frame(lapply(data_df,function(x){
      x. <- x
      if(is.numeric(x.))x.[is.nan(x.)|is.infinite(x.)] <- NA
      return(x.)
    }))
    
    data_sp <- SpatialPointsDataFrame(coords = data_df[,c("long", "lat")],
                                      data = data_df[,which(!colnames(data_df) %in% c("long", "lat", "name", "ID"))],
                                      proj4string = CRS(SRS_string = "EPSG:4326"))
    row.names(data_sp) <- data_df$ID
    return(data_sp)
  }else{
    if(verbose)cat("\nHourly results are returned\n")
    if(service=="meteocat") {
      data_out <-data_df[,c("station_id", "lon", "lat", "station_name", "altitude", "timestamp",
                            "temperature", 
                            "precipitation", "relative_humidity", "wind_direction", "wind_speed")]
      colnames(data_out) <- c("ID", "long", "lat", "name", "elevation", "timestamp", 
                              "MeanTemperature", 
                              "Precipitation", "MeanRelativeHumidity", "WindDirection", "WindSpeed")
    } else {
      data_out <-data_df[,c("station_id", "lon", "lat", "station_name", "altitude", "timestamp",
                            "temperature", "min_temperature", "max_temperature", 
                            "precipitation", "relative_humidity", "wind_direction", "wind_speed")]
      colnames(data_out) <- c("ID", "long", "lat", "name", "elevation", "timestamp", 
                              "MeanTemperature", "MinTemperature", "MaxTemperature",
                              "Precipitation", "MeanRelativeHumidity", "WindDirection", "WindSpeed")
    }
    return(data_out)
  }
}

#### AEMET
downloadAEMETcurrentday <- function(api, daily = TRUE, verbose=TRUE){
  if(verbose) cat("Downloading hourly data from all available stations")
  api_options <- meteospain::aemet_options(resolution = 'current_day', api_key = api)
  data_ms <- meteospain::get_meteo_from("aemet", api_options)
  return(.reshapemeteospain_current(data_ms, daily = daily, service = "aemet", verbose = verbose))
}



#### SMC
downloadSMCcurrentday <- function(api, daily=TRUE, station_id=NULL, 
                                  date = Sys.Date(), verbose=TRUE){
  api_options <- meteospain::meteocat_options(resolution = 'hourly', api_key = api, 
                                  start_date = date, stations = station_id)
  if(verbose)cat("Downloading hourly data from all available stations\n")
  data_ms <- meteospain::get_meteo_from("meteocat", api_options)
  return(.reshapemeteospain_current(data_ms, daily = daily,  service = "meteocat",verbose = verbose))
}

 
#### MeteoGalicia
downloadMGcurrentday <- function(station_id=NULL, daily = TRUE, verbose = TRUE) {
  if(verbose) cat("Downloading hourly data from all available stations")
  api_options <- meteospain::meteogalicia_options(resolution = 'current_day', stations = station_id)
  data_ms <- meteospain::get_meteo_from("meteogalicia", api_options)
  return(.reshapemeteospain_current(data_ms, daily = daily,  service = "meteogalicia", verbose = verbose))
}

#### Meteoclimatic
downloadMETEOCLIMATICcurrentday <- function(station_id = "ESCAT") {
  api_options <- meteospain::meteoclimatic_options(stations = station_id, resolution = "current_day")
  data_ms <- meteospain::get_meteo_from("meteoclimatic", api_options)
  data_sp <- as(data_ms, "Spatial")
  data_sp@data <- data_sp@data[,c("station_id", "station_name", "min_temperature", "max_temperature", "min_relative_humidity",
                                  "max_relative_humidity", "precipitation")]
  names(data_sp@data)<-c("ID", "name", "MinimumTemperature", "MaximumTemperature", "MinimumRelativeHumidity",
                         "MaximumRelativeHumidity", "Precipitation")
  return(data_sp)
}