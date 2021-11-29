.reshapemeteospain_current<-function(data_ms, daily = TRUE, verbose = TRUE) {
  spdf_data <-sf::as_Spatial(data_ms)
  data_df <- spdf_data@data
  # Isolate coordinates and elevation
  coords_data <- as.data.frame(coordinates(spdf_data))
  data_df$lon = coords_data$coords.x1
  data_df$lat = coords_data$coords.x2
  
  if(verbose)cat("\nFormating data")
  varnames <-c("station_id", "lon", "lat", "station_name", "altitude", "timestamp", "temperature", "min_temperature", "max_temperature",  "precipitation",
               "relative_humidity", "wind_direction", "wind_speed")
  data_df <- data_df[,varnames]
  numvar <- c("lon", "lat","altitude","temperature", "min_temperature", "max_temperature",  "precipitation", "relative_humidity", "wind_direction", "wind_speed")
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
    data_df <- data.frame(ID = as.character(data_agg$station_id), name = data_agg$station_name, 
                          long = data_agg$lon[,"mean"],lat = data_agg$lat[,"mean"], elevation = data_agg$altitude[,"mean"],
                          MeanTemperature = data_agg$temperature[,"mean"], MinTemperature = data_agg$min_temperature[,"min"], MaxTemperature = data_agg$max_temperature[,"max"],
                          Precipitation = data_agg$precipitation[,"sum"], WindSpeed = data_agg$wind_speed[,"mean"], WindDirection = dv_agg$dv,
                          MeanRelativeHumidity = data_agg$relative_humidity[,"mean"], MinRelativeHumidity = data_agg$relative_humidity[,"min"], MaxRelativeHumidity = data_agg$relative_humidity[,"max"])
    
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
    data_out <-data_df[,c("station_id", "lon", "lat", "station_name", "altitude", "timestamp",
                          "temperature", "min_temperature", "max_temperature", 
                          "precipitation", "relative_humidity", "wind_direction", "wind_speed")]
    colnames(data_out) <- c("ID", "long", "lat", "name", "elevation", "timestamp", 
                            "MeanTemperature", "MinTemperature", "MaxTemperature",
                            "Precipitation", "MeanRelativeHumidity", "WindDirection", "WindSpeed")
    return(data_out)
  }
}

#### AEMET
downloadAEMETcurrentday <- function(api, daily = TRUE, verbose=TRUE){
  if(verbose) cat("Downloading hourly data from all available stations")
  api_options <- aemet_options(resolution = 'current_day', api_key = api)
  data_ms <- get_meteo_from("aemet", api_options)
  return(.reshapemeteospain_current(data_ms, daily = daily, verbose = verbose))
}



#### SMC
# download the variables metadata
downloadSMCvarmetadata <- function(api, type = "current"){
  type = match.arg(type, c("current", "historical"))
  if(type=="current") {
    apidest <- "/variables/mesurades/metadades"
  } else if(type == "historical") {
    apidest <- "/variables/estadistics/diaris/metadades"
  }
  data <- .get_data_smc(apidest,api)
  rownames(data) <- data$codi
  return(data)
}


# download the met data
downloadSMCcurrentday <- function(api, daily_meteoland=TRUE, variable_code=NULL, station_id=NULL, date = Sys.Date(), verbose=TRUE){

  if(daily_meteoland) variable_code <- c(32, 33, 35, 36, 46, 47) 
  else if(is.null(variable_code)) stop("variable_code must be specified")
  
  variable_names = SMCvarcodes[as.character(variable_code),"nom"]
  
  if(verbose)cat("Downloading hourly data from all available stations\n")
  date_split <- strsplit(as.character(date), split = "-")[[1]]
  
  # download variable per variable
  for(i in 1:length(variable_code)){
    apidest <- paste("/variables/mesurades", variable_code[i], date_split[1], date_split[2], date_split[3], sep = "/")
    if(!is.null(station_id)){apidest<-paste0(apidest,"?codiEstacio=", station_id)}
    data_list <- .get_data_smc(apidest, api)
    data_list$variables <- sapply(data_list$variables, FUN = function(x)x$lectures)
    
    data_i <- data.frame()
    for(j in 1:length(data_list$codi)){
      data_j <- data_list$variables[[j]][,c("data", "valor")]
      data_j$ID <- data_list$codi[[j]]
      data_i <- rbind(data_i,data_j)
    }
    
    colnames(data_i) <- c("date", as.character(variable_code[i]), "ID")
    if(i == 1) {data <- data_i}else{data <- merge(data, data_i, all=T)}
  }


  
  if(verbose)cat("\nFormating data\n")
  colsel <- !colnames(data) %in% c("date", "ID")
  colnames(data)[colsel] <- SMCvarcodes[colnames(data)[colsel], "nom"]
  data$date <- sub("T", " ", data$date)
  data$date <- sub("Z", "", data$date)
  data$date <- as.POSIXlt(sub("T", " ",data$date), format = "%Y-%m-%d %H:%M")
  
  if(daily_meteoland){
    if(verbose)cat("\nDownloading station info\n")
    SMCstation_sp = downloadSMCstationlist(api, date = date)
    
    if(verbose)cat("\nAggregating hourly data to 24h-scale\n")
    options(warn=-1)
    data$date <- as.Date(data$date)
    numvar <- !colnames(data) %in% c("date", "date", "ID")
    
    data_agg <- aggregate(data[,numvar],list(ID = data$ID), 
                          function(x){mean<-mean(x,na.rm=T);min<-min(x,na.rm=T);max<-max(x,na.rm=T);sum<-sum(x,na.rm=T)
                          return(c(mean=mean,min=min,max=max,sum=sum))})
    
    # wind direction
    dv_agg <- aggregate(list(dv = data[,variable_names[6]]),
                        list(ID = data$ID),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })
    options(warn=0)
    IDmissing = data_agg$ID[!(data_agg$ID %in% row.names(SMCstation_sp@data))]
    if(length(IDmissing)>0) {
      if(verbose) warning(paste0(length(IDmissing), " stations not found in station list and observation will be removed"))
      data_agg = data_agg[!(data_agg$ID %in% IDmissing), ]
      dv_agg = dv_agg[!(dv_agg$ID %in% IDmissing),]
    }
    data_df <- data.frame(ID = data_agg$ID, name = SMCstation_sp@data[data_agg$ID,"name"], 
                          long = SMCstation_sp@coords[data_agg$ID,"long"],
                          lat = SMCstation_sp@coords[data_agg$ID,"lat"], 
                          elevation = SMCstation_sp@data[data_agg$ID,"elevation"],
                          MeanTemperature = data_agg[[variable_names[1]]][ ,"mean"], 
                          MinTemperature = data_agg[[variable_names[1]]][ ,"min"], 
                          MaxTemperature = data_agg[[variable_names[1]]][ ,"max"],
                          Precipitation = data_agg[[variable_names[3]]][ ,"sum"], 
                          WindSpeed = data_agg[[variable_names[5]]][ ,"mean"], 
                          WindDirection = dv_agg$dv,
                          MeanRelativeHumidity = data_agg[[variable_names[2]]][ ,"mean"], 
                          MinRelativeHumidity = data_agg[[variable_names[2]]][ ,"min"], 
                          MaxRelativeHumidity = data_agg[[variable_names[2]]][ ,"max"],
                          Radiation = data_agg[[variable_names[4]]][ ,"mean"])
    
    data_df <- as.data.frame(lapply(data_df,function(x){
      x. <- x
      if(is.numeric(x.))x.[is.nan(x.)|is.infinite(x.)] <- NA
      return(x.)
    }))
    data_df$Radiation = data_df$Radiation*(3600*24)/1e6 # From W/m2 to MJ/m2
    
    data_sp <- SpatialPointsDataFrame(coords = data_df[,c("long", "lat")],
                                      data = data_df[,which(!colnames(data_df) %in% c("long", "lat", "name", "ID"))],
                                      proj4string = CRS(SRS_string = "EPSG:4326"))
    row.names(data_sp) <- data_df$ID
    return(data_sp)
  }else{
    if(verbose)cat("\nHourly results are returned\n")
    return(data)
  }
}

 
#### MeteoGalicia
downloadMGcurrentday <- function(station_id=NULL, daily = TRUE, verbose = TRUE) {
  if(verbose) cat("Downloading hourly data from all available stations")
  api_options <- meteogalicia_options(resolution = 'current_day', stations = station_id)
  data_ms <- get_meteo_from("meteogalicia", api_options)
  return(.reshapemeteospain_current(data_ms, daily = daily, verbose = verbose))
}

#### Meteoclimatic
downloadMETEOCLIMATICcurrentday <- function(station_id = "ESCAT") {
  api_options <- meteoclimatic_options(stations = station_id, resolution = "current_day")
  data_ms <- get_meteo_from("meteoclimatic", api_options)
  data_sp <- as(data_ms, "Spatial")
  data_sp@data <- data_sp@data[,c("station_id", "station_name", "min_temperature", "max_temperature", "min_relative_humidity",
                                  "max_relative_humidity", "precipitation")]
  names(data_sp@data)<-c("ID", "name", "MinimumTemperature", "MaximumTemperature", "MinimumRelativeHumidity",
                         "MaximumRelativeHumidity", "Precipitation")
  return(data_sp)
}