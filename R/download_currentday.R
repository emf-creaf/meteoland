# Function to download daily met data, format it and save it on the disk

#### AEMET
downloadAEMETcurrentday <- function(api, daily = TRUE, verbose=TRUE){
  # # Utilitary functions
  # nonUTF8 = "\u00D1\u00C0\u00C1\u00C8\u00C9\u00D2\u00D3\u00CC\u00CD\u00DC\u00CF"
  # cname.func <- function(x){
  #   regmatches(x,gregexpr('(?<=\\n\\s{2}\\")[[:print:]]+(?=\\"\\s\\:)', x, perl = T))[[1]]
  # }
  # value.func <- function(x){
  #   value <- regmatches(x,gregexpr(paste0("(?<=\\:\\s)([[:print:]]|[",nonUTF8,"])*(?=\\n)"), x, perl = T))[[1]]
  #   value <- gsub('\\",', "", value)
  #   value <- gsub('\\"', "", value)
  #   value <- gsub(',', "", value)
  # }
  # 
  # 
  # # set options
  # h = new_handle()
  # handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  # handle_setopt(h, ssl_verifypeer=FALSE)
  # handle_setopt(h, customrequest="GET")
  # 
  # url <- "https://opendata.aemet.es/opendata/api/observacion/convencional/todas"
  # # get data url
  # urldata_raw <- curl_fetch_memory(url, h)$content
  # urldata_string <- value.func(rawToChar(urldata_raw))
  # 
  # if(urldata_string[2]=="401"){
  #   stop("Invalid API key. (API keys are valid for 3 months.)")
  # }
  # 
  # urldata <- urldata_string[3]
  # 
  # if(verbose)cat("Downloading hourly data from all available stations")
  # data_raw <- curl_fetch_memory(urldata, h)$content
  # 
  # if(verbose)cat("\nFormating data")
  # data_string <- rawToChar(data_raw)
  # #Add local encoding information to data_string
  # # Encoding(data_string) <-"latin1"
  # enclocal <- l10n_info()
  # if(enclocal[[2]]) Encoding(data_string) <-"UTF-8"
  # else if(enclocal[[3]]) Encoding(data_string) <-"latin1"
  # data_string <- strsplit(data_string,"}\\,\\s{1}\\{")[[1]]
  # cname <- lapply(data_string,FUN = cname.func)
  # value <- lapply(data_string,FUN = value.func)
  # value <- mapply(FUN = function(x,y){names(x) <- y;return(x)}, x = value,y = cname)
  # unique_cname <- cname[[which.max(sapply(cname,FUN = length))]]
  # value <- mapply(FUN = function(x,y){x[y]}, x = value, y = list(unique_cname))
  # 
  # data_df <- data.frame(matrix(t(value), ncol = length(unique_cname), dimnames = list(NULL, unique_cname)),
  #                       stringsAsFactors = F)
  
  if(verbose)cat("Downloading hourly data from all available stations")
  apidest = "/api/observacion/convencional/todas"
  data_df = .get_data_aemet(apidest, api)
  
  if(verbose)cat("\nFormating data")
  varnames <-c("idema", "lon","lat", "ubi", "alt", "fint", "ta", "tamin", "tamax",  "prec", "hr", "dv", "vv")
  data_df <- data_df[,varnames]
  numvar <- c("lon","lat","alt","ta", "tamin", "tamax",  "prec", "hr", "dv", "vv")
  data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
  data_df$fint <- as.POSIXlt(sub("T", " ",data_df$fint), format = "%Y-%m-%d %H:%M:%S")
  
  if(daily){
    if(verbose)cat("\nAggregating hourly data to 24h-scale\n")
    options(warn=-1)
    data_agg <- aggregate(data_df[,numvar],list(idema = data_df$idema, ubi = data_df$ubi), 
                          function(x){mean<-mean(x,na.rm=T);min<-min(x,na.rm=T);max<-max(x,na.rm=T);sum<-sum(x,na.rm=T)
                          return(c(mean=mean,min=min,max=max,sum=sum))})
    # wind direction
    dv_agg <- aggregate(list(dv = data_df$dv),list(idema = data_df$idema, ubi = data_df$ubi),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })
    options(warn=0)
    data_df <- data.frame(ID = as.character(data_agg$idema), name = data_agg$ubi, 
                          long = data_agg$lon[,"mean"],lat = data_agg$lat[,"mean"], elevation = data_agg$alt[,"mean"],
                          MeanTemperature = data_agg$ta[,"mean"], MinTemperature = data_agg$ta[,"min"], MaxTemperature = data_agg$ta[,"max"],
                          Precipitation = data_agg$prec[,"sum"], WindSpeed = data_agg$vv[,"mean"], WindDirection = dv_agg$dv,
                          MeanRelativeHumidity = data_agg$hr[,"mean"], MinRelativeHumidity = data_agg$hr[,"min"], MaxRelativeHumidity = data_agg$hr[,"max"])

    data_df <- as.data.frame(lapply(data_df,function(x){
      x. <- x
      if(is.numeric(x.))x.[is.nan(x.)|is.infinite(x.)] <- NA
      return(x.)
      }))
    
    data_sp <- SpatialPointsDataFrame(coords = data_df[,c("long", "lat")],
                                      data = data_df[,which(!colnames(data_df) %in% c("long", "lat", "name", "ID"))],
                                      proj4string = CRS("+proj=longlat"))
    row.names(data_sp) <- data_df$ID
    return(data_sp)
  }else{
    if(verbose)cat("\nHourly results are returned\n")
    colnames(data_df) <- c("ID", "long", "lat", "name", "elevation", "date", 
                           "MeanTemperature", "MinTemperature", "MaxTemperature",
                           "Precipitation", "MeanRelativeHumidity", "WindDirection", "WindSpeed")
    return(data_df)
  }
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
                                      proj4string = CRS("+proj=longlat"))
    row.names(data_sp) <- data_df$ID
    return(data_sp)
  }else{
    if(verbose)cat("\nHourly results are returned\n")
    return(data)
  }
}


#### MeteoGalicia
downloadMGcurrentday <- function(station_id=NULL, daily = TRUE, verbose = TRUE) {
  url_base <- "http://servizos.meteogalicia.es/rss/observacion/ultimosHorariosEstacions.action?numHoras=24"
  idpar <- c(MeanTemperature = "TA_AVG_1.5m", 
             MinTemperature = "TA_MIN_1.5m", 
             MaxTemperature = "TA_MAX_1.5m", 
             MeanRelativeHumidity = "HR_AVG_1.5m", 
             Precipitation = "PP_SUM_1.5m", 
             WindSpeed = "VV_AVG_2m")
  if(!is.null(station_id)) {
    if(verbose) cat(paste0("Downloading hourly data from: ", paste(station_id,collapse = ","),"...\n"))
    url <- paste0(url_base,"&idEst=",paste(station_id,collapse = ","),"&idParam=",paste(idpar,collapse=","))
  } else {
    if(verbose)cat("Downloading hourly data from all available stations...\n")
    url <- paste0(url_base,"&idParam=",paste(idpar,collapse=","))
  }
  data_df <- jsonlite::fromJSON(txt=url)[[1]]
  
  if(verbose) cat(paste0("Arranging hourly data...\n"))
  ids <- data_df[["idEstacion"]]
  stationNames <- data_df[["estacion"]]
  instantes <- data_df[["listaInstantes"]]
  df_all = NULL
  for(i in 1:length(instantes)) {
    lec_i = instantes[[i]]$instanteLecturaUTC
    med_i = instantes[[i]]$listaMedidas
    df_i = data.frame(ID = rep(ids[i], length(lec_i)),
                    Time = as.POSIXlt(sub("T", " ",lec_i), format = "%Y-%m-%d %H:%M:%S"))
    for(var in names(idpar)) {
      df_i[[var]] = NA
      for(h in 1:length(med_i)) {
        med_ih = med_i[[h]]
        if(idpar[[var]] %in% med_ih$codigoParametro) df_i[h,var] <- mean(med_ih[med_ih$codigoParametro==idpar[[var]],"valor"], na.rm=T)
      }
    }
    if(is.null(df_all)) df_all = df_i
    else df_all = rbind(df_all, df_i)
  }
  if(daily) {
    if(verbose)cat("Downloading station info...\n")
    MGstation_sp = downloadMGstationlist()
    if(verbose)cat("Aggregating to daily scale...\n")
    MGstation_sp<- MGstation_sp[MGstation_sp$ID %in% unique(df_all$ID),]
    data_agg <- aggregate(df_all[,names(idpar)],list(ID = df_all$ID), 
                          function(x){
                            if(sum(is.na(x))<length(x)) {
                              mean<-mean(x,na.rm=T)
                              min<-min(x,na.rm=T)
                              max<-max(x,na.rm=T)
                              sum<-sum(x,na.rm=T)
                              return(c(mean=mean,min=min,max=max,sum=sum))
                            } 
                            return(c(mean=NA, min=NA, max=NA, sum=NA))})
    rownms = as.character(data_agg$ID)
    data_df <- data.frame(ID = data_agg$ID, name = MGstation_sp@data[rownms,"name"], 
                          lon = MGstation_sp@data[rownms,"lon"],
                          lat = MGstation_sp@data[rownms,"lat"], 
                          elevation = MGstation_sp@data[rownms,"elevation"],
                          MeanTemperature = data_agg[["MeanTemperature"]][ ,"mean"], 
                          MinTemperature = data_agg[["MinTemperature"]][ ,"min"], 
                          MaxTemperature = data_agg[["MaxTemperature"]][ ,"max"],
                          Precipitation = data_agg[["Precipitation"]][ ,"sum"], 
                          MeanRelativeHumidity = data_agg[["MeanRelativeHumidity"]][ ,"mean"], 
                          MinRelativeHumidity = data_agg[["MeanRelativeHumidity"]][ ,"min"], 
                          MaxRelativeHumidity = data_agg[["MeanRelativeHumidity"]][ ,"max"],
                          WindSpeed = data_agg[["WindSpeed"]][ ,"mean"])
    data_sp = SpatialPointsDataFrame(SpatialPoints(MGstation_sp@coords[rownms,], MGstation_sp@proj4string), data_df)
    return(data_sp)
  }
  if(verbose)cat("\nHourly results are returned\n")
  return(df_all)
}

#### Meteoclimatic
downloadMETEOCLIMATICcurrentday <- function(station_id = "ESCAT") {
  
  # Stations coords extraction ----------------------------------------------------------------------------
  # We use the stationlist function for meteoclimatic, but no spatial format
  meteoclimatic_stations <- downloadMETEOCLIMATICstationlist(station_id, spatial = FALSE)
  
  # Station meteo data extraction -------------------------------------------------------------------------
  # In this case, we need to access the data feed, instead of the stations feed. But the process is really
  # similar to the ine in downloadMETEOCLIMATICstationlist.
  data_xml_source <- paste0("http://meteoclimatic.com/feed/xml/", station_id)
  meteoclimatic_data <- data.frame()
  
  for (station in data_xml_source) {
    data_xml_body <- xml2::read_xml(station)
    # But, here extracting the data is not as easy as before. Variables (tmin, tmax, precipitation...) can
    # be missing in some stations, and extracting the info with xml_find_all does not includes NAs. So, we are
    # gonna need to be a little creative with this one (check .safe_xml_find helper)
    nodes <- xml2::xml_path(xml2::xml_find_all(data_xml_body, '//meteodata/stations/station'))
    station_data <- data.frame(
      station_id = sapply(nodes, .safe_xml_find, data = data_xml_body, extra_path = '/id', transform_function = xml2::xml_text),
      MaxTemperature = sapply(nodes, .safe_xml_find, data = data_xml_body, extra_path = '/stationdata/temperature/max', transform_function = xml2::xml_double),
      MinTemperature = sapply(nodes, .safe_xml_find, data = data_xml_body, extra_path = '/stationdata/temperature/min', transform_function = xml2::xml_double),
      MaxRelativeHumidity = sapply(nodes, .safe_xml_find, data = data_xml_body, extra_path = '/stationdata/humidity/max', transform_function = xml2::xml_double),
      MinRelativeHumidity = sapply(nodes, .safe_xml_find, data = data_xml_body, extra_path = '/stationdata/humidity/min', transform_function = xml2::xml_double),
      Precipitation = sapply(nodes, .safe_xml_find, data = data_xml_body, extra_path = '/stationdata/rain/total', transform_function = xml2::xml_double)
    )
    meteoclimatic_data <- rbind(meteoclimatic_data, station_data)
  }
  
  # Final data --------------------------------------------------------------------------------------------
  # After obtaining the data, we need to join the stations info and create the spatial object to return
  all_data <- merge(meteoclimatic_stations, meteoclimatic_data, by = 'station_id')
  # And now the spatial object. To be consistent with other meteoland functions, the data must have only the
  # climatic variables with station station_id as rownames and coords must be in WGS84 latlong
  rownames(all_data) <- all_data$station_id
  all_data$station_id <- NULL
  all_data$name <- NULL
  sp::coordinates(all_data) <- ~long+lat
  sp::proj4string(all_data) <- sp::CRS("+proj=longlat")
  
  return(all_data)
}