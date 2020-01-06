# Function to download daily met data, format it and save it on the disk

### AEMET
downloadAEMEThistorical <- function(api, dates, station_id, export = FALSE, exportDir = getwd(),
                                exportFormat = "meteoland/txt",
                                metadataFile = "MP.txt", verbose=TRUE){
  # nonUTF8 = "\u00D1\u00C0\u00C1\u00C8\u00C9\u00D2\u00D3\u00CC\u00CD\u00DC\u00CF"
  # cname.func <- function(x){
  #   regmatches(x,gregexpr('(?<=\\n\\s{2}\\")[[:print:]]+(?=\\"\\s\\:)', x, perl = T))[[1]]
  # }
  # value.func <- function(x){
  #   value <- regmatches(x,gregexpr(paste0("(?<=\\:\\s)([[:print:]]|[",nonUTF8,"])*(?=\\n)"), x, perl = T))[[1]]
  #   value <- gsub('\\",', "", value)
  #   value <- gsub('\\"', "", value)
  #   value <- gsub(',', ".", value)
  # }
  # 
  # # dates must be a vector containing dates written as "yyyy-mm-dd"
  # # station_id is the id code of the wanted AEMET station
  # 
  # # set options
  # h = new_handle()
  # handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  # handle_setopt(h, ssl_verifypeer=FALSE)
  # handle_setopt(h, customrequest="GET")
  # 
  ## Metadata
  station_list = downloadAEMEThistoricalstationlist(api)
  
  if(sum(station_id %in% row.names(station_list)) != length(station_id)){
    warning(paste("The metadata for the following station(s) was not found:\n", 
                  paste(station_id[!station_id %in% row.names(station_list)],collapse=", "), "\n",sep=""),
            immediate. = TRUE)
  }
  
  station_id = station_id[station_id %in% row.names(station_list)]
  station_list = station_list[station_id,]
  points = as(station_list,"SpatialPoints")
  dfout = station_list@data
  
  npoints = length(station_id)
  ndays = length(dates)
  if(verbose) cat(paste0("\n  Downloading data for ", npoints, " station(s) and ", ndays," day(s).\n"))
  
  ## Output data frame meta data
  dfout$dir = as.character(rep(exportDir, npoints)) 
  dfout$filename = as.character(paste(station_id,".txt",sep=""))
  rownames(dfout) = station_id

  ## Met data
  intlength = 31 #maximum range length

  # convert dates into 31 days interval initial date and end date covering the specified period
  steps_ini <- seq(1,length(dates), by = intlength)
  steps_fin <- pmin(steps_ini+intlength-1, length(dates)) # does not allow holes in the date sequence
  dates_seq <- list(ini = dates[steps_ini], fin = dates[steps_fin])
  
  # # express dates as url
  # url_header <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/"
  # station_url <- rep(station_id, each = length(steps_ini))
  # url <- matrix(
  #   paste(url_header, "fechaini/", 
  #         dates_seq$ini, "T00%3A00%3A00UTC/fechafin/", 
  #         dates_seq$fin, "T00%3A00%3A00UTC/estacion/", 
  #         station_url, sep = ""),
  # ncol = length(station_id), dimnames = list(NULL, station_id))
  # # send request and format the output
  # value_list <- list()
  # # cname_list <- list()
  # 
  # for(j in 1:ncol(url)){
  #   if(verbose) cat(paste("\n  Downloading data for station '",station_id[j],"' (",j,"/",npoints,")\n",sep=""))
  #   if(verbose) pb = txtProgressBar(0, max=nrow(url), style=3)
  #   for(i in 1:nrow(url)){
  #     urldatos_raw <- curl_fetch_memory(url[i,j], h)
  #     data_string = rawToChar(urldatos_raw$content)
  #     urldatos_string <- value.func(data_string)
  #     
  #     if(urldatos_string[2]=="429"){
  #       cat("\n  The number of downloads per minute on the AEMET server is limited. Please wait.")
  #       for(t in 1:10){(Sys.sleep(6));cat(".");if(t == 10)cat("\n")}
  #       urldatos_raw <- curl_fetch_memory(url[i,j], h)
  #       urldatos_string <- value.func(rawToChar(urldatos_raw$content))
  #     }
  #     
  #     if(urldatos_string[2]=="404"){
  #       value = NA
  #     }else{
  #       urldatos <- urldatos_string[3]
  #       data_raw <- curl_fetch_memory(urldatos, h)
  #       
  #       data_string <- rawToChar(data_raw$content)
  #       Encoding(data_string) <-"latin1"
  #       data_string <- strsplit(data_string,"}\\,\\s{1}\\{")[[1]]
  #       cname <- lapply(data_string,FUN = cname.func)
  #       # cname_list <- c(cname_list,cname)
  #       value <- lapply(data_string,FUN = value.func)
  #       value <- mapply(FUN = function(x,y){names(x) <- y;return(x)}, x = value,y = cname, SIMPLIFY = F)
  #     }
  #     value_list <- c(value_list,value)
  #     
  #     if(verbose) setTxtProgressBar(pb,i)
  #   }
  # }
  # cat("\n")
  # varnames <- c("indicativo", "fecha", "prec", "tmed", "tmin", "tmax", "dir", "velmedia", "sol")
  # value <- mapply(FUN = function(x,y){x[y]}, x = value_list, y = list(varnames))
  # data_df <- data.frame(matrix(t(value), ncol = length(varnames), dimnames = list(NULL, varnames)),
  #                       stringsAsFactors = F)
  # data_df <- data_df[!is.na(data_df$fecha),]
  
  # numvar <- c("prec", "tmed", "tmin", "tmax", "dir", "velmedia", "sol")
  # options(warn = -1) # some residual character strings ("Ip") raise NA values when converted to numeric
  # data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
  # 
  # options(warn = 0) #
  # data_df$fecha <- as.Date(data_df$fecha)
  # colnames(data_df) <- c("ID", "Date", "Precipitation", "MeanTemperature", "MinTemperature", "MaxTemperature",
  #                        "WindDirection", "WindSpeed", "Radiation")
  # data_df$WindDirection[data_df$WindDirection == 99] <- NA
  # data_df$WindDirection <- data_df$WindDirection*10 # The raw data are given in degrees/10
  # # data_df$Radiation <- NA
  # # data_df$MeanRelativeHumidity <- NA
  # # data_df$MinRelativeHumidity <- NA
  # # data_df$MaxRelativeHumidity <- NA
  # 
  # stationfound <- station_id %in% data_df$ID
  # 
  url_header <- "/api/valores/climatologicos/diarios/datos/"
  station_url <- rep(station_id, each = length(steps_ini))
  url <- matrix(
    paste(url_header, "fechaini/", 
          dates_seq$ini, "T00%3A00%3A00UTC/fechafin/", 
          dates_seq$fin, "T00%3A00%3A00UTC/estacion/", 
          station_url, sep = ""),
    ncol = length(station_id), dimnames = list(NULL, station_id))
  
  varnames <- c("indicativo", "fecha", "prec", "tmed", "tmin", "tmax", "dir", "velmedia", "sol")
  data_df = NULL
  for(j in 1:ncol(url)){
    if(verbose) cat(paste("\n  Downloading data for station '",station_id[j],"' (",j,"/",npoints,")\n",sep=""))
    if(verbose) pb = txtProgressBar(0, max=nrow(url), style=3)
    for(i in 1:nrow(url)){
      df1 = .get_data_aemet(url[i,j], api)
      if(!is.null(df1)) {
        for(var in varnames) if(!(var %in% names(df1))) df1[var] = NA
        df1 = df1[varnames]
        if(is.null(data_df)) data_df = df1
        else data_df = rbind(data_df, df1)
      }
      if(verbose) setTxtProgressBar(pb,i)
    }
  }
  cat("\n")
  

  numvar <- c("prec", "tmed", "tmin", "tmax", "dir", "velmedia", "sol")
  options(warn = -1) # some residual character strings ("Ip") raise NA values when converted to numeric
  data_df[,numvar] <- sapply(data_df[,numvar],function(x) {gsub(',', ".", x)})
  data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
  options(warn = 0) #
  data_df$fecha <- as.Date(data_df$fecha)
  colnames(data_df) <- c("ID", "Date", "Precipitation", "MeanTemperature", "MinTemperature", "MaxTemperature",
                         "WindDirection", "WindSpeed", "SunshineHours")
  data_df$WindDirection[data_df$WindDirection == 99] <- NA
  data_df$WindDirection <- data_df$WindDirection*10 # The raw data are given in degrees/10
  # data_df$Radiation <- NA
  # data_df$MeanRelativeHumidity <- NA
  # data_df$MinRelativeHumidity <- NA
  # data_df$MaxRelativeHumidity <- NA
  
  stationfound <- station_id %in% data_df$ID
  
  data_list <- by(data_df,list(ID=data_df$ID),FUN = function(x){
    row.names(x) <- x$Date
    x <- x[as.character(dates),]
    row.names(x) <- dates
    x <- x[!colnames(x) %in% c("ID", "Date")]
  })
  data_list <-c(data_list)
  
  
  # Export the data
  dfout <- dfout[stationfound,]
  points <- points[stationfound,]
  
  if(sum(stationfound)==0){
    warning(paste("\nNo data found."))
    if(!export){return(NA)}
  }else{
    if(sum(!stationfound)>0){
    warning(paste("\nNo data found for the following station(s): \n  ",
                                        paste(station_id[!stationfound], collapse=", "), ".\n", sep=""))
    }
    if(export){
      for(i in 1:nrow(dfout)){
        if(dfout$dir[i]!="") {
          f = paste(dfout$dir[i],dfout$filename[i], sep="/")
        }else {
          f = dfout$filename[i]
        }
        writemeteorologypoint(data_list[[i]], f, exportFormat)
        if(verbose) cat(paste("\n  File written to ",f, "\n", sep=""))
        if(exportDir!=""){
          f = paste(exportDir,metadataFile, sep="/")
        }else{f = metadataFile}
        spdf = SpatialPointsDataFrame(points, dfout)
        write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
      }
    }else{
      return(SpatialPointsMeteorology(points = points, 
                                      data = data_list, 
                                      dates = dates))
    }
  }
    
  # invisible(spdf)
}



#### SMC

# download the met data
downloadSMChistorical <- function(api, dates, station_id=NULL, variable_code=NULL, 
                                  export = FALSE, exportDir = getwd(), exportFormat = "meteoland/txt", metadataFile = "MP.txt",
                                  verbose=TRUE){

  # defaults to variables required in meteoland
  if(is.null(variable_code)) {
    variable_code <- c(1000, 1001, 1002, 1100, 1101, 1102, 1300, 1400, 1505,1511)
    daily_meteoland = TRUE
  }

  if(verbose)cat("Downloading daily data from all available stations\n")
  dates_round <- regmatches(dates,regexpr("[[:digit:]]{4}-[[:digit:]]{2}", dates))
  dates_round <- unique(dates_round)
  dates_split <- lapply(dates_round, function(x)strsplit(x, split = "-")[[1]])
  
  # download variable per variable
  for(i in 1:length(variable_code)){
    
    data_i <- data.frame()
    for(j in 1:length(dates_split)){
      apidest <- paste0("/variables/estadistics/diaris/", variable_code[i], "?any=", dates_split[[j]][1], 
                       "&mes=", dates_split[[j]][2])

      data_list <- .get_data_smc(apidest, api)
      # data_list$variables <- sapply(data_list$variables, FUN = function(x)x$lectures)
      
      data_j <- data.frame()
      for(k in 1:length(data_list$codiEstacio)){
        data_k <- data_list$valors[[k]][,c("data", "valor")]
        data_k$ID <- data_list$codiEstacio[[k]]
        # data_k$variable_code <- data_list$codiVariable[[k]]
        data_j <- rbind(data_j,data_k)
      }
      data_i <- rbind(data_i,data_j)
    }
    
  colnames(data_i)[1:2] <- c("date", as.character(variable_code[i]))
  if(i == 1) {data <- data_i}else{data <- merge(data, data_i, all=T)}
  }
  
  # data$date <- sub("T", " ", data$date)
  data$date <- sub("Z", "", data$date)
  data$date <- as.Date(data$date)
  data <- data[data$date %in% as.Date(dates),]
  
  #Subset station data if required
  if(!is.null(station_id)) {
    data <- data[data$ID %in% station_id,]
  }
  
  if(daily_meteoland){
    if(verbose)cat("\nDownloading station info\n")
    SMCstation_sp = downloadSMCstationlist(api)
    
    if(verbose)cat("\nFormating data as SpatialMeteorologyPoints\n")
    data_df <- data.frame(ID = data$ID, name = SMCstation_sp@data[data$ID,"name"], 
                          long = SMCstation_sp@coords[data$ID,"long"],
                          lat = SMCstation_sp@coords[data$ID,"lat"], 
                          elevation = SMCstation_sp@data[data$ID,"elevation"],
                          date = data[ ,"date"],
                          MeanTemperature = data[ ,"1000"], 
                          MinTemperature = data[ ,"1002"], 
                          MaxTemperature = data[ ,"1001"],
                          Precipitation = data[ ,"1300"], 
                          WindSpeed = data[ ,"1505"], 
                          WindDirection = data[ ,"1511"],
                          MeanRelativeHumidity = data[ ,"1100"], 
                          MinRelativeHumidity = data[ ,"1102"], 
                          MaxRelativeHumidity = data[ ,"1101"],
                          Radiation = data[ ,"1400"])
    
    data_df <- as.data.frame(lapply(data_df,function(x){
      x. <- x
      if(is.numeric(x.))x.[is.nan(x.)|is.infinite(x.)] <- NA
      return(x.)
    }))
    
    vars <- colnames(data_df)
    vars <- vars[!vars %in% c("ID","name","long","lat","elevation", "date")]
    ID <- unique(data_df$ID)
    data_list <- list()
    coords <- matrix(nrow=0, ncol=2)
    elevation<-numeric(0)
    for(i in 1:length(ID)){
      seli = data$ID == ID[i]
      first = which(seli)[1]
      dfi <- data.frame(matrix(NA, nrow=length(dates), ncol=length(vars)),
                        row.names = as.character(dates))
      names(dfi) <- vars
      dfi[as.character(data_df[seli, "date"]),] <- data_df[seli,vars]
      coords = rbind(coords, data_df[first,c("long", "lat")])
      elevation = c(elevation, data_df[first, "elevation"])
      data_list[[i]] <- dfi
    }
    names(data_list) <- ID
    rownames(coords) <- ID
    data_sp <- SpatialPoints(coords = coords, proj4string = CRS("+proj=longlat"))
    
    npoints <- length(data_sp)
    data_spm <- SpatialPointsMeteorology(points = data_sp,
                                         data = data_list, 
                                         dates = dates)
    
    dfout = data.frame(coords, elevation = elevation, row.names = ID)
    if(export){
      
      ## Output data frame meta data
      dfout$dir = as.character(rep(exportDir, npoints)) 
      dfout$filename = as.character(paste(ID,".txt",sep=""))

      for(i in 1:nrow(dfout)){
        if(dfout$dir[i]!="") {
          f = paste(dfout$dir[i],dfout$filename[i], sep="/")
        }else {
          f = dfout$filename[i]
        }
        writemeteorologypoint(data_list[[i]], f, exportFormat)
        if(verbose) cat(paste("\n  File written to ",f, "\n", sep=""))
        if(exportDir!=""){
          f = paste(exportDir,metadataFile, sep="/")
        }else{f = metadataFile}
        write.table(dfout,file= f,sep="\t", quote=FALSE)
      }
    } else{
      return(data_spm)
    }
  } else {
    if(verbose)cat("\nNon-formated results are returned\n")
    colsel <- !colnames(data) %in% c("date", "ID")
    colnames(data)[colsel] <- SMChistvarcodes[colnames(data)[colsel], "nom"]
    return(data)
  }
}

#### MeteoGalicia
downloadMGhistorical <- function(date_from, date_to, station_id = NULL, verbose=TRUE) {
  url_base <- "http://servizos.meteogalicia.es/rss/observacion/datosDiariosEstacionsMeteo.action?"
  idpar <- c(MeanTemperature = "TA_AVG_1.5m", 
             MinTemperature = "TA_MIN_1.5m", 
             MaxTemperature = "TA_MAX_1.5m", 
             MeanRelativeHumidity = "HR_AVG_1.5m", 
             MinRelativeHumidity = "HR_MIN_1.5m", 
             MaxRelativeHumidity = "HR_MAX_1.5m", 
             Precipitation = "PP_SUM_1.5m", 
             WindSpeed = "VV_AVG_2m",
             WindDirection = "DVP_MODA_2m",
             Radiation = "IRD_SUM_1.5m",
             PET = "ET0_SUM_1.5m")
  date_from = format(as.Date(date_from),"%d/%m/%Y")
  date_to = format(as.Date(date_to),"%d/%m/%Y")
  if(!is.null(station_id)) {
    if(verbose) cat(paste0("Downloading historic daily data from: ", paste(station_id,collapse = ","),"...\n"))
    url <- paste0(url_base,"idEst=",paste(station_id,collapse = ","),"&idParam=",paste(idpar,collapse=","), 
                  "&dataIni=",date_from,"&dataFin=",date_to)
  } else {
    if(verbose)cat("Downloading historic daily data from all available stations...\n")
    url <- paste0(url_base,"idParam=",paste(idpar,collapse=","),
                  "&dataIni=",date_from,"&dataFin=",date_to)
  }
  data_df <- jsonlite::fromJSON(txt=url)[[1]]

  if(verbose) cat(paste0("Arranging station data...\n"))
  dates = as.Date(data_df$data)
  #get unique station codes
  if(is.null(station_id)) {
    station_id = character()
    for(i in 1:length(data_df$listaEstacions)) {
      station_id = c(station_id, data_df$listaEstacions[[i]]$idEstacion)
    }
    station_id = unique(station_id)
  }
  #get station coordinates
  cc = matrix(nrow=length(station_id), ncol=2)
  colnames(cc) <- c("utmx", "utmy")
  rownames(cc) <- station_id
  # stdata = data.frame(ID = station_id,
  #                   name = NA,
  #                   municipality = NA,
  #                   province = NA)
  for(i in 1:length(station_id)) {
    for(j in 1:length(data_df$listaEstacions)) {
      w = which(data_df$listaEstacions[[j]]$idEstacion==station_id[i])
      if(length(w)==1) {
        # stdata[i,"name"] = data_df$listaEstacions[[j]]$estacion
        # stdata[i,"municipality"] = data_df$listaEstacions[[j]]$concello
        # stdata[i,"province"] = data_df$listaEstacions[[j]]$provincia
        cc[i,1] = as.numeric(data_df$listaEstacions[[j]]$utmx[w])
        cc[i,2] = as.numeric(data_df$listaEstacions[[j]]$utmy[w])
      }
    }
  }
  if(verbose) cat(paste0("Arranging daily weather data...\n"))
  #get meteo data
  data_vec = vector("list", length(station_id))
  names(data_vec)<-as.character(station_id)
  for(i in 1:length(station_id)) {
    df_i = data.frame(DOY = as.POSIXlt(dates)$yday+1, row.names=dates)
    df_i[names(idpar)] = NA
    for(j in 1:length(data_df$listaEstacions)) {
      w = which(data_df$listaEstacions[[j]]$idEstacion==station_id[i]) #Find position of this station
      if(length(w)==1) {
        med_ij = data_df$listaEstacions[[j]]$listaMedidas[[w]]
        for(var in names(idpar)) {
          if(idpar[[var]] %in% med_ij$codigoParametro) df_i[j,var] <- mean(med_ij[med_ij$codigoParametro==idpar[[var]],"valor"], na.rm=T)
        }
      }
    } 
    df_i$Radiation = df_i$Radiation/100 # From 10kJ to MJ
    df_i[df_i==-9999] = NA # Change missing values
    data_vec[[i]] = df_i
  }
  
  return(SpatialPointsMeteorology(SpatialPoints(cc, CRS("+init=epsg:25829")),
                                  dates = dates,
                                  data = data_vec))
}
