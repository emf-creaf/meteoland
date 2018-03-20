# Function to download daily met data from AEMET, format it and save it on the disk
downloadAEMEThistorical <- function(api, dates, station_id, export = FALSE, exportDir = getwd(),
                                exportFormat = "meteoland/txt",
                                metadatafile = "MP.txt", verbose=TRUE){
  nonUTF8 = "\u00D1\u00C0\u00C1\u00C8\u00C9\u00D2\u00D3\u00CC\u00CD\u00DC\u00CF"
  cname.func <- function(x){
    regmatches(x,gregexpr('(?<=\\n\\s{2}\\")[[:print:]]+(?=\\"\\s\\:)', x, perl = T))[[1]]
  }
  value.func <- function(x){
    value <- regmatches(x,gregexpr(paste0("(?<=\\:\\s)([[:print:]]|[",nonUTF8,"])*(?=\\n)"), x, perl = T))[[1]]
    value <- gsub('\\",', "", value)
    value <- gsub('\\"', "", value)
    value <- gsub(',', ".", value)
  }
  
  # dates must be a vector containing dates written as "yyyy-mm-dd"
  # station_id is the id code of the wanted AEMET station
  
  # set options
  h = new_handle()
  handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  handle_setopt(h, ssl_verifypeer=FALSE)
  handle_setopt(h, customrequest="GET")
  
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
  if(verbose) cat(paste0("\n  Downloading data for ", npoints, " stations and ", ndays," days.\n"))
  
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
  
  # express dates as url
  url_header <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/"
  station_url <- rep(station_id, each = length(steps_ini))
  url <- matrix(
    paste(url_header, "fechaini/", 
          dates_seq$ini, "T00%3A00%3A00UTC/fechafin/", 
          dates_seq$fin, "T00%3A00%3A00UTC/estacion/", 
          station_url, sep = ""),
  ncol = length(station_id), dimnames = list(NULL, station_id))
  # send request and format the output
  value_list <- list()
  # cname_list <- list()
  
  for(j in 1:ncol(url)){
    if(verbose) cat(paste("\n  Downloading data for station '",station_id[j],"' (",j,"/",npoints,")\n",sep=""))
    if(verbose) pb = txtProgressBar(0, max=nrow(url), style=3)
    for(i in 1:nrow(url)){
      urldatos_raw <- curl_fetch_memory(url[i,j], h)
      data_string = rawToChar(urldatos_raw$content)
      urldatos_string <- value.func(data_string)
      
      if(urldatos_string[2]=="429"){
        cat("\n  The number of downloads per minute on the AEMET server is limited. Please wait.")
        for(t in 1:10){(Sys.sleep(6));cat(".");if(t == 10)cat("\n")}
        urldatos_raw <- curl_fetch_memory(url[i,j], h)
        urldatos_string <- value.func(rawToChar(urldatos_raw$content))
      }
      
      urldatos <- urldatos_string[3]
      data_raw <- curl_fetch_memory(urldatos, h)

      data_string <- rawToChar(data_raw$content)
      Encoding(data_string) <-"latin1"
      data_string <- strsplit(data_string,"}\\,\\s{1}\\{")[[1]]
      cname <- lapply(data_string,FUN = cname.func)
      # cname_list <- c(cname_list,cname)
      value <- lapply(data_string,FUN = value.func)
      value <- mapply(FUN = function(x,y){names(x) <- y;return(x)}, x = value,y = cname, SIMPLIFY = F)
      value_list <- c(value_list,value)
      
      if(verbose) setTxtProgressBar(pb,i)
    }
  }
  cat("\n")
  
  varnames <- c("indicativo", "fecha", "prec", "tmed", "tmin", "tmax", "dir", "velmedia", "sol")
  value <- mapply(FUN = function(x,y){x[y]}, x = value_list, y = list(varnames))
  data_df <- data.frame(matrix(t(value), ncol = length(varnames), dimnames = list(NULL, varnames)),
  stringsAsFactors = F)
  data_df <- data_df[!is.na(data_df$fecha),]
  numvar <- c("prec", "tmed", "tmin", "tmax", "dir", "velmedia", "sol")
  options(warn = -1) # some residual character strings ("Ip") raise NA values when converted to numeric
  data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
  options(warn = 0) #
  data_df$fecha <- as.Date(data_df$fecha)
  colnames(data_df) <- c("ID", "Date", "Precipitation", "MeanTemperature", "MinTemperature", "MaxTemperature",
                         "WindDirection", "WindSpeed", "Radiation")
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
        f = paste(exportDir,metadatafile, sep="/")
      }else{f = metadatafile}
      spdf = SpatialPointsDataFrame(points, dfout)
      write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
    }
  }
  
  
  if(sum(!stationfound)>0) warning(paste("\nNo data were found for the following station(s): \n  ",
                                        paste(station_id[!stationfound], collapse=", "), ".\n", sep=""))
  
  if(!export) return(SpatialPointsMeteorology(points = points, 
                                              data = data_list, 
                                              dates = dates))
  invisible(spdf)
}

