# Function to download daily met data from AEMET, format it and save it on the disk
downloadAEMETpoints <- function(api, dates, station_id, export = FALSE, exportDir = getwd(),
                                exportFormat = "meteoland",
                                metadatafile = "MP.txt", verbose=TRUE){
  # dates must be a vector containing dates written as "yyyy-mm-dd"
  # station_id is the id code of the wanted AEMET station
  
  # set options
  h = new_handle()
  handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  handle_setopt(h, ssl_verifypeer=FALSE)
  handle_setopt(h, customrequest="GET")
  
  ## Metadata
  spdflist = downloadAEMETstationdata(api, station_id)

  if(sum(station_id %in% rownames(spdflist@data)) != length(station_id)){
    warning(paste("Could not find the following station(s):\n", paste(station_id[!station_id %in% rownames(spdflist@data)],collapse=", "), ".\n",sep=""),
            immediate. = TRUE)
  }
  station_id = station_id[station_id %in% rownames(spdflist@data)]
  spdflist = spdflist[station_id,]
  points =as(spdflist,"SpatialPoints")
  dfout = spdflist@data
  
  npoints = length(station_id)
  ndays = length(dates)
  if(verbose) cat(paste0("\n  Downloading data for ", npoints, " stations and ", ndays," days.\n"))
  
  ## Output data frame meta data
  dfout$dir = as.character(rep(exportDir, npoints)) 
  dfout$filename=as.character(paste(station_id,".txt",sep=""))
  rownames(dfout) = station_id

  stationfound = rep(FALSE, length(station_id))
  
  ## Met data
  intlength = 31 #maximum range length
  maxCallsPerMinute = 10 #Maximum calls per minute
  
  # convert dates into 31 days interval initial date and end date covering the specified period
  steps_ini <- seq(1,length(dates), by = intlength)
  steps_fin <- pmin(steps_ini+intlength-1, length(dates)) # does not allow holes in the date sequence
  dates_seq <- list(ini = dates[steps_ini], fin = dates[steps_fin])
  
  # express dates as url
  url_header <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/"
  station_url <- rep(station_id, each = length(steps_ini))
  url <- matrix(
    paste(url_header, "fechaini/", dates_seq$ini, "T00%3A00%3A00UTC/fechafin/", dates_seq$fin, "T00%3A00%3A00UTC/estacion/", station_url, sep = ""),
  ncol = length(station_id), dimnames = list(NULL, station_id))
  # send request and format the output
  data_list <- vector("list",npoints)
  names(data_list) <- colnames(url)
  
  count = 0;
  for(j in 1:ncol(url)){
    if(verbose) cat(paste("\n  Downloading data for station '",station_id[j],"' (",j,"/",npoints,")\n",sep=""))
    dfread <- matrix(NA, ncol = 7, nrow = 0, dimnames = list(NULL, c("date", "MeanTemperature", "Precipitation",
                                                       "MinTemperature", "MaxTemperature", "WindDirection", "WindSpeed")))
    t0 <- c()
    t1 <- c()
    t2 <- c()
    if(verbose) pb = txtProgressBar(0, max=nrow(url), style=3)
    for(i in 1:nrow(url)){
      if(verbose) setTxtProgressBar(pb,i)
      t0[i] <- Sys.time()
      data_raw <- curl_fetch_memory(url[i,j], h)
      urldatos <- strsplit(rawToChar(data_raw$content), split="\"")[[1]][10]
      data_raw <- curl_fetch_memory(urldatos, h)
      t1[i] <- Sys.time()
      
      data_raw <- data_raw$content[-which(data_raw$content %in% c("5b", "20", "7b", "20", "20", "22", "5d"))]
      data_raw <- strsplit(rawToChar(data_raw), split = "}")
      data_raw <- strsplit(data_raw[[1]], split = "\n")
      
      data_names <- lapply(data_raw, FUN = function(x){strsplit(x, split = ":")})
      data_names <- lapply(data_names, FUN = function(x){unlist(lapply(x, FUN = function(x){x[1]}))})
      data_names_val <- lapply(data_names, FUN = function(x){v <- which(x %in% c("fecha", "tmed", "prec", "tmin", "tmax", "dir", "velmedia")) ; names(v) <- x[v] ; return(v)})
      data_names_info <- lapply(data_names, FUN = function(x){which(x %in% c("indicativo", "nombre", "provincia", "altitud"))})[[1]]
        
      # data_info <- unlist(lapply(data_raw, FUN = function(x){strsplit(x[data_names_info], split = ":")})[[1]]) # UNUSED for now
      # data_info <- matrix(sub(data_info, pattern = ",", replacement = ""), ncol = 2, dimnames = list(NULL, c("attribute", "value")), byrow = T)
      
      data_value <- list()
      for(k in 1:length(data_names_val)){
        data_value[[k]] <- strsplit(data_raw[[k]][data_names_val[[k]][c("fecha", "tmed", "prec", "tmin", "tmax", "dir", "velmedia")]], split = ":")
      }
      data_value <- unlist(lapply(data_value, FUN = function(x){lapply(x, FUN = function(x){x[length(x)]})}))
      data_value <- unlist(lapply(data_value, FUN = function(x){
        if(!is.na(x)){
          if(substr(x, start = nchar(x), stop = nchar(x)) == ","){
            r <- substr(x, start = 1, stop = nchar(x)-1)
            return(r)
          }else(return(x))
        }else(return(NA))
      }))
      data_value <- sub(data_value, pattern = ",", replacement = ".")
      data_value <- sub(data_value, pattern = "Ip", replacement = "NA")
      data_value <- matrix(data_value, ncol = 7, byrow = T)
      
      dfread <- rbind(dfread, data_value)
      t2[i] <- Sys.time()
      count = count +1
      if(count==maxCallsPerMinute) {
        Sys.sleep(60)
        count=0
      }
    }
    
    # print(paste("Station ", colnames(url)[j],": downloading time = ", round(sum(t1-t0)/sum(t2-t0)*100), "% of total", sep =""))
    
    df <- data.frame(
      DOY =as.POSIXlt(dates)$yday,
      MeanTemperature=rep(NA, ndays),
      MinTemperature=rep(NA, ndays),
      MaxTemperature=rep(NA, ndays),
      Precipitation=rep(NA, ndays),
      MeanRelativeHumidity=rep(NA, ndays),
      MinRelativeHumidity=rep(NA, ndays),
      MaxRelativeHumidity=rep(NA, ndays),
      Radiation=rep(NA, ndays),
      WindSpeed=rep(NA, ndays),
      WindDirection=rep(NA, ndays))
    rownames(df) = as.character(dates)
    
    if(sum(!is.na(dfread[,"date"]))>0) {
      stationfound[j] = TRUE
      dfread = dfread[!is.na(dfread[,"date"]),]
      data_date <- dfread[,"date"]
      df[data_date,"MeanTemperature"] <- as.numeric(dfread[,"MeanTemperature"])
      df[data_date,"MinTemperature"] <- as.numeric(dfread[,"MinTemperature"])
      df[data_date,"MaxTemperature"] <- as.numeric(dfread[,"MaxTemperature"])
      df[data_date,"Precipitation"] <- as.numeric(dfread[,"Precipitation"])
      df[data_date,"WindDirection"] <- as.numeric(dfread[,"WindDirection"])
      df[data_date,"WindSpeed"] <- as.numeric(dfread[,"WindSpeed"])
      # Export the data
      if(export){
        if(dfout$dir[j]!="") f = paste(dfout$dir[j],dfout$filename[j], sep="/")
        else f = dfout$filename[j]
        writemeteorologypoint(df, f, exportFormat)
        if(verbose) cat(paste("\n  File written to ",f, "\n", sep=""))
        if(exportDir!="") f = paste(exportDir,metadatafile, sep="/")
        else f = metadatafile
        spdf = SpatialPointsDataFrame(points, dfout)
        write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
      } else {
        data_list[[j]] = df
      }
    } else {
      stationfound[j] = FALSE
      data_list[[j]] = df
    }

  }

  if(length(stationfound)>0) warning(paste("\nStations with missing values: \n  ",
                                        paste(station_id[!stationfound], collapse=", "), ".\n", sep=""))
  if(!export) return(SpatialPointsMeteorology(points = points[stationfound], data = data_list[stationfound], dates = dates))
  invisible(spdf)
}

