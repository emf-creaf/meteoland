# Function to download daily met data from AEMET, format it and save it on the disk
downloadAEMETcurrentday <- function(api, station_id, verbose=TRUE){
  # station_id is the id code of the wanted AEMET station
  
  # set options
  h = new_handle()
  handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  handle_setopt(h, ssl_verifypeer=FALSE)
  handle_setopt(h, customrequest="GET")
  
  ## Metadata
  npoints = length(station_id)
  dfout = data.frame(lon = rep(NA, npoints), lat = rep(NA, npoints), 
                     name = rep(NA, npoints), elevation = rep(NA, npoints))
  rownames(dfout) = station_id
  

  dfout$MeanTemperature = NA
  dfout$MinTemperature = NA
  dfout$MaxTemperature = NA
  dfout$Precipitation = NA
  dfout$MeanRelativeHumidity = NA
  dfout$MinRelativeHumidity = NA
  dfout$MaxRelativeHumidity = NA
  dfout$Radiation = NA
  dfout$WindSpeed = NA
  dfout$WindDirection = NA
  
  # express dates as url
  url_header <- "https://opendata.aemet.es/opendata/api/observacion/convencional/datos/estacion/"
  url <- paste(url_header,  station_id, sep = "")
  if(verbose) pb = txtProgressBar(0, max=length(url), style=3)
  for(i in 1:length(url)) {
    if(verbose) setTxtProgressBar(pb,i)
    data_raw<-curl_fetch_memory(url[i],h)
    data_raw <- data_raw$content[-which(data_raw$content %in% c("5b", "20", "7b", "20", "20", "22", "5d"))]
    data_raw <- strsplit(rawToChar(data_raw), split = c("}"))
    data_raw <- strsplit(data_raw[[1]], split = c("\n"), fixed=TRUE)
    if(data_raw[1]!="mensaje_operacional:Nohaydatosquesatisfaganesoscriterios") {
      data_raw <- data_raw[[1]]
      line_url<-as.character(data_raw[startsWith(data_raw,"datos:")])
      url_string<-substr(line_url,7, nchar(line_url)-1) #URL to get data from
      data_raw<-curl_fetch_memory(url_string,h)
      data_raw <- data_raw$content[-which(data_raw$content %in% c("5b", "20", "7b", "20", "20", "22", "5d"))]
      data_raw <- strsplit(rawToChar(data_raw), split = c("}"))
      data_raw <- strsplit(data_raw[[1]], split = c("\n"), fixed=TRUE)
      varnames = c("lon","lat", "ubi", "alt", "ta", "tamin", "tamax",  "prec", "hr", "dv", "vv")
      
      m = data.frame(matrix(NA, nrow=length(data_raw), ncol=length(varnames)))
      names(m)<-varnames
      for(k in 1:length(data_raw)) {
        data_hour = data_raw[[k]]
        eds = endsWith(data_hour,",")
        data_hour[eds] = substr(data_hour[eds],1,nchar(data_hour[eds])-1)
        var_list = strsplit(data_hour,":")
        for(j in 1:length(var_list)) {
          if(length(var_list[[j]])==2) {
            vname <- var_list[[j]][1]
            vval  <- var_list[[j]][2]
            if(vname=="lon") m[k,"lon"]  <- as.numeric(vval)
            else if(vname=="lat") m[k,"lat"]  <- as.numeric(vval)
            else if(vname=="ubi") m[k,"ubi"]  <- vval
            else if(vname=="alt") m[k,"alt"]  <- as.numeric(vval)
            else if(vname=="ta") m[k,"ta"]  <- as.numeric(vval)
            else if(vname=="tamax") m[k,"tamax"]  <- as.numeric(vval)
            else if(vname=="tamin") m[k,"tamin"]  <- as.numeric(vval)
            else if(vname=="prec") m[k,"prec"]  <- as.numeric(vval)
            else if(vname=="hr") m[k,"hr"]  <- as.numeric(vval)
            else if(vname=="dv") m[k,"dv"]  <- as.numeric(vval)
            else if(vname=="vv") m[k,"vv"]  <- as.numeric(vval)
          }
        }
      }
      
      dfout$lon[i] = m[1,"lon"]
      dfout$lat[i] = m[1,"lat"]
      dfout$name[i] = m[1,"ubi"]
      dfout$elevation[i] = m[1,"alt"]
      mtvec = m[,"ta"]
      mtvec = mtvec[!is.na(mtvec)]
      if(length(mtvec)>0) dfout$MeanTemperature[i] = mean(mtvec,na.rm=TRUE)
      mxtvec = m[,"tamax"]
      mxtvec = mxtvec[!is.na(mxtvec)]
      if(length(mxtvec)>0) dfout$MaxTemperature[i] = max(mxtvec,na.rm=TRUE)
      mntvec = m[,"tamin"]
      mntvec = mntvec[!is.na(mntvec)]
      if(length(mntvec)>0) dfout$MinTemperature[i] = min(mntvec,na.rm=TRUE)
      dfout$Precipitation[i] = sum(m[,"prec"],na.rm=TRUE)
      hrvec = m[,"hr"]
      hrvec = hrvec[hrvec>0] #Remove zero values
      hrvec = hrvec[!is.na(hrvec)]
      if(length(hrvec)>0) {
        dfout$MeanRelativeHumidity[i] = mean(hrvec,na.rm=TRUE)
        dfout$MaxRelativeHumidity[i] = max(hrvec,na.rm=TRUE)
        dfout$MinRelativeHumidity[i] = min(hrvec,na.rm=TRUE)
      }
      vvvec = m[,"vv"]
      vvvec = vvvec[vvvec>0] #Remove zero values
      vvvec = vvvec[!is.na(vvvec)]
      if(length(vvvec)>0)  dfout$WindSpeed[i] = mean(vvvec,na.rm=TRUE)
      dvvec = m[,"dv"]
      dvvec = dvvec[dvvec>0] #Remove zero values
      dvvec = dvvec[!is.na(dvvec)]
      if(length(dvvec)>0)  {
        y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
        x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
        if(!is.na(x) && !is.na(y)) {
          dfout$WindDirection[i] = (180/pi)*atan(y/x)
          if(dfout$WindDirection[i]<0) dfout$WindDirection[i] = dfout$WindDirection[i]+360
        }
      }
    }
  }
  
  sel = (!is.na(dfout$lon)) & (!is.na(dfout$lat) & (!is.na(dfout$name))) 
  discarded = station_id[!sel]
  dfout = dfout[sel,]
  if(length(discarded)>0) warning(paste("\nInformation could not be retrieved for the following stations: \n  ",
                        paste(discarded, collapse=", "), ".\n", sep=""))
  if(sum(sel)>0) {
    points <- SpatialPoints(coords = cbind(dfout$lon,dfout$lat), CRS("+proj=longlat"))
    return(SpatialPointsDataFrame(points, dfout[,-c(1,2)]))
  } else {
    warning("No stations with results!")
    return(NULL)
  }
}

