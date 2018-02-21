# Function to download daily met data from AEMET, format it and save it on the disk
downloadAEMETcurrentday <- function(api, verbose=TRUE, daily = T){
  # Utilitary functions
  cname.func <- function(x){
    regmatches(x,gregexpr('(?<=\\n\\s{2}\\")[[:print:]]+(?=\\"\\s\\:)', x, perl = T))[[1]]
  }
  value.func <- function(x){
    value <- regmatches(x,gregexpr('(?<=\\:\\s)[[:print:]]*(?=\\n)', x, perl = T))[[1]]
    value <- gsub('\\",', "", value)
    value <- gsub('\\"', "", value)
    value <- gsub(',', "", value)
  }
  
  
  # set options
  h = new_handle()
  handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  handle_setopt(h, ssl_verifypeer=FALSE)
  handle_setopt(h, customrequest="GET")
  
  url <- "https://opendata.aemet.es/opendata/api/observacion/convencional/todas"
  # get data url
  urldata_raw <- curl_fetch_memory(url, h)$content
  urldata_string <- value.func(rawToChar(urldata_raw))
  
  if(urldata_string[2]=="401"){
    stop("Invalid API key. (API keys are valid for 3 months.)")
  }
  
  urldata <- urldata_string[3]
  
  if(verbose)cat("Downloading hourly data from all available stations")
  data_raw <- curl_fetch_memory(urldata, h)$content
  
  if(verbose)cat("\nFormating data")
  data_string <- rawToChar(data_raw)
  data_string <- strsplit(data_string,"}\\,\\s{1}\\{")[[1]]
  cname <- lapply(data_string,FUN = cname.func)
  value <- lapply(data_string,FUN = value.func)
  value <- mapply(FUN = function(x,y){names(x) <- y;return(x)}, x = value,y = cname)
  unique_cname <- cname[[which.max(sapply(cname,FUN = length))]]
  value <- mapply(FUN = function(x,y){x[y]}, x = value, y = list(unique_cname))
  
  data_df <- data.frame(matrix(t(value), ncol = length(unique_cname), dimnames = list(NULL, unique_cname)),
                        stringsAsFactors = F)
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
                                      data = data_df[,which(!colnames(data_df) %in% c("long", "lat", "name", "ID", "elevation"))],
                                      proj4string = CRS("+proj=longlat"))
    row.names(data_sp) <- data_df$ID
    return(data_sp)
  }else{
    if(verbose)cat("\nHourly results are returned\n")
    colnames(data_df) <- c("ID", "long", "lat", "name", "elevation", "date_time", 
                           "MeanTemperature", "MinTemperature", "MaxTemperature",
                           "Precipitation", "MeanRelativeHumidity", "WindDirection", "WindSpeed")
    return(data_df)
  }
}


