importworldmet<-function(hourly_data, output="SpatialPointsMeteorology", complete=TRUE, verbose = TRUE) {
  output <- match.arg(output, c("SpatialPointsMeteorology", "SpatialPointsTopography", "MeteorologyInterpolationData"))
  x= as.data.frame(hourly_data)
  s = split(x, x$code)
  codes = names(s)
  
  nstations = length(s)
  elevation = rep(NA, nstations)
  l = vector("list", nstations)
  names(l) <- codes
  coords = data.frame(lon = rep(NA, nstations), lat = rep(NA, nstations),
                      row.names = codes)
  dates = character(0)
  
  if(verbose) {
    cat("\nParsing hourly data...\n")
    pb = txtProgressBar(0, nstations, style = 3)
  } 
  for(i in 1:nstations) {
    if(verbose) setTxtProgressBar(pb, i)
    data_df = s[[i]]
    varnames <-c("code", "lon","lat", "station", "elev", "date", "air_temp", "atmos_pres", "RH",  "precip_6", "wd", "ws")
    data_df <- data_df[,varnames]
    numvar <- c("lon","lat","elev","air_temp", "precip_6", "RH", "wd", "ws")
    data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
    
    df_dates = levels(as.factor(as.Date(data_df$date)))
    dates = sort(unique(c(dates,as.character(df_dates))))
                      
    data_agg <- aggregate(data_df[,numvar],list(date = as.Date(data_df$date)), 
                          function(x){
                            mean<-mean(x,na.rm=T)
                            min<-min(x,na.rm=T)
                            max<-max(x,na.rm=T)
                            sum<-sum(x,na.rm=T)
                            if(is.infinite(mean)) mean <- NA
                            if(is.infinite(min)) min <- NA
                            if(is.infinite(max)) max <- NA
                            if(is.infinite(sum)) sum <- NA
                            return(c(mean=mean,min=min,max=max,sum=sum))})

    # wind direction
    wd_agg <- aggregate(list(wd = data_df$wd),list(date = as.Date(data_df$date)),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })
    
    coords$lon[i] = data_agg$lon[1,"mean"]
    coords$lat[i] = data_agg$lat[1,"mean"]
    elevation[i] = data_agg$elev[1,"mean"]
    
    data_out <- data.frame(MeanTemperature = data_agg$air_temp[,"mean"], MinTemperature = data_agg$air_temp[,"min"], MaxTemperature = data_agg$air_temp[,"max"],
                          Precipitation = data_agg$precip_6[,"sum"], 
                          MeanRelativeHumidity = data_agg$RH[,"mean"], MinRelativeHumidity = data_agg$RH[,"min"], MaxRelativeHumidity = data_agg$RH[,"max"],
                          WindSpeed = data_agg$ws[,"mean"], WindDirection = wd_agg$wd,
                          row.names = as.character(df_dates))
    if(complete) data_out<-meteocomplete(data_out, 
                                         latitude = coords$lat[i],
                                         elevation = elevation[i],
                                         aspect= NA,
                                         slope = NA)
    l[[i]] <- data_out
  }
  # Complete dates with missing
  for(i in 1:nstations) {
    df <- l[[i]]
    df<-df[dates,]
    rownames(df) <- dates
    l[[i]] <- df
  }
  
  sp <- SpatialPoints(coords = coords,
                      proj4string = CRS("+proj=longlat"))
  spt <- SpatialPointsTopography(sp, elevation)
  spm <- SpatialPointsMeteorology(sp, l, dates = as.Date(dates))
  if(output=="SpatialPointsMeteorology") return(spm)
  else if(output=="SpatialPointsTopography") return(spt)
  else if(output=="MeteorologyInterpolationData") {
    mid = MeteorologyInterpolationData(spm, elevation = elevation)
    return(mid)
  }
}
