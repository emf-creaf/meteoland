worldmet2SPM<-function(x) {
  x= as.data.frame(x)
  s = split(x, x$code)
  
  nstations = length(s)
  l = vector("list", nstations)
  coords = data.frame(lon = rep(NA, nstations), lat = rep(NA, nstations))
  dates = as.Date(character(0))
  
  for(i in 1:nstations) {
    data_df = s[[i]]
    varnames <-c("code", "lon","lat", "station", "elev", "date", "air_temp", "atmos_pres", "RH",  "precip_6", "wd", "ws")
    data_df <- data_df[,varnames]
    numvar <- c("lon","lat","elev","air_temp", "precip_6", "RH", "wd", "ws")
    data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)
    
    
    data_agg <- aggregate(data_df[,numvar],list(code = data_df$code, station = data_df$station, date = as.Date(data_df$date)), 
                          function(x){mean<-mean(x,na.rm=T);min<-min(x,na.rm=T);max<-max(x,na.rm=T);sum<-sum(x,na.rm=T)
                          return(c(mean=mean,min=min,max=max,sum=sum))})
    # wind direction
    wd_agg <- aggregate(list(wd = data_df$wd),list(code = data_df$code, station = data_df$station, date = as.Date(data_df$date)),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })
    
    coords$lon[i] = data_agg$lon[,"mean"]
    coords$lat[i] = data_agg$lat[,"mean"]
    data_out <- data.frame(ID = as.character(data_agg$code), name = data_agg$station, 
                          elevation = data_agg$elev[,"mean"],
                          MeanTemperature = data_agg$air_temp[,"mean"], MinTemperature = data_agg$air_temp[,"min"], MaxTemperature = data_agg$air_temp[,"max"],
                          Precipitation = data_agg$precip_6[,"sum"], WindSpeed = data_agg$ws[,"mean"], WindDirection = wd_agg$wd,
                          MeanRelativeHumidity = data_agg$RH[,"mean"], MinRelativeHumidity = data_agg$RH[,"min"], MaxRelativeHumidity = data_agg$RH[,"max"])
    
  }
  sp <- SpatialPoints(coords = coords,
                      proj4string = CRS("+proj=longlat"))
  return(SpatialPointsMeteorology(sp, l, dates))
}