#' @describeIn reshapemeteospain `r lifecycle::badge("deprecated")`
#' @export
reshapeworldmet<-function(hourly_data, output="SpatialPointsMeteorology",
                          proj4string = NULL, complete=TRUE, verbose = TRUE) {
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "reshapeworldmet()", with = "worldmet2meteoland()",
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Adapting worldmet meteo output to meteoland can be done with worldmet2meteoland()"
  )

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
    varnames <-c("code", "longitude","latitude", "station", "elev", "date", "air_temp", "atmos_pres", "RH",  "precip_6", "wd", "ws")
    varnames <- varnames[varnames %in% names(data_df)]
    data_df <- data_df[,varnames]
    numvar <- c("longitude","latitude","elev","air_temp", "precip_6", "RH", "wd", "ws")
    numvar <- numvar[numvar %in% names(data_df)]
    data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)

    df_dates = levels(as.factor(as.Date(data_df$date)))
    dates = sort(unique(c(dates,as.character(df_dates))))

    data_agg <- aggregate(data_df[,numvar],list(date = as.Date(data_df$date)),
                          function(x){
                            if(sum(is.na(x))<length(x)) {
                              mean<-mean(x,na.rm=T)
                              min<-min(x,na.rm=T)
                              max<-max(x,na.rm=T)
                              sum<-sum(x,na.rm=T)
                              return(c(mean=mean,min=min,max=max,sum=sum))
                            }
                            return(c(mean=NA, min=NA, max=NA, sum=NA))})

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

    data_out <- data.frame(row.names = as.character(df_dates))
    if("air_temp" %in% varnames) {
      data_out$MeanTemperature = data_agg$air_temp[,"mean"]
      data_out$MinTemperature = data_agg$air_temp[,"min"]
      data_out$MaxTemperature = data_agg$air_temp[,"max"]
    } else {
      data_out$MeanTemperature = NA
      data_out$MinTemperature = NA
      data_out$MaxTemperature = NA
    }
    if("precip_6" %in% varnames) {
      data_out$Precipitation = data_agg$precip_6[,"sum"]
    } else {
      data_out$Precipitation = NA
    }
    if("RH" %in% varnames) {
      data_out$MeanRelativeHumidity = data_agg$RH[,"mean"]
      data_out$MinRelativeHumidity = data_agg$RH[,"min"]
      data_out$MaxRelativeHumidity = data_agg$RH[,"max"]
    } else {
      data_out$MeanRelativeHumidity = NA
      data_out$MinRelativeHumidity = NA
      data_out$MaxRelativeHumidity = NA
    }
    if("ws" %in% varnames) data_out$WindSpeed = data_agg$ws[,"mean"]
    else data_out$WindSpeed = NA
    if("wd" %in% varnames) data_out$WindDirection = wd_agg$wd
    else data_out$WindDirection = NA
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
                      proj4string = CRS(SRS_string = "EPSG:4326"))
  if(!is.null(proj4string)) {
    if(inherits(proj4string,"character")) proj4string = CRS(proj4string)
    sp = spTransform(sp, proj4string)
    colnames(sp@coords)<-c("x","y")
    rownames(sp@bbox)<-c("x","y")
  }
  spt <- SpatialPointsTopography(sp, elevation)
  spm <- SpatialPointsMeteorology(sp, l, dates = as.Date(dates))
  if(output=="SpatialPointsMeteorology") return(spm)
  else if(output=="SpatialPointsTopography") return(spt)
  else if(output=="MeteorologyInterpolationData") {
    mid = MeteorologyInterpolationData(spm, elevation = elevation)
    return(mid)
  }
}
