#' @describeIn reshapemeteospain `r lifecycle::badge("deprecated")`
#' @export
reshapeweathercan<-function(hourly_data, daily_data = NULL, output="SpatialPointsMeteorology",
                            proj4string = NULL, complete=TRUE, verbose = TRUE) {

  lifecycle::deprecate_warn(
    when = "2.0.0", what = "reshapeweathercan()", with = NULL,
    details = "reshapeweathercan is deprecated and will dissapear from future versions of meteoland"
  )

  output <- match.arg(output, c("SpatialPointsMeteorology", "SpatialPointsTopography", "MeteorologyInterpolationData"))
  hourly_data= as.data.frame(hourly_data)
  s_hourly = split(hourly_data, hourly_data$station_id)
  codes_hourly = names(s_hourly)
  if(!is.null(daily_data)) {
    daily_data = as.data.frame(daily_data)
    s_daily = split(daily_data, daily_data$station_id)
    codes_daily = names(s_daily)
    codes = sort(unique(c(codes_daily, codes_hourly)))
  } else {
    codes = codes_hourly
  }
  nstations = length(codes)

  elevation = rep(NA, nstations)
  l = vector("list", nstations)
  names(l) <- codes
  coords = data.frame(lon = rep(NA, nstations), lat = rep(NA, nstations),
                      row.names = codes)
  dates = character(0)


  #Process hourly data
  if(verbose) {
    cat("\nParsing hourly data...\n")
    pb = txtProgressBar(0, length(s_hourly), style = 3)
  }
  for(i in 1:length(s_hourly)) {
    if(verbose) setTxtProgressBar(pb, i)
    data_df = s_hourly[[i]]
    ic = which(codes==codes_hourly[i])

    varnames <-c("station_id", "lon","lat", "station_name", "elev", "date", "temp", "pressure", "rel_hum", "wind_dir", "wind_spd")
    data_df <- data_df[,varnames]
    numvar <- c("lon","lat","elev","temp", "rel_hum", "wind_dir", "wind_spd")
    data_df[,numvar] <- sapply(data_df[,numvar],as.numeric)

    df_dates = levels(as.factor(as.Date(data_df$date)))
    dates = sort(unique(c(dates,as.character(df_dates))))

    data_agg <- aggregate(data_df[,numvar],list(date = as.Date(data_df$date)),
                          function(x){
                            if(!all(is.na(x))) {
                              mean<-mean(x,na.rm=T)
                              min<-min(x,na.rm=T)
                              max<-max(x,na.rm=T)
                              sum<-sum(x,na.rm=T)
                            } else {
                              mean <- NA
                              min <- NA
                              max <- NA
                              sum <- NA
                            }
                            return(c(mean=mean,min=min,max=max,sum=sum))})

    # wind direction (tens of degrees)
    wd_agg <- aggregate(list(wd = data_df$wind_dir*10),list(date = as.Date(data_df$date)),
                        function(dvvec){
                          y = sum(cos(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          x = sum(sin(dvvec*pi/180), na.rm=TRUE)/length(dvvec)
                          dv = (180/pi)*atan(y/x)
                          dv[dv<0] <- dv[dv<0]+360
                          return(dv)
                        })

    coords$lon[ic] = data_agg$lon[1,"mean"]
    coords$lat[ic] = data_agg$lat[1,"mean"]
    elevation[ic] = data_agg$elev[1,"mean"]

    data_out <- data.frame(MeanTemperature = data_agg$temp[,"mean"],
                           MinTemperature = data_agg$temp[,"min"],
                           MaxTemperature = data_agg$temp[,"max"],
                           Precipitation = NA,
                           MeanRelativeHumidity = data_agg$rel_hum[,"mean"],
                           MinRelativeHumidity = data_agg$rel_hum[,"min"],
                           MaxRelativeHumidity = data_agg$rel_hum[,"max"],
                           WindSpeed = data_agg$wind_spd[,"mean"]/3.6, #from km/h to m/s
                           WindDirection = wd_agg$wd,
                           row.names = as.character(df_dates))
    l[[i]] <- data_out
  }
  #Process daily data
  if(!is.null(daily_data)) {
    if(verbose) {
      cat("\nParsing daily data...\n")
      pb = txtProgressBar(0, length(s_daily), style = 3)
    }
    for(i in 1:length(s_daily)) {
      if(verbose) setTxtProgressBar(pb, i)
      data_df = s_daily[[i]]
      df_dates = as.Date(data_df$date)
      ic = which(codes==codes_daily[i])

      dates = sort(unique(c(dates,as.character(df_dates))))

      coords$lon[ic] = data_df$lon[1]
      coords$lat[ic] = data_df$lat[1]
      elevation[ic] = data_df$elev[1]

      data_out <- l[[ic]]
      if(is.null(data_out)) {
        data_out <- data.frame(MeanTemperature = data_df$mean_temp,
                               MinTemperature = data_df$min_temp,
                               MaxTemperature = data_df$max_temp,
                               Precipitation = data_df$total_precip,
                               MeanRelativeHumidity = NA,
                               MinRelativeHumidity = NA,
                               MaxRelativeHumidity = NA,
                               WindSpeed = NA,
                               WindDirection = NA,
                               row.names = as.character(data_df$date))
      } else {
        data_out[as.character(data_df$date),"Precipitation"] = data_df$total_precip
      }

      l[[ic]] <- data_out
    }
  }
  # Complete dates with missing
  if(complete) {
    for(i in 1:nstations) {
      df <- l[[i]]
      if(!is.null(df)) {
        df<-df[dates,]
        rownames(df) <- dates
        df<-meteocomplete(df,
                          latitude = coords$lat[i],
                          elevation = elevation[i],
                          aspect= NA,
                          slope = NA)
        l[[i]] <- df
      }
    }
  }
  sp <- SpatialPoints(coords = coords,
                      proj4string = CRS(SRS_string = "EPSG:4326"))
  if(!is.null(proj4string)) {
    if(inherits(proj4string,"character")) proj4string = CRS(proj4string)
    sp = spTransform(sp, proj4string)
    colnames(sp@coords)<-c("x","y")
    rownames(sp@bbox)<-c("x","y")
  }
  if(output %in% c("SpatialPointsMeteorology", "MeteorologyInterpolationData")) {
    spm <- SpatialPointsMeteorology(sp, l, dates = as.Date(dates))
    if(output == "SpatialPointsMeteorology") {
      return(spm)
    } else {
      return(MeteorologyInterpolationData(spm, elevation = elevation))
    }
  } else if(output=="SpatialPointsTopography") {
    spt <- SpatialPointsTopography(sp, elevation)
    return(spt)
  }
}
