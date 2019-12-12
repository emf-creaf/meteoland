.interpolatePixelsDay<-function(object, pixels, latitude, d) {
  i = which(object@dates == d)
  if(length(i)==0) stop("Date not found. Date 'd' has to be comprised within the dates specified in 'object'.")
  #Transform projection of pixel coordinates to that of object
  sp = spTransform(SpatialPoints(pixels@coords, pixels@proj4string), object@proj4string)
  cc = sp@coords
  z = pixels@data$elevation
  mPar = object@params

  tmin = .interpolateTemperatureSeriesPoints(Xp= cc[,1], Yp =cc[,2], Zp = z,
                                             X = object@coords[,1],
                                             Y = object@coords[,2],
                                             Z = object@elevation,
                                             T = as.matrix(object@MinTemperature)[,i,drop=FALSE],
                                             iniRp = mPar$initial_Rp,
                                             alpha = mPar$alpha_MinTemperature,
                                             N = mPar$N_MinTemperature,
                                             iterations = mPar$iterations)
  tmax = .interpolateTemperatureSeriesPoints(Xp= cc[,1], Yp =cc[,2], Zp = z,
                                             X = object@coords[,1],
                                             Y = object@coords[,2],
                                             Z = object@elevation,
                                             T = as.matrix(object@MaxTemperature)[,i,drop=FALSE],
                                             iniRp = mPar$initial_Rp,
                                             alpha = mPar$alpha_MaxTemperature,
                                             N = mPar$N_MaxTemperature,
                                             iterations = mPar$iterations)
  tmean = 0.606*tmax+0.394*tmin
  prec = .interpolatePrecipitationSeriesPoints(Xp= cc[,1], Yp =cc[,2], Zp = z,
                                               X = object@coords[,1],
                                               Y = object@coords[,2],
                                               Z = object@elevation,
                                               P = as.matrix(object@Precipitation)[,i,drop=FALSE],
                                               Psmooth = object@SmoothedPrecipitation[,i,drop=FALSE],
                                               iniRp = mPar$initial_Rp,
                                               alpha_event = mPar$alpha_PrecipitationEvent,
                                               alpha_amount = mPar$alpha_PrecipitationAmount,
                                               N_event = mPar$N_PrecipitationEvent,
                                               N_amount = mPar$N_PrecipitationAmount,
                                               iterations = mPar$iterations,
                                               popcrit = mPar$pop_crit,
                                               fmax = mPar$f_max)
  #relative humidity
  if(is.null(object@RelativeHumidity)) { #Estimate VP assuming that dew-point temperature is equal to Tmin
    rhmean = .relativeHumidityFromMinMaxTemp(tmin, tmax)
    VP = .temp2SVP(tmin) #kPa
    rhmax = rep(100, length(rhmean))
    rhmin = pmax(0,.relativeHumidityFromDewpointTemp(tmax, tmin))
  } else {
    TdewM = .dewpointTemperatureFromRH(0.606*as.matrix(object@MaxTemperature[,i,drop=FALSE])+0.394*as.matrix(object@MinTemperature[,i,drop=FALSE]),
                                       as.matrix(object@RelativeHumidity))
    tdew = .interpolateTdewSeriesPoints(Xp= cc[,1], Yp =cc[,2], Zp = z,
                                               X = object@coords[,1],
                                               Y = object@coords[,2],
                                               Z = object@elevation,
                                               T = TdewM,
                                               iniRp = mPar$initial_Rp,
                                               alpha = mPar$alpha_DewTemperature,
                                               N = mPar$N_DewTemperature,
                                               iterations = mPar$iterations)
    rhmean = .relativeHumidityFromDewpointTemp(tmean, tdew)
    VP = .temp2SVP(tdew) #kPa
    rhmin = pmax(0,.relativeHumidityFromDewpointTemp(tmax, tdew))
    rhmax = pmin(100,.relativeHumidityFromDewpointTemp(tmin, tdew))
  }

  #radiation
  doy = as.numeric(format(object@dates[i],"%j"))
  J = radiation_dateStringToJulianDays(d)
  diffTemp = tmax-tmin
  diffTempMonth = .interpolateTemperatureSeriesPoints(Xp= cc[,1], Yp =cc[,2], Zp = z,
                                                      X = object@coords[,1],
                                                      Y = object@coords[,2],
                                                      Z = object@elevation,
                                                      T = as.matrix(object@SmoothedTemperatureRange)[,i,drop=FALSE],
                                                      iniRp = mPar$initial_Rp,
                                                      alpha = mPar$alpha_MinTemperature,
                                                      N = mPar$N_MinTemperature,
                                                      iterations = mPar$iterations)

  latrad = latitude * (pi/180)
  asprad = pixels$aspect * (pi/180)
  slorad = pixels$slope  * (pi/180)
  rad = .radiationPoints(latrad, pixels$elevation, slorad, asprad, J, 
                         diffTemp, diffTempMonth, VP, prec)
  #wind
  if((!is.null(object@WFIndex)) && (!is.null(object@WFFactor))) {
    wstopo = getGridTopology(object@WindFields$windSpeed)
    wdtopo = getGridTopology(object@WindFields$windDirection)
    indws = getGridIndex(cc, wstopo)
    indwd = getGridIndex(cc, wdtopo)
    WS = as.matrix(object@WindFields$windSpeed@data[indws,])
    WD = as.matrix(object@WindFields$windDirection@data[indwd,])
    Wp = .interpolateWindFieldSeriesPoints(Xp= cc[,1], Yp =cc[,2], WS[,i,drop=FALSE], WD[,i,drop=FALSE],
                                      X = object@coords[,1],
                                      Y = object@coords[,2],
                                      I = object@WFIndex[,i,drop=FALSE],
                                      F = object@WFFactor[,i,drop=FALSE],
                                      iniRp = mPar$initial_Rp,
                                      alpha = mPar$alpha_Wind,
                                      N = mPar$N_Wind,
                                      iterations = mPar$iterations)
    Ws = as.vector(Wp$WS)
    Wd = as.vector(Wp$WD)
  } else if((!is.null(object@WindSpeed)) && (!is.null(object@WindDirection))) {
    Wp = .interpolateWindStationSeriesPoints(Xp= cc[,1], Yp =cc[,2],
                                            WS = object@WindSpeed[,i,drop=FALSE], WD = object@WindDirection[,i,drop=FALSE],
                                           X = object@coords[,1],
                                           Y = object@coords[,2],
                                           iniRp = mPar$initial_Rp,
                                           alpha = mPar$alpha_Wind,
                                           N = mPar$N_Wind,
                                           iterations = mPar$iterations)
    Ws = as.vector(Wp$WS)
    Wd = as.vector(Wp$WD)
  } else {
    Ws = rep(NA,nrow(cc))
    Wd = rep(NA,nrow(cc))
  }
  #PET
  pet = .PenmanPETPointsDay(latrad, pixels$elevation, slorad, asprad, J, tmin, tmax,
                            rhmin, rhmax, rad, Ws, mPar$wind_height,
                            0.001, 0.25);
  df = data.frame(MeanTemperature = units::set_units(as.vector(tmean),"celsius"),
                  MinTemperature = units::set_units(as.vector(tmin),"celsius"),
                  MaxTemperature = units::set_units(as.vector(tmax),"celsius"),
                  Precipitation = units::set_units(as.vector(prec),"L/m^2"),
                  MeanRelativeHumidity = units::set_units(rhmean,"%"),
                  MinRelativeHumidity = units::set_units(rhmin,"%"),
                  MaxRelativeHumidity = units::set_units(rhmax, "%"),
                  Radiation = units::set_units(rad,"MJ"),
                  WindSpeed = units::set_units(Ws,"m/s"),
                  WindDirection = units::set_units(Wd,"degrees"),
                  PET = units::set_units(pet,"L/m^2"))
  return(SpatialPixelsDataFrame(pixels@coords, data = df, grid=pixels@grid, proj4string= pixels@proj4string))
}

interpolationpixels<-function(object, pixels, dates,
                            export=FALSE, exportDir = getwd(), exportFormat = "netCDF",
                            metadatafile = "MG.txt", verbose = TRUE) {
  if(!inherits(object,"MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'.")
  if(!inherits(pixels,"SpatialPixelsTopography")) stop("'pixels' has to be of class 'SpatialPixelsTopography'.")
  if(!is.null(dates)) {
    if(class(dates)!="Date") stop("'dates' has to be of class 'Date'.")
    if(sum(as.character(dates) %in% as.character(object@dates))<length(dates)) 
      stop("At least one of the dates is outside the time period for which interpolation is possible.")
  }
  else dates = object@dates
  bbox = object@bbox
  if(proj4string(pixels)!=proj4string(object))  {
    warning("CRS projection in 'pixels' adapted to that of 'object'.")
    sp = spTransform(SpatialPoints(pixels@coords, pixels@proj4string), object@proj4string)
    gbbox = sp@bbox
  } else {
    gbbox = pixels@bbox
  }
  insidebox = (gbbox[1,1]>=bbox[1,1]) && (gbbox[1,2]<=bbox[1,2]) && (gbbox[2,1]>=bbox[2,1]) && (gbbox[2,2]<=bbox[2,2])
  if(!insidebox) stop("Boundary box of target grid is not within boundary box of interpolation data object.")
  longlat = spTransform(as(pixels,"SpatialPoints"),CRS("+proj=longlat"))
  latitude = longlat@coords[,2]
  ndates = length(dates)
  if(ndates==1) return(.interpolatePixelsDay(object, pixels, latitude, dates))
  # Define vector of data frames
  l = vector("list", ndates)

  # Define meta data frame
  dfout = data.frame(dir = rep(exportDir, ndates), filename=rep("", ndates))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  rownames(dfout) = dates

  for(i in 1:ndates) {
    if(verbose) cat(paste("Date ", dates[i], " (",i,"/",ndates,") -",sep=""))
    m = .interpolatePixelsDay(object, pixels, latitude, dates[i])
    if(export) {
      if(exportFormat=="netCDF") dfout$filename[i] = paste(dates[i],".nc", sep="")
      if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
      else f = dfout$filename[i]
      .writemeteorologypixelsNetCDF(m@data, pixels, proj4string(m),dates[i],f,exportFormat)
      if(verbose) cat(paste(" written to ",f, sep=""))
      if(exportDir!="") f = paste(exportDir,metadatafile, sep="/")
      else f = metadatafile
      write.table(dfout,file= f,sep="\t", quote=FALSE)
    } else {
      l[[i]] = m@data
      if(verbose) cat(" done")
    }
    if(verbose) cat(".\n")
  }
  if(!export) {
    names(l) = dates
    return(SpatialPixelsMeteorology(as(pixels,"SpatialPoints"), data = l, dates, grid = pixels@grid, proj4string = pixels@proj4string))
  } else {
    invisible(dfout)
  }
}
