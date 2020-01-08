.interpolateGridDay<-function(object, grid, latitude, d) {
  i = which(object@dates == d)
  if(length(i)==0) stop("Date not found. Date 'd' has to be comprised within the dates specified in 'object'.")
  #Transform projection of pixel coordinates to that of object
  sp = spTransform(SpatialPoints(coordinates(grid), grid@proj4string), object@proj4string)
  cc = sp@coords
  z = grid@data$elevation
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
  asprad = grid$aspect * (pi/180)
  slorad = grid$slope  * (pi/180)
  rad = .radiationPoints(latrad, grid$elevation, slorad, asprad, J, 
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
  pet = .PenmanPETPointsDay(latrad, grid$elevation, slorad, asprad, J, tmin, tmax,
                            rhmin, rhmax, rad, Ws, mPar$wind_height,
                            0.001, 0.25);
  df = data.frame(MeanTemperature = as.vector(tmean),#"celsius"),
                  MinTemperature = as.vector(tmin),#"celsius"),
                  MaxTemperature = as.vector(tmax),#"celsius"),
                  Precipitation = as.vector(prec),#"L/m^2"),
                  MeanRelativeHumidity = rhmean,#"%"),
                  MinRelativeHumidity = rhmin,#"%"),
                  MaxRelativeHumidity = rhmax, #"%"),
                  Radiation = rad,#"MJ"),
                  WindSpeed = Ws,#"m/s"),
                  WindDirection = Wd,#"degrees"),
                  PET = pet) #"L/m^2"))
  return(SpatialGridDataFrame(grid@grid, df, grid@proj4string))
}

interpolationgrid<-function(object, grid, dates = NULL,
                            exportFile = NULL, exportFormat = "netCDF", add = FALSE, overwrite = FALSE,
                            verbose = TRUE) {
  if(!inherits(object,"MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'.")
  if(!inherits(grid,"SpatialGridTopography")) stop("'grid' has to be of class 'SpatialGridTopography'.")
  if(!is.null(dates)) {
    if(class(dates)!="Date") stop("'dates' has to be of class 'Date'.")
    if(sum(as.character(dates) %in% as.character(object@dates))<length(dates)) 
      stop("At least one of the dates is outside the time period for which interpolation is possible.")
  }
  else dates = object@dates
  bbox = object@bbox
  if(proj4string(grid)!=proj4string(object))  {
    warning("CRS projection in 'grid' adapted to that of 'object'.")
    sp = spTransform(SpatialPoints(coordinates(grid), grid@proj4string), object@proj4string)
    gbbox = sp@bbox
  } else {
    gbbox = grid@bbox
  }
  insidebox = (gbbox[1,1]>=bbox[1,1]) && (gbbox[1,2]<=bbox[1,2]) && (gbbox[2,1]>=bbox[2,1]) && (gbbox[2,2]<=bbox[2,2])
  if(!insidebox) warning("Boundary box of target grid is not within boundary box of interpolation data object.")
  longlat = spTransform(as(grid,"SpatialPoints"),CRS("+proj=longlat"))
  latitude = longlat@coords[,2]
  ndates = length(dates)
  #Is export?
  export = !is.null(exportFile)
  if((ndates==1) && !export) return(.interpolateGridDay(object, grid, latitude, dates))
  # Define vector of data frames
  l = vector("list", ndates)

  if(export) nc =  .openwritegridNetCDF(grid@grid, proj4string(grid), 
                                    dates = dates, file = exportFile, add = add, overwrite = overwrite, verbose = verbose)
  for(i in 1:ndates) {
    if(verbose) cat(paste("Interpolating day '", dates[i], "' (",i,"/",ndates,") - ",sep=""))
    m = .interpolateGridDay(object, grid, latitude, dates[i])
    if(export) {
      dl = list(m@data)
      names(dl) = as.character(dates[i])
      .writemeteorologygridNetCDF(dl,m@grid, proj4string(m), nc, verbose = verbose)
    } else {
      l[[i]] = m@data
      if(verbose) cat("done.\n")
    }
  }
  if(!export) {
    names(l) = dates
    return(SpatialGridMeteorology(grid@grid, grid@proj4string, l, dates))
  } else {
    .closeNetCDF(exportFile,nc, verbose = verbose)
  }
}
