.interpolatePointSeries = function(object, latitude, x, y, elevation, slope, aspect, dates = NULL) {
  if(!is.null(dates)) {
    if(sum(as.character(dates) %in% as.character(object@dates))<length(dates)) stop("Dates outside the valid range")
    dayIndices = which(as.character(object@dates) %in% as.character(dates))
  } else {
    dayIndices = 1:length(object@dates)
    dates = object@dates
  }
  mPar = object@params
  if(!("debug" %in% names(mPar))) mPar$debug = FALSE
  tmin = .interpolateTemperatureSeriesPoints(Xp= x, Yp =y, Zp = elevation,
                                             X = object@coords[,1],
                                             Y = object@coords[,2],
                                             Z = object@elevation,
                                             T = object@MinTemperature[,dayIndices, drop=FALSE],
                                             iniRp = mPar$initial_Rp,
                                             alpha = mPar$alpha_MinTemperature,
                                             N = mPar$N_MinTemperature,
                                             iterations = mPar$iterations,
                                             debug = mPar$debug)
  tmax = .interpolateTemperatureSeriesPoints(Xp= x, Yp =y, Zp = elevation,
                                             X = object@coords[,1],
                                             Y = object@coords[,2],
                                             Z = object@elevation,
                                             T = object@MaxTemperature[,dayIndices, drop=FALSE],
                                             iniRp = mPar$initial_Rp,
                                             alpha = mPar$alpha_MaxTemperature,
                                             N = mPar$N_MaxTemperature,
                                             iterations = mPar$iterations,
                                             debug = mPar$debug)
  tmean = 0.606*tmax+0.394*tmin
  prec = .interpolatePrecipitationSeriesPoints(Xp= x, Yp =y, Zp = elevation,
                                               X = object@coords[,1],
                                               Y = object@coords[,2],
                                               Z = object@elevation,
                                               P = object@Precipitation[,dayIndices, drop=FALSE],
                                               Psmooth = object@SmoothedPrecipitation[,dayIndices, drop=FALSE],
                                               iniRp = mPar$initial_Rp,
                                               alpha_event = mPar$alpha_PrecipitationEvent,
                                               alpha_amount = mPar$alpha_PrecipitationAmount,
                                               N_event = mPar$N_PrecipitationEvent,
                                               N_amount = mPar$N_PrecipitationAmount,
                                               iterations = mPar$iterations,
                                               popcrit = mPar$pop_crit,
                                               fmax = mPar$f_max,
                                               debug = mPar$debug)
  DOY = as.numeric(format(dates,"%j"))
  J = radiation_dateStringToJulianDays(as.character(dates))
  if(is.null(object@RelativeHumidity)) { #Estimate VP assuming that dew-point temperature is equal to Tmin
    rhmean = .relativeHumidityFromMinMaxTemp(tmin, tmax)
    VP = .temp2SVP(tmin) #kPA
    rhmax = rep(100, length(rhmean))
    rhmin = pmax(0,.relativeHumidityFromDewpointTemp(tmax, tmin))
  } else {
    TdewM = .dewpointTemperatureFromRH(0.606*object@MaxTemperature[,dayIndices, drop=FALSE]+
                                         0.394*object@MinTemperature[,dayIndices, drop=FALSE],
                                       object@RelativeHumidity[,dayIndices, drop=FALSE])
    tdew = .interpolateTdewSeriesPoints(Xp= x, Yp =y, Zp = elevation,
                                        X = object@coords[,1],
                                        Y = object@coords[,2],
                                        Z = object@elevation,
                                        T = TdewM,
                                        iniRp = mPar$initial_Rp,
                                        alpha = mPar$alpha_DewTemperature,
                                        N = mPar$N_DewTemperature,
                                        iterations = mPar$iterations,
                                        debug = mPar$debug)
    rhmean = .relativeHumidityFromDewpointTemp(tmean, tdew)
    VP = .temp2SVP(tdew) #kPa
    rhmax = pmin(100,.relativeHumidityFromDewpointTemp(tmin,tdew))
    rhmin = pmax(0,.relativeHumidityFromDewpointTemp(tmax,tdew))
  }
  #radiation
  diffTemp = abs(tmax-tmin)
  diffTempMonth = .interpolateTemperatureSeriesPoints(Xp= x, Yp =y, Zp = elevation,
                                                      X = object@coords[,1],
                                                      Y = object@coords[,2],
                                                      Z = object@elevation,
                                                      T = abs(object@SmoothedTemperatureRange[,dayIndices, drop=FALSE]),
                                                      iniRp = mPar$initial_Rp,
                                                      alpha = mPar$alpha_MinTemperature,
                                                      N = mPar$N_MinTemperature,
                                                      iterations = mPar$iterations,
                                                      debug = mPar$debug)
  latrad = latitude * (pi/180)
  slorad = slope * (pi/180)
  asprad = aspect* (pi/180)
  rad = .radiationSeries(latrad, elevation, slorad, asprad, J,
                         diffTemp, diffTempMonth, VP, prec)
  #wind
  if((!is.null(object@WFIndex)) && (!is.null(object@WFFactor))) {
    wstopo = getGridTopology(object@WindFields$windSpeed)
    wdtopo = getGridTopology(object@WindFields$windDirection)
    indws = getGridIndex(cbind(x,y), wstopo)
    indwd = getGridIndex(cbind(x,y), wdtopo)
    WS = as.matrix(object@WindFields$windSpeed@data[indws,])
    WD = as.matrix(object@WindFields$windDirection@data[indwd,])
    Wp = .interpolateWindFieldSeriesPoints(Xp= x, Yp =y, WS, WD,
                                      X = object@coords[,1],
                                      Y = object@coords[,2],
                                      I = object@WFIndex[,dayIndices, drop=FALSE],
                                      F = object@WFFactor[,dayIndices, drop=FALSE],
                                      iniRp = mPar$initial_Rp,
                                      alpha = mPar$alpha_Wind,
                                      N = mPar$N_Wind,
                                      iterations = mPar$iterations)
    Wsp = as.vector(Wp$WS)
    Wdp = as.vector(Wp$WD)
  } else if((!is.null(object@WindSpeed)) && (!is.null(object@WindDirection))){
    Wp = .interpolateWindStationSeriesPoints(Xp= x, Yp =y, object@WindSpeed[,dayIndices, drop=FALSE], 
                                             object@WindDirection[,dayIndices, drop=FALSE],
                                           X = object@coords[,1],
                                           Y = object@coords[,2],
                                           iniRp = mPar$initial_Rp,
                                           alpha = mPar$alpha_Wind,
                                           N = mPar$N_Wind,
                                           iterations = mPar$iterations)
    Wsp = as.vector(Wp$WS)
    Wdp = as.vector(Wp$WD)
  } else {
    Wsp = rep(NA,length(dates))
    Wdp = rep(NA,length(dates))
  }

  #PET
  pet = .penmanpoint(latrad, elevation, slorad, asprad, J, tmin, tmax,
                    rhmin, rhmax, rad, Wsp, mPar$wind_height,
                    0.001, 0.25);

  df = data.frame(DOY = DOY,
                  MeanTemperature = as.vector(tmean),#"celsius"),
                  MinTemperature = as.vector(tmin),#"celsius"),
                  MaxTemperature = as.vector(tmax),#"celsius"),
                  Precipitation = as.vector(prec), #"L/m^2"),
                  MeanRelativeHumidity = rhmean,#"%"),
                  MinRelativeHumidity = rhmin,#"%"),
                  MaxRelativeHumidity = rhmax, #"%"),
                  Radiation = rad,#"MJ"),
                  WindSpeed = Wsp,#"m/s"),
                  WindDirection = Wdp,#"degrees"),
                  PET = pet,#,"L/m^2"),
                  row.names = dates)
  return(df)
}

interpolationpoints<-function(object, points, dates = NULL,
                              export=FALSE, exportDir = getwd(), exportFile = NULL, 
                              exportFormat = "meteoland/txt",
                              metadataFile = "MP.txt", verbose=TRUE) {
  if(export) exportFormat = match.arg(exportFormat, c("meteoland/txt", "meteoland/rds", "castanea/txt", "castanea/rds", "netCDF"))
  
  if(!inherits(object,"MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'.")
  if(!inherits(points,"SpatialPointsTopography")) stop("'points' has to be of class 'SpatialPointsTopography'.")
  intpoints = as(points, "SpatialPoints")
  if(!identicalCRS(intpoints,object)) {
    warning("CRS projection of 'points' adapted to that of 'object'.")
    intpoints = spTransform(intpoints, object@proj4string)
  }
  if(!is.null(dates)) {
    if(class(dates)!="Date") stop("'dates' has to be of class 'Date'.")
    if(sum(as.character(dates) %in% as.character(object@dates))<length(dates)) 
      stop("At least one of the dates is outside the time period for which interpolation is possible.")
  }
  cc = coordinates(intpoints) #
  ids = row.names(cc)
  npoints = nrow(cc)
  if(is.null(ids) || length(ids)==0) {
    ids = 1:npoints
    rownames(points@coords) = ids
  }
  elevation = points@data$elevation
  slope = points@data$slope
  aspect = points@data$aspect
  longlat = spTransform(points,CRS(SRS_string = "EPSG:4326"))
  latitude = longlat@coords[,2]
  bbox = object@bbox
  
  
  if(exportFormat %in% c("meteoland/txt","castanea/txt")) formatType = "txt"
  else if (exportFormat %in% c("meteoland/rds","castanea/rds")) formatType = "rds"
  else if (exportFormat %in% c("netCDF")) formatType = "netCDF"
  
  # Define vector of data frames
  
  if(export & exportFormat %in% c("meteoland/txt","castanea/txt", "meteoland/rds","castanea/rds")) {
    dfout = data.frame(dir = rep(exportDir, npoints), filename=paste0(ids,".", formatType))
    dfout$dir = as.character(dfout$dir)
    dfout$filename = as.character(dfout$filename)
    dfout$format = exportFormat
    rownames(dfout) = ids
    spdf = SpatialPointsDataFrame(as(points,"SpatialPoints"), dfout)
    colnames(spdf@coords)<-c("x","y")
  }
  else if(export & exportFormat=="netCDF") {
    if(is.null(exportFile)) stop("File 'exportFile' cannot be null when exporting to netCDF!")
    ncfile = exportFile
    if(is.null(dates)) dates = object@dates
    nc <-.openwritepointNetCDF(coordinates(points), proj4string(points), dates = dates,  vars = NULL,
                               file = ncfile, overwrite = TRUE, verbose = verbose)
  }
  else {
    dfvec = vector("list",npoints)
  }
  for(i in 1:npoints) {
    if(verbose) cat(paste("Processing point '",ids[i],"' (",i,"/",npoints,") -",sep=""))
    insidebox = (cc[i,1]>=bbox[1,1] && cc[i,1]<=bbox[1,2]) && (cc[i,2]>=bbox[2,1] && cc[i,2]<=bbox[2,2])
    if(!insidebox) warning(paste("Point '",ids[i],"' outside the boundary box of interpolation data object.",sep=""))
    df = .interpolatePointSeries(object, latitude[i],cc[i,1], cc[i,2], elevation[i], slope[i], aspect[i], dates)
    if(is.null(dates)) dates = as.Date(rownames(df))
    if(!export) {
      dfvec[[i]] =df
      if(verbose) cat(paste(" done"))
    }
    else {
      if(exportFormat!="netCDF") {
        if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
        else f = dfout$filename[i]
        writemeteorologypoint(df, f, exportFormat)
        if(verbose) cat(paste(" written to ",f, sep=""))
        if(exportDir!="") f = paste(exportDir,metadataFile, sep="/")
        else f = metadataFile
        write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
      } else {
        if(verbose) cat(paste0(" written to netCDF"))
        .writemeteorologypointNetCDF(df,nc,i)
      }
    }
    if(verbose) cat(".\n")
  }
  if(!export) return(SpatialPointsMeteorology(points, dfvec, dates))
  if(exportFormat!="netCDF") {
    invisible(spdf)
  } else {
    .closeNetCDF(ncfile, nc)
  }
}
