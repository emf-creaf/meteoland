MeteorologyInterpolationData<-function(points, elevation = NULL, slope = NULL, aspect = NULL,
                                       MinTemperature = NULL, MaxTemperature = NULL, Precipitation = NULL, RelativeHumidity = NULL,
                                       Radiation = NULL, WindSpeed = NULL, WindDirection = NULL, WindFields = NULL,
                                       params = defaultInterpolationParams()) {
  if((!inherits(points, "SpatialPoints")) && (!inherits(points, "SpatialPointsTopography")) && (!inherits(points, "SpatialPointsMeteorology"))) stop("'points' has to be of class 'SpatialPointsMeteorology', 'SpatialPointsTopography' or 'SpatialPoints'")
  isPoints = inherits(points,"SpatialPoints")
  isPointsTopo = inherits(points, "SpatialPointsTopography")
  isSpatialPointsMeteorology = inherits(points, "SpatialPointsMeteorology")
  coords = coordinates(points)
  nstations = nrow(coords)
  if(isPointsTopo) {
    elevation = as.numeric(points@data$elevation)
    slope = as.numeric(points@data$slope)
    aspect = as.numeric(points@data$aspect)
  } else {
    if(!is.null(elevation)) {
      if(length(elevation)!=nstations) stop("'elevation' has to be of the same length as the number of points.")
    } else {
      stop("'elevation' has to be provided if 'points' is of class 'SpatialPointsMeteorology' or 'SpatialPoints'")
    }
    if(!is.null(slope)) {
      if(length(slope)!=nstations) stop("'slope' has to be of the same length as the number of points.")
    } else {
      slope = numeric(nstations)
    }
    if(!is.null(aspect)) {
      if(length(aspect)!=nstations) stop("'aspect' has to be of the same length as the number of points.")
    } else {
      aspect = numeric(nstations)
    }
  }
  
  if((isPoints || isPointsTopo) && (!isSpatialPointsMeteorology)) { # Build from variable matrices
    if(!inherits(MinTemperature,"matrix")) stop("'MinTemperature' has to be a numeric matrix")
    dates = as.Date(colnames(MinTemperature))
    ndays = length(dates)
    if(!inherits(MaxTemperature,"matrix")) stop("'MaxTemperature' has to be a numeric matrix.")
    if(nrow(MinTemperature)!=nstations) stop("Number of rows in 'MinTemperature' have to be equal to the number of points.")
    if(nrow(MaxTemperature)!=nstations) stop("Number of rows in 'MaxTemperature' have to be equal to the number of points.")
    if(!is.null(Precipitation)) {
      if(!inherits(Precipitation,"matrix")) stop("'Precipitation' has to be a numeric matrix.")
      if(nrow(Precipitation)!=nstations) stop("Number of rows in 'Precipitation' have to be equal to the number of points.")
    } else {
      Precipitation = matrix(NA, nrow = nstations, ncol=ndays)
    }
    if(!is.null(RelativeHumidity)) {
      if(!inherits(RelativeHumidity,"matrix")) stop("'RelativeHumidity' has to be a numeric matrix.")
      if(nrow(RelativeHumidity)!=nstations) stop("Number of rows in 'RelativeHumidity' have to be equal to the number of points.")
    } else {
      RelativeHumidity = matrix(NA, nrow = nstations, ncol=ndays)
    }
    if(!is.null(WindSpeed)) {
      if(!inherits(WindSpeed,"matrix")) stop("'WindSpeed' has to be a numeric matrix.")
      if(nrow(WindSpeed)!=nstations) stop("Number of rows in 'WindSpeed' have to be equal to the number of points.")
    } else {
      WindSpeed = matrix(NA, nrow = nstations, ncol=ndays)
    }
    if(!is.null(WindDirection)) {
      if(!inherits(WindDirection,"matrix")) stop("'WindDirection' has to be a numeric matrix.")
      if(nrow(WindDirection)!=nstations) stop("Number of rows in 'WindDirection' have to be equal to the number of points.")
    } else {
      WindDirection = matrix(NA, nrow = nstations, ncol=ndays)
    }
    if(!is.null(Radiation)) {
      if(!inherits(Radiation,"matrix")) stop("'Radiation' has to be a numeric matrix.")
      if(nrow(Radiation)!=nstations) stop("Number of rows in 'Radiation' have to be equal to the number of points.")
    } else {
      Radiation = matrix(NA, nrow = nstations, ncol=ndays)
    }
  } else if(isSpatialPointsMeteorology) { # Build from SpatialPointsMeteorology
    stnames = names(points@data)
    nstations = length(stnames)
    dates = points@dates
    ndays = length(dates)
    #Re-shape data
    MinTemperature = matrix(NA, nrow = nstations, ncol=ndays)
    rownames(MinTemperature)= stnames
    colnames(MinTemperature) = as.character(dates)
    MaxTemperature = MinTemperature
    Precipitation = MinTemperature
    RelativeHumidity = MinTemperature
    Radiation = MinTemperature
    WindSpeed = MinTemperature
    WindDirection = MinTemperature
    for(i in 1:nstations) {
      sti_data = points@data[[i]]
      if("MinTemperature" %in% names(sti_data)) MinTemperature[i,] = sti_data[, "MinTemperature"]
      if("MaxTemperature" %in% names(sti_data)) MaxTemperature[i,] = sti_data[, "MaxTemperature"]
      if("Precipitation" %in% names(sti_data)) Precipitation[i,] = sti_data[, "Precipitation"]
      if("MeanRelativeHumidity" %in% names(sti_data)) RelativeHumidity[i,] = sti_data[, "MeanRelativeHumidity"]
      if("Radiation" %in% names(sti_data)) Radiation[i,] = sti_data[, "Radiation"]
      if("WindSpeed" %in% names(sti_data)) WindSpeed[i,] = sti_data[, "WindSpeed"]
      if("WindDirection" %in% names(sti_data)) WindDirection[i,] = sti_data[, "WindDirection"]
    }
  }
  MinTemperature[is.infinite(MinTemperature)] = NA
  MaxTemperature[is.infinite(MaxTemperature)] = NA
  Precipitation[is.infinite(Precipitation)] = NA
  RelativeHumidity[is.infinite(RelativeHumidity)] = NA
  Radiation[is.infinite(Radiation)] = NA
  WindDirection[is.infinite(WindDirection)] = NA
  
  SmoothedPrecipitation = .temporalSmoothing(as.matrix(Precipitation), params$St_Precipitation, TRUE)
  SmoothedTemperatureRange = .temporalSmoothing(as.matrix(MaxTemperature-MinTemperature), params$St_TemperatureRange, FALSE)
  WFIndex = NULL
  WFFactor = NULL
  if(!is.null(WindFields)) {
    if(!inherits(WindFields,"windparams")) stop("'WindFields' has to be an object of S3 class 'windparams'.")
  }
  if((!is.null(WindFields)) && (!is.null(WindSpeed)) && (!is.null(WindDirection))) {
    wstopo = getGridTopology(WindFields$windSpeed)
    wdtopo = getGridTopology(WindFields$windDirection)
    indws = getGridIndex(coords, wstopo)
    indwd = getGridIndex(coords, wdtopo)
    l = .getWindFieldIndexAndFactor(as.matrix(WindSpeed),
                                    as.matrix(WindDirection),
                                    as.matrix(WindFields$windSpeed@data)[indws,],
                                    as.matrix(WindFields$windDirection@data)[indwd,])
    WFIndex = l$Index
    WFFactor = l$Factor
  }
  
  #Adapt initial_Rp to mean distance between stations
  params$initial_Rp = mean(dist(coords))
    
  ilm = new("MeteorologyInterpolationData",
           coords = coords,
           bbox = points@bbox,
           proj4string = points@proj4string,
           dates = dates,
           elevation = elevation,
           slope = slope,
           aspect = aspect,
           MinTemperature = MinTemperature,
           MaxTemperature = MaxTemperature,
           SmoothedTemperatureRange = SmoothedTemperatureRange,
           Precipitation = Precipitation,
           SmoothedPrecipitation = SmoothedPrecipitation,
           RelativeHumidity = RelativeHumidity,
           Radiation = Radiation,
           WindSpeed = WindSpeed,
           WindDirection = WindDirection,
           WindFields = WindFields,
           WFIndex = WFIndex,
           WFFactor = WFFactor,
           params = params)
  return(ilm)
}
