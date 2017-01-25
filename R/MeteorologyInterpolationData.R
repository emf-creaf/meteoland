MeteorologyInterpolationData<-function(points, elevation, slope, aspect,
                                                      MinTemperature, MaxTemperature, Precipitation, RelativeHumidity,
                                                      Radiation = NULL, WindSpeed = NULL, WindDirection = NULL, WindFields = NULL,
                                                      params = defaultInterpolationParams()) {
  if(!inherits(points, "SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'")
  coords = coordinates(points)
  nstations = nrow(coords)
  if(length(elevation)!=nstations) stop("'elevation' has to be of the same length as the number of points.")
  if(length(slope)!=nstations) stop("'slope' has to be of the same length as the number of points.")
  if(length(aspect)!=nstations) stop("'aspect' has to be of the same length as the number of points.")
  if(!inherits(MinTemperature,"matrix")) stop("'MinTemperature' has to be a numeric matrix")
  if(!inherits(MaxTemperature,"matrix")) stop("'MaxTemperature' has to be a numeric matrix.")
  if(!inherits(Precipitation,"matrix")) stop("'Precipitation' has to be a numeric matrix.")
  if(nrow(MinTemperature)!=nstations) stop("Number of rows in 'MinTemperature' have to be equal to the number of points.")
  if(nrow(MaxTemperature)!=nstations) stop("Number of rows in 'MaxTemperature' have to be equal to the number of points.")
  if(nrow(Precipitation)!=nstations) stop("Number of rows in 'Precipitation' have to be equal to the number of points.")

  SmoothedPrecipitation = .temporalSmoothing(as.matrix(Precipitation), params$St_Precipitation, TRUE)
  SmoothedTemperatureRange = .temporalSmoothing(as.matrix(MaxTemperature-MinTemperature), params$St_TemperatureRange, FALSE)

  if(!is.null(RelativeHumidity)) {
    if(!inherits(RelativeHumidity,"matrix")) stop("'RelativeHumidity' has to be a numeric matrix.")
    if(nrow(RelativeHumidity)!=nstations) stop("Number of rows in 'RelativeHumidity' have to be equal to the number of points.")
  }
  if(!is.null(WindSpeed)) {
    if(!inherits(WindSpeed,"matrix")) stop("'WindSpeed' has to be a numeric matrix.")
    if(nrow(WindSpeed)!=nstations) stop("Number of rows in 'WindSpeed' have to be equal to the number of points.")
  }
  if(!is.null(WindDirection)) {
    if(!inherits(WindDirection,"matrix")) stop("'WindDirection' has to be a numeric matrix.")
    if(nrow(WindDirection)!=nstations) stop("Number of rows in 'WindDirection' have to be equal to the number of points.")
  }
  if(!is.null(Radiation)) {
    if(!inherits(Radiation,"matrix")) stop("'Radiation' has to be a numeric matrix.")
    if(nrow(Radiation)!=nstations) stop("Number of rows in 'Radiation' have to be equal to the number of points.")
  }

  if(!is.null(WindFields)) {
    if(!inherits(WindFields,"windparams")) stop("'WindFields' has to be an object of S3 class 'windparams'.")
  }
  WFIndex = NULL
  WFFactor = NULL
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
  dates = as.Date(colnames(Precipitation))

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
