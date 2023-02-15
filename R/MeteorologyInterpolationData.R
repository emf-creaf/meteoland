#' Creates an object of class 'MeteorologyInterpolationData'
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Initializes an object for meteorology interpolation over landscapes using
#' weather station data and the methods described in Thornton et al. (1997) and
#' Thornton & Running (1999).
#' 
#' @details
#' There are three ways of building an object of
#' \code{\linkS4class{MeteorologyInterpolationData}}: \enumerate{ \item{The
#' first way is using an object of
#' \code{\linkS4class{SpatialPointsMeteorology}} containing both the
#' coordinates and meteorological series of stations. In this case
#' \code{elevation} has to be provided, but \code{aspect} and \code{slope} may
#' be omitted. Parameters \code{MinTemperature} to \code{WindDirection} can be
#' left as \code{NULL}.} \item{The second way is using an object of class of
#' \code{\link{SpatialPointsTopography}} containing the coordinates of stations
#' and topographic variables. In this case parameters \code{MinTemperature} and
#' \code{MaxTemperature} will need to be supplied, each being a matrix with
#' weather stations in rows and days in columns, but \code{Precipitation},
#' \code{RelativeHumidity}, \code{Radiation}, \code{WindSpeed} and
#' \code{WindDirection} may be left as \code{NULL}.} \item{The third way is using
#' an object of \code{\link{SpatialPoints}} containing the coordinates of
#' stations only. In this case \code{elevation} has to be provided, but
#' \code{aspect} and \code{slope} may be omitted. As in the second case,
#' parameters \code{MinTemperature} and \code{MaxTemperature} will need to be
#' supplied, each being a matrix with weather stations in rows and days in
#' columns, but other variables may be left as \code{NULL}.} }
#' 
#' @param points An object of class \code{\link{SpatialPointsMeteorology}}, an
#' object of \code{\link{SpatialPointsTopography}} or an object of class
#' \code{\link{SpatialPoints}} (see 'Details'').
#' @param elevation A numeric vector with elevation values of weather stations
#' (in meters).
#' @param slope A numeric vector with slope values of weather stations (in
#' degrees). Needed for cross-validation of interpolation routines.
#' @param aspect A numeric vector with aspect values of weather stations (in
#' degrees from North). Needed for cross-validation of interpolation routines.
#' @param MinTemperature A matrix with minimum temperature recordings (in
#' degrees Celsius) for all weather stations (in rows) and all days (in
#' columns).
#' @param MaxTemperature A matrix with maximum temperature recordings (in
#' degrees Celsius) for all weather stations (in rows) and all days (in
#' columns).
#' @param Precipitation A matrix with precipitation recordings (in mm of water)
#' for all weather stations (in rows) and all days (in columns).
#' @param RelativeHumidity A matrix with (mean) relative humidity recordings
#' (in percent) for all weather stations (in rows) and all days (in columns).
#' @param Radiation A matrix with relative radiation recordings (in MJ/m2) for
#' all weather stations (in rows) and all days (in columns). Needed for
#' cross-validation only.
#' @param WindSpeed A matrix with wind speed recordings (in m/s) for all
#' weather stations (in rows) and all days (in columns).
#' @param WindDirection A matrix with wind direction recordings (in degrees
#' from North) for all weather stations (in rows) and all days (in columns).
#' @param WindFields Object of class \code{"list"}. See function
#' \code{\link{readWindNinjaWindFields}}.
#' @param params A list containing interpolation parameters.
#' @return An object of class
#' \code{\linkS4class{MeteorologyInterpolationData}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\linkS4class{MeteorologyInterpolationData}},
#' \code{\link{defaultInterpolationParams}}.
#' @references Thornton, P.E., Running, S.W., 1999. An improved algorithm for
#' estimating incident daily solar radiation from measurements of temperature,
#' humidity, and precipitation. Agric. For. Meteorol. 93, 211–228.
#' doi:10.1016/S0168-1923(98)00126-9.
#' 
#' Thornton, P.E., Running, S.W., White, M. a., 1997. Generating surfaces of
#' daily meteorological variables over large regions of complex terrain. J.
#' Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.
#' @export
MeteorologyInterpolationData<-function(points, elevation = NULL, slope = NULL, aspect = NULL,
                                       MinTemperature = NULL, MaxTemperature = NULL, Precipitation = NULL, RelativeHumidity = NULL,
                                       Radiation = NULL, WindSpeed = NULL, WindDirection = NULL, WindFields = NULL,
                                       params = defaultInterpolationParams()) {
  
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "MeteorologyInterpolationData()", with = "create_meteo_interpolator()",
    details = "MeteorologyInterpolationData class is soft deprecated.
    Interpolator object should be created with create_meteo_interpolator() which returns an star data cube object"
  )
  
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
