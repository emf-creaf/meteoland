setClass("MeteorologyProcedureData", slots = list(dates = "Date"), contains="Spatial")
setClass("MeteorologyDownscalingData",
         slots = list(coords="matrix", historicdata = "ANY", futuredata = "ANY",
                      params = "list"),
         contains="MeteorologyProcedureData")
setClass("MeteorologyInterpolationData",
         slots=list(
           coords = "matrix",
           elevation = "numeric",
           slope = "numeric",
           aspect = "numeric",
           MinTemperature = "matrix",
           MaxTemperature = "matrix",
           SmoothedPrecipitation = "matrix",
           Precipitation = "matrix",
           SmoothedTemperatureRange = "matrix",
           RelativeHumidity = "matrix",
           Radiation = "ANY", #to allow null values, should be matrix
           WindSpeed = "ANY", #to allow null values, should be matrix
           WindDirection = "ANY", #to allow null values, should be matrix
           WindFields = "ANY",
           WFIndex = "ANY",
           WFFactor = "ANY",
           params = "list"),
         contains="MeteorologyProcedureData")
setGeneric("subsample", valueClass=c("MeteorologyProcedureData"), function(object, bbox = NULL, dates = NULL, buffer = 0){
  standardGeneric("subsample")
})

setMethod("subsample", signature("MeteorologyInterpolationData"), definition =
    function(object, bbox = NULL, dates = NULL, buffer = 0) {
      cc = object@coords
      if(is.null(bbox)) bbox = object@bbox
      selpoints = (cc[,1]>=bbox[1,1]-buffer) & (cc[,2]>=bbox[2,1]-buffer) &
        (cc[,1]<=bbox[1,2]+buffer) & (cc[,2]<=bbox[2,2]+buffer)
      if(sum(selpoints)==0) stop("Empty subset of stations!")
      if(!is.null(dates)) {
        if(class(dates)!="Date") stop("'dates' has to be of class 'Date'.")
        seldates = as.character(object@dates) %in% as.character(dates)
        if(sum(seldates)==0) stop("Empty subset of dates!")
      } else {
        seldates = rep(TRUE, length(object@dates))
      }
      points = SpatialPoints(cc[selpoints,],object@proj4string)
      ilm = new("MeteorologyInterpolationData",
                coords = cc[selpoints,],
                bbox = points@bbox,
                proj4string = points@proj4string,
                dates = object@dates[seldates],
                elevation = object@elevation[selpoints],
                slope = object@slope[selpoints],
                aspect = object@aspect[selpoints],
                MinTemperature = object@MinTemperature[selpoints, seldates],
                MaxTemperature = object@MaxTemperature[selpoints, seldates],
                SmoothedTemperatureRange = object@SmoothedTemperatureRange[selpoints, seldates],
                Precipitation = object@Precipitation[selpoints, seldates],
                SmoothedPrecipitation = object@SmoothedPrecipitation[selpoints, seldates],
                RelativeHumidity = object@RelativeHumidity[selpoints, seldates],
                Radiation = object@Radiation[selpoints, seldates],
                WindSpeed = object@WindSpeed[selpoints, seldates],
                WindDirection = object@WindDirection[selpoints, seldates],
                WindFields = object@WindFields,
                WFIndex = object@WFIndex,
                WFFactor = object@WFFactor,
                params = object@params)
      if(!is.null(ilm@WFIndex)) ilm@WFIndex = ilm@WFIndex[selpoints, seldates]
      if(!is.null(ilm@WFFactor)) ilm@WFFactor = ilm@WFFactor[selpoints, seldates]
      return(ilm)
      
    }
)
setMethod("subsample", signature("MeteorologyDownscalingData"), definition =
            function(object, bbox = NULL, dates = NULL, buffer = 0) {
              cc = object@coords
              if(is.null(bbox)) bbox = object@bbox
              selpoints = (cc[,1]>=bbox[1,1]-buffer) & (cc[,2]>=bbox[2,1]-buffer) &
                (cc[,1]<=bbox[1,2]+buffer) & (cc[,2]<=bbox[2,2]+buffer)
              if(sum(selpoints)==0) stop("Empty subset of stations!")
              if(!is.null(dates)) {
                if(class(dates)!="Date") stop("'dates' has to be of class 'Date'.")
                seldates = as.character(object@dates) %in% as.character(dates)
                if(sum(seldates)==0) stop("Empty subset of dates!")
              } else {
                seldates = rep(TRUE, length(object@dates))
              }
              points = SpatialPoints(cc[selpoints,],object@proj4string)
              historicdata = object@historicdata
              futuredata = object@futuredata
              if(!inherits(historicdata,"data.frame")){
                historicdata = historicdata[selpoints,]
              } else {
                historicdata = historicdata[selpoints]
              }
              if(!inherits(futuredata,"data.frame")){
                futuredata = futuredata[selpoints,]
              } else {
                futuredata = futuredata[selpoints]
              }
              spm = new("MeteorologyDownscalingData",
                        coords = points@coords,
                        bbox = points@bbox,
                        proj4string = points@proj4string,
                        historicdata = historicdata,
                        futuredata = futuredata,
                        dates = object@dates[seldates],
                        params = object@params)
              return(spm)
            }
)