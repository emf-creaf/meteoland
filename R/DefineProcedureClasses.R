#' Class \code{"MeteorologyProcedureData"}
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' A virtual class for estimating meteorology over landscapes
#' 
#' 
#' @name MeteorologyProcedureData-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyInterpolationData-class}},
#' \code{\link{MeteorologyUncorrectedData-class}}
#' @keywords classes
#' @examples
#' 
#' showClass("MeteorologyProcedureData")
#' 
#' @export
setClass("MeteorologyProcedureData", slots = list(dates = "Date"), contains="Spatial")

#' Class \code{"MeteorologyUncorrectedData"}
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' An S4 class to conduct statistical correction of meteorology over a
#' landscape.
#' 
#' 
#' @name MeteorologyUncorrectedData-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("MeteorologyUncorrectedData", ...)}, or by calls to the function
#' \code{\link{MeteorologyUncorrectedData}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyUncorrectedData}},
#' \code{\link{MeteorologyProcedureData-class}},
#' \code{\link{examplecorrectiondata}}, \code{\link{subsample}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("MeteorologyUncorrectedData")
#' 
#' @export
setClass("MeteorologyUncorrectedData",
         slots = list(coords="matrix", reference_data = "ANY", projection_data = "ANY",
                      params = "list"),
         contains="MeteorologyProcedureData")

#' Class \code{"MeteorologyInterpolationData"}
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' An S4 class to interpolate meteorology over a landscape.
#' 
#' 
#' @name MeteorologyInterpolationData-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("MeteorologyInterpolationData", ...)}, or by calls to the function
#' \code{\link{MeteorologyInterpolationData}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyInterpolationData}},
#' \code{\link{MeteorologyProcedureData-class}}, \code{\link{subsample}}
#' @keywords classes
#' @examples
#' 
#' #Structure of the S4 object
#' showClass("MeteorologyInterpolationData")
#'
#' @export
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
setGeneric("subsample", valueClass=c("MeteorologyProcedureData"), function(object, bbox = NULL, stations=NULL, dates = NULL, buffer = 0){
  standardGeneric("subsample")
})

#' Sub-sampling procedure data
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Generates a spatial and/or temporal subset of procedure data
#' 
#' 
#' @name subsample
#' @aliases subsample subsample-methods
#' subsample,MeteorologyUncorrectedData-method
#' subsample,MeteorologyInterpolationData-method
#' @docType methods
#' @param object An object of a sub-class
#' \code{\link{MeteorologyProcedureData-class}}.
#' @param bbox A 2x2 numeric matrix with the boundaries of the target area. If
#' \code{NULL}, the original boundary box is kept (except if \code{stations} is
#' specified).
#' @param stations A numeric, character or logical vector specifying a subset
#' of weather stations. If \code{NULL} all original weather stations are kept
#' (except if \code{bbox} is specified).
#' @param dates A vector of \code{\link{Date}} with the subset of dates of
#' interest. If \code{NULL}, all dates are kept.
#' @param buffer A buffer to put around bbox for the spatial selection of data.
#' @return An object of the same class as \code{object}.
#' @section Methods: \describe{ \item{subsample}{\code{signature(object =
#' "MeteorologyUncorrectedData")}: Generates a
#' \code{\link{MeteorologyUncorrectedData}} object for a smaller area and a
#' subset of dates. }
#' 
#' \item{subsample}{\code{signature(object = "MeteorologyInterpolationData")}:
#' Generates a \code{\link{MeteorologyInterpolationData}} object for a smaller
#' area and a subset of dates. } }
#' @keywords methods
#' @examples
#' 
#' \donttest{
#' data(exampleinterpolationdata)
#' 
#' oridates = exampleinterpolationdata@dates
#' 
#' #Interpolation data using the first ten dates (same boundary box)
#' subdata = subsample(exampleinterpolationdata, dates = oridates[1:10])
#' }
#' 
#' @export
setMethod("subsample", signature("MeteorologyInterpolationData"), definition =
    function(object, bbox = NULL,  stations=NULL, dates = NULL, buffer = 0) {
      cc = object@coords
      if(is.null(bbox)) bbox = object@bbox
      
      # Process station selection
      if(is.null(stations)) {
        stations = (cc[,1]>=bbox[1,1]-buffer) & (cc[,2]>=bbox[2,1]-buffer) &
          (cc[,1]<=bbox[1,2]+buffer) & (cc[,2]<=bbox[2,2]+buffer)
      } else if(is.character(stations)) {
        stations = (rownames(object@MinTemperature) %in% stations)
      } else if(is.numeric(stations)) {
        stations = ((1:nrow(cc)) %in% stations)
      }
      if(sum(stations)==0) stop("Empty subset of stations!")
      
      # Process dates selection
      if(!is.null(dates)) {
        if(!inherits(dates,"Date")) stop("'dates' has to be of class 'Date'.")
        seldates = as.character(object@dates) %in% as.character(dates)
        if(sum(seldates)==0) stop("Empty subset of dates!")
      } else {
        seldates = rep(TRUE, length(object@dates))
      }
      if(sum(seldates)<2) stop("Selected dates should be more than one.")
      
      points = SpatialPoints(cc[stations,],object@proj4string)
      ilm = new("MeteorologyInterpolationData",
                coords = cc[stations,],
                bbox = points@bbox,
                proj4string = points@proj4string,
                dates = object@dates[seldates],
                elevation = object@elevation[stations],
                slope = object@slope[stations],
                aspect = object@aspect[stations],
                MinTemperature = object@MinTemperature[stations, seldates],
                MaxTemperature = object@MaxTemperature[stations, seldates],
                SmoothedTemperatureRange = object@SmoothedTemperatureRange[stations, seldates],
                Precipitation = object@Precipitation[stations, seldates],
                SmoothedPrecipitation = object@SmoothedPrecipitation[stations, seldates],
                RelativeHumidity = object@RelativeHumidity[stations, seldates],
                Radiation = object@Radiation[stations, seldates],
                WindSpeed = object@WindSpeed[stations, seldates],
                WindDirection = object@WindDirection[stations, seldates],
                WindFields = object@WindFields,
                WFIndex = object@WFIndex,
                WFFactor = object@WFFactor,
                params = object@params)
      if(!is.null(ilm@WFIndex)) ilm@WFIndex = ilm@WFIndex[stations, seldates]
      if(!is.null(ilm@WFFactor)) ilm@WFFactor = ilm@WFFactor[stations, seldates]
      
      # Check for stations with no data in any variable
      sel = rep(FALSE, nrow(ilm@coords))
      sel = sel | (rowSums(!is.na(ilm@MinTemperature))>0)
      sel = sel | (rowSums(!is.na(ilm@MaxTemperature))>0)
      sel = sel | (rowSums(!is.na(ilm@RelativeHumidity))>0)
      sel = sel | (rowSums(!is.na(ilm@Precipitation))>0)
      sel = sel | (rowSums(!is.na(ilm@Radiation))>0)
      sel = sel | (rowSums(!is.na(ilm@WindSpeed))>0)
      sel = sel | (rowSums(!is.na(ilm@WindDirection))>0)
  
      ilm@coords = ilm@coords[sel, ]
      ilm@elevation = ilm@elevation[sel]
      ilm@slope = ilm@slope[sel]
      ilm@aspect = ilm@aspect[sel]
      ilm@MinTemperature = ilm@MinTemperature[sel, ]
      ilm@MaxTemperature = ilm@MaxTemperature[sel, ]
      ilm@SmoothedTemperatureRange = ilm@SmoothedTemperatureRange[sel, ]
      ilm@Precipitation = ilm@Precipitation[sel, ]
      ilm@SmoothedPrecipitation = ilm@SmoothedPrecipitation[sel, ]
      ilm@RelativeHumidity = ilm@RelativeHumidity[sel, ]
      ilm@Radiation = ilm@Radiation[sel, ]
      ilm@WindSpeed = ilm@WindSpeed[sel, ]
      ilm@WindDirection = ilm@WindDirection[sel, ]
      if(!is.null(ilm@WFIndex)) ilm@WFIndex = ilm@WFIndex[sel, ]
      if(!is.null(ilm@WFFactor)) ilm@WFFactor = ilm@WFFactor[sel, ]
      
      if(sum(!sel)>0) warning(paste0(sum(!sel)," stations had no data after subsetting and were removed."))
      return(ilm)
    }
)

#' @describeIn subsample Signature for MeteorologyUncorrectedData
#' @export
setMethod("subsample", signature("MeteorologyUncorrectedData"), definition =
            function(object, bbox = NULL, stations=NULL, dates = NULL, buffer = 0) {
              cc = object@coords
              if(is.null(bbox)) bbox = object@bbox
              if(is.null(stations)) {
                stations = (cc[,1]>=bbox[1,1]-buffer) & (cc[,2]>=bbox[2,1]-buffer) &
                  (cc[,1]<=bbox[1,2]+buffer) & (cc[,2]<=bbox[2,2]+buffer)
              } else if(is.character(stations)) {
                stations = (rownames(object@MinTemperature) %in% stations)
              } else if(is.numeric(stations)) {
                stations = ((1:nrow(cc)) %in% stations)
              }
              if(sum(stations)==0) stop("Empty subset of stations!")
              if(!is.null(dates)) {
                if(!inherits(dates,"Date")) stop("'dates' has to be of class 'Date'.")
                seldates = as.character(object@dates) %in% as.character(dates)
                if(sum(seldates)==0) stop("Empty subset of dates!")
              } else {
                seldates = rep(TRUE, length(object@dates))
              }
              points = SpatialPoints(cc[stations,],object@proj4string)
              reference_data = object@reference_data
              projection_data = object@projection_data
              if(!inherits(reference_data,"data.frame")){
                reference_data = reference_data[stations,]
              } else {
                reference_data = reference_data[stations]
              }
              if(!inherits(projection_data,"data.frame")){
                projection_data = projection_data[stations,]
              } else {
                projection_data = projection_data[stations]
              }
              spm = new("MeteorologyUncorrectedData",
                        coords = points@coords,
                        bbox = points@bbox,
                        proj4string = points@proj4string,
                        reference_data = reference_data,
                        projection_data = projection_data,
                        dates = object@dates[seldates],
                        params = object@params)
              return(spm)
            }
)
