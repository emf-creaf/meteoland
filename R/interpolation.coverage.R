interpolation.coverage<-function(object, type="spatial", percent = FALSE) {
  if(!inherits(object, "MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'")
  type = match.arg(type, c("spatial","temporal"))
  if(type=="spatial") {
    cov_df = data.frame(MinTemperature=rowSums(!is.na(object@MinTemperature)),
                      MaxTemperature=rowSums(!is.na(object@MaxTemperature)),
                      Precipitation=rowSums(!is.na(object@Precipitation)),
                      RelativeHumidity=rowSums(!is.na(object@RelativeHumidity)),
                      Radiation=rowSums(!is.na(object@Radiation)),
                      WindSpeed=rowSums(!is.na(object@WindSpeed)),
                      WindDirection=rowSums(!is.na(object@WindDirection)))
    if(percent) cov_df = 100*cov_df/length(object@dates)
    spdf =  SpatialPointsDataFrame(object@coords, data = cov_df, proj4string = object@proj4string)
    return(spdf)
  } else if(type=="temporal") {
    cov_df = data.frame(MinTemperature=colSums(!is.na(object@MinTemperature)),
                        MaxTemperature=colSums(!is.na(object@MaxTemperature)),
                        Precipitation=colSums(!is.na(object@Precipitation)),
                        RelativeHumidity=colSums(!is.na(object@RelativeHumidity)),
                        Radiation=colSums(!is.na(object@Radiation)),
                        WindSpeed=colSums(!is.na(object@WindSpeed)),
                        WindDirection=colSums(!is.na(object@WindDirection)))
    if(percent) cov_df = 100*cov_df/nrow(object@coords)
    row.names(cov_df) = as.character(object@dates)
    return(cov_df)
  }
}
