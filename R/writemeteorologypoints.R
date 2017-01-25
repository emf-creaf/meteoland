writemeteorologypoint<-function(data, file, format = "meteoland") {
  if(format=="castanea") {
    Year=as.numeric(format(as.Date(rownames(data)),"%Y"))
    Month=as.numeric(format(as.Date(rownames(data)),"%m"))
    Day=as.numeric(format(as.Date(rownames(data)),"%d"))
    data2 = data.frame(Year= Year, Month=Month, Day=Day,
                     Radiation = data$Radiation, WindSpeed = data$WindSpeed,
                     Precipitation = data$Precipitation, MaxTemperature = data$MaxTemperature,
                     MinTemperature = data$MinTemperature,MeanTemperature = data$MeanTemperature,
                     MeanRelativeHumidity = data$MeanRelativeHumidity)
    write.table(data2,file, sep=",", col.names=FALSE, row.names = FALSE)
  } else {
    write.table(data,file, sep="\t", col.names=TRUE, row.names = TRUE, quote=FALSE)
  }
}
writemeteorologypointfiles<-function(object, dir=getwd(), format ="meteoland",
                                     metadatafile="MP.txt") {
  if(!inherits(object,"SpatialPointsMeteorology")) stop("'object' has to be of class 'SpatialPointsMeteorology'.")
  npoints = length(object@data)
  if(!is.null(names(object@data))) ids = names(object@data)
  else ids = 1:npoints
  dfout = data.frame(dir = rep(dir, npoints), filename=paste(ids,".txt", sep=""))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  rownames(dfout) = ids
  spdf = SpatialPointsDataFrame(as(object,"SpatialPoints"), dfout)
  colnames(spdf@coords)<-c("x","y")
  for(i in 1:npoints) {
    if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
    else f = dfout$filename[i]
    writemeteorologypoint(object@data[[i]], f, format)
  }
  if(dir!="") f = paste(dir,metadatafile, sep="/")
  else f = metadatafile
  write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
  invisible(spdf)
}
