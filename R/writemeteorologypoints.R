# Writes one file with meteorology data series of a data frame


#' Writes point meteorology to the disk
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Functions to write point meteorological data to disk files in different
#' formats.
#' 
#' @details
#' Function \code{writemeteorologypoint} writes data series of a single
#' location (i.e. a data frame) to ascii or rds files. Function
#' \code{writemeteorologypointfiles} takes an object of
#' \code{\link{SpatialPointsMeteorology-class}} and writes one file for each
#' data point in the same formats as \code{writemeteorologypoint}. In addition,
#' it writes a metadata file (see argument \code{metadataFile}) with point
#' coordinates, station names and file names. Both functions share the same
#' accepted formats, which are \code{"meteoland/txt"}, \code{"meteoland/rds"},
#' \code{"castanea/txt"} and \code{"castanea/rds"}.
#' 
#' Function \code{writemeteorologypoints} takes an object of
#' \code{\link{SpatialPointsMeteorology-class}} and writes all its content in a
#' single file (i.e. a netCDF). The same function can be used to replace data
#' from an existing file or to add new points to the netCDF. This is done by
#' using \code{add=TRUE}, and profits from the fact that some netCDF dimensions
#' (in this case the number of points) can be defined as unlimited. If data is
#' added to an existing netCDF, the coordinate reference system and the dates
#' of \code{object} must be the same as those in the netCDF.
#' 
#' @aliases writemeteorologypoint writemeteorologypoints
#' writemeteorologypointfiles
#' @param data An data frame with meteorological data.
#' @param file A string with the file name to be written.
#' @param format Output format of meteorological data. Current accepted formats
#' are \code{"meteoland/txt"}, \code{"meteoland/rds"}, \code{"castanea/txt"}
#' and \code{"castanea/rds"}.
#' @param object An object of class
#' \code{\link{SpatialPointsMeteorology-class}} with the meteorological data to
#' be written.
#' @param dir Output directory for meteorology data.
#' @param metadataFile The name of the file that will store the meta data
#' describing all written files.
#' @param add Boolean flag to indicate that NetCDF exists and data should be
#' added/replaced (see details).
#' @param overwrite Boolean flag to force overwriting an existing NetCDF.
#' @param verbose A logical flag to output process information in the console.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' Nicolas Martin, INRA-Avignon
#' @seealso \code{\link{readmeteorologypoint}},
#' \code{\link{SpatialPointsMeteorology-class}}
#' @export
writemeteorologypoint<-function(data, file, format = "meteoland/txt") {
  format = match.arg(format, c("meteoland/txt", "meteoland/rds", "castanea/txt", "castanea/rds"))
  if(format=="castanea/txt" || format=="castanea/rds") {
    Year=as.numeric(format(as.Date(rownames(data)),"%Y"))
    Month=as.numeric(format(as.Date(rownames(data)),"%m"))
    Day=as.numeric(format(as.Date(rownames(data)),"%d"))
    data2 = data.frame(Year= Year, Month=Month, Day=Day,
                     Radiation = data$Radiation, WindSpeed = data$WindSpeed,
                     Precipitation = data$Precipitation, MaxTemperature = data$MaxTemperature,
                     MinTemperature = data$MinTemperature,MeanTemperature = data$MeanTemperature,
                     MeanRelativeHumidity = data$MeanRelativeHumidity)
    if(format=="castanea/txt") {
      write.table(data2,file, sep=",", col.names=FALSE, row.names = FALSE)
    } else if(format =="castanea/rds") {
      saveRDS(data2,file)
    }
  } else if(format=="meteoland/txt") {
    write.table(data,file, sep="\t", col.names=TRUE, row.names = TRUE, quote=FALSE)
  } else if(format=="meteoland/rds") {
    saveRDS(data,file)
  }
}
# Writes multiple files, one for each point
#' @describeIn writemeteorologypoint `r lifecycle::badge("deprecated")`
#' @export
writemeteorologypointfiles<-function(object, dir=getwd(), format ="meteoland/txt",
                                     metadataFile="MP.txt") {
  format = match.arg(format, c("meteoland/txt", "meteoland/rds", "castanea/txt", "castanea/rds"))
  if(!inherits(object,"SpatialPointsMeteorology")) stop("'object' has to be of class 'SpatialPointsMeteorology'.")
  npoints = length(object@data)
  if(!is.null(names(object@data))) ids = names(object@data)
  else ids = 1:npoints
  
  if(format %in% c("meteoland/txt","castanea/txt")) formatType = "txt"
  else if (format %in% c("meteoland/rds","castanea/rds")) formatType = "rds"
  
  
  dfout = data.frame(dir = rep(dir, npoints), filename=paste0(ids,".", formatType))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  dfout$format = format
  rownames(dfout) = ids
  spdf = SpatialPointsDataFrame(as(object,"SpatialPoints"), dfout)
  colnames(spdf@coords)<-c("x","y")
  for(i in 1:npoints) {
    if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
    else f = dfout$filename[i]
    writemeteorologypoint(object@data[[i]], f, format)
  }
  if(dir!="") f = paste(dir,metadataFile, sep="/")
  else f = metadataFile
  write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
  invisible(spdf)
}
# Writes point meteorology to netCDF
#' @describeIn writemeteorologypoint `r lifecycle::badge("deprecated")`
#' @export
writemeteorologypoints<-function(object, file, format = "netCDF", add = FALSE, 
                                 overwrite = FALSE, verbose = FALSE) {
  if(!inherits(object,"SpatialPointsMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  if(!add) {
    vars = NULL
    if(length(object@data)>0) vars = names(object@data[[1]])
    nc = .openwritepointNetCDF(object@coords, proj4string = proj4string(object), dates = object@dates, vars = vars,
                               file=file, overwrite = overwrite, verbose = verbose)
    .writemeteorologypointsNetCDF(data = object@data, nc=nc, verbose = verbose)
  } else {
    nc = .openaddNetCDF(file, verbose=verbose)
    .addreplacemeteorologypointsNetCDF(object, nc=nc, verbose = verbose)
  }
  .closeNetCDF(file,nc, verbose = verbose)
}
