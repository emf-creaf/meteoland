#' Reads point meteorology from the disk
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Functions to read point meteorological data from the disks in different
#' formats.
#' 
#' @details
#' Function \code{readmeteorologypoint} reads data series of a single location
#' from an ascii or rds file and returns a data frame. Function
#' \code{readmeteorologypointfiles} can be used to read multiple ascii/rds
#' files and build an object of \code{\link{SpatialPointsMeteorology-class}}.
#' This is done by supplying an \code{points} object of class
#' \code{SpatialPointsDataFrame-class} with point meta data. In
#' \code{readmeteorologypointfiles} the value of \code{format} is used as
#' default but can be overloaded if \code{points} includes a column
#' '\code{format}'.
#' 
#' Function \code{readmeteorologypoints} is used to read multiple point data
#' from a netCDF. In this case, a mapping can be supplied to map variable names
#' in the netCDF to variables used in meteoland.
#' 
#' @aliases readmeteorologypoint readmeteorologypointfiles
#' readmeteorologypoints
#' @param file A string of the file to be read.
#' @param points An object of class \code{\link{SpatialPoints-class}} (in this
#' case \code{files} cannot be \code{NULL}) or object of class
#' \code{\link{SpatialPointsDataFrame-class}} with two data columns:
#' '\code{dir}' and '\code{filename}' (and possibly '\code{format}').
#' @param files A vector of strings to be read (when \code{points} is of class
#' \code{\link{SpatialPoints-class}}). Length and order must match
#' \code{points}.
#' @param dates Object of class \code{"Date"} describing a subset of dates to
#' be extracted from meteorological series. If \code{NULL} the whole period
#' read from files is kept.
#' @param format Format of meteorological data. Current accepted formats for
#' \code{readmeteorologypoint} and \code{readmeteorologypointfiles} are
#' \code{"meteoland/txt"}, \code{"meteoland/rds"}, \code{"castanea/txt"} and
#' \code{"castanea/rds"}. The only accepted format for
#' \code{readmeteorologypoints} is \code{"netCDF"}.
#' @param sep The field separator character for ascii text files (see
#' \code{\link{read.table}}).
#' @param stations An integer vector or string vector identifying point indices
#' or station names in the netCDF.
#' @param varmapping Named character vector specifying a mapping of variables
#' in the NetCDF into variables used in meteoland (e.g. \code{c(MinTemperature
#' = "tmn")} specifies a map of variable 'tmn' to MinTemperature).
#' @param verbose A logical flag to output process information in the console.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' Nicolas Martin, INRA-Avignon
#' @seealso \code{\link{writemeteorologypoint}}, \code{\link{read.table}},
#' \code{\link{SpatialPointsMeteorology-class}}
#' @export
readmeteorologypoint<-function(file, dates = NULL, format="meteoland/txt", sep="\t") {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "readmeteorologypoint()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Read of meteorology source data must be done with the corresponding package (sf, terra, stars...)
    and meteorology data converted to a sf points object"
  )
  
  format = match.arg(format, c("meteoland/txt", "meteoland/rds", "castanea/txt", "castanea/rds"))
  CASTANEAvarnames = c("Year","Month","Day","Radiation","WindSpeed","Precipitation","MaxTemperature","MinTemperature","MeanTemperature","MeanRelativeHumidity")
  if(format=="castanea/txt") {
    df = read.table(file,sep=",", header=FALSE)
    names(df)<-CASTANEAvarnames
    rownames(df)<-paste(df$Year,df$Month,df$Day, sep="-")
    df$MinRelativeHumidity = NA
    df$MaxRelativeHumidity = NA
    df$WindDirection = NA
    df = df[,c(9,8,7,6,10,11,12,4,5,13)]
  } else if(format=="castanea/rds") {
    df = readRDS(file)
    names(df)<-CASTANEAvarnames
    rownames(df)<-paste(df$Year,df$Month,df$Day, sep="-")
    df$MinRelativeHumidity = NA
    df$MaxRelativeHumidity = NA
    df$WindDirection = NA
    df = df[,c(9,8,7,6,10,11,12,4,5,13)]
  } else if(format=="meteoland/txt") {
    df = read.table(file,sep=sep, header=TRUE)
    if(!is.null(dates)) {
      if(sum(as.character(dates) %in% rownames(df))<length(dates)) stop("Dates outside the period in data files.")
      df = df[as.character(dates),]
    }
  } else if(format=="meteoland/rds") {
    df = readRDS(file)
    if(!is.null(dates)) {
      if(sum(as.character(dates) %in% rownames(df))<length(dates)) stop("Dates outside the period in data files.")
      df = df[as.character(dates),]
    }
  }
  return(df)
}
#' @describeIn readmeteorologypoint `r lifecycle::badge("deprecated")`
#' @export
readmeteorologypointfiles<-function(points, files=NULL, dates = NULL, format="meteoland/txt", sep="\t") {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "readmeteorologypointfiles()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Read of meteorology source data must be done with the corresponding package (sf, terra, stars...)
    and meteorology data converted to a sf points object"
  )
  
  format = match.arg(format, c("meteoland/txt", "meteoland/rds", "castanea/txt", "castanea/rds"))
  if(!inherits(points,"SpatialPoints") && !inherits(points,"SpatialPointsDataFrame")) stop("'points' has to be of class 'SpatialPoints' or 'SpatialPointsDataFrame'.")
  if((inherits(points,"SpatialPoints")) && is.null(files)) stop("Please, provide argument 'files'")
  if(!is.null(files)) if(!inherits(files,"character")) stop("'files' has to be a vector of strings.")
  if(!is.null(dates)) if(!inherits(dates,"Date")) stop("'dates' has to be of class 'Date'.")
  ff =rep(format,length(points))
  if(inherits(points,"SpatialPointsDataFrame")) {
    if((!("dir" %in% names(points@data))) || (!("filename" %in% names(points@data)))) stop("'points' does not contain file information (columns 'dir' and 'filename')")
    files = paste(points@data$dir, points@data$filename, sep="/")
    if("format" %in% names(points@data)) ff = points@data$format
  }
  nfiles = length(files)
  if(length(points)!=nfiles) stop("Number of spatial points must be equal to the number of files")
  cat(paste("  Reading ", nfiles," files...\n", sep=""))
  dfvec = vector("list",nfiles)
  pb = txtProgressBar(0, nfiles, 0, style = 3)
  for(i in 1:nfiles) {
    setTxtProgressBar(pb, i)
    dfvec[[i]] = readmeteorologypoint(files[i], dates,ff[i],sep=sep)
    if(is.null(dates)) dates = as.Date(rownames(dfvec[[i]]))
    else {
      if(sum(rownames(dfvec[[i]])==as.character(dates))<length(dates)) stop("All data frames should have the same row names (dates)")
    }
  }
  cat("\n")
  return(SpatialPointsMeteorology(as(points,"SpatialPoints"), dfvec, dates))
}
#' @describeIn readmeteorologypoint `r lifecycle::badge("deprecated")`
#' @export
readmeteorologypoints<-function(files, dates = NULL, stations = NULL, format = "netCDF", varmapping = NULL, verbose = FALSE) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "readmeteorologypoints()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Read of meteorology source data must be done with the corresponding package (sf, terra, stars...)
    and meteorology data converted to a sf points object"
  )
  
  nfiles = length(files)
  l = vector("list", nfiles)
  for(i in 1:nfiles) {
    nc = .openreadpointsNetCDF(files[i], verbose = verbose)
    if(!is.null(dates)) {
      if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
      spm = .readmeteorologypointsNetCDF(nc, dates = as.Date(dates), stations = stations, varmapping = varmapping, verbose = verbose)
    } else {
      spm = .readmeteorologypointsNetCDF(nc, stations = stations, varmapping = varmapping, verbose = verbose)
    }
    .closeNetCDF(files[i],nc, verbose = verbose)
    l[[i]] = spm
  }
  if(nfiles==1) return(l[[1]])
  if(verbose) cat("\nMerging point data...\n")
  return(mergepoints(l, verbose = verbose))
}
