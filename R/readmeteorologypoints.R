readmeteorologypoint<-function(file, dates = NULL, format="meteoland", sep="\t") {
  CASTANEAvarnames = c("Year","Month","Day","Radiation","WindSpeed","Precipitation","MaxTemperature","MinTemperature","MeanTemperature","MeanRelativeHumidity")
  if(format=="castanea") {
    df = read.table(file,sep=",", header=FALSE)
    names(df)<-CASTANEAvarnames
    rownames(df)<-paste(df$Year,df$Month,df$Day, sep="-")
    df$MinRelativeHumidity = NA
    df$MaxRelativeHumidity = NA
    df$WindDirection = NA
    df = df[,c(9,8,7,6,10,11,12,4,5,13)]
  } else {
    df = read.table(file,sep=sep, header=TRUE)
    if(!is.null(dates)) {
      if(sum(as.character(dates) %in% rownames(df))<length(dates)) stop("Dates outside the period in data files.")
      df = df[as.character(dates),]
    }
  }
  return(df)
}
readmeteorologypointfiles<-function(points, files=NULL, dates = NULL, format="meteoland", sep="\t") {
  if(!inherits(points,"SpatialPoints") && !inherits(points,"SpatialPointsDataFrame")) stop("'points' has to be of class 'SpatialPoints' or 'SpatialPointsDataFrame'.")
  if((class(points)=="SpatialPoints") && is.null(files)) stop("Please, provide argument 'files'")
  if(!is.null(files)) if(!inherits(files,"character")) stop("'files' has to be a vector of strings.")
  if(!is.null(dates)) if(!inherits(dates,"Date")) stop("'dates' has to be of class 'Date'.")
  if(inherits(points,"SpatialPointsDataFrame")) {
    if((!("dir" %in% names(points@data))) || (!("filename" %in% names(points@data)))) stop("'points' does not contain file information (columns 'dir' and 'filename')")
    files = paste(points@data$dir, points@data$filename, sep="/")
  }
  nfiles = length(files)
  if(length(points)!=nfiles) stop("Number of spatial points must be equal to the number of files")
  cat(paste("  Reading ", nfiles," files...\n", sep=""))
  dfvec = vector("list",nfiles)
  pb = txtProgressBar(0, nfiles, 0, style = 3)
  for(i in 1:nfiles) {
    setTxtProgressBar(pb, i)
    dfvec[[i]] = readmeteorologypoint(files[i], dates,format,sep=sep)
    if(is.null(dates)) dates = as.Date(rownames(dfvec[[i]]))
    else {
      if(sum(rownames(dfvec[[i]])==as.character(dates))<length(dates)) stop("All data frames should have the same row names (dates)")
    }
  }
  cat("\n")
  return(SpatialPointsMeteorology(as(points,"SpatialPoints"), dfvec, dates))
}
