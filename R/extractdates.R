extractvars<-function(object, vars, verbose = FALSE) {
  if((!inherits(object,"SpatialPointsMeteorology"))
     && (!inherits(object,"SpatialGridMeteorology"))
     && (!inherits(object,"SpatialPixelsMeteorology"))
     && (!inherits(object,"character"))) 
    stop("'object' has to be of class 'Spatial*Meteorology' of a file name.")
  
  for(i in 1:length(vars)) vars[i]<- match.arg(vars[i], c("MeanTemperature", "MinTemperature", "MaxTemperature",
                                                          "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
                                                          "Precipitation", "Radiation", "WindSpeed", "WindDirection", "SpecificHumidity", "PET"))
  if(inherits(object,"SpatialGridMeteorology")) {
    dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(vars))
    names(res)<-vars
    npts = nrow(coordinates(object))
    for(i in 1:length(vars)) {
      df = data.frame(row.names=1:npts)
      for(j in 1:length(dates)) {
        dfj = object@data[[j]]
        df[[dates[j]]] = dfj[[vars[i]]]
      }
      res[[i]] = SpatialGridDataFrame(object@grid, df, object@proj4string)
    }
  }
  else if(inherits(object,"SpatialPixelsMeteorology")) {
    dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(vars))
    names(res)<-vars
    npts = nrow(coordinates(object))
    for(i in 1:length(vars)) {
      df = data.frame(row.names=1:npts)
      for(j in 1:length(dates)) {
        dfj = object@data[[j]]
        df[[dates[j]]] = dfj[[vars[i]]]
      }
      res[[i]] = SpatialPixelsDataFrame(object@coords, data = df, 
                                        grid = object@grid, proj4string = object@proj4string)
    }
  }
  if(inherits(object,"SpatialPointsMeteorology")) {
    dates = object@dates
    npoints = length(object@data)
    if(!is.null(names(object@data))) ids = names(object@data)
    else ids = 1:npoints
    ptsout = as(object,"SpatialPoints")
    
    ndates = length(dates)
    dateschar =as.character(dates)
    res = vector("list", length(vars))
    names(res)<-vars
    
    for(i in 1:length(vars)) {
      df = data.frame(row.names=ids)
      for(j in 1:npoints) {
        dfj = object@data[[j]]
        df[j, dateschar] = dfj[[vars[i]]]
      }
      res[[i]] = SpatialPointsDataFrame(ptsout,df)
    }
  } 
  if(length(res)==1) return(res[[1]])
  return(res)  
}
extractdates<-function(object, dates = NULL, verbose=FALSE) {
  if((!inherits(object,"SpatialPointsMeteorology"))
     && (!inherits(object,"SpatialGridMeteorology"))
     && (!inherits(object,"SpatialPixelsMeteorology"))
     && (!inherits(object,"character"))) 
    stop("'object' has to be of class 'Spatial*Meteorology' of a file name.")
  if(!is.null(dates)) 
    if((!inherits(dates,"Date")) && (!inherits(dates,"character"))) 
      stop("'dates' has to be of class 'Date' or 'character'.")
  if(inherits(object,"SpatialGridMeteorology")) {
    if(is.null(dates)) dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(dates))
    names(res)<-dates
    for(i in 1:length(dates)) {
      if(!(dates[i] %in% as.character(object@dates))) stop(" Date '",paste0(dates[i], "' not found in data."))
      res[[i]] = SpatialGridDataFrame(object@grid, object@data[[dates[i]]], object@proj4string)
    }
  }
  else if(inherits(object,"SpatialPixelsMeteorology")) {
    if(is.null(dates)) dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(dates))
    names(res)<-dates
    for(i in 1:length(dates)) {
      if(!(dates[i] %in% as.character(object@dates))) stop(" Date '",paste0(dates[i], "' not found in data."))
      res[[i]] = SpatialPixelsDataFrame(object@coords, data = object@data[[dates[i]]], 
                                        grid = object@grid, proj4string = object@proj4string)
    }
  }
  if(inherits(object,"SpatialPointsMeteorology")) {
    if(is.null(dates)) dates = object@dates
    npoints = length(object)
    dfvec = vector("list",npoints)
    if(!is.null(names(object@data))) ids = names(object@data)
    else ids = 1:npoints
    ptsout = as(object,"SpatialPoints")
    
    npoints = length(ids)
    ndates = length(dates)
    dateschar =as.character(dates)
    if(verbose) cat(paste("  Extracting ", ndates, " dates from ", npoints," points...\n", sep=""))
    res = vector("list", ndates)
    
    if(verbose)  pb = txtProgressBar(0, npoints, 0, style = 3)
    for(i in 1:npoints) {
      if(verbose) setTxtProgressBar(pb, i)
      obs = object@data[[i]]
      for(d in 1:ndates) {
        if(is.null(res[[d]])) {
          res[[d]] = data.frame(matrix(NA, npoints, ncol(obs)))
          row.names(res[[d]])<-ids
          names(res[[d]])<-names(obs)
        }
        res[[d]][i,]<-obs[dateschar[d],]
      }
    }
    if(verbose) cat("\n")
    
    for(d in 1:ndates) {
      res[[d]] = SpatialPointsDataFrame(ptsout,res[[d]])
    }
  } 
  if(length(res)==1) return(res[[1]])
  return(res)  
}


