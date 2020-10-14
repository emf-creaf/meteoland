# Bias-correction of a single series
correction_series<-function(obs, mod, proj = NULL, method = "unbias", isPrec=TRUE, qstep=0.01) {
  if(method=="unbias") {
    corr<-mean(obs-mod, na.rm=TRUE)
  } else if(method=="scaling") {
    corr<- as.numeric(lm(obs~mod-1)$coefficients) #slope of a regression through the origin
  } else if(method=="quantmap") {
    corr<-fitQmapDeque(obs,mod, isPrec, qstep)
  }
  if(!is.null(proj)) return(.corrApply(proj, corr, method))
  return(.corrApply(mod, corr, method))
}
# Bias-correction of all variables for a single point
correctionpoint<-function(obs, mod, proj, dates = NULL, params = defaultCorrectionParams(), verbose=TRUE){
  
  #Calculate mean temperature if absent
  if(!("MeanTemperature" %in% names(obs))) {
    obs$MeanTemperature = 0.606*obs$MaxTemperature+0.394*obs$MinTemperature
  }
  
  #Call statistical correction routine
  mbias = .monthbiasonepoint(obs,mod, 
                             params$varmethods, 
                             qstep = params$qstep, 
                             verbose = verbose)
  
  # if(verbose) print(mbias)
  df = .correctiononepoint(mbias,proj, dates, 
                           fill_wind = params$fill_wind, 
                           allow_saturated =params$allow_saturated,
                           verbose = verbose)
  return(list(monthlyBias = mbias, correctedProj = df))
}
# Bias-correction of all variables for multiple points
correctionpoints<-function(object, points, topodata = NULL, dates = NULL, export = FALSE,
                           exportDir = getwd(), exportFile = NULL, exportFormat = "meteoland/txt",
                           metadataFile = "MP.txt", corrOut = FALSE, verbose=TRUE) {

  if(export) exportFormat = match.arg(exportFormat, c("meteoland/txt", "meteoland/rds", "castanea/txt", "castanea/rds", "netCDF"))
  
  #Check input classes
  if(!inherits(object,"MeteorologyUncorrectedData")) stop("'object' has to be of class 'MeteorologyUncorrectedData'.")
  if(!inherits(points,"SpatialPointsMeteorology") && !inherits(points,"SpatialPointsDataFrame")) stop("'points' has to be of class 'SpatialPointsMeteorology' or 'SpatialPointsDataFrame'.")

  mPar = object@params

  npoints = length(points)

  if(verbose) cat(paste("Points to correct: ", npoints,"\n", sep=""))

  #Project points into long/lat coordinates to check if they are inside the boundary box
  cchist = spTransform(points, object@proj4string)
  sel = (cchist@coords[,1] >= object@bbox[1,1] & cchist@coords[,1] <=object@bbox[1,2]) &
    (cchist@coords[,2] >= object@bbox[2,1] & cchist@coords[,2] <=object@bbox[2,2])
  if(sum(sel)<npoints) {
    warning("At least one target point is outside the boundary box of 'object'.\n", call. = FALSE, immediate.=TRUE)
  } else if(verbose) cat(paste("All points inside boundary box.\n", sep=""))

  longlat = spTransform(points,CRS("+proj=longlat"))
  latitude = longlat@coords[,2]

  #Project long/lat coordinates of predicted climatic objects into the projection of points
  xypred = spTransform(SpatialPoints(object@coords,object@proj4string,object@bbox), points@proj4string)
  colnames(xypred@coords)<-c("x","y")
  if(!is.null(topodata)) {
    latrad = latitude*(pi/180)
    elevation = topodata$elevation
    slorad = topodata$slope*(pi/180)
    asprad = topodata$aspect*(pi/180)
  }
  # Define vector of data frames
  mbiasvec = vector("list", npoints)
  
  if(inherits(points,"SpatialPointsMeteorology")) {
    if(!is.null(names(points@data))) ids = names(points@data)
    else ids = 1:npoints
  } else {
    if(!is.null(rownames(points@data))) ids = rownames(points@data)
    else ids = 1:npoints
  }
  
  if(exportFormat %in% c("meteoland/txt","castanea/txt")) formatType = "txt"
  else if (exportFormat %in% c("meteoland/rds","castanea/rds")) formatType = "rds"
  else if (exportFormat %in% c("netCDF")) formatType = "netCDF"
  

  if(export & exportFormat %in% c("meteoland/txt","castanea/txt", "meteoland/rds","castanea/rds")) {
    dfout = data.frame(dir = rep(exportDir, npoints), filename=paste0(ids,".", formatType))
    dfout$dir = as.character(dfout$dir)
    dfout$filename = as.character(dfout$filename)
    dfout$format = exportFormat
    rownames(dfout) = ids
    spdf = SpatialPointsDataFrame(as(points,"SpatialPoints"), dfout)
    colnames(spdf@coords)<-c("x","y")
  }
  else if(export & exportFormat=="netCDF") {
    if(is.null(exportFile)) stop("File 'exportFile' cannot be null when exporting to netCDF!")
    ncfile = exportFile
    if(is.null(dates)) dates = object@dates
    nc <-.openwritepointNetCDF(coordinates(points), proj4string(points), dates = dates, vars = NULL,
                               file = ncfile, overwrite = TRUE, verbose = verbose)
  } else {
    dfvec = vector("list",npoints)
  }
  
  #Loop over all points
  for(i in 1:npoints) {
    if(verbose) cat(paste("Correcting point '",ids[i],"' (",i,"/",npoints,") -",sep=""))
    xy = points@coords[i,]
    #observed data frame
    if(inherits(points,"SpatialPointsMeteorology")) {
      obs = points@data[[i]]
    } else {
      f = paste(points@data$dir[i], points@data$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      if("format" %in% names(points@data)) { ##Format specified
        obs = readmeteorologypoint(f, format=points@data$format[i])
      } else {
        obs = readmeteorologypoint(f)
      }
    }
    #Find closest predicted climatic cell for reference/projection periods (ideally the same)
    d = sqrt(rowSums(sweep(xypred@coords,2,xy,"-")^2))
    ipred = which.min(d)[1]
    if(verbose) cat(paste(" ipred = ",ipred, sep=""))
    #predicted climatic data frames
    if(inherits(object@reference_data,"list")) {
      rcmhist = object@reference_data[[ipred]]
    } else if(inherits(object@reference_data,"character")) {
      rcmhist = readmeteorologypoints(object@reference_data, stations = ipred)@data[[1]]
    } else {
      if(("dir" %in% names(object@reference_data))&&("filename" %in% names(object@reference_data))) {
        f = paste(object@reference_data$dir[ipred], object@reference_data$filename[ipred],sep="/")
        if(!file.exists(f)) stop(paste("Reference meteorology file '", f,"' does not exist!", sep=""))
        if("format" %in% names(object@reference_data)) { ##Format specified
          rcmhist = readmeteorologypoint(f, format=object@reference_data$format[ipred])
        } else {
          rcmhist = readmeteorologypoint(f)
        }
      } else if(nrow(object@coords)==1) {
        rcmhist = object@reference_data
      } else {
        stop("Cannot access reference meteorology data")
      }
    }
    if(inherits(object@projection_data,"list")) {
      rcmfut = object@projection_data[[ipred]]
    } else if(inherits(object@projection_data,"character")) {
      rcmfut = readmeteorologypoints(object@projection_data, stations = ipred)@data[[1]]
    } else {
      if(("dir" %in% names(object@projection_data))&&("filename" %in% names(object@projection_data))) {
        f = paste(object@projection_data$dir[ipred], object@projection_data$filename[ipred],sep="/")
        if(!file.exists(f)) stop(paste("Projection meteorology file '", f,"' does not exist!",sep=""))
        if("format" %in% names(object@projection_data)) { ##Format specified
          rcmfut = readmeteorologypoint(f, format=object@projection_data$format[ipred])
        } else {
          rcmfut = readmeteorologypoint(f)
        }
      } else if(nrow(object@coords)==1) {
        rcmfut = object@projection_data
      } else {
        stop("Cannot access projection meteorology data")
      }
    }
 
    #Call statistical correction routine
    res = correctionpoint(obs, rcmhist, rcmfut, dates, mPar, verbose)
    df = res$correctedProj
    mbiasvec[[i]] = res$monthlyBias
    
    if(is.null(dates)) dates = as.Date(rownames(df))
    #Calculate PET
    if(!is.null(topodata)) {
      J = radiation_dateStringToJulianDays(as.character(dates))
      df$PET = .penmanpoint(latrad[i], elevation[i],slorad[i], asprad[i], J, 
                         df$MinTemperature, df$MaxTemperature,
                         df$MinRelativeHumidity, df$MaxRelativeHumidity, df$Radiation,
                         df$WindSpeed, mPar$wind_height,
                         0.001, 0.25);
    }

    #Write file
    if(!export) {
      dfvec[[i]] =df
      if(verbose) cat(" done")
    } else {
      if(exportFormat!="netCDF") {
        if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
        else f = dfout$filename[i]
        writemeteorologypoint(df, f, dfout$format[i])
        if(verbose) cat(paste(" written to ",f, sep=""))
        if(exportDir!="") f = paste(exportDir,metadataFile, sep="/")
        else f = metadataFile
        write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
      } else {
        if(verbose) cat(paste0(" written to netCDF"))
        .writemeteorologypointNetCDF(df,nc,i)
      }
    }
    if(verbose) cat(".\n")
  }
  if(!export) {
    if(!corrOut) return(SpatialPointsMeteorology(points = points, data = dfvec, dates = dates))
    else {
      return(list(SpatialPointsMeteorology(points = points, data = dfvec, dates = dates),
                  mbiasvec))
    }
  } else {
    if(exportFormat!="netCDF") {
      if(!corrOut) invisible(spdf)
      else invisible(list(spdf,mbiasvec))
    } else {
      .closeNetCDF(ncfile, nc)
      if(corrOut) invisible(mbiasvec)
    }    
  }
}

