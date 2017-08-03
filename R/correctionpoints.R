#Calculates biases or other correction parameters for a given data period
.corrParam<-function(DatTemp, ModelTempHist, varmethods, varname, varnamemean = NULL, verbose=TRUE) {
  if(varmethods[varname]=="unbias") {
    corr<-mean(ModelTempHist[,varname]-DatTemp[,varname], na.rm=TRUE)
  } else if(varmethods[varname]=="scaling") {
    if(!is.null(varnamemean)) {
      difHist = (ModelTempHist[,varname]-ModelTempHist[,varnamemean])
      difDat = (DatTemp[,varname]-DatTemp[,varnamemean])
    } else {
      difHist = ModelTempHist[,varname]
      difDat = DatTemp[,varname]
    }
    corr<- as.numeric(lm(difDat~difHist-1)$coefficients) #slope of a regression through the origin
  } else if(varmethods[varname]=="quantmap") {
    corr<-fitQmap(DatTemp[,varname],ModelTempHist[,varname],method=c("QUANT"))
  } else {
    stop(paste("Wrong correction method for variable:", varname))
  }
  return(corr)
}
#Apply correction depending on the correction method
.corrApply<-function(varuncor, varbias, varmethod) {
  if(varmethod=="unbias") {
    corrected <- (varuncor-varbias)
  } else if(varmethod=="scaling") {
    corrected <- (varuncor*varbias)
  } else if(varmethod=="quantmap") {
    corrected<-doQmap(varuncor, varbias)
  } else {
    stop(paste("Wrong correction method:", varmethod))
  }
  return(corrected)
}



#Calculates monthly biases/params for all twelve months
.monthbiasonepoint<-function(Data, MODHist, varmethods, verbose=TRUE) {
  sel1 = rownames(MODHist) %in% rownames(Data)
  sel2 = rownames(Data) %in% rownames(MODHist)

  # print(varmethods)
  
  #subset compatible data
  MODHist = MODHist[sel1,]
  Data = Data[sel2,]
  # if(verbose) cat(paste(", # historic records = ", nrow(MODHist), sep=""))

  corrPrec<-vector("list",12)
  corrRad<-vector("list",12)
  corrTmean<-vector("list",12)
  corrTmin<-vector("list",12)
  corrTmax<-vector("list",12)
  corrWS<-vector("list",12) #WindSpeed
  corrHS<-vector("list",12) #SpecificHumidity

  Data.months = as.numeric(format(as.Date(rownames(Data)),"%m"))
  MODHist.months = as.numeric(format(as.Date(rownames(MODHist)),"%m"))
  for (m in 1:12){
    # if(verbose) cat(".")
    DatTemp<-Data[Data.months==m,]
    ModelTempHist<-MODHist[MODHist.months==m,]

    #Calculate correction params depending on the correction method
    corrTmean[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "MeanTemperature")
    corrTmin[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "MinTemperature", "MeanTemperature")
    corrTmax[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "MaxTemperature", "MeanTemperature")
    corrPrec[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "Precipitation")
    corrRad[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "Radiation")
    corrWS[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "WindSpeed")
    HSData<-.HRHS(Tc=DatTemp[,"MeanTemperature"] ,HR=DatTemp[,"MeanRelativeHumidity"])
    HSmodelHist<-.HRHS(Tc=ModelTempHist[,"MeanTemperature"] ,HR=ModelTempHist[,"MeanRelativeHumidity"])
    if(varmethods["MeanRelativeHumidity"]=="unbias") {
      corrHS[[m]]<-mean(HSmodelHist-HSData, na.rm=TRUE)
    } else if(varmethods["MeanRelativeHumidity"]=="scaling") {
      corrHS[[m]]<- as.numeric(lm(HSData~HSmodelHist-1)$coefficients) #slope of a regression through the origin
    } else if(varmethods["MeanRelativeHumidity"]=="quantmap") {
      corrHS[[m]]<-fitQmap(HSData,HSmodelHist,method=c("QUANT"))
    } else {
      stop(paste("Wrong correction method for variable:", "MeanRelativeHumidity"))
    }
  }
  return(list(varmethods = varmethods, corrTmean=corrTmean,corrTmin=corrTmin,corrTmax=corrTmax,
              corrPrec=corrPrec,corrRad=corrRad,corrHS=corrHS,corrWS=corrWS))
}

#Apply monthly corrections for one point
.correctiononepoint<-function(mbias, MODFut, dates = NULL, fill_wind = FALSE, verbose=TRUE){
  if(!is.null(dates)) {
    sel3 = rownames(MODFut) %in% as.character(dates)
    if(sum(sel3)!=length(dates)) stop("Some dates are outside the predicted period.")
    MODFut = MODFut[sel3,]
  }
  ndays = nrow(MODFut)

  #Result
  ResCor <- data.frame(
    DOY =as.POSIXlt(as.Date(rownames(MODFut)))$yday,
    MeanTemperature=rep(NA, ndays),
    MinTemperature=rep(NA, ndays),
    MaxTemperature=rep(NA, ndays),
    Precipitation=rep(NA, ndays),
    MeanRelativeHumidity=rep(NA, ndays),
    MinRelativeHumidity=rep(NA, ndays),
    MaxRelativeHumidity=rep(NA, ndays),
    Radiation=rep(NA, ndays),
    WindSpeed=rep(NA, ndays),
    WindDirection=rep(NA, ndays),
    PET=rep(NA, ndays))
  rownames(ResCor) = rownames(MODFut)


  
  
  MODFut.months = as.numeric(format(as.Date(rownames(MODFut)),"%m"))
  for (m in 1:12){
    selFut <- (MODFut.months==m)
    ModelTempFut<-MODFut[selFut,]

    

    #Correction Tmean
    ModelTempFut.TM.cor <-.corrApply(ModelTempFut$MeanTemperature, mbias$corrTmean[[m]], mbias$varmethods["MeanTemperature"])
    
    #Correction Tmin
    if(mbias$varmethods["MinTemperature"]=="scaling") {
      ModelTempFut.TN.cor<-ModelTempFut.TM.cor + ((ModelTempFut$MinTemperature-ModelTempFut$MeanTemperature)*mbias$corrTmin[[m]])
    } else {
      ModelTempFut.TN.cor<-.corrApply(ModelTempFut$MinTemperature, mbias$corrTmin[[m]], mbias$varmethods["MinTemperature"])
    }
    
    #Correction Tmax
    if(mbias$varmethods["MinTemperature"]=="scaling") {
      ModelTempFut.TX.cor<-ModelTempFut.TM.cor + ((ModelTempFut$MaxTemperature-ModelTempFut$MeanTemperature)*mbias$corrTmax[[m]])
    } else {
      ModelTempFut.TX.cor<-.corrApply(ModelTempFut$MaxTemperature, mbias$corrTmax[[m]], mbias$varmethods["MaxTemperature"])
    }
    
    #Correction Precipitation
    ModelTempFut.rain.cor<-.corrApply(ModelTempFut$Precipitation, mbias$corrPrec[[m]], mbias$varmethods["Precipitation"])

    #Correction Rg
    ModelTempFut.Rg.cor<-.corrApply(ModelTempFut$Radiation, mbias$corrRad[[m]], mbias$varmethods["Radiation"])
    ModelTempFut.Rg.cor[ModelTempFut.Rg.cor<0]<-0

    #Correction WS (if NA then use input WS)
    if(!is.na(mbias$corrWS[[m]])) {
      ModelTempFut.WS.cor<-.corrApply(ModelTempFut$WindSpeed, mbias$corrWS[[m]], mbias$varmethods["WindSpeed"])
    }
    else if(fill_wind) ModelTempFut.WS.cor<-ModelTempFut$WindSpeed
    ModelTempFut.WS.cor[ModelTempFut.WS.cor<0]<-0
    
    #Correction RH
    #First transform RH into specific humidity
    HSmodelFut<-.HRHS(Tc=ModelTempFut.TM.cor ,HR=ModelTempFut[,"MeanRelativeHumidity"])
    #Second compute and apply the bias to specific humidity
    HSmodelFut.cor<-.corrApply(HSmodelFut, mbias$corrHS[[m]], mbias$varmethods["MeanRelativeHumidity"])
    #Back transform to relative humidity (mean, max, min)
    ModelTempFut.RHM.cor<-.HSHR(Tc=ModelTempFut.TM.cor ,HS=HSmodelFut.cor)
    ModelTempFut.RHM.cor[ModelTempFut.RHM.cor<0]<-0
    ModelTempFut.RHM.cor[ModelTempFut.RHM.cor>100]<-100
    ModelTempFut.RHX.cor<-.HSHR(Tc=ModelTempFut.TN.cor ,HS=HSmodelFut.cor)
    ModelTempFut.RHX.cor[ModelTempFut.RHX.cor<0]<-0
    ModelTempFut.RHX.cor[ModelTempFut.RHX.cor>100]<-100
    ModelTempFut.RHN.cor<-.HSHR(Tc=ModelTempFut.TX.cor ,HS=HSmodelFut.cor)
    ModelTempFut.RHN.cor[ModelTempFut.RHN.cor<0]<-0
    ModelTempFut.RHN.cor[ModelTempFut.RHN.cor>100]<-100

    #Check RHmin <= RHmean <= RHmax
    ModelTempFut.RHN.cor = pmin(ModelTempFut.RHN.cor, ModelTempFut.RHX.cor)
    ModelTempFut.RHX.cor = pmax(ModelTempFut.RHN.cor, ModelTempFut.RHX.cor)
    ModelTempFut.RHM.cor = pmin(pmax(ModelTempFut.RHM.cor, ModelTempFut.RHN.cor),ModelTempFut.RHX.cor)
    
    #Store results
    ResCor$MeanTemperature[selFut]=ModelTempFut.TM.cor
    ResCor$MinTemperature[selFut]=ModelTempFut.TN.cor
    ResCor$MaxTemperature[selFut]=ModelTempFut.TX.cor
    ResCor$Precipitation[selFut]=ModelTempFut.rain.cor
    ResCor$MeanRelativeHumidity[selFut]=ModelTempFut.RHM.cor
    ResCor$MinRelativeHumidity[selFut]=ModelTempFut.RHN.cor
    ResCor$MaxRelativeHumidity[selFut]=ModelTempFut.RHX.cor
    ResCor$Radiation[selFut]=ModelTempFut.Rg.cor
    ResCor$WindSpeed[selFut]=ModelTempFut.WS.cor
  }
  ResCor[is.na(ResCor)] = NA
  return(ResCor)
}

correctionpoints<-function(object, points, topodata = NULL, dates = NULL, export = FALSE,
                            exportDir = getwd(), exportFormat = "meteoland",
                            metadatafile = "MP.txt", corrOut = FALSE, verbose=TRUE) {

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
  dfvec = vector("list",npoints)
  mbiasvec = vector("list", npoints)
  
  if(inherits(points,"SpatialPointsMeteorology")) {
    if(!is.null(names(points@data))) ids = names(points@data)
    else ids = 1:npoints
  } else {
    if(!is.null(rownames(points@data))) ids = rownames(points@data)
    else ids = 1:npoints
  }
  dfout = data.frame(dir = rep(exportDir, npoints), filename=paste(ids,".txt",sep=""))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  rownames(dfout) = ids
  spdf = SpatialPointsDataFrame(as(points,"SpatialPoints"), dfout)
  colnames(spdf@coords)<-c("x","y")


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
      obs = readmeteorologypoint(f)
    }
    #Find closest predicted climatic cell for reference/projection periods (ideally the same)
    d = sqrt(rowSums(sweep(xypred@coords,2,xy,"-")^2))
    ipred = which.min(d)[1]
    if(verbose) cat(paste(" ipred = ",ipred, sep=""))
    #predicted climatic data frames
    if(inherits(object@reference_data,"list")) {
      rcmhist = object@reference_data[[ipred]]
    } else {
      if(("dir" %in% names(object@reference_data))&&("filename" %in% names(object@reference_data))) {
        f = paste(object@reference_data$dir[ipred], object@reference_data$filename[ipred],sep="/")
        if(!file.exists(f)) stop(paste("Reference meteorology file '", f,"' does not exist!", sep=""))
        rcmhist = readmeteorologypoint(f)
      } else if(nrow(object@coords)==1) {
        rcmhist = object@reference_data
      } else {
        stop("Cannot access reference meteorology data")
      }
    }
    if(inherits(object@projection_data,"list")) {
      rcmfut = object@projection_data[[ipred]]
    } else {
      if(("dir" %in% names(object@projection_data))&&("filename" %in% names(object@projection_data))) {
        f = paste(object@projection_data$dir[ipred], object@projection_data$filename[ipred],sep="/")
        if(!file.exists(f)) stop(paste("Projection meteorology file '", f,"' does not exist!",sep=""))
        rcmfut = readmeteorologypoint(f)
      } else if(nrow(object@coords)==1) {
        rcmfut = object@projection_data
      } else {
        stop("Cannot access projection meteorology data")
      }
    }

    #Call statistical correction routine
    mbias = .monthbiasonepoint(obs,rcmhist, mPar$varmethods, verbose)
    mbiasvec[[i]] = mbias
    
    # if(verbose) print(mbias)
    df = .correctiononepoint(mbias,rcmfut, dates, mPar$fill_wind, verbose)

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
      if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
      else f = dfout$filename[i]
      writemeteorologypoint(df, f, exportFormat)
      if(verbose) cat(paste(" written to ",f, sep=""))
      if(exportDir!="") f = paste(exportDir,metadatafile, sep="/")
      else f = metadatafile
      write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
    }
    if(verbose) cat(".\n")
  }
  if(!export) {
    if(!corrOut) return(SpatialPointsMeteorology(points = points, data = dfvec, dates = dates))
    else {
      return(list(SpatialPointsMeteorology(points = points, data = dfvec, dates = dates),
                  mbiasvec))
    }
  }
  if(!corrOut) invisible(spdf)
  else invisible(list(spdf,mbiasvec))
}

