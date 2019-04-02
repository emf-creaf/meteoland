# Adaptation of the function in package 'qmap 'to solve the treatment of zeroes
#  Empirical quantile mapping as in Deque (2007)
# Deque M (2007) Frequency of precipitation and temperature extremes over France in an anthropogenic scenario: Model results and statistical correction according to observed values. 
# Glob Planet Change 57:16–26. doi: 10.1016/j.gloplacha.2006.11.030
fitQmapDeque <- function(obs,mod, isPrec = TRUE, qstep=0.01){
  ### fits an nonparametric transfere function for quantile
  ### mapping using local linear least square regression
  ###
  ### Based on code from John Bjornar Bremnes
  ys <- na.omit(obs)
  xs <- na.omit(mod)
  #Addresses differences in length
  if(length(xs)!=length(ys)){
    hn <- min(length(xs),length(ys))
    ## quantile algorithm 'type=8' appeares to
    ## be reccomended. See help(quantile) and
    ## Hyndman & Fan (1996) mentioned therein
    ys <- quantile(ys,seq(0,1,length.out=hn),type=8)
    xs <- quantile(xs,seq(0,1,length.out=hn),type=8)
  } else {
    xs <- sort(xs)
    ys <- sort(ys)
  } 
  ## Subsample quantiles at which a fit is wanted
  if(!isPrec){ #Other variables
    newx <- quantile(xs, probs=seq(0,1,by=qstep),type=8)
    newy <- quantile(ys, probs=seq(0,1,by=qstep),type=8)
    newy0 <- NULL
  } else { #Precipitation
    #Subsample quantiles for the non-zero part (zeroes can still exist in quantiles for observed)
    newx <- quantile(xs[xs>0], probs=seq(0,1,by=qstep),type=8)
    newy <- quantile(ys[xs>0], probs=seq(0,1,by=qstep),type=8)
    #Store the observed values corresponding to zero precipitation in the model (i.e. observed cumulative frequency equal to probability of zero precipitation in the model)
    newy0 <- ys[xs==0] 
  }
  ppar <- list(modq=as.numeric(newx),
               fitq=as.numeric(newy),
               fitq0 = as.numeric(newy0),
               isPrec=isPrec)
  return(ppar)
}

# Adaptation of the function in package 'qmap' to solve the treatment of zeroes
# Empirical quantile mapping as in Deque (2007)
# Deque M (2007) Frequency of precipitation and temperature extremes over France in an anthropogenic scenario: Model results and statistical correction according to observed values. 
# Glob Planet Change 57:16–26. doi: 10.1016/j.gloplacha.2006.11.030
doQmapDeque <- function(x,fobj){
  nonzerosel <- (x>0)
  if(!fobj$isPrec) { #If is not precipitation correct all values
    nonzerosel <- rep(TRUE, length(x))
  }
  out <- rep(NA,length.out=length(x))

  #Quantile mapping of (nonzero values)
  out[nonzerosel] <- approx(x=fobj$modq, y=fobj$fitq,
                            xout=x[nonzerosel], method="linear",
                            rule=2, ties=mean)$y
  #Extreme values (shift according to the difference in maximum values)
  nq <- nrow(fobj$modq)
  largex <- x>fobj$modq[nq]
  if(any(largex)){         
    max.delta <- fobj$modq[nq] - fobj$fitq[nq]
    out[largex] <- x[largex] - max.delta
  }
  
  #Sample random values among the observed distribution corresponding to non precipitation in the model series for calibration
  nzero =sum(!nonzerosel)
  if(nzero>0) {
    out[!nonzerosel] = sample(fobj$fitq0, nzero, replace=TRUE)
  }
  return(out)
}


#Calculates biases or other correction parameters for a given data period
.corrParam<-function(DatTemp, ModelTempHist, varmethods, varname, varnamemean = NULL, qstep = 0.01, verbose=TRUE) {
  if(sum(!is.na(ModelTempHist))==0 || sum(!is.na(DatTemp))==0) return(NA)
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
    if(!is.null(varnamemean)) {
      difHist = (ModelTempHist[,varname]-ModelTempHist[,varnamemean])
      difDat = (DatTemp[,varname]-DatTemp[,varnamemean])
    } else {
      difHist = ModelTempHist[,varname]
      difDat = DatTemp[,varname]
    }
    corr<-fitQmapDeque(difDat,difHist, isPrec = (varname=="Precipitation"), qstep = qstep)
  } else if(varmethods[varname]=="none") {
    corr<-0
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
    corrected<-doQmapDeque(varuncor, varbias)
  } else if(varmethod=="none") {
    corrected<-varuncor
  } else {
    stop(paste("Wrong correction method:", varmethod))
  }
  return(corrected)
}

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


#Calculates monthly biases/params for all twelve months
.monthbiasonepoint<-function(Data, MODHist, varmethods, qstep = 0.01, verbose=TRUE) {
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
    corrTmean[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "MeanTemperature", qstep=qstep)
    if(varmethods["MinTemperature"]=="unbias" && varmethods["MeanTemperature"]=="unbias") {#for unbias use tmean delta (to avoid tmin > tmean)
      corrTmin[[m]] = corrTmean[[m]]
    } else {
      corrTmin[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "MinTemperature", "MeanTemperature", qstep=qstep)
    }
    if(varmethods["MaxTemperature"]=="unbias" && varmethods["MeanTemperature"]=="unbias") {#for unbias use tmean delta (to avoid tmax < tmean)
      corrTmax[[m]] = corrTmean[[m]]
    } else {
      corrTmax[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "MaxTemperature", "MeanTemperature", qstep=qstep)
    }
    corrPrec[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "Precipitation", qstep=qstep)
    corrRad[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "Radiation", qstep=qstep)
    corrWS[[m]] = .corrParam(DatTemp, ModelTempHist, varmethods, "WindSpeed", qstep=qstep)
    HSData<-.HRHS(Tc=DatTemp[,"MeanTemperature"] ,HR=DatTemp[,"MeanRelativeHumidity"])
    HSmodelHist<-.HRHS(Tc=ModelTempHist[,"MeanTemperature"] ,HR=ModelTempHist[,"MeanRelativeHumidity"])
    if(varmethods["MeanRelativeHumidity"]=="unbias") {
      corrHS[[m]]<-mean(HSmodelHist-HSData, na.rm=TRUE)
    } else if(varmethods["MeanRelativeHumidity"]=="scaling") {
      corrHS[[m]]<- as.numeric(lm(HSData~HSmodelHist-1)$coefficients) #slope of a regression through the origin
    } else if(varmethods["MeanRelativeHumidity"]=="quantmap") {
      corrHS[[m]]<-fitQmapDeque(HSData,HSmodelHist, isPrec=FALSE, qstep = qstep)
    } else if(varmethods["MeanRelativeHumidity"]=="none"){
      corrHS[[m]]<-0
    } else {
      stop(paste("Wrong correction method for variable:", "MeanRelativeHumidity"))
    }
  }
  return(list(varmethods = varmethods, corrTmean=corrTmean,corrTmin=corrTmin,corrTmax=corrTmax,
              corrPrec=corrPrec,corrRad=corrRad,corrHS=corrHS,corrWS=corrWS))
}

#Apply monthly corrections for one point
.correctiononepoint<-function(mbias, MODFut, dates = NULL, fill_wind = FALSE, allow_saturated = FALSE, verbose=TRUE){
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
    ModelTempFut.TM.cor <-.corrApply(ModelTempFut$MeanTemperature, 
                                     mbias$corrTmean[[m]], 
                                     mbias$varmethods["MeanTemperature"])
    
    #Correction Tmin
    if(mbias$varmethods["MinTemperature"]=="scaling") {
      ModelTempFut.TN.cor<-ModelTempFut.TM.cor + (pmin(ModelTempFut$MinTemperature-ModelTempFut$MeanTemperature,0)*mbias$corrTmin[[m]])
    } else if(mbias$varmethods["MinTemperature"]=="quantmap") {
      ModelTempFut.TN.cor<-ModelTempFut.TM.cor + .corrApply(pmin(ModelTempFut$MinTemperature-ModelTempFut$MeanTemperature,0), 
                                                            mbias$corrTmin[[m]], mbias$varmethods["MinTemperature"])
    } else {#unbias/none
      ModelTempFut.TN.cor<-.corrApply(ModelTempFut$MinTemperature, mbias$corrTmin[[m]], 
                                      mbias$varmethods["MinTemperature"])
    }
    
    #Correction Tmax
    if(mbias$varmethods["MaxTemperature"]=="scaling") {
      ModelTempFut.TX.cor<-ModelTempFut.TM.cor + (pmax(ModelTempFut$MaxTemperature-ModelTempFut$MeanTemperature,0)*mbias$corrTmax[[m]])
    } else if(mbias$varmethods["MaxTemperature"]=="quantmap") {
      ModelTempFut.TX.cor<-ModelTempFut.TM.cor + .corrApply(pmax(ModelTempFut$MaxTemperature-ModelTempFut$MeanTemperature,0), 
                                                            mbias$corrTmax[[m]], mbias$varmethods["MaxTemperature"])
    } else { #unbias/none
      ModelTempFut.TX.cor<-.corrApply(ModelTempFut$MaxTemperature, mbias$corrTmax[[m]], mbias$varmethods["MaxTemperature"])
    }
    
    #Correction Precipitation
    ModelTempFut.rain.cor<-.corrApply(ModelTempFut$Precipitation, mbias$corrPrec[[m]], mbias$varmethods["Precipitation"])

    #Correction Rg
    ModelTempFut.Rg.cor<-.corrApply(ModelTempFut$Radiation, mbias$corrRad[[m]], mbias$varmethods["Radiation"])
    ModelTempFut.Rg.cor[ModelTempFut.Rg.cor<0]<-0

    #Correction WS (if NA then use input WS)
    if(!(is.na(mbias$corrWS[[m]])[1]))  {
      ModelTempFut.WS.cor<-.corrApply(ModelTempFut$WindSpeed, mbias$corrWS[[m]], mbias$varmethods["WindSpeed"])
    }
    else if(fill_wind) ModelTempFut.WS.cor<-ModelTempFut$WindSpeed
    ModelTempFut.WS.cor[ModelTempFut.WS.cor<0]<-0 #Truncate to minimum value

    #Correction RH
    #First transform RH into specific humidity
    HSmodelFut<-.HRHS(Tc=ModelTempFut[,"MeanTemperature"] ,HR=ModelTempFut[,"MeanRelativeHumidity"])
    #Second compute and apply the bias to specific humidity
    HSmodelFut.cor<-.corrApply(HSmodelFut, mbias$corrHS[[m]], mbias$varmethods["MeanRelativeHumidity"])
    #Back transform to relative humidity (mean, max, min)
    ModelTempFut.RHM.cor<-.HSHR(Tc=ModelTempFut.TM.cor ,HS=HSmodelFut.cor, allow_saturated)
    ModelTempFut.RHM.cor[ModelTempFut.RHM.cor<0]<-0
    ModelTempFut.RHM.cor[ModelTempFut.RHM.cor>100]<-100
    ModelTempFut.RHX.cor<-.HSHR(Tc=ModelTempFut.TN.cor ,HS=HSmodelFut.cor, allow_saturated)
    ModelTempFut.RHX.cor[ModelTempFut.RHX.cor<0]<-0
    ModelTempFut.RHX.cor[ModelTempFut.RHX.cor>100]<-100
    ModelTempFut.RHN.cor<-.HSHR(Tc=ModelTempFut.TX.cor ,HS=HSmodelFut.cor, allow_saturated)
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

correctionpoint<-function(obs, mod, proj, dates = NULL, params = defaultCorrectionParams(), verbose=TRUE){
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

correctionpoints<-function(object, points, topodata = NULL, dates = NULL, export = FALSE,
                            exportDir = getwd(), exportFormat = "meteoland/txt",
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
  
  if(exportFormat %in% c("meteoland/txt","castanea/txt")) formatType = "txt"
  else if (exportFormat %in% c("meteoland/rds","castanea/rds")) formatType = "rds"
  
  dfout = data.frame(dir = rep(exportDir, npoints), filename=paste0(ids,".", formatType))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  dfout$format = exportFormat
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
      if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
      else f = dfout$filename[i]
      writemeteorologypoint(df, f, dfout$format[i])
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

