.residualonepoint<-function(Data, MODHist, varmethods, allow_saturated = FALSE, qstep = 0.01, verbose=TRUE){
  
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
  
  #Copy original data
  DataCV = Data
  DataCV[,2:ncol(DataCV)]= NA

  for (m in 1:12){
    if(verbose) cat(".")
    indices = which(Data.months==m)
    DatTempMonth<-Data[Data.months==m,]
    ModelTempHistMonth<-MODHist[MODHist.months==m,]
    
    for(i in 1:length(indices)) {
      #Remove day of interest
      DatTemp = DatTempMonth[-i,]
      ModelTempHist = ModelTempHistMonth[-i, ]
      #Calculate correction params depending on the correction method
      corrTmean = .corrParam(DatTemp, ModelTempHist, varmethods, "MeanTemperature", qstep = qstep)
      if(varmethods["MinTemperature"]=="unbias" && varmethods["MeanTemperature"]=="unbias") {#for unbias use tmean delta (to avoid tmin > tmean)
        corrTmin = corrTmean
      } else {
        corrTmin = .corrParam(DatTemp, ModelTempHist, varmethods, "MinTemperature", "MeanTemperature", qstep = qstep)
      }
      if(varmethods["MaxTemperature"]=="unbias" && varmethods["MeanTemperature"]=="unbias") {#for unbias use tmean delta (to avoid tmax < tmean)
        corrTmax = corrTmean
      } else {
        corrTmax = .corrParam(DatTemp, ModelTempHist, varmethods, "MaxTemperature", "MeanTemperature", qstep = qstep)
      }
      corrPrec = .corrParam(DatTemp, ModelTempHist, varmethods, "Precipitation", qstep = qstep)
      corrRad = .corrParam(DatTemp, ModelTempHist, varmethods, "Radiation", qstep = qstep)
      corrWS = .corrParam(DatTemp, ModelTempHist, varmethods, "WindSpeed", qstep = qstep)
      HSData<-humidity_relative2specific(Tc=DatTemp[,"MeanTemperature"] ,HR=DatTemp[,"MeanRelativeHumidity"])
      HSmodelHist<-humidity_relative2specific(Tc=ModelTempHist[,"MeanTemperature"] ,HR=ModelTempHist[,"MeanRelativeHumidity"])
      if(varmethods["MeanRelativeHumidity"]=="unbias") {
        corrHS<-mean(HSmodelHist-HSData, na.rm=TRUE)
      } else if(varmethods["MeanRelativeHumidity"]=="scaling") {
        corrHS<- as.numeric(lm(HSData~HSmodelHist-1)$coefficients) #slope of a regression through the origin
      } else if(varmethods["MeanRelativeHumidity"]=="quantmap") {
        corrHS<-fitQmapDeque(HSData,HSmodelHist, isPrec = FALSE, qstep = qstep)
      } else if(varmethods["MeanRelativeHumidity"]=="none") {
        corrHS<-0
      } else {
        stop(paste("Wrong correction method for variable:", "MeanRelativeHumidity"))
      }
      
      #Apply correction 
      #Correction Tmean
      DataCV$MeanTemperature[indices[i]] <-.corrApply(ModelTempHistMonth$MeanTemperature[i], corrTmean, varmethods["MeanTemperature"])
      
      #Correction Tmin
      if(varmethods["MinTemperature"]=="scaling") {
        DataCV$MinTemperature[indices[i]]<-DataCV$MeanTemperature[indices[i]] + (pmin(ModelTempHistMonth$MinTemperature[i]-ModelTempHistMonth$MeanTemperature[i],0)*corrTmin)
      } else if(varmethods["MinTemperature"]=="quantmap") {
        DataCV$MinTemperature[indices[i]]<-DataCV$MeanTemperature[indices[i]] + .corrApply(pmin(ModelTempHistMonth$MinTemperature[i]-ModelTempHistMonth$MeanTemperature[i],0), 
                                                              corrTmin, varmethods["MinTemperature"])
      } else {#unbias/none
        DataCV$MinTemperature[indices[i]]<-.corrApply(ModelTempHistMonth$MinTemperature[i], corrTmin, varmethods["MinTemperature"])
      }
      
      #Correction Tmax
      if(varmethods["MaxTemperature"]=="scaling") {
        DataCV$MaxTemperature[indices[i]]<-DataCV$MeanTemperature[indices[i]] + (pmax(ModelTempHistMonth$MaxTemperature[i]-ModelTempHistMonth$MeanTemperature[i],0)*corrTmax)
      } else if(varmethods["MaxTemperature"]=="quantmap") {
        DataCV$MaxTemperature[indices[i]]<-DataCV$MeanTemperature[indices[i]] + .corrApply(pmax(ModelTempHistMonth$MaxTemperature[i]-ModelTempHistMonth$MeanTemperature[i],0), 
                                                                                           corrTmax, varmethods["MaxTemperature"])
      } else {#unbias/none
        DataCV$MaxTemperature[indices[i]]<-.corrApply(ModelTempHistMonth$MaxTemperature[i], corrTmax, varmethods["MaxTemperature"])
      }

      #Correction Precipitation
      DataCV$Precipitation[indices[i]]<-.corrApply(ModelTempHistMonth$Precipitation[i], corrPrec, varmethods["Precipitation"])


      #Correction Rg
      DataCV$Radiation[indices[i]]<-.corrApply(ModelTempHistMonth$Radiation[i], corrRad, varmethods["Radiation"])
      if(DataCV$Radiation[indices[i]]<0)  DataCV$Radiation[indices[i]]=0


      #Correction WS (if NA then use input WS)
      if(!(is.na(corrWS)[1])) {
        DataCV$WindSpeed[indices[i]]<-.corrApply(ModelTempHistMonth$WindSpeed[i], corrWS, varmethods["WindSpeed"])
      }
      if(DataCV$WindSpeed[indices[i]]<0)  DataCV$WindSpeed[indices[i]]=0

      #Correction RH
      #First transform RH into specific humidity
      HSmodelFut<-humidity_relative2specific(Tc=ModelTempHistMonth$MeanTemperature[i] ,HR=ModelTempHistMonth$MeanRelativeHumidity[i])
      #Second apply the bias to specific humidity
      HSmodelFut.cor<-.corrApply(HSmodelFut, corrHS, varmethods["MeanRelativeHumidity"])
      #Back transform to relative humidity (mean, max, min)
      DataCV$MeanRelativeHumidity[indices[i]]<-min(100,max(0,humidity_specific2relative(Tc=DataCV$MeanTemperature[indices[i]] ,HS=HSmodelFut.cor, allow_saturated)))
      DataCV$MaxRelativeHumidity[indices[i]]<-min(100,max(0,humidity_specific2relative(Tc=DataCV$MinTemperature[indices[i]] ,HS=HSmodelFut.cor, allow_saturated)))
      DataCV$MinRelativeHumidity[indices[i]]<-min(100,max(0,humidity_specific2relative(Tc=DataCV$MaxTemperature[indices[i]] ,HS=HSmodelFut.cor, allow_saturated)))

      #Check RHmin <= RHmean <= RHmax
      DataCV$MinRelativeHumidity[indices[i]] = pmin(DataCV$MinRelativeHumidity[indices[i]], DataCV$MaxRelativeHumidity[indices[i]])
      DataCV$MaxRelativeHumidity[indices[i]] = pmax(DataCV$MinRelativeHumidity[indices[i]], DataCV$MaxRelativeHumidity[indices[i]])
      DataCV$MeanRelativeHumidity[indices[i]] = pmin(pmax(DataCV$MeanRelativeHumidity[indices[i]], DataCV$MinRelativeHumidity[indices[i]]),DataCV$MaxRelativeHumidity[indices[i]])
      
    }
  }
  return(DataCV)
}

correctionpoints.errors<-function(object, points, topodata = NULL, 
                                  error.type = "residuals.cv", keep.data = FALSE, verbose = FALSE) {
  
  #Check input classes
  if(!inherits(object,"MeteorologyUncorrectedData")) stop("'object' has to be of class 'MeteorologyUncorrectedData'.")
  if(!inherits(points,"SpatialPointsMeteorology") && !inherits(points,"SpatialPointsDataFrame")) stop("'points' has to be of class 'SpatialPointsMeteorology' or 'SpatialPointsDataFrame'.")
  
  error.type = match.arg(error.type, c("before","residuals", "residuals.cv"))
    
  mPar = object@params
  
  npoints = length(points)
  
  if(verbose) {
    cat(paste("Points to evaluate: ", npoints,"\n", sep=""))
    cat(paste("Error type: ", error.type,"\n", sep=""))
  }
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
  
  if(inherits(points,"SpatialPointsMeteorology")) {
    if(!is.null(names(points@data))) ids = names(points@data)
    else ids = 1:npoints
  } else {
    if(!is.null(rownames(points@data))) ids = rownames(points@data)
    else ids = 1:npoints
  }
  
  if(keep.data) {
    dataout = vector("list", npoints)
  }
  res = data.frame(matrix(NA, nrow=npoints, ncol= 22))
  names(res) = c("MeanTemperature-Bias", "MeanTemperature-MAE",
                   "MinTemperature-Bias", "MinTemperature-MAE",
                   "MaxTemperature-Bias", "MaxTemperature-MAE",
                   "Precipitation-Total", "Precipitation-DPD","Precipitation-Bias", "Precipitation-MAE",
                   "MeanRelativeHumidity-Bias", "MeanRelativeHumidity-MAE",
                   "MinRelativeHumidity-Bias", "MinRelativeHumidity-MAE",
                   "MaxRelativeHumidity-Bias", "MaxRelativeHumidity-MAE",
                   "Radiation-Bias", "Radiation-MAE",
                   "WindSpeed-Bias", "WindSpeed-MAE",
                   "PET-Bias", "PET-MAE")
  row.names(res) = ids
  
  #Loop over all points
  for(i in 1:npoints) {
    if(verbose) cat(paste("Evaluating error for point '",ids[i],"' (",i,"/",npoints,") -",sep=""))
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
        rcmhist = readmeteorologypoint(f)
      } else if(nrow(object@coords)==1) {
        rcmhist = object@reference_data
      } else {
        stop("Cannot access reference meteorology data")
      }
    }
    
    #subset compatible data
    sel1 = rownames(rcmhist) %in% rownames(obs)
    sel2 = rownames(obs) %in% rownames(rcmhist)
    rcmhist = rcmhist[sel1,]
    obs = obs[sel2,]
    
    
    if(error.type=="before") {#Errors before correction
      dataone = rcmhist
      #Fill minimum and maximum relative humidity if missing
      if(!("MinRelativeHumidity" %in% names(dataone))) {
        dataone$MinRelativeHumidity=humidity_specific2relative(dataone$MaxTemperature, dataone$SpecificHumidity, mPar$allow_saturated)
      }
      if(!("MaxRelativeHumidity" %in% names(dataone))) {
        dataone$MaxRelativeHumidity=humidity_specific2relative(dataone$MinTemperature, dataone$SpecificHumidity, mPar$allow_saturated)
      }
    }else if(error.type=="residuals") {#Residuals before correction
      mbias = .monthbiasonepoint(obs,rcmhist, mPar$varmethods, verbose)
      dataone = .correctiononepoint(mbias, rcmhist, fill_wind = mPar$fill_wind, allow_saturated = mPar$allow_saturated, verbose = verbose)
    } else if(error.type == "residuals.cv") {#Residuals before correction (including cross-validation)
      dataone = .residualonepoint(obs,rcmhist, mPar$varmethods, 
                                  allow_saturated = mPar$allow_saturated, 
                                  qstep = mPar$qstep, verbose)
    }

    #Calculate PET
    if(!is.null(topodata) && ("PET" %in% names(obs))) {
      J = radiation_dateStringToJulianDays(row.names(obs))
      dataone$PET = .penmanpoint(latrad[i], elevation[i],slorad[i], asprad[i], J, 
                                    dataone$MinTemperature, dataone$MaxTemperature,
                                    dataone$MinRelativeHumidity, dataone$MaxRelativeHumidity, dataone$Radiation,
                                    dataone$WindSpeed, mPar$wind_height,
                                    0.001, 0.25);
    }
    
    res[i, "MeanTemperature-Bias"] = mean(dataone$MeanTemperature-obs$MeanTemperature, na.rm=T)
    res[i, "MeanTemperature-MAE"] = mean(abs(dataone$MeanTemperature-obs$MeanTemperature), na.rm=T)
    res[i, "MinTemperature-Bias"] = mean(dataone$MinTemperature-obs$MinTemperature, na.rm=T)
    res[i, "MinTemperature-MAE"] = mean(abs(dataone$MinTemperature-obs$MinTemperature), na.rm=T)
    res[i, "MaxTemperature-Bias"] = mean(dataone$MaxTemperature-obs$MaxTemperature, na.rm=T)
    res[i, "MaxTemperature-MAE"] = mean(abs(dataone$MaxTemperature-obs$MaxTemperature), na.rm=T)
    res[i, "Precipitation-Total"] = sum(dataone$Precipitation, na.rm=T)-sum(obs$Precipitation, na.rm=T)
    res[i, "Precipitation-DPD"] = sum(dataone$Precipitation>0, na.rm=T)/sum(!is.na(dataone$Precipitation))-sum(obs$Precipitation>0, na.rm=T)/sum(!is.na(obs$Precipitation))
    res[i, "Precipitation-Bias"] = mean(dataone$Precipitation[dataone$Precipitation>0]-obs$Precipitation[dataone$Precipitation>0], na.rm=T)
    res[i, "Precipitation-MAE"] = mean(abs(dataone$Precipitation[dataone$Precipitation>0]-obs$Precipitation[dataone$Precipitation>0]), na.rm=T)
    res[i, "MeanRelativeHumidity-Bias"] = mean(dataone$MeanRelativeHumidity-obs$MeanRelativeHumidity, na.rm=T)
    res[i, "MeanRelativeHumidity-MAE"] = mean(abs(dataone$MeanRelativeHumidity-obs$MeanRelativeHumidity), na.rm=T)
    res[i, "MinRelativeHumidity-Bias"] = mean(dataone$MinRelativeHumidity-obs$MinRelativeHumidity, na.rm=T)
    res[i, "MinRelativeHumidity-MAE"] = mean(abs(dataone$MinRelativeHumidity-obs$MinRelativeHumidity), na.rm=T)
    res[i, "MaxRelativeHumidity-Bias"] = mean(dataone$MaxRelativeHumidity-obs$MaxRelativeHumidity, na.rm=T)
    res[i, "MaxRelativeHumidity-MAE"] = mean(abs(dataone$MaxRelativeHumidity-obs$MaxRelativeHumidity), na.rm=T)
    res[i, "Radiation-Bias"] = mean(dataone$Radiation-obs$Radiation, na.rm=T)
    res[i, "Radiation-MAE"] = mean(abs(dataone$Radiation-obs$Radiation), na.rm=T)
    res[i, "WindSpeed-Bias"] = mean(dataone$WindSpeed-obs$WindSpeed, na.rm=T)
    res[i, "WindSpeed-MAE"] = mean(abs(dataone$WindSpeed-obs$WindSpeed), na.rm=T)
    if(!is.null(topodata) && ("PET" %in% names(obs))) {
      res[i, "PET-Bias"] = mean(dataone$PET-obs$PET, na.rm=T)
      res[i, "PET-MAE"] = mean(abs(dataone$PET-obs$PET), na.rm=T)
    }
    #Store cross validation data if required
    if(keep.data) dataout[[i]] = dataone
    
    if(verbose) cat(".\n")
  }
  if(keep.data) return(list(data = dataout, evaluation = res))
  else return(res)
}