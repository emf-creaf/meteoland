.residualonepoint<-function(Data, MODHist, varmethods, verbose=TRUE){
  
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
      corrTmean = .corrParam(DatTemp, ModelTempHist, varmethods, "MeanTemperature")
      corrTmin = .corrParam(DatTemp, ModelTempHist, varmethods, "MinTemperature", "MeanTemperature")
      corrTmax = .corrParam(DatTemp, ModelTempHist, varmethods, "MaxTemperature", "MeanTemperature")
      corrPrec = .corrParam(DatTemp, ModelTempHist, varmethods, "Precipitation")
      corrRad = .corrParam(DatTemp, ModelTempHist, varmethods, "Radiation")
      corrWS = .corrParam(DatTemp, ModelTempHist, varmethods, "WindSpeed")
      HSData<-.HRHS(Tc=DatTemp[,"MeanTemperature"] ,HR=DatTemp[,"MeanRelativeHumidity"])
      HSmodelHist<-.HRHS(Tc=ModelTempHist[,"MeanTemperature"] ,HR=ModelTempHist[,"MeanRelativeHumidity"])
      if(varmethods["MeanRelativeHumidity"]=="unbias") {
        corrHS<-mean(HSmodelHist-HSData, na.rm=TRUE)
      } else if(varmethods["MeanRelativeHumidity"]=="scaling") {
        corrHS<- as.numeric(lm(HSData~HSmodelHist-1)$coefficients) #slope of a regression through the origin
      } else if(varmethods["MeanRelativeHumidity"]=="quantmap") {
        corrHS<-fitQmap(HSData,HSmodelHist,method=c("QUANT"))
      } else {
        stop(paste("Wrong correction method for variable:", "MeanRelativeHumidity"))
      }
      
      #Apply correction 
      #Correction Tmean
      DataCV$MeanTemperature[indices[i]] <-.corrApply(ModelTempHistMonth$MeanTemperature[i], corrTmean, varmethods["MeanTemperature"])
      
      #Correction Tmin
      if(varmethods["MinTemperature"]=="scaling") {
        DataCV$MinTemperature[indices[i]]<-DataCV$MeanTemperature[indices[i]] + ((ModelTempHistMonth$MinTemperature[i]-ModelTempHistMonth$MeanTemperature[i])*corrTmin)
      } else {
        DataCV$MinTemperature[indices[i]]<-.corrApply(ModelTempHistMonth$MinTemperature[i], corrTmin, varmethods["MinTemperature"])
      }
      
      #Correction Tmax
      if(varmethods["MaxTemperature"]=="scaling") {
        DataCV$MaxTemperature[indices[i]]<-DataCV$MeanTemperature[indices[i]] + ((ModelTempHistMonth$MaxTemperature[i]-ModelTempHistMonth$MeanTemperature[i])*corrTmax)
      } else {
        DataCV$MaxTemperature[indices[i]]<-.corrApply(ModelTempHistMonth$MaxTemperature[i], corrTmax, varmethods["MaxTemperature"])
      }

      #Correction Precipitation
      DataCV$Precipitation[indices[i]]<-.corrApply(ModelTempHistMonth$Precipitation[i], corrPrec, varmethods["Precipitation"])


      #Correction Rg
      DataCV$Radiation[indices[i]]<-.corrApply(ModelTempHistMonth$Radiation[i], corrRad, varmethods["Radiation"])
      if(DataCV$Radiation[indices[i]]<0)  DataCV$Radiation[indices[i]]=0


      #Correction WS (if NA then use input WS)
      if(!is.na(corrWS)) {
        DataCV$WindSpeed[indices[i]]<-.corrApply(ModelTempHistMonth$WindSpeed[i], corrWS, varmethods["WindSpeed"])
      }
      if(DataCV$WindSpeed[indices[i]]<0)  DataCV$WindSpeed[indices[i]]=0

      #Correction RH
      #First transform RH into specific humidity
      HSmodelFut<-.HRHS(Tc=DataCV$MeanTemperature[indices[i]] ,HR=ModelTempHistMonth$MeanRelativeHumidity[i])
      #Second apply the bias to specific humidity
      HSmodelFut.cor<-.corrApply(HSmodelFut, corrHS, varmethods["MeanRelativeHumidity"])
      #Back transform to relative humidity (mean, max, min)
      DataCV$MeanRelativeHumidity[indices[i]]<-min(100,max(0,.HSHR(Tc=DataCV$MeanTemperature[indices[i]] ,HS=HSmodelFut.cor)))
      DataCV$MaxRelativeHumidity[indices[i]]<-min(100,max(0,.HSHR(Tc=DataCV$MinTemperature[indices[i]] ,HS=HSmodelFut.cor)))
      DataCV$MinRelativeHumidity[indices[i]]<-min(100,max(0,.HSHR(Tc=DataCV$MaxTemperature[indices[i]] ,HS=HSmodelFut.cor)))

      #Check RHmin <= RHmean <= RHmax
      DataCV$MinRelativeHumidity[indices[i]] = pmin(DataCV$MinRelativeHumidity[indices[i]], DataCV$MaxRelativeHumidity[indices[i]])
      DataCV$MaxRelativeHumidity[indices[i]] = pmax(DataCV$MinRelativeHumidity[indices[i]], DataCV$MaxRelativeHumidity[indices[i]])
      DataCV$MeanRelativeHumidity[indices[i]] = pmin(pmax(DataCV$MeanRelativeHumidity[indices[i]], DataCV$MinRelativeHumidity[indices[i]]),DataCV$MaxRelativeHumidity[indices[i]])
      
    }
  }
  return(DataCV)
}

correctionpoints.residuals<-function(object, points, topodata = NULL, keep.cvdata = FALSE, verbose = FALSE) {
  
  #Check input classes
  if(!inherits(object,"MeteorologyUncorrectedData")) stop("'object' has to be of class 'MeteorologyUncorrectedData'.")
  if(!inherits(points,"SpatialPointsMeteorology") && !inherits(points,"SpatialPointsDataFrame")) stop("'points' has to be of class 'SpatialPointsMeteorology' or 'SpatialPointsDataFrame'.")
  
  mPar = object@params
  
  npoints = length(points)
  
  if(verbose) cat(paste("Points to evaluate: ", npoints,"\n", sep=""))
  
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
  
  if(keep.cvdata) {
    cvout = vector("list", npoints)
  }
  cvres = data.frame(matrix(NA, nrow=npoints, ncol= 22))
  names(cvres) = c("MeanTemperature-Bias", "MeanTemperature-MAE",
                   "MinTemperature-Bias", "MinTemperature-MAE",
                   "MaxTemperature-Bias", "MaxTemperature-MAE",
                   "Precipitation-Total", "Precipitation-DPD","Precipitation-Bias", "Precipitation-MAE",
                   "MeanRelativeHumidity-Bias", "MeanRelativeHumidity-MAE",
                   "MinRelativeHumidity-Bias", "MinRelativeHumidity-MAE",
                   "MaxRelativeHumidity-Bias", "MaxRelativeHumidity-MAE",
                   "Radiation-Bias", "Radiation-MAE",
                   "WindSpeed-Bias", "WindSpeed-MAE",
                   "PET-Bias", "PET-MAE")
  row.names(cvres) = ids
  
  #Loop over all points
  for(i in 1:npoints) {
    if(verbose) cat(paste("Evaluating cross-validation residual error for point '",ids[i],"' (",i,"/",npoints,") -",sep=""))
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
    
    #subset compatible data
    sel1 = rownames(rcmhist) %in% rownames(obs)
    sel2 = rownames(obs) %in% rownames(rcmhist)
    rcmhist = rcmhist[sel1,]
    obs = obs[sel2,]
    
    cvone = .residualonepoint(obs,rcmhist, mPar$varmethods, verbose)

    #Calculate PET
    if(!is.null(topodata) && ("PET" %in% names(obs))) {
      J = radiation_dateStringToJulianDays(row.names(obs))
      cvone$PET = .penmanpoint(latrad[i], elevation[i],slorad[i], asprad[i], J, 
                                    cvone$MinTemperature, cvone$MaxTemperature,
                                    cvone$MinRelativeHumidity, cvone$MaxRelativeHumidity, cvone$Radiation,
                                    cvone$WindSpeed, mPar$wind_height,
                                    0.001, 0.25);
    }
    
    cvres[i, "MeanTemperature-Bias"] = mean(cvone$MeanTemperature-obs$MeanTemperature, na.rm=T)
    cvres[i, "MeanTemperature-MAE"] = mean(abs(cvone$MeanTemperature-obs$MeanTemperature), na.rm=T)
    cvres[i, "MinTemperature-Bias"] = mean(cvone$MinTemperature-obs$MinTemperature, na.rm=T)
    cvres[i, "MinTemperature-MAE"] = mean(abs(cvone$MinTemperature-obs$MinTemperature), na.rm=T)
    cvres[i, "MaxTemperature-Bias"] = mean(cvone$MaxTemperature-obs$MaxTemperature, na.rm=T)
    cvres[i, "MaxTemperature-MAE"] = mean(abs(cvone$MaxTemperature-obs$MaxTemperature), na.rm=T)
    cvres[i, "Precipitation-Total"] = sum(cvone$Precipitation, na.rm=T)-sum(obs$Precipitation, na.rm=T)
    cvres[i, "Precipitation-DPD"] = sum(cvone$Precipitation>0, na.rm=T)/sum(!is.na(cvone$Precipitation))-sum(obs$Precipitation>0, na.rm=T)/sum(!is.na(obs$Precipitation))
    cvres[i, "Precipitation-Bias"] = mean(cvone$Precipitation[cvone$Precipitation>0]-obs$Precipitation[cvone$Precipitation>0], na.rm=T)
    cvres[i, "Precipitation-MAE"] = mean(abs(cvone$Precipitation[cvone$Precipitation>0]-obs$Precipitation[cvone$Precipitation>0]), na.rm=T)
    cvres[i, "MeanRelativeHumidity-Bias"] = mean(cvone$MeanRelativeHumidity-obs$MeanRelativeHumidity, na.rm=T)
    cvres[i, "MeanRelativeHumidity-MAE"] = mean(abs(cvone$MeanRelativeHumidity-obs$MeanRelativeHumidity), na.rm=T)
    cvres[i, "MinRelativeHumidity-Bias"] = mean(cvone$MinRelativeHumidity-obs$MinRelativeHumidity, na.rm=T)
    cvres[i, "MinRelativeHumidity-MAE"] = mean(abs(cvone$MinRelativeHumidity-obs$MinRelativeHumidity), na.rm=T)
    cvres[i, "MaxRelativeHumidity-Bias"] = mean(cvone$MaxRelativeHumidity-obs$MaxRelativeHumidity, na.rm=T)
    cvres[i, "MaxRelativeHumidity-MAE"] = mean(abs(cvone$MaxRelativeHumidity-obs$MaxRelativeHumidity), na.rm=T)
    cvres[i, "Radiation-Bias"] = mean(cvone$Radiation-obs$Radiation, na.rm=T)
    cvres[i, "Radiation-MAE"] = mean(abs(cvone$Radiation-obs$Radiation), na.rm=T)
    cvres[i, "WindSpeed-Bias"] = mean(cvone$WindSpeed-obs$WindSpeed, na.rm=T)
    cvres[i, "WindSpeed-MAE"] = mean(abs(cvone$WindSpeed-obs$WindSpeed), na.rm=T)
    if(!is.null(topodata) && ("PET" %in% names(obs))) {
      cvres[i, "PET-Bias"] = mean(cvone$PET-obs$PET, na.rm=T)
      cvres[i, "PET-MAE"] = mean(abs(cvone$PET-obs$PET), na.rm=T)
    }
    #Store cross validation data if required
    if(keep.cvdata) cvout[[i]] = cvone
    
    if(verbose) cat(".\n")
  }
  if(keep.cvdata) return(list(cvdata = cvout, cvres = cvres))
  else return(cvres)
}