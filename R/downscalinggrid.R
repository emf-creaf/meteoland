downscalinggrid<-function(object, gridfiles, topodata = NULL, dates = NULL, maxreadcells = 50, export = FALSE,
                          exportDir = getwd(), exportFormat = "netCDF",
                          metadatafile="MG.txt", verbose=TRUE) {
  #Check input classes
  if(!inherits(object,"MeteorologyDownscalingData")) stop("'object' has to be of class 'MeteorologyDownscalingData'.")
  if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'")
  if(!inherits(gridfiles,"data.frame") && !inherits(gridfiles,"character")) stop("'gridfiles' has to be a 'data.frame' or a 'character' vector.")
  if(inherits(gridfiles,"data.frame")) {
    nfiles = nrow(gridfiles)
    filevec = rep("", nfiles)
    for(i in 1:nfiles) {
      if(gridfiles$dir[i]!="") filevec[i] = paste(gridfiles$dir[i], gridfiles$filename[i], sep="/")
      else filevec[i] = gridfiles$filename[i]
    }
    gridfiles = filevec
  } else {
    nfiles = length(gridfiles)
  }
  ndates = length(dates)

  mPar = object@params

  # Define vector of data frames
  l = vector("list", ndates)

  # Define meta data frame
  dfout = data.frame(dir = rep(exportDir, ndates), filename=rep("", ndates))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  rownames(dfout) = dates

  #Read first grid
  grid1 = readmeteorologygrid(gridfiles[1])
  cc = coordinates(grid1)
  cells.dim = grid1@grid@cells.dim
  ncells = cells.dim[1]*cells.dim[2]

  #Extract latitude
  longlat = spTransform(as(grid1,"SpatialPoints"),CRS("+proj=longlat"))
  latitude = longlat@coords[,2]

  #Project grid into long/lat coordinates to check if they are inside the boundary box
  cchist = spTransform(as(grid1,"SpatialPoints"), object@proj4string)
  if(verbose) cat(paste("Grid cells to process: ", length(cchist),"\n", sep=""))
  sel = (cchist@coords[,1] >= object@bbox[1,1] & cchist@coords[,1] <=object@bbox[1,2]) &
    (cchist@coords[,2] >= object@bbox[2,1] & cchist@coords[,2] <=object@bbox[2,2])
  if(sum(sel)<length(sel)) warning("Some target grid cells are outside the boundary box of 'object'", call. = FALSE, immediate. = TRUE)
  if(verbose) cat(paste("All grid cells inside boundary box.\n", sep=""))


  #Project long/lat coordinates of predicted climatic objects into the projection of points
  if(verbose) cat(paste("Determining nearest predicted cells.\n", sep=""))
  xypred = spTransform(SpatialPoints(object@coords,object@proj4string,object@bbox), grid1@proj4string)
  colnames(xypred@coords)<-c("x","y")
  ipred = numeric(ncells)
  for(i in 1:ncells) {
    #Find closest predicted climatic cell for historical/projection periods (ideally the same)
    d = sqrt(rowSums(sweep(xypred@coords,2,cc[i,],"-")^2))
    ipred[i] = which.min(d)[1]
  }
  unique.preds = unique(ipred)

  #Calculate biases for all cells and 12 months
  cellbiases = vector("list",ncells)

  for(ip in unique.preds) {
    if(inherits(object@historicdata,"list")) {
      rcmhist = object@historicdata[[ip]]
    } else {
      f = paste(object@historicdata$dir[ip], object@historicdata$filename[ip],sep="/")
      if(!file.exists(f)) stop(paste("Predicted climate historic file '", f,"' does not exist!",sep=""))
      rcmhistoric = readmeteorologypoint(f)
    }
    icells = which(ipred==ip)
    while(length(icells)>0) {
      range = 1:min(length(icells), maxreadcells)
      if(verbose) cat(paste("Calculating biases for cells: ", paste(icells[range],collapse = ","),".\n", sep=""))
      spm.cells = readmeteorologygridcells(gridfiles, icells[range])
      for(j in 1:length(spm.cells@data)) {
        ic = icells[j]
        cellbiases[[ic]] = .monthbiasonepoint(spm.cells@data[[j]], rcmhist)
      }
      icells = icells[-range]
    }
  }

  dates.months = as.numeric(format(as.Date(dates),"%m"))

  dfcor <- data.frame(
    MeanTemperature=rep(NA, ncells),
    MinTemperature=rep(NA, ncells),
    MaxTemperature=rep(NA, ncells),
    Precipitation=rep(NA, ncells),
    MeanRelativeHumidity=rep(NA, ncells),
    MinRelativeHumidity=rep(NA, ncells),
    MaxRelativeHumidity=rep(NA, ncells),
    Radiation=rep(NA, ncells),
    WindSpeed=rep(NA, ncells),
    WindDirection=rep(NA, ncells),
    PET = rep(NA, ncells))

  #Process month by month
  for(m in unique(dates.months)) {
    idays = which(dates.months==m)
    for(id in idays) {
      if(verbose) cat(paste("Downscaling date (",dates[id],") -",sep=""))
      doy = as.numeric(format(dates[id],"%j"))
      for(ip in unique.preds) {
         if(inherits(object@futuredata,"list")) {
           rcmfut = object@futuredata[[ip]]
         } else {
           f = paste(object@futuredata$dir[ip], object@futuredata$filename[ip],sep="/")
           if(!file.exists(f)) stop(paste("Predicted climate future file '", f,"' does not exist!",sep=""))
           rcmfut = readmeteorologypoint(f)
         }
         irow = which(as.character(object@dates)==as.character(dates[id]))
         if(length(irow)==0) stop("Date outside the predicted period.")
         rcmfutdate = as.list(rcmfut[irow,])
         HSmodelFut<-.HRHS(Tc=rcmfutdate$MeanTemperature ,HR=rcmfutdate$MeanRelativeHumidity)
         icells = which(ipred==ip)
         for(i in icells) {
           mbias = cellbiases[[i]]
           dfcor$Precipitation[i] = doQmap(rcmfutdate$Precipitation, mbias$corrPrec[[m]])
           dfcor$Radiation[i] = rcmfutdate$Radiation-(mbias$corrRad[[m]])
           dfcor$MeanTemperature[i] = rcmfutdate$MeanTemperature-(mbias$corrTmean[[m]])
           dfcor$MinTemperature[i] = rcmfutdate$MinTemperature-(mbias$corrTmin[[m]])
           dfcor$MaxTemperature[i] = rcmfutdate$MaxTemperature-(mbias$corrTmax[[m]])
           if(!is.na(mbias$corrWS[[m]])) dfcor$WindSpeed[i] = rcmfutdate$WindSpeed-(mbias$corrWS[[m]])
           else if(mPar$fill_wind) dfcor$WindSpeed[i] = rcmfutdate$WindSpeed
           HSmodelFut.cor<-HSmodelFut-mbias$corrHS[[m]]
           dfcor$MeanRelativeHumidity[i]<-.HSHR(Tc=dfcor$MeanTemperature[i] ,HS=HSmodelFut.cor)
           dfcor$MaxRelativeHumidity[i]<-.HSHR(Tc=dfcor$MinTemperature[i] ,HS=HSmodelFut.cor)
           dfcor$MinRelativeHumidity[i]<-.HSHR(Tc=dfcor$MaxTemperature[i] ,HS=HSmodelFut.cor)

         }
      }
      dfcor$Radiation[dfcor$Radiation<0]<-0
      dfcor$MeanRelativeHumidity[dfcor$MeanRelativeHumidity<0]<-0
      dfcor$MeanRelativeHumidity[dfcor$MeanRelativeHumidity>100]<-100
      dfcor$MaxRelativeHumidity[dfcor$MaxRelativeHumidity<0]<-0
      dfcor$MaxRelativeHumidity[dfcor$MaxRelativeHumidity>100]<-100
      dfcor$MinRelativeHumidity[dfcor$MinRelativeHumidity<0]<-0
      dfcor$MinRelativeHumidity[dfcor$MinRelativeHumidity>100]<-100

      if(!is.null(topodata)){
        latrad = latitude*(pi/180)
        elevation = topodata$elevation
        slorad = topodata$slope*(pi/180)
        asprad = topodata$aspect*(pi/180)
        J = radiation_dateStringToJulianDays(as.character(dates[id]))
        dfcor$PET =.PenmanPETPointsDay(latrad, elevation, slorad, asprad, J,
                                       dfcor$MinTemperature, dfcor$MaxTemperature,
                                       dfcor$MinRelativeHumidity, dfcor$MaxRelativeHumidity,
                                       dfcor$Radiation, dfcor$WindSpeed, mPar$wind_height,
                                       0.001, 0.25);
      }
      if(export) {
        if(exportFormat=="netCDF") dfout$filename[id] = paste(dates[id],".nc", sep="")
        if(dfout$dir[id]!="") f = paste(dfout$dir[id],dfout$filename[id], sep="/")
        else f = dfout$filename[id]
        .writemeteorologygrid(dfcor,grid1@grid, proj4string(grid1),dates[id],f,exportFormat)
        if(verbose) cat(paste(" written to ",f, sep=""))
        if(exportDir!="") f = paste(exportDir,metadatafile, sep="/")
        else f = metadatafile
        write.table(dfout,file= f,sep="\t", quote=FALSE)
      } else {
        l[[id]] = dfcor
        if(verbose) cat(" done")
      }
      if(verbose) cat(".\n")
    }
  }
  if(!export) {
    names(l) = dates
    return(SpatialGridMeteorology(grid1@grid, grid1@proj4string, l, as.Date(dates)))
  } else {
    invisible(dfout)
  }
}
