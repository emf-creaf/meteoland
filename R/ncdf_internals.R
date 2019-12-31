#Opens/creates a NetCDF for writing data
.openwriteNetCDF<-function(grid, proj4string, dates, file, add=FALSE, overwrite = FALSE) {
  if(!add) {
    if(file.exists(file) & !overwrite) stop(paste0("File '",file,"' already exist. Use 'overwrite = TRUE' to force overwriting or 'add = TRUE' to add/replace content."))
    cat(paste0("Creating '", file,"'.\n"))
    nx = grid@cells.dim[1]
    ny = grid@cells.dim[2]
    tunits = "days since 1970-01-01 00:00:00.0 -0:00"
    dimX <- ncdim_def( "X", "meters", sort(unique(coordinates(grid)[,1])))
    dimY <- ncdim_def( "Y", "meters", sort(unique(coordinates(grid)[,2])))
    time <- ncdim_def("time", tunits, as.double(as.Date(dates)))
    varMeanTemp <- ncvar_def( "MeanTemperature", "Celsius", list(dimX,dimY, time), NA)
    varMinTemp <- ncvar_def( "MinTemperature", "Celsius", list(dimX,dimY, time), NA)
    varMaxTemp <- ncvar_def( "MaxTemperature", "Celsius", list(dimX,dimY, time), NA)
    varPrec <- ncvar_def( "Precipitation", "L/m2", list(dimX,dimY, time), NA)
    varMeanRH <- ncvar_def( "MeanRelativeHumidity", "%", list(dimX,dimY, time), NA)
    varMinRH <- ncvar_def( "MinRelativeHumidity", "%", list(dimX,dimY, time), NA)
    varMaxRH <- ncvar_def( "MaxRelativeHumidity", "%", list(dimX,dimY, time), NA)
    varRad <- ncvar_def( "Radiation", "MJ/m2", list(dimX,dimY, time), NA)
    varWindSpeed <- ncvar_def( "WindSpeed", "m/s", list(dimX,dimY, time), NA)
    varWindDirection <- ncvar_def( "WindDirection", "Degrees", list(dimX,dimY, time), NA)
    varPET <- ncvar_def( "PET", "L/m2", list(dimX,dimY, time), NA)
    nc <- nc_create(file, list(varMeanTemp,varMinTemp,varMaxTemp,varPrec,
                               varMeanRH, varMinRH,varMaxRH,
                               varRad, varWindSpeed, varWindDirection, varPET) )
    ncatt_put(nc, 0, "proj4string", as.character(proj4string))
  } else {
    if(!file.exists(file)) stop(paste0("File '", file, "' does not exist."))
    cat(paste0("Opening '", file,"' to add/replace data.\n"))
    nc <- nc_open(file, write = T)
  }
  return(nc)
}
#Opens a NetCDF for reading data
.openreadNetCDF<-function(file, verbose =TRUE) {
  if(!file.exists(file)) stop(paste0("File '", file, "' does not exist."))
  if(verbose) cat(paste0("Opening '", file,"' to read data.\n"))
  return(nc_open(file))
}

#Reads values of time dimension
.readdatesNetCDF<-function(ncin) {
  tunits <- ncin$dim$time$units
  s <- strsplit(tunits, " ")[[1]]
  origin <- as.Date(s[3])
  return(as.Date(ncin$dim$time$vals, origin=origin))
}
#writes a grid/pixels for a single variable and day
.putvardataday<-function(nc, var, datavec, day, index=NULL) {
  nx = nc$dim$X$len
  ny = nc$dim$Y$len
  if(!is.null(index)) {
    datavecfull = rep(NA, ny*nx)
    datavecfull[index] = datavec
  } else {
    datavecfull = datavec
  }
  for(i in 1:ny) ncvar_put(nc, varid=var, vals=datavecfull[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1, day), count=c(nx,1,1))
}
#Reads grid/pixels for a single variable and day
.readvardataday<-function(ncin, nx, ny, varname, day) {
  v <- rep(NA, nx*ny)
  #Reads rows in decreasing order
  for(i in 1:ny) {
    v[((i-1)*nx+1):(i*nx)] = ncvar_get(ncin, varname,start=c(1,ny-i+1,day), count=c(nx,1,1))
  }
  return(v)
}
#Reads data for a single pixel and period 
.readvardatapixel<-function(ncin, ny, nt, varname, i, j) {
  return(ncvar_get(ncin, varname,start=c(i,ny-j+1,1), count=c(1,1,nt)))
}
.readdatapixel<-function(ncin, ny, nt, i, j) {
  varMeanTemp = ncin$var$MeanTemperature
  varMinTemp = ncin$var$MinTemperature
  varMaxTemp = ncin$var$MaxTemperature
  varPrec = ncin$var$Precipitation
  varMeanRH = ncin$var$MeanRelativeHumidity
  varMinRH = ncin$var$MinRelativeHumidity
  varMaxRH = ncin$var$MaxRelativeHumidity
  varRad = ncin$var$Radiation
  varWindSpeed = ncin$var$WindSpeed
  varWindDirection = ncin$var$WindDirection
  varPET = ncin$var$PET
  df = data.frame(MeanTemperature = .readvardatapixel(ncin, ny, nt, varMeanTemp, i,j),
                  MinTemperature = .readvardatapixel(ncin,ny, nt, varMinTemp, i,j),
                  MaxTemperature = .readvardatapixel(ncin,ny, nt, varMaxTemp, i,j),
                  Precipitation = .readvardatapixel(ncin,ny, nt, varPrec, i,j),
                  MeanRelativeHumidity = .readvardatapixel(ncin,ny, nt, varMeanRH, i,j),
                  MinRelativeHumidity = .readvardatapixel(ncin,ny, nt, varMinRH, i,j),
                  MaxRelativeHumidity = .readvardatapixel(ncin,ny, nt, varMaxRH, i,j),
                  Radiation = .readvardatapixel(ncin,ny, nt, varRad, i,j),
                  WindSpeed = .readvardatapixel(ncin,ny, nt, varWindSpeed, i,j),
                  WindDirection = .readvardatapixel(ncin,ny, nt, varWindDirection, i,j),
                  PET = .readvardatapixel(ncin,ny, nt, varPET, i,j),
                  row.names = as.character(.readdatesNetCDF(ncin)))
  return(df)
}
#Writes full NetCDF grids
.writemeteorologygridNetCDF<-function(data, grid, proj4string, nc, index=NULL) {
  grid_nc = .readgridtopologyNetCDF(nc)
  nx = grid_nc@cells.dim[1]
  ny = grid_nc@cells.dim[2]
  dates = as.Date(names(data))
  dates_file = .readdatesNetCDF(nc)
  if(nx != grid@cells.dim[1]) stop("Number of x-axis values does not match X dimension in nc file")
  if(ny != grid@cells.dim[2]) stop("Number of y-axis values does not match Y dimension in nc file")
  varMeanTemp = nc$var$MeanTemperature
  varMinTemp = nc$var$MinTemperature
  varMaxTemp = nc$var$MaxTemperature
  varPrec = nc$var$Precipitation
  varMeanRH = nc$var$MeanRelativeHumidity
  varMinRH = nc$var$MinRelativeHumidity
  varMaxRH = nc$var$MaxRelativeHumidity
  varRad = nc$var$Radiation
  varWindSpeed = nc$var$WindSpeed
  varWindDirection = nc$var$WindDirection
  varPET = nc$var$PET
  dates_file = .readdatesNetCDF(nc)
  if(sum(dates %in% dates_file)<length(dates)) stop("Time axis of nc file does not include all supplied dates")
  
  for(j in 1:length(dates)) {
    if(as.character(dates[j]) %in% names(data)) {
      day = which(dates_file==dates[j])
      cat(paste0("Writing data for day '", as.character(dates[j]), "' at time position [",day, "].\n"))
      df = data[[as.character(dates[j])]]
      if("MeanTemperature" %in% names(df)) .putvardataday(nc,varMeanTemp, df$MeanTemperature,day, index)
      if("MinTemperature" %in% names(df)) .putvardataday(nc,varMinTemp, df$MinTemperature,day, index)
      if("MaxTemperature" %in% names(df)) .putvardataday( nc, varMaxTemp, df$MaxTemperature,day, index)
      if("Precipitation" %in% names(df)) .putvardataday( nc, varPrec, df$Precipitation,day, index)
      if("MeanRelativeHumidity" %in% names(df)) .putvardataday( nc, varMeanRH, df$MeanRelativeHumidity,day, index)
      if("MinRelativeHumidity" %in% names(df)) .putvardataday( nc, varMinRH, df$MinRelativeHumidity,day, index)
      if("MaxRelativeHumidity" %in% names(df)) .putvardataday( nc, varMaxRH, df$MaxRelativeHumidity,day, index)
      if("Radiation" %in% names(df)) .putvardataday( nc, varRad, df$Radiation,day, index)
      if("WindSpeed" %in% names(df)) .putvardataday( nc, varWindSpeed, df$WindSpeed,day, index)
      if("WindDirection" %in% names(df)) .putvardataday( nc, varWindDirection, df$WindDirection,day, index)
      if("PET" %in% names(df)) .putvardataday( nc, varPET, df$PET,day, index)
    }
  }
}
#Writes pixels in a NetCDF grid
.writemeteorologypixelsNetCDF<-function(data, pixels, proj4string, nc) {
  .writemeteorologygridNetCDF(data, pixels@grid, proj4string, nc, index=pixels@grid.index)
}
#Closes NetCDF
.closeNetCDF<-function(file,nc, verbose=TRUE) {
  if(verbose) cat(paste0("Closing '", file,"'.\n"))
  nc_close(nc)
}
#Reads NetCDF grid topology
.readgridtopologyNetCDF<-function(ncin) {
  if(("X" %in% names(ncin$dim)) && ("Y" %in% names(ncin$dim))) {
    dimX <- ncvar_get(ncin, "X")
    dimY <- ncvar_get(ncin, "Y")
    cellcentre.offset = c(X = min(dimX), Y = min(dimY))
  }
  else if(("lon" %in% names(ncin$dim)) && ("lat" %in% names(ncin$dim))) {
    dimX <- ncvar_get(ncin, "lon")
    dimY <- ncvar_get(ncin, "lat")
    cellcentre.offset = c(lon = min(dimX), lat = min(dimY))
  }
  cellsize = c(dimX[2]-dimX[1], dimY[2]-dimY[1])
  nx = length(dimX)
  ny = length(dimY)
  cells.dim = c(nx, ny)
  grid = GridTopology(cellcentre.offset, cellsize, cells.dim)
  return(grid)
}
.readCRSNetCDF<-function(ncin) {
  crs = CRS(as.character(NA))
  patt <- ncatt_get(ncin,0, "proj4string")
  if(patt$hasatt) {
    proj4string = patt$value
    if(proj4string!="NA") crs = CRS(proj4string)
  }
  return(crs)
}
.defaultMapping<-function() {
  varmapping = c(MeanTemperature = "MeanTemperature",
                 MinTemperature = "MinTemperature",
                 MaxTemperature = "MaxTemperature",
                 Precipitation = "Precipitation",
                 MeanRelativeHumidity = "MeanRelativeHumidity",
                 MinRelativeHumidity = "MinRelativeHumidity",
                 MaxRelativeHumidity = "MaxRelativeHumidity",
                 Radiation = "Radiation",
                 WindSpeed = "WindSpeed",
                 WindDirection = "WindDirection",
                 PET = "PET")
}
.unitConversion<-function(df, ncin, varmapping) {
  if("Precipitation" %in% names(varmapping)) {
    if(varmapping[["Precipitation"]] %in% names(ncin$var)) {
      prunits = ncin$var[[varmapping[["Precipitation"]]]]$units
      if(prunits == "kg m-2 s-1") {
        df$Precipitation =  df$Precipitation*24*3600
      }
    }
  }
  if("MeanTemperature" %in% names(varmapping)) {
    if(varmapping[["MeanTemperature"]] %in% names(ncin$var)) {
      prunits = ncin$var[[varmapping[["MeanTemperature"]]]]$units
      if(prunits == "K") {
        df$MeanTemperature =  df$MeanTemperature - 273.15
      }
    }
  }
  if("MinTemperature" %in% names(varmapping)) {
    if(varmapping[["MinTemperature"]] %in% names(ncin$var)) {
      prunits = ncin$var[[varmapping[["MinTemperature"]]]]$units
      if(prunits == "K") {
        df$MinTemperature =  df$MinTemperature - 273.15
      }
    }
  }
  if("MaxTemperature" %in% names(varmapping)) {
    if(varmapping[["MaxTemperature"]] %in% names(ncin$var)) {
      prunits = ncin$var[[varmapping[["MaxTemperature"]]]]$units
      if(prunits == "K") {
        df$MaxTemperature =  df$MaxTemperature - 273.15
      }
    }
  }
  return(df)
}
#Reads NetCDF grid/pixels
.readmeteorologyNetCDF<-function(ncin, dates = NULL, pixels = FALSE, varmapping = NULL) {
  crs <- .readCRSNetCDF(ncin)
  grid <- .readgridtopologyNetCDF(ncin)
  nx <- grid@cells.dim[1]
  ny <- grid@cells.dim[2]
  dates_file <- .readdatesNetCDF(ncin)
  dates_file <- as.character(dates_file)
  if(!is.null(dates)) {
    dates <- as.character(dates)
    if(sum(dates %in% dates_file)<length(dates)) stop("Time axis of nc file does not include all supplied dates")
  } else {
    dates = dates_file
  }
  data = vector("list", length(dates))
  names(data)<- as.character(dates)
  if(is.null(varmapping)) varmapping = .defaultMapping()
  for(j in 1:length(dates)) {
    day = which(dates_file==dates[j])
    cat(paste0("Reading data for day '", as.character(dates[j]), "' at time position [",day, "].\n"))
    df = data.frame(row.names = 1:(nx*ny))
    for(var in names(varmapping)) {
      if(varmapping[[var]] %in% names(ncin$var)) {
        df[[var]] = .readvardataday(ncin, nx, ny, varmapping[[var]], day)
      }
    }
    if(ncol(df)>0) {
      df = .unitConversion(df, ncin, varmapping)
      for(i in 1:ncol(df)) df[is.na(df[,i]),i] =NA
    }
    data[[j]] = df
  }
  sm = NULL
  if(!pixels) {
    sm = SpatialGridMeteorology(grid, proj4string = crs, 
                                 data = data, dates=as.Date(dates))
  } else {
    #Remove empty grid cells
    ccgrid = coordinates(grid)
    sel = apply(as.matrix(df),1, function(x) {sum(is.na(x))<length(x)}) #Select points for which at least one value is non-missing
    pts = SpatialPoints(ccgrid[sel,, drop=FALSE], proj4string = crs)
    for(i in 1:length(data)) {
      data[[i]] = data[[i]][sel,, drop=FALSE]
    }
    sm = SpatialPixelsMeteorology(pts, proj4string=crs, 
                             data= data, dates= as.Date(dates),
                             grid = grid)
  }
  return(sm)
}
