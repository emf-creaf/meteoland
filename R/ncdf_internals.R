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
.openreadNetCDF<-function(file) {
  if(!file.exists(file)) stop(paste0("File '", file, "' does not exist."))
  cat(paste0("Opening '", file,"' to read data.\n"))
  return(nc_open(file))
}

#Reads values of time dimension
.readdatesNetCDF<-function(nc) {
  return(as.Date(nc$dim$time$vals, origin="1970-01-01"))
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
.readvardataday<-function(ncin, varname, day) {
  nx = ncin$dim$X$len
  ny = ncin$dim$Y$len
  v <- rep(NA, nx*ny)
  #Reads rows in decreasing order
  for(i in 1:ny) {
    v[((i-1)*nx+1):(i*nx)] = ncvar_get(ncin, varname,start=c(1,ny-i+1,day), count=c(nx,1,1))
  }
  return(v)
}
#Reads data for a single pixel and period 
.readvardatapixel<-function(nci, varname, i, j) {
  ny = ncin$dim$Y$len
  nt = ncin$dim$time$len
  return(ncvar_get(ncin, varname,start=c(1,ny-i+1,1), count=c(1,1,nt)))
}
#Writes full NetCDF grids
.writemeteorologygridNetCDF<-function(data, grid, proj4string, nc, index=NULL) {
  nx = nc$dim$X$len
  ny = nc$dim$Y$len
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
      .putvardataday(nc,varMeanTemp, df$MeanTemperature,day, index)
      .putvardataday(nc,varMinTemp, df$MinTemperature,day, index)
      .putvardataday( nc, varMaxTemp, df$MaxTemperature,day, index)
      .putvardataday( nc, varPrec, df$Precipitation,day, index)
      .putvardataday( nc, varMeanRH, df$MeanRelativeHumidity,day, index)
      .putvardataday( nc, varMinRH, df$MinRelativeHumidity,day, index)
      .putvardataday( nc, varMaxRH, df$MaxRelativeHumidity,day, index)
      .putvardataday( nc, varRad, df$Radiation,day, index)
      .putvardataday( nc, varWindSpeed, df$WindSpeed,day, index)
      .putvardataday( nc, varWindDirection, df$WindDirection,day, index)
      .putvardataday( nc, varPET, df$PET,day, index)
    }
  }
}
#Writes pixels in a NetCDF grid
.writemeteorologypixelsNetCDF<-function(data, pixels, proj4string, nc) {
  .writemeteorologygridNetCDF(data, pixels@grid, proj4string, nc, index=pixels@grid.index)
}
#Closes NetCDF
.closeNetCDF<-function(file,nc) {
  cat(paste0("Closing '", file,"'.\n"))
  nc_close(nc)
}
#Reads NetCDF grid topology
.readgridtopologyNetCDF<-function(ncin) {
  dimX <- ncvar_get(ncin, "X")
  dimY <- ncvar_get(ncin, "Y")
  cellcentre.offset = c(min(dimX), min(dimY))
  cellsize = c(dimX[2]-dimX[1], dimY[2]-dimY[1])
  nx = length(dimX)
  ny = length(dimY)
  cells.dim = c(nx, ny)
  grid = GridTopology(cellcentre.offset, cellsize, cells.dim)
  return(grid)
}
#Reads NetCDF grid/pixels
.readmeteorologyNetCDF<-function(ncin, dates = NULL, pixels = FALSE) {
  proj4string <- ncatt_get(ncin,0, "proj4string")$value
  if(proj4string!="NA") crs = CRS(proj4string)
  else crs = CRS(as.character(NA))
  dimX <- ncvar_get(ncin, "X")
  dimY <- ncvar_get(ncin, "Y")
  dates_file <- .readdatesNetCDF(ncin)
  if(!is.null(dates)) {
    if(sum(dates %in% dates_file)<length(dates)) stop("Time axis of nc file does not include all supplied dates")
  } else {
    dates = dates_file
  }
  cellcentre.offset = c(min(dimX), min(dimY))
  cellsize = c(dimX[2]-dimX[1], dimY[2]-dimY[1])
  nx = length(dimX)
  ny = length(dimY)
  cells.dim = c(nx, ny)
  grid = GridTopology(cellcentre.offset, cellsize, cells.dim)
  data = vector("list", length(dates))
  names(data)<- as.character(dates)
  for(j in 1:length(dates)) {
    day = which(dates_file==dates[j])
    cat(paste0("Reading data for day '", as.character(dates[j]), "' at time position [",day, "].\n"))
    df = data.frame(MeanTemperature = .readvardataday(ncin, "MeanTemperature", day),
                    MinTemperature = .readvardataday(ncin, "MinTemperature", day),
                    MaxTemperature = .readvardataday(ncin, "MaxTemperature", day),
                    Precipitation = .readvardataday(ncin, "Precipitation", day),
                    MeanRelativeHumidity = .readvardataday(ncin, "MeanRelativeHumidity", day),
                    MinRelativeHumidity = .readvardataday(ncin, "MinRelativeHumidity", day),
                    MaxRelativeHumidity = .readvardataday(ncin, "MaxRelativeHumidity", day),
                    Radiation = .readvardataday(ncin, "Radiation", day),
                    WindSpeed = .readvardataday(ncin, "WindSpeed", day),
                    WindDirection = .readvardataday(ncin, "WindDirection", day),
                    PET = .readvardataday(ncin, "PET", day))
    for(i in 1:ncol(df)) df[is.na(df[,i]),i] =NA
    data[[j]] = df
  }
  sm = NULL
  if(!pixels) {
    sm = SpatialGridMeteorology(grid, proj4string = CRS(proj4string), 
                                 data = data, dates=dates)
  } else {
    #Remove empty grid cells
    ccgrid = coordinates(grid)
    sel = apply(as.matrix(df),1, function(x) {sum(is.na(x))<length(x)}) #Select points for which at least one value is non-missing
    pts = SpatialPoints(ccgrid[sel,], proj4string = CRS(proj4string))
    for(i in 1:length(data)) {
      data[[i]] = data[[i]][sel,]
    }
    sm = SpatialPixelsMeteorology(pts, proj4string=pts@proj4string, 
                             data= data, dates= dates,
                             grid = grid)
  }
  return(sm)
}
