.readmeteorologypixelsNetCDF<-function(file, format = "netCDF") {
  readvargriddata<-function(ncin, varname, nx, ny) {
    v <- rep(NA, nx*ny)
    #Reads rows in decreasing order
    for(i in 1:ny) {
      v[((i-1)*nx+1):(i*nx)] = ncvar_get(ncin, varname,start=c(1,ny-i+1), count=c(nx,1))
    }
    return(v)
  }
  if(format=="netCDF") {
    ncin <- nc_open(file)
    proj4string <- ncatt_get(ncin,0, "proj4string")$value
    if(proj4string!="NA") crs = CRS(proj4string)
    else crs = CRS(as.character(NA))
    date <- ncatt_get(ncin,0, "date")$value
    dimX <- ncvar_get(ncin, "X")
    dimY <- ncvar_get(ncin, "Y")
    cellcentre.offset = c(min(dimX), min(dimY))
    cellsize = c(dimX[2]-dimX[1], dimY[2]-dimY[1])
    nx = length(dimX)
    ny = length(dimY)
    cells.dim = c(nx, ny)
    grid = GridTopology(cellcentre.offset, cellsize, cells.dim)
    df = data.frame(MeanTemperature = readvargriddata(ncin, "MeanTemperature", nx,ny),
                    MinTemperature = readvargriddata(ncin, "MinTemperature", nx,ny),
                    MaxTemperature = readvargriddata(ncin, "MaxTemperature", nx,ny),
                    Precipitation = readvargriddata(ncin, "Precipitation", nx,ny),
                    MeanRelativeHumidity = readvargriddata(ncin, "MeanRelativeHumidity", nx,ny),
                    MinRelativeHumidity = readvargriddata(ncin, "MinRelativeHumidity", nx,ny),
                    MaxRelativeHumidity = readvargriddata(ncin, "MaxRelativeHumidity", nx,ny),
                    Radiation = readvargriddata(ncin, "Radiation", nx,ny),
                    WindSpeed = readvargriddata(ncin, "WindSpeed", nx,ny),
                    WindDirection = readvargriddata(ncin, "WindDirection", nx,ny),
                    PET = readvargriddata(ncin, "PET", nx,ny))
    
    for(i in 1:ncol(df)) df[is.na(df[,i]),i] =NA

  }
}


readmeteorologypixelsfiles<-function(files, format="netCDF") {
  if((!inherits(files,"character"))&&(!inherits(files,"data.frame"))) stop("'files' has to be a character vector or a data frame with columns 'dir' and 'filename'.")
  if(inherits(files,"data.frame")) {
    nfiles = nrow(files)
    filevec = rep("", nfiles)
    for(i in 1:nfiles) {
      if(files$dir[i]!="") filevec[i] = paste(files$dir[i], files$filename[i], sep="/")
      else filevec[i] = files$filename[i]
    }
    files = filevec
  } else {
    nfiles = length(files)
  }
  day1met = .readmeteorologypixelsNetCDF(files[1],format)
  pixels = as(day1met$spdf, "SpatialPixels")
  proj4string = day1met$spdf@proj4string
  dstringvec = rep("", nfiles)
  if(nfiles>1) {
    dfvec = vector("list",nfiles)
    dfvec[[1]] = day1met$spdf@data
    names(dfvec)[1] = day1met$date
    dstringvec[1] =  day1met$date
    for(i in 2:nfiles) {
      dayimet = .readmeteorologypixelsNetCDF(files[i],format)
      dfvec[[i]] = dayimet$spdf@data
      names(dfvec)[i] = dayimet$date
      dstringvec[i] =  dayimet$date
    }
    return(SpatialPixelsMeteorology(as(pixels,"SpatialPoints"), proj4string=pixels@proj4string, 
                                    data= dfvec, dates= as.Date(dstringvec),
                                    grid = pixels@grid))
  }
  return(day1met$sgdf)
}
