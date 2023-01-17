.isLongLat<-function(proj4string) {
  if(inherits(proj4string, "CRS")) args = proj4string@projargs
  else args = proj4string
  if(is.na(args)) return(FALSE)
  s = strsplit(args," ")[[1]]
  return("+proj=longlat" %in% s)
}
.projUnits<-function(proj4string) {
  if(inherits(proj4string, "CRS")) args = proj4string@projargs
  else args = proj4string
  if(!is.na(args)) {
    s = strsplit(strsplit(args," ")[[1]],"=")
    for(i in 1:length(s)) {
      if(s[[i]][1]=="+units") return(s[[i]][2])
    }
  }
  return("unknown")
}
#writes a grid/pixels for a single variable and day
.putgridvardataday<-function(nc, var, datavec, day, index=NULL) {
  if("x" %in% names(nc$dim)) nx = nc$dim$x$len
  else nx = nc$dim$lon$len

  if("y" %in% names(nc$dim)) ny = nc$dim$y$len
  else ny = nc$dim$lat$len

  if(!is.null(index)) {
    datavecfull = rep(NA, ny*nx)
    datavecfull[index] = datavec
  } else {
    datavecfull = datavec
  }
  # Assumes time dimension is the last
  for(i in 1:ny) ncdf4::ncvar_put(nc, varid=var, vals=datavecfull[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1, day), count=c(nx,1,1))
}
.putgridday<-function(nc, df, day, index=NULL) {
  if("MeanTemperature" %in% names(df)) .putgridvardataday(nc,"MeanTemperature", df$MeanTemperature,day, index)
  if("MinTemperature" %in% names(df)) .putgridvardataday(nc,"MinTemperature", df$MinTemperature,day, index)
  if("MaxTemperature" %in% names(df)) .putgridvardataday( nc, "MaxTemperature", df$MaxTemperature,day, index)
  if("Precipitation" %in% names(df)) .putgridvardataday( nc, "Precipitation", df$Precipitation,day, index)
  if("MeanRelativeHumidity" %in% names(df)) .putgridvardataday( nc, "MeanRelativeHumidity", df$MeanRelativeHumidity,day, index)
  if("MinRelativeHumidity" %in% names(df)) .putgridvardataday( nc, "MinRelativeHumidity", df$MinRelativeHumidity,day, index)
  if("MaxRelativeHumidity" %in% names(df)) .putgridvardataday( nc, "MaxRelativeHumidity", df$MaxRelativeHumidity,day, index)
  if("Radiation" %in% names(df)) .putgridvardataday( nc, "Radiation", df$Radiation,day, index)
  if("WindSpeed" %in% names(df)) .putgridvardataday( nc, "WindSpeed", df$WindSpeed,day, index)
  if("WindDirection" %in% names(df)) .putgridvardataday( nc, "WindDirection", df$WindDirection,day, index)
  if("PET" %in% names(df)) .putgridvardataday( nc, "PET", df$PET,day, index)
}
#writes a grid/pixels for a single variable (no temporal dimension)
.putgridvardata<-function(nc, var, datavec) {
  nx = nc$dim$x$len
  ny = nc$dim$y$len
  for(i in 1:ny) ncdf4::ncvar_put(nc, varid=var, vals=datavec[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1), count=c(nx,1))
}
#writes data for a single pixel starting at time position t
.putgridvardatapixel<-function(ncin, ny, varname, i, j, t, datavec) {
  nt = length(datavec)
  # Assumes time dimension is the last
  return(ncdf4::ncvar_put(ncin, varname, vals = datavec, start=c(i,ny-j+1,t), count=c(1,1,nt)))
}
.putgridpixel<-function(ncin, ny, i,j,t, df) {
  if("MeanTemperature" %in% names(df)) .putgridvardatapixel(ncin,ny,"MeanTemperature",  i,j,t, df$MeanTemperature)
  if("MinTemperature" %in% names(df)) .putgridvardatapixel(ncin,ny,"MinTemperature",  i,j,t, df$MinTemperature)
  if("MaxTemperature" %in% names(df)) .putgridvardatapixel( ncin, ny,"MaxTemperature",  i,j,t, df$MaxTemperature)
  if("Precipitation" %in% names(df)) .putgridvardatapixel( ncin, ny,"Precipitation",  i,j,t, df$Precipitation)
  if("MeanRelativeHumidity" %in% names(df)) .putgridvardatapixel( ncin, ny,"MeanRelativeHumidity",  i,j,t, df$MeanRelativeHumidity)
  if("MinRelativeHumidity" %in% names(df)) .putgridvardatapixel( ncin, ny,"MinRelativeHumidity",  i,j,t, df$MinRelativeHumidity)
  if("MaxRelativeHumidity" %in% names(df)) .putgridvardatapixel( ncin, ny,"MaxRelativeHumidity",  i,j,t, df$MaxRelativeHumidity)
  if("Radiation" %in% names(df)) .putgridvardatapixel( ncin, ny,"Radiation",  i,j,t, df$Radiation)
  if("WindSpeed" %in% names(df)) .putgridvardatapixel( ncin, ny,"WindSpeed",  i,j,t, df$WindSpeed)
  if("WindDirection" %in% names(df)) .putgridvardatapixel( ncin, ny,"WindDirection", i,j,t, df$WindDirection)
  if("PET" %in% names(df)) .putgridvardatapixel( ncin, ny,"PET", i,j,t, df$PET)
}
#Opens/creates a NetCDF to add data
.openaddNetCDF<-function(file, verbose = FALSE) {
  if(!file.exists(file)) stop(paste0("File '", file, "' does not exist."))
  if(verbose) cat(paste0("Opening '", file,"' to add/replace data.\n"))
  nc <- ncdf4::nc_open(file, write = T)
  return(nc)
}
#Opens/creates a NetCDF for writing grid data
.openwritegridNetCDF<-function(grid, proj4string, dates, vars, file,
                               byPixel = FALSE, chunksizes = NA,
                               add=FALSE, overwrite = FALSE, verbose = FALSE) {
  if(!add) {
    if(is.null(vars)) vars = .defaultVars()
    if(file.exists(file) & !overwrite) stop(paste0("File '",file,"' already exist. Use 'overwrite = TRUE' to force overwriting or 'add = TRUE' to add/replace content."))
    if(verbose) cat(paste0("\nCreating '", file,"'.\n"))
    nx = grid@cells.dim[1]
    ny = grid@cells.dim[2]
    tunits = "days since 1970-01-01 00:00:00.0 -0:00"
    if(.isLongLat(proj4string)) {
      dimX <- ncdf4::ncdim_def( "lon", "degrees_east", sort(unique(coordinates(grid)[,1])))
      dimY <- ncdf4::ncdim_def( "lat", "degrees_north", sort(unique(coordinates(grid)[,2])))
    } else {
      pr_units = .projUnits(proj4string)
      dimX <- ncdf4::ncdim_def( "x", pr_units, sort(unique(coordinates(grid)[,1])), longname = "x coordinate of projection")
      dimY <- ncdf4::ncdim_def( "y", pr_units, sort(unique(coordinates(grid)[,2])), longname = "y coordinate of projection")
    }
    time <- ncdf4::ncdim_def("time", tunits, as.double(as.Date(dates)), longname = "time of measurement",unlim = TRUE)
    nt = length(dates)
    if(byPixel) {
      chunksizes = c(1,1,length(dates))
    }
    varlist = vector("list")
    if("MeanTemperature" %in% vars) varlist[["varMeanTemp"]] = ncdf4::ncvar_def( "MeanTemperature", "Celsius", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("MinTemperature" %in% vars) varlist[["varMinTemp"]] = ncdf4::ncvar_def( "MinTemperature", "Celsius", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("MaxTemperature" %in% vars) varlist[["varMaxTemp"]] = ncdf4::ncvar_def( "MaxTemperature", "Celsius", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("Precipitation" %in% vars) varlist[["varPrec"]] = ncdf4::ncvar_def( "Precipitation", "l m-2", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("MeanRelativeHumidity" %in% vars) varlist[["varMeanRH"]] = ncdf4::ncvar_def( "MeanRelativeHumidity", "%", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("MinRelativeHumidity" %in% vars) varlist[["varMinRH"]] =  ncdf4::ncvar_def( "MinRelativeHumidity", "%", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("MaxRelativeHumidity" %in% vars) varlist[["varMaxRH"]] =  ncdf4::ncvar_def( "MaxRelativeHumidity", "%", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("Radiation" %in% vars) varlist[["varRad"]] =  ncdf4::ncvar_def( "Radiation", "MJ m-2", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("WindSpeed" %in% vars) varlist[["varWindSpeed"]] =  ncdf4::ncvar_def( "WindSpeed", "m s-1", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("WindDirection" %in% vars) varlist[["varWindDirection"]] =  ncdf4::ncvar_def( "WindDirection", "degrees_north", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if("PET" %in% vars) varlist[["varPET"]] =  ncdf4::ncvar_def( "PET", "l m-2", list(dimX,dimY,time), NULL, chunksizes = chunksizes)
    if(.isLongLat(proj4string)) {
      nc <- ncdf4::nc_create(file, varlist, force_v4 = T)
    } else {
      # Fill data for lon/lat variables
      if(!is.na(proj4string)) {
        #Define additional lon/lat variables
        varLon <- ncdf4::ncvar_def( "lon", "degrees_east", list(dimX,dimY), missval = NULL, longname = "longitude")
        varLat <- ncdf4::ncvar_def( "lat", "degrees_north", list(dimX,dimY), missval =NULL, longname = "latitude")
        varlist2 = list("varX" = varLon, "varY" = varLat)
        for(i in 1:length(varlist)) varlist2[[names(varlist)[i]]] = varlist[[i]]
        nc <- ncdf4::nc_create(file, varlist2, force_v4 = T)
        spt_lonlat = spTransform(SpatialPoints(coordinates(grid), CRS(proj4string)), CRS(SRS_string = "EPSG:4326"))
        lonlat = coordinates(spt_lonlat)
        .putgridvardata(nc, varLon, lonlat[,1])
        .putgridvardata(nc, varLat, lonlat[,2])
      } else {
        nc <- ncdf4::nc_create(file, varlist, force_v4 = T)
      }

      # Indicate axes
      ncdf4::ncatt_put(nc, "x", "axis", "X")
      ncdf4::ncatt_put(nc, "y", "axis", "Y")
    }
    ncdf4::ncatt_put(nc, 0, "proj4string", as.character(proj4string))
    ncdf4::ncatt_put(nc, "time", "axis", "T")
  }
  else {
    return(.openaddNetCDF(file, verbose = verbose))
  }
  return(nc)
}
#Opens/creates a NetCDF for writing point data
.openwritepointNetCDF<-function(coords, proj4string, dates, vars,
                                file, overwrite = FALSE, verbose = FALSE) {
  if(is.null(vars)) vars = .defaultVars()
  if(file.exists(file) & !overwrite) stop(paste0("File '",file,"' already exist. Use 'overwrite = TRUE' to force overwriting or 'add = TRUE' to add/replace content."))
  if(verbose) cat(paste0("\nCreating '", file,"'.\n"))
  tunits = "days since 1970-01-01 00:00:00.0 -0:00"
  time <- ncdf4::ncdim_def("time", tunits, as.double(as.Date(dates)), longname = "time of measurement")
  np = nrow(coords)
  station <- ncdf4::ncdim_def("station", "", vals = 1:np, unlim = TRUE)


  stationName = ncdf4::ncvar_def("station_name", "", list(station), missval = NULL, longname = "station name")

  varlist = vector("list")
  if("MeanTemperature" %in% vars) varlist[["varMeanTemp"]] = ncdf4::ncvar_def( "MeanTemperature", "Celsius", list(station, time), NULL)
  if("MinTemperature" %in% vars) varlist[["varMinTemp"]] = ncdf4::ncvar_def( "MinTemperature", "Celsius", list(station, time), NULL)
  if("MaxTemperature" %in% vars) varlist[["varMaxTemp"]] = ncdf4::ncvar_def( "MaxTemperature", "Celsius", list(station, time), NULL)
  if("Precipitation" %in% vars) varlist[["varPrec"]] = ncdf4::ncvar_def( "Precipitation", "l m-2", list(station, time), NULL)
  if("MeanRelativeHumidity" %in% vars) varlist[["varMeanRH"]] = ncdf4::ncvar_def( "MeanRelativeHumidity", "%", list(station, time), NULL)
  if("MinRelativeHumidity" %in% vars) varlist[["varMinRH"]] = ncdf4::ncvar_def( "MinRelativeHumidity", "%", list(station, time), NULL)
  if("MaxRelativeHumidity" %in% vars) varlist[["varMaxRH"]] = ncdf4::ncvar_def( "MaxRelativeHumidity", "%", list(station, time), NULL)
  if("Radiation" %in% vars) varlist[["varRad"]] = ncdf4::ncvar_def( "Radiation", "MJ m-2", list(station, time), NULL)
  if("WindSpeed" %in% vars) varlist[["varWindSpeed"]] = ncdf4::ncvar_def( "WindSpeed", "m s-1", list(station, time), NULL)
  if("WindDirection" %in% vars) varlist[["varWindDirection"]] = ncdf4::ncvar_def( "WindDirection", "degrees_north", list(station, time), NULL)
  if("PET" %in% vars) varlist[["varPET"]] = ncdf4::ncvar_def( "PET", "l m-2", list(station, time), NULL)
  if(.isLongLat(proj4string)) {
    varX <- ncdf4::ncvar_def( "lon", "degrees_east", list(station), missval = NULL)
    varY <- ncdf4::ncvar_def( "lat", "degrees_north", list(station), missval = NULL)
    varlist2 = list("stationName" = stationName, "varX" = varX, "varY" = varY)
    for(i in 1:length(varlist)) varlist2[[names(varlist)[i]]] = varlist[[i]]
    nc <- ncdf4::nc_create(file, varlist2, force_v4 = T)
    ncdf4::ncvar_put(nc, varid=varX, vals=coords[,1], start=c(1), count=c(np))
    ncdf4::ncvar_put(nc, varid=varY, vals=coords[,2], start=c(1), count=c(np))
  } else {
    pr_units = .projUnits(proj4string)
    varX <- ncdf4::ncvar_def( "x", pr_units, list(station), missval = NULL, longname = "x coordinate of projection")
    varY <- ncdf4::ncvar_def( "y", pr_units, list(station), missval = NULL, longname = "y coordinate of projection")
    varLon <- ncdf4::ncvar_def( "lon", "degrees_east", list(station), missval = NULL)
    varLat <- ncdf4::ncvar_def( "lat", "degrees_north", list(station), missval = NULL)
    varlist2 = list("stationName" = stationName, "varX" = varX, "varY" = varY, "varLon" = varLon, "varLat" = varLat)
    for(i in 1:length(varlist)) varlist2[[names(varlist)[i]]] = varlist[[i]]
    nc <- ncdf4::nc_create(file, varlist2, force_v4 = T)
    ncdf4::ncvar_put(nc, varid=varX, vals=coords[,1], start=c(1), count=c(np))
    ncdf4::ncvar_put(nc, varid=varY, vals=coords[,2], start=c(1), count=c(np))
    spt_lonlat = spTransform(SpatialPoints(coords, CRS(proj4string)), CRS(SRS_string = "EPSG:4326"))
    lonlat = coordinates(spt_lonlat)
    ncdf4::ncvar_put(nc, varid=varLon, vals=lonlat[,1], start=c(1), count=c(np))
    ncdf4::ncvar_put(nc, varid=varLat, vals=lonlat[,2], start=c(1), count=c(np))
  }
  rn = rownames(coords)
  if(is.null(rn)) rn = 1:nrow(coords)
  ncdf4::ncvar_put(nc, varid=stationName, vals=rn, start=c(1), count=c(np))

  # Indicate axes
  ncdf4::ncatt_put(nc, stationName, "cf_role", "timeseries_id")
  ncdf4::ncatt_put(nc, 0, "featureType", "timeSeries")
  ncdf4::ncatt_put(nc, 0, "proj4string", as.character(proj4string))
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  return(nc)
}

.writemeteorologypointNetCDF<-function(df, nc, i) {
  nt = nc$dim$time$len
  if("MeanTemperature" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$MeanTemperature, vals=df$MeanTemperature, start=c(i,1), count=c(1,nt))
  if("MinTemperature" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$MinTemperature, vals=df$MinTemperature, start=c(i,1), count=c(1,nt))
  if("MaxTemperature" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$MaxTemperature, vals=df$MaxTemperature, start=c(i,1), count=c(1,nt))
  if("Precipitation" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$Precipitation, vals=df$Precipitation, start=c(i,1), count=c(1,nt))
  if("MeanRelativeHumidity" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$MeanRelativeHumidity, vals=df$MeanRelativeHumidity, start=c(i,1), count=c(1,nt))
  if("MinRelativeHumidity" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$MinRelativeHumidity, vals=df$MinRelativeHumidity, start=c(i,1), count=c(1,nt))
  if("MaxRelativeHumidity" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$MaxRelativeHumidity, vals=df$MaxRelativeHumidity, start=c(i,1), count=c(1,nt))
  if("Radiation" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$Radiation, vals=df$Radiation, start=c(i,1), count=c(1,nt))
  if("WindSpeed" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$WindSpeed, vals=df$WindSpeed, start=c(i,1), count=c(1,nt))
  if("WindDirection" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$WindDirection, vals=df$WindDirection, start=c(i,1), count=c(1,nt))
  if("PET" %in% names(df)) ncdf4::ncvar_put(nc, varid=nc$var$PET, vals=df$PET, start=c(i,1), count=c(1,nt))
}
#Writes full NetCDF points
.writemeteorologypointsNetCDF<-function(data, nc, verbose = FALSE) {
  np = nc$dim$station$len
  if(length(data)!=np) stop("Number of points does not match netCDF definition!")
  for(i in 1:np) {
    if(verbose) cat(paste0("Writing data for point '", names(data)[i], "'.\n"))
    .writemeteorologypointNetCDF(data[[i]], nc, i)
    ncdf4::nc_sync(nc) # Flushes writing so that we avoid losing data if process crashes
  }
}
#adds or replaces points to NetCDF
.addreplacemeteorologypointsNetCDF<-function(object, nc, verbose = FALSE) {
  crs <- .readCRSNetCDF(nc)
  dates_files <- .readdatesNetCDF(nc)
  if(sum(dates_files==object@dates)<length(dates_files)) stop("Dates do not match between netCDF and object")
  if(crs@projargs != proj4string(object)) stop("Point data does not have the same CRS as the netCDF")
  data <- object@data
  coords <- object@coords
  spt_lonlat = spTransform(SpatialPoints(coords, crs), CRS(SRS_string = "EPSG:4326"))
  lonlat = coordinates(spt_lonlat)
  IDs <- ncdf4::ncvar_get(nc, "station_name")
  cnt = length(IDs)
  for(i in 1:length(data)) {
    stnamei = names(data)[i]
    index = which(IDs == stnamei)
    if(length(index)==1) {
      if(verbose) cat(paste0("Replacing existing coordinates and data for point '", stnamei, "'.\n"))
      if(.isLongLat(crs)) {
        ncdf4::ncvar_put(nc, varid="lon", vals=coords[i,1], start=index, count=1)
        ncdf4::ncvar_put(nc, varid="lat", vals=coords[i,2], start=index, count=1)
      } else {
        ncdf4::ncvar_put(nc, varid="x", vals=coords[i,1], start=index, count=1)
        ncdf4::ncvar_put(nc, varid="y", vals=coords[i,2], start=index, count=1)
        ncdf4::ncvar_put(nc, varid="lon", vals=lonlat[i,1], start=index, count=1)
        ncdf4::ncvar_put(nc, varid="lat", vals=lonlat[i,2], start=index, count=1)
      }
      .writemeteorologypointNetCDF(data[[i]], nc, index)
      ncdf4::nc_sync(nc) # Flushes writing so that we avoid losing data if process crashes
    } else {
      cnt = cnt+1
      if(verbose) cat(paste0("Adding new coordinates and data for point '", stnamei, "'.\n"))
      if(.isLongLat(crs)) {
        ncdf4::ncvar_put(nc, varid="lon", vals=coords[i,1], start=cnt, count=1)
        ncdf4::ncvar_put(nc, varid="lat", vals=coords[i,2], start=cnt, count=1)
      } else {
        ncdf4::ncvar_put(nc, varid="x", vals=coords[i,1], start=cnt, count=1)
        ncdf4::ncvar_put(nc, varid="y", vals=coords[i,2], start=cnt, count=1)
        ncdf4::ncvar_put(nc, varid="lon", vals=lonlat[i,1], start=cnt, count=1)
        ncdf4::ncvar_put(nc, varid="lat", vals=lonlat[i,2], start=cnt, count=1)
      }
      ncdf4::ncvar_put(nc, varid="station_name", vals=rownames(coords)[i], start=cnt, count=1)
      .writemeteorologypointNetCDF(data[[i]], nc, cnt)
      ncdf4::nc_sync(nc) # Flushes writing so that we avoid losing data if process crashes
    }
  }
}

#Writes full NetCDF grids
.writemeteorologygridNetCDF<-function(data, grid, proj4string, nc,
                                      index=NULL,
                                      byPixel = FALSE, verbose = FALSE) {
  grid_nc = .readgridtopologyNetCDF(nc)
  nx = grid_nc@cells.dim[1]
  ny = grid_nc@cells.dim[2]
  dates = as.Date(names(data))
  dates_file = .readdatesNetCDF(nc)
  if(nx != grid@cells.dim[1]) stop("Number of x-axis values does not match X dimension in nc file")
  if(ny != grid@cells.dim[2]) stop("Number of y-axis values does not match Y dimension in nc file")
  dates_file = .readdatesNetCDF(nc)

  w = which(!(dates %in% dates_file))
  if(length(w)>0) {
    if(verbose) cat(paste0("Adding new ", length(w), " dates.\n"))
    for(i in 1:length(w)) {
      if(as.Date(dates[w[i]])>dates_file[length(dates_file)]) {
        ncdf4::ncvar_put(nc,"time", as.double(as.Date(dates[w[i]])), start= length(dates_file)+1, count = 1)
      } else {
        stop(paste0("New date ", dates[w[i]]," is not later than the last date in nc file!"))
      }
      dates_file = .readdatesNetCDF(nc)
    }
  }
  if(!byPixel) {
    for(j in 1:length(dates)) {
      if(as.character(dates[j]) %in% names(data)) {
        day = which(dates_file==dates[j])
        if(verbose) cat(paste0("Writing data for day '", as.character(dates[j]), "' at time position [",day, "].\n"))
        .putgridday(nc = nc, df = data[[as.character(dates[j])]],
                    day = day, index = index)
        ncdf4::nc_sync(nc) # Flushes writing
      }
    }
  } else {
    cv = coordinatevalues(grid_nc)
    cc = coordinates(grid_nc)
    if(!is.null(index)) cc = cc[index,]
    varnames = names(data[[1]])
    t = which(as.character(dates_file)==as.character(dates[1]))
    df = data.frame(matrix(NA, nrow = length(dates), ncol=length(varnames)))
    colnames(df)= varnames
    rownames(df)=as.character(dates)
    if(verbose) pb = txtProgressBar(0, nrow(cc), style=3)
    for(px in 1:nrow(cc)) {
      if(verbose) setTxtProgressBar(pb,px)
      for(d in 1:length(dates)) df[d,] = data[[d]][px,]
      i = which(cv[[1]]==cc[px,1])
      j = which(cv[[2]]==cc[px,2])
      .putgridpixel(nc,ny, i,j,t,df)
    }
  }
}
#Writes pixels in a NetCDF grid
.writemeteorologypixelsNetCDF<-function(data, pixels, proj4string, nc,
                                        byPixel, verbose = FALSE) {
  .writemeteorologygridNetCDF(data, pixels@grid, proj4string, nc = nc,
                              index=pixels@grid.index, byPixel = byPixel, verbose = verbose)
}

#Opens a NetCDF for reading data
.openreadgridNetCDF<-function(file, verbose =FALSE) {
  if(!file.exists(file)) stop(paste0("File '", file, "' does not exist."))
  if(verbose) cat(paste0("\nOpening '", file,"' to read data.\n"))
  nc = ncdf4::nc_open(file)
  return(nc)
}

.openreadpointsNetCDF<-function(file, verbose =FALSE) {
  if(!file.exists(file)) stop(paste0("File '", file, "' does not exist."))
  if(verbose) cat(paste0("\nOpening '", file,"' to read data.\n"))
  return(ncdf4::nc_open(file))
}
#Reads values of time dimension
.readdatesNetCDF<-function(ncin) {
  tunits <- ncin$dim$time$units
  s <- strsplit(tunits, " ")[[1]]
  origin <- as.Date(s[3])
  return(as.Date(ncdf4::ncvar_get(ncin,"time"), origin=origin))
}

#Reads grid/pixels for a single variable and day
.readgridvardataday<-function(ncin, nx, ny, varname, day, selection = NULL) {
  timefirst = (ncin$var[[varname]]$dim[[1]]$name=="time")
  v <- rep(NA, nx*ny)
  #Reads rows in decreasing order
  if(timefirst) { # Time is first dimension
    for(i in 1:ny) {
      v[((i-1)*nx+1):(i*nx)] = ncdf4::ncvar_get(ncin, varname,start=c(day,1,ny-i+1), count=c(1,nx,1))
    }
  } else {
    for(i in 1:ny) { # Time is last dimension
      v[((i-1)*nx+1):(i*nx)] = ncdf4::ncvar_get(ncin, varname,start=c(1,ny-i+1,day), count=c(nx,1,1))
    }
  }
  if(!is.null(selection)) v = v[selection]
  return(v)
}
#Reads data for a single pixel and period
.readgridvardatapixel<-function(ncin, ny, nt, varname, i, j) {
  if(varname %in% names(ncin$var)) {
    timefirst = (ncin$var[[varname]]$dim[[1]]$name=="time")
    if(timefirst) {
      v = ncdf4::ncvar_get(ncin, varname,start=c(1,i,ny-j+1), count=c(nt,1,1))
    } else {
      v = ncdf4::ncvar_get(ncin, varname,start=c(i,ny-j+1,1), count=c(1,1,nt))
    }
  } else {
    v = rep(NA, nt)
  }
  return(v)
}
.readgriddatapixel<-function(ncin, ny, nt, i, j) {
  df = data.frame(row.names = as.character(.readdatesNetCDF(ncin)))
  if("MeanTemperature" %in% names(ncin$var)) df$MeanTemperature = .readgridvardatapixel(ncin, ny, nt, "MeanTemperature", i,j)
  if("MinTemperature" %in% names(ncin$var)) df$MinTemperature = .readgridvardatapixel(ncin,ny, nt, "MinTemperature", i,j)
  if("MaxTemperature" %in% names(ncin$var)) df$MaxTemperature = .readgridvardatapixel(ncin,ny, nt, "MaxTemperature", i,j)
  if("Precipitation" %in% names(ncin$var)) df$Precipitation = .readgridvardatapixel(ncin,ny, nt, "Precipitation", i,j)
  if("MeanRelativeHumidity" %in% names(ncin$var)) df$MeanRelativeHumidity = .readgridvardatapixel(ncin,ny, nt, "MeanRelativeHumidity", i,j)
  if("MinRelativeHumidity" %in% names(ncin$var)) df$MinRelativeHumidity = .readgridvardatapixel(ncin,ny, nt, "MinRelativeHumidity", i,j)
  if("MaxRelativeHumidity" %in% names(ncin$var)) df$MaxRelativeHumidity = .readgridvardatapixel(ncin,ny, nt, "MaxRelativeHumidity", i,j)
  if("Radiation" %in% names(ncin$var)) df$Radiation = .readgridvardatapixel(ncin,ny, nt, "Radiation", i,j)
  if("WindSpeed" %in% names(ncin$var)) df$WindSpeed = .readgridvardatapixel(ncin,ny, nt, "WindSpeed", i,j)
  if("WindDirection" %in% names(ncin$var)) df$WindDirection = .readgridvardatapixel(ncin,ny, nt, "WindDirection", i,j)
  if("PET" %in% names(ncin$var)) df$PET = .readgridvardatapixel(ncin,ny, nt, "PET", i,j)
  return(df)
}

#Closes NetCDF
.closeNetCDF<-function(file,nc, verbose=FALSE) {
  if(verbose) cat(paste0("\nClosing '", file,"'.\n"))
  ncdf4::nc_close(nc)
}
#Reads NetCDF grid topology
.readgridtopologyNetCDF<-function(ncin) {
  if(("X" %in% names(ncin$dim)) && ("Y" %in% names(ncin$dim))) {
    dimX <- ncdf4::ncvar_get(ncin, "X")
    dimY <- ncdf4::ncvar_get(ncin, "Y")
    cellcentre.offset = c(X = min(dimX), Y = min(dimY))
  }
  else if(("x" %in% names(ncin$dim)) && ("y" %in% names(ncin$dim))) {
    dimX <- ncdf4::ncvar_get(ncin, "x")
    dimY <- ncdf4::ncvar_get(ncin, "y")
    cellcentre.offset = c(x = min(dimX), y = min(dimY))
  }
  else if(("lon" %in% names(ncin$dim)) && ("lat" %in% names(ncin$dim))) {
    dimX <- ncdf4::ncvar_get(ncin, "lon")
    dimY <- ncdf4::ncvar_get(ncin, "lat")
    cellcentre.offset = c(lon = min(dimX), lat = min(dimY))
  }
  else if(("rlon" %in% names(ncin$dim)) && ("rlat" %in% names(ncin$dim))) {
    dimX <- ncdf4::ncvar_get(ncin, "rlon")
    dimY <- ncdf4::ncvar_get(ncin, "rlat")
    cellcentre.offset = c(rlon = min(dimX), rlat = min(dimY))
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
  patt <- ncdf4::ncatt_get(ncin,0, "proj4string")
  if(patt$hasatt) {
    proj4string = patt$value
    if(proj4string!="NA") crs = CRS(proj4string)
  }
  if(("lon" %in% names(ncin$dim)) && ("lat" %in% names(ncin$dim))) {
    crs = CRS(SRS_string = "EPSG:4326")
  }
  if(("rlon" %in% names(ncin$dim)) && ("rlat" %in% names(ncin$dim))) {
    crs = CRS(SRS_string = "EPSG:4326")
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
  return(varmapping)
}
.defaultVars<-function() {
  var = c("MeanTemperature",
          "MinTemperature",
          "MaxTemperature",
          "Precipitation",
          "MeanRelativeHumidity",
          "MinRelativeHumidity",
          "MaxRelativeHumidity",
          "Radiation",
          "WindSpeed",
          "WindDirection",
          "PET")
  return(var)
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
  if("Radiation" %in% names(varmapping)) {
    if(varmapping[["Radiation"]] %in% names(ncin$var)) {
      prunits = ncin$var[[varmapping[["Radiation"]]]]$units
      if(prunits == "W m-2") {
        df$Radiation =  df$Radiation*3600*24/1000000 #From W/m2 to MJ/m2
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
.readmeteorologygridNetCDF<-function(ncin, dates = NULL, pixels = FALSE,
                                 bbox = NULL, offset = 0,
                                 varmapping = NULL, verbose = FALSE) {
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
  sel <- rep(TRUE, nx*ny)
  if(!is.null(bbox)) {
    # print(grid)
    cc = coordinates(grid)
    cn = colnames(cc)
    vec_x<-(cc[,cn[1]]+offset >=bbox[cn[1],1]) & (cc[,cn[1]] - offset <=bbox[cn[1],2])
    vec_y<-(cc[,cn[2]]+offset >=bbox[cn[2],1]) & (cc[,cn[2]] -offset <=bbox[cn[2],2])
    sel=vec_y & vec_x
    grid <- points2grid(SpatialPoints(cc[sel,]))
  }
  if(is.null(varmapping)) varmapping = .defaultMapping()
  if(verbose) pb = txtProgressBar(1, length(dates), style=3)
  for(j in 1:length(dates)) {
    day = which(dates_file==dates[j])
    if(verbose) setTxtProgressBar(pb,j)
    # cat(paste0("Reading data for day '", as.character(dates[j]), "' at time position [",day, "].\n"))
    df = data.frame(row.names = 1:sum(sel))
    for(var in names(varmapping)) {
      if(varmapping[[var]] %in% names(ncin$var)) {
        df[[var]] = .readgridvardataday(ncin, nx, ny, varmapping[[var]], day, sel)
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


# Functions copied from package ncdf4.helpers to avoid dependencies
.nc_get_dimnames <- function(f, v) {
  if(missing(v)) {
    d <- unlist(lapply(f$dim, function(x) { return(x$name) }))
    names(d) <- NULL
    return(d)
  } else
    return(unlist(lapply(f$var[[v]]$dim, function(x) { return(x$name) })))
}
.nc_get_climatologybounds_varlist <- function(f) {
  dim.list <- names(f$dim)
  is.climatology<- sapply(dim.list, function(x) {
    if(f$dim[[x]]$create_dimvar && f$dim[[x]]$unlim) {
      a <- ncdf4::ncatt_get(f, x, "climatology")
      if(a$hasatt)
        return(a$value)
    }
    return(NA)
  })
  return(unique(is.climatology[!is.na(is.climatology)]))
}
.nc_get_dimbounds_varlist <- function(f, v=NULL) {
  dimension.vars <- names(f$dim)
  dim.names <- if(is.null(v)) names(f$dim) else .nc_get_dimnames(f, v)
  return(unlist(sapply(names(f$dim), function(x) {
    if(f$dim[[x]]$create_dimvar) {
      a <- ncdf4::ncatt_get(f, x, "bounds");
      if(a$hasatt)
        return(a$value);
    }

    ## Heuristic detection for broken files
    bnds.vars <- c(paste(x, "bnds", sep="_"), paste("bounds", x, sep="_"))
    bnds.present <- bnds.vars %in% names(f$var)
    if(any(bnds.present))
      return(bnds.vars[bnds.present])

    return(NULL);
  } )))
}
.nc_get_varlist <- function(f, min.dims=1) {
  var.list <- names(f$var)
  enough.dims <- sapply(var.list, function(v) { length(f$var[[v]]$dim) >= min.dims } )
  bounds <- .nc_get_dimbounds_varlist(f)
  climatology.bounds <- .nc_get_climatologybounds_varlist(f)
  has.axis <- unlist(lapply(var.list, function(x) { a <- ncdf4::ncatt_get(f, x, "axis"); if(a$hasatt & nchar(a$value) == 1) return(x); return(NULL); } ))

  ## When things get really broken, we'll need this...
  bnds.heuristic <- !grepl("_bnds", var.list)

  var.mask <- bnds.heuristic & enough.dims & (!(var.list %in% c(bounds, has.axis, climatology.bounds, "lat", "lon") | unlist(lapply(f$var, function(x) { return(x$prec == "char" | x$ndims == 0) }))))

  return(var.list[var.mask])
}
.readmeteorologygridpointsNetCDF<-function(ncin, dates = NULL,
                                           bbox = NULL, offset = 0,
                                           varmapping = NULL, verbose = FALSE) {
  dates_file <- .readdatesNetCDF(ncin)
  dates_file <- as.character(dates_file)
  if(!is.null(dates)) {
    dates <- as.character(dates)
    if(sum(dates %in% dates_file)<length(dates)) stop("Time axis of nc file does not include all supplied dates")
  } else {
    dates = dates_file
  }
  if(is.null(varmapping)) varmapping = .defaultMapping()

  crs <- .readCRSNetCDF(ncin)
  grid <- .readgridtopologyNetCDF(ncin) # Can be a rotated topology
  rotated = (names(grid@cellcentre.offset)[1] == "rlon")
  if(!rotated) {
    nx <- grid@cells.dim[1]
    ny <- grid@cells.dim[2]

    sel <- rep(TRUE, nx*ny)
    if(!is.null(bbox)) {
      # print(grid)
      cc = coordinates(grid)
      cn = colnames(cc)
      vec_x<-(cc[,cn[1]]+offset >=bbox[cn[1],1]) & (cc[,cn[1]] - offset <=bbox[cn[1],2])
      vec_y<-(cc[,cn[2]]+offset >=bbox[cn[2],1]) & (cc[,cn[2]] -offset <=bbox[cn[2],2])
      sel=vec_y & vec_x
    }

  } else {
    lat <- ncdf4::ncvar_get(ncin, "lat")
    lon <- ncdf4::ncvar_get(ncin, "lon")
    nx = nrow(lat)
    ny = ncol(lat)
    sel = matrix(FALSE, nrow=nx, ncol=ny)
    if(!is.null(bbox)) {
      veclat<-(lat+offset >=bbox[2,1]) & (lat -offset <=bbox[2,2])
      veclon<-(lon+offset >=bbox[1,1]) & (lon - offset <=bbox[1,2])
      sel=veclat & veclon
    }
  }

  npts = sum(sel)
  nt = length(dates_file)
  data = vector("list", npts)
  if(verbose) {
    cat(paste0("Defining ", npts," points:\n"))
    pb = txtProgressBar(0, npts, style=3)
  }

  if(rotated) {
    cc = matrix(nrow=0, ncol=2)
    xy = matrix(nrow=0, ncol=2)
    colnames(cc)<-c("lon", "lat")
    cnt = 1
    for(xi in 1:nrow(sel)) {
      for(yi in 1:ncol(sel)) {
        if(sel[xi,yi]) {
          if(verbose) setTxtProgressBar(pb,cnt)
          cc_i = c(lon[xi, yi], lat[xi, yi])
          xy = rbind(xy, c(xi,yi))
          cc = rbind(cc, cc_i)
          df = data.frame(row.names = as.character(dates))
          df[,"DOY"] = as.POSIXlt(as.Date(dates))$yday+1
          data[[cnt]] = df
          cnt = cnt+1
        }
      }
    }
    ## Read variables
    for(var in names(varmapping)) {
      if(varmapping[[var]] %in% names(ncin$var)) {
        if(verbose) {
          cat(paste0("\nMapping variable ", varmapping[[var]], " to ", var,"...\n"))
          pb = txtProgressBar(0, npts, style=3)
        }
        timefirst = (ncin$var[[varmapping[[var]]]]$dim[[1]]$name=="time")
        values = ncdf4::ncvar_get(ncin, varmapping[[var]]) # read whole variable
        for(i in 1:nrow(xy)) {
          if(verbose) setTxtProgressBar(pb,i)
          df = data[[i]]
          if(timefirst) {
            df[[var]] = values[,xy[i,1],xy[i,2]]
          } else  {
            df[[var]] = values[xy[i,1],xy[i,2],]
          }
          data[[i]] = df
        }
      }
    }
    for(i in 1:nrow(xy)) {
      df = data[[i]]
      if(ncol(df)>0) {
        df = .unitConversion(df, ncin, varmapping)
        for(j in 1:ncol(df)) df[is.na(df[j]),j] =NA
        data[[i]] = df
      }
    }
  }
  rownames(cc)<-1:nrow(cc)
  spm = SpatialPointsMeteorology(SpatialPoints(cc, proj4string = crs),
                                data = data, dates=as.Date(dates))
  return(spm)
}

.readpointcoordinatesNetCDF<-function(ncin, crs) {
  if(.isLongLat(crs)) {
    x <- ncdf4::ncvar_get(ncin, "lon")
    y <- ncdf4::ncvar_get(ncin, "lat")
    cc = cbind(x,y)
    colnames(cc)<-c("lon", "lat")
  } else {
    x <- ncdf4::ncvar_get(ncin, "x")
    y <- ncdf4::ncvar_get(ncin, "y")
    cc = cbind(x,y)
  }
  ids <- ncdf4::ncvar_get(ncin, "station_name")
  rownames(cc)<-ids
  return(cc)
}
.readmeteorologypointNetCDF<-function(ncin, i, dates_file, varmapping) {
  nt = length(dates_file)
  df = data.frame(row.names = as.character(dates_file))
  df[,"DOY"] = as.POSIXlt(as.Date(dates_file))$yday+1
  for(var in names(varmapping)) {
    if(varmapping[[var]] %in% names(ncin$var)) {
      df[[var]] = ncdf4::ncvar_get(ncin, varmapping[[var]], start=c(i,1), count=c(1,nt))
    }
  }
  if(ncol(df)>0) {
    df = .unitConversion(df, ncin, varmapping)
    for(j in 1:ncol(df)) df[is.na(df[,j]),j] =NA
  }
  return(df)
}
.readmeteorologypointsNetCDF<-function(ncin, dates = NULL, stations = NULL,
                                       varmapping = NULL, verbose = FALSE) {
  dates_file <- .readdatesNetCDF(ncin)
  dates_file <- as.character(dates_file)
  if(!is.null(dates)) {
    dates <- as.character(dates)
    if(sum(dates %in% dates_file)<length(dates)) stop("Time axis of nc file does not include all supplied dates")
  } else {
    dates = dates_file
  }
  if(is.null(varmapping)) varmapping = .defaultMapping()

  crs <- .readCRSNetCDF(ncin)
  cc <- .readpointcoordinatesNetCDF(ncin, crs)
  indices = 1:nrow(cc)
  if(!is.null(stations)) {
    ids = rownames(cc)
    if(inherits(stations, "character")) {
      if(sum(!(stations %in% ids))>0) {
        ncdf4::nc_close(ncin)
        stop("Some station names are not recognized.")
      }
      indices = rep(NA, length(stations))
      for(i in 1:length(stations)) {
        indices[i] = which(ids==stations[i])[1]
      }
    } else if(inherits(stations, "integer") || inherits(stations, "numeric")) {
      if(sum(!(stations %in% (1:length(ids))))>0) {
        stop("Some station indices are outside the valid range.")
      }
      indices = stations
    }
  }
  cc =cc[indices, , drop=FALSE]
  npts = nrow(cc)
  nt = length(dates_file)
  data = vector("list", npts)
  names(data)<-rownames(cc)

  if(verbose) pb = txtProgressBar(0, npts, style=3)
  for(i in 1:npts) {
    if(verbose) setTxtProgressBar(pb,i)
    df = .readmeteorologypointNetCDF(ncin, indices[i], dates_file, varmapping)
    df = df[as.character(dates),]
    data[[i]] = df
  }
  spm = SpatialPointsMeteorology(SpatialPoints(cc, proj4string = crs),
                                 data = data, dates=as.Date(dates))
  return(spm)
}
