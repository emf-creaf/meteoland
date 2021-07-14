

extractNetCDF<-function(ncdf_files, bbox = NULL, offset = 0, cells = NULL, export = TRUE, 
                        exportDir = getwd(), exportFormat = "meteoland/txt", mpfilename = "MP.txt") {

  nfiles = length(ncdf_files)
  cat(paste("Number of NetCDFs: ", nfiles,"\n", sep=""))

  #Read spatial info from first file
  ncname<-ncdf_files[1]
  ncin <- nc_open(ncname)
  lat <- ncvar_get(ncin, "lat")
  lon <- ncvar_get(ncin, "lon")
  varlist <- .nc_get_varlist(ncin)
  nx = nrow(lat)
  ny = ncol(lat)
  cat(paste("NetCDF grid: nx",nx, "ny",ny,"ncells", nx*ny,"\n"))
  
  sel = matrix(FALSE, nrow=nx, ncol=ny)
  vertices = FALSE
  if(!is.null(bbox)) {
    if((ncol(bbox)!= 2)||(nrow(bbox)!= 2)) stop("Wrong dimensions of bbox")
    if(is.null(dimnames(bbox))){
      colnames(bbox)<-c("min","max")
      rownames(bbox)<-c("lon","lat")
    }
    vertices = ("lat_vertices" %in% varlist) & ("lon_vertices" %in% varlist)
    if(vertices) {
      lat_ver <- ncvar_get(ncin, "lat_vertices")
      lon_ver <- ncvar_get(ncin, "lon_vertices")
      #Select target cells when at least one vertex falls in the boundary box
      for(v in 1:4) {
        sel1 = (lon_ver[v,,] +offset >= bbox[1,1]) &
          (lon_ver[v,,] - offset <= bbox[1,2]) &
          (lat_ver[v,,] +offset >= bbox[2,1]) &
          (lat_ver[v,,] -offset <= bbox[2,2])
        sel = sel | sel1
      }
      minlon = pmin(lon_ver[1,,], lon_ver[2,,], lon_ver[3,,], lon_ver[4,,])
      maxlon = pmax(lon_ver[1,,], lon_ver[2,,], lon_ver[3,,], lon_ver[4,,])
      minlat = pmin(lat_ver[1,,], lat_ver[2,,], lat_ver[3,,], lat_ver[4,,])
      maxlat = pmax(lat_ver[1,,], lat_ver[2,,], lat_ver[3,,], lat_ver[4,,])
      #Select one cell if boundary box is within it
      selbox = (bbox[1,1]>=minlon) &
        (bbox[2,1]>=minlat) &
        (bbox[1,2]<=maxlon) &
        (bbox[2,2]<=maxlat)
      sel = sel | selbox
    } else {
      veclat<-(lat+offset >=bbox[2,1]) & (lat -offset <=bbox[2,2])
      veclon<-(lon+offset >=bbox[1,1]) & (lon - offset <=bbox[1,2])
      sel=veclat & veclon
    }
  } else if(!is.null(cells)) {
    if(!is.matrix(cells)) stop("'cells' has to be a matrix")
    if(ncol(cells)!=2) stop("'cells' has to be a matrix of two columns")
    for(i in 1:nrow(cells)) sel[cells[i,1],cells[i,2]] = TRUE
  } else {
    cat("No user cell selection. All cells will be extracted.")
  }
  nc_close(ncin)
  
  ncells = sum(sel)
  cat(paste("Cells to extract: ", ncells,"\n", sep=""))

  if(ncells==0) stop("No cells to extract. Stopping.")
  #Extract dates from all files
  dates = NULL
  for(filei in 1:nfiles) {
    ncin <- nc_open(ncdf_files[filei])
    t <- ncvar_get(ncin, "time")
    nt = length(t)
    tunits <- ncatt_get(ncin, "time", "units")
    nc_close(ncin)
    s = strsplit(tunits$value, " ")[[1]]
    s = s[3]
    t <- floor(t)
    if(length(unique(t))!=length(t)) stop("Duplicated days!")
    maxday <-max(t)
    minday <-min(t)
    refDate = as.Date(s)
    datesfile <- as.character(seq.Date(refDate, length.out=maxday, by="day")[t])
    dates = sort(unique(c(dates, datesfile)))
  }
  ndates <-length(dates)
  cat(paste("Period to extract: ", dates[1]," to ", dates[length(dates)]," (", ndates," days)\n", sep=""))
  cat("\n\n")

  # Define vector of data frames
  dfvec = vector("list",ncells)
  dfout = data.frame(xi = rep(NA,ncells), yi = rep(NA,ncells), dir = rep("", ncells),
                     filename=rep("", ncells), format = rep(exportFormat, ncells),
                     v1_lon = rep(NA,ncells), v1_lat = rep(NA,ncells),
                     v2_lon = rep(NA,ncells), v2_lat = rep(NA,ncells),
                     v3_lon = rep(NA,ncells), v3_lat = rep(NA,ncells),
                     v4_lon = rep(NA,ncells), v4_lat = rep(NA,ncells))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  rownames(dfout) = 1:ncells
  cc = cbind(rep(NA, ncells), rep(NA, ncells))
  rownames(cc)<-1:ncells
  colnames(cc)<-c("lon","lat")
  cnt = 1
  for(xi in 1:nrow(sel)) {
    for(yi in 1:ncol(sel)) {
      if(sel[xi,yi]) {
        cc[cnt,] = c(lon[xi,yi],lat[xi,yi])
        cnt = cnt+1
      }
    }
  }
  points = SpatialPoints(cc, proj4string = CRS(SRS_string = "EPSG:4326"))
  spdf = SpatialPointsDataFrame(points, dfout)

  cnt = 1
  for(xi in 1:nrow(sel)) {
    for(yi in 1:ncol(sel)) {
      if(sel[xi,yi]) {
        spdf@data$xi[cnt] = xi
        spdf@data$yi[cnt] = yi
        cat(paste("Extracting data for cell (",cnt," of ",ncells,"): [",xi,", ",yi,"]\n",sep=""))

        df = data.frame(matrix(NA, nrow = ndates, ncol = 9), row.names = as.character(dates))
        names(df) = c("DOY","MeanTemperature","MinTemperature",
                      "MaxTemperature","Precipitation","SpecificHumidity", "MeanRelativeHumidity",
                      "Radiation","WindSpeed")
        df[,"DOY"] = as.POSIXlt(as.Date(dates))$yday+1
        # Process all files
        pb = txtProgressBar(0, nfiles, 0, style = 3)
        for(filei in 1:nfiles) {
          setTxtProgressBar(pb, filei-1)
          ncin <- nc_open(ncdf_files[filei])
          #get dates
          t <- ncvar_get(ncin, "time")
          nt = length(t)
          tunits <- ncatt_get(ncin, "time", "units")
          s = strsplit(tunits$value, " ")[[1]]
          s = s[3]
          t <- floor(t)
          if(length(unique(t))!=length(t)) stop("Duplicated days!")
          maxday <-max(t)
          minday <-min(t)
          refDate = as.Date(s)
          datesfile <- as.character(seq.Date(refDate, length.out=maxday, by="day")[t])

          varlist = .nc_get_varlist(ncin)
          for(var in varlist) {
            if(var=="huss") {
              vec = ncvar_get(ncin,varid = var, start = c(xi, yi, 1), count=c(1,1,length(datesfile)))
              df[datesfile,"SpecificHumidity"] = vec #kg/kg
            } else if(var=="tas")  {
              vec = ncvar_get(ncin,varid = var, start = c(xi, yi, 1), count=c(1,1,length(datesfile)))
              df[datesfile,"MeanTemperature"] = vec - 273.15 #From degrees K to degrees C
            } else if(var=="tasmin")  {
              vec = ncvar_get(ncin,varid = var, start = c(xi, yi, 1), count=c(1,1,length(datesfile)))
              df[datesfile,"MinTemperature"] = vec - 273.15 #From degrees K to degrees C
            } else if(var=="tasmax")  {
              vec = ncvar_get(ncin,varid = var, start = c(xi, yi, 1), count=c(1,1,length(datesfile)))
              df[datesfile,"MaxTemperature"] = vec - 273.15 #From degrees K to degrees C
            } else if(var=="pr")  {
              vec = ncvar_get(ncin,varid = var, start = c(xi, yi, 1), count=c(1,1,length(datesfile)))
              df[datesfile,"Precipitation"] = vec*3600*24 #From kg/m2/s to L/m2/day
            } else if(var=="rsds")  {
              vec = ncvar_get(ncin,varid = var, start = c(xi, yi, 1), count=c(1,1,length(datesfile)))
              df[datesfile,"Radiation"] = vec*3600*24/1000000 #From W/m2 to MJ/m2
            } else if(var=="sfcWind")  {
              vec = ncvar_get(ncin,varid = var, start = c(xi, yi, 1), count=c(1,1,length(datesfile)))
              df[datesfile,"WindSpeed"] = vec #in m/s
            }
          }
          nc_close(ncin)
          cat("\n")
        }
        close(pb)
        if(sum((!is.na(df$MeanTemperature)) & (!is.na(df$SpecificHumidity)))>0) {
          df$MeanRelativeHumidity = humidity_specific2relative(Tc=df$MeanTemperature ,HS=df$SpecificHumidity, allowSaturated = TRUE)
        }
        if(!export) {
          dfvec[[cnt]] = df
        } else {
          if(exportFormat %in% c("meteoland/txt","castanea/txt")) formatType = "txt"
          else if (exportFormat %in% c("meteoland/rds","castanea/rds")) formatType = "rds"
          
          filename = paste0("P_",xi,"_",yi,".",formatType)
          if(exportDir!="") dir = paste(getwd(),exportDir,sep="/")
          else dir = getwd()
          spdf@data$dir[cnt] = dir
          spdf@data$filename[cnt] = filename
          if(vertices) {
            spdf@data$v1_lat[cnt] = lat_ver[1,xi,yi]
            spdf@data$v2_lat[cnt] = lat_ver[2,xi,yi]
            spdf@data$v3_lat[cnt] = lat_ver[3,xi,yi]
            spdf@data$v4_lat[cnt] = lat_ver[4,xi,yi]
            spdf@data$v1_lon[cnt] = lon_ver[1,xi,yi]
            spdf@data$v2_lon[cnt] = lon_ver[2,xi,yi]
            spdf@data$v3_lon[cnt] = lon_ver[3,xi,yi]
            spdf@data$v4_lon[cnt] = lon_ver[4,xi,yi]
          }
          if(exportDir!="") f = paste(exportDir,filename, sep="/")
          else f = filename
          writemeteorologypoint(df,f, exportFormat)
          cat(paste("  File output: ",f, "\n", sep=""))
          if(exportDir!="") f = paste(exportDir,mpfilename, sep="/")
          else f = mpfilename
          write.table(as.data.frame(spdf),
                      file= f ,sep="\t", quote=FALSE)
        }
        cnt = cnt+1
        cat(paste("\n"))
      }
    }
  }
  cat(paste("Done.","\n"))
  if(is.null(exportFormat)) return(SpatialPointsMeteorology(points = points,data = dfvec, dates = dates))
  return(spdf)
}

