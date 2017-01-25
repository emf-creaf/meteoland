readWindNinjaWindFields<-function(filebase, resolution = "100m",
                                  directionClasses = c(0,45,90,135,180,225,270,315),
                                  speedClasses = c(5,15,25),
                                  proj4string = CRS(as.character(NA))) {
  nraster = (8*length(speedClasses))
  indexTable = matrix(1:nraster,
                       nrow=8, ncol=length(speedClasses),
                       dimnames=list(directionClasses, 1:length(speedClasses)))
  firstfilename = paste(filebase,directionClasses[1],"_",speedClasses[1],"_",resolution,"_vel.asc",sep="")
  a = read.asciigrid(firstfilename)
  gt = getGridTopology(a)
  ncells = nrow(a@data)
  dfws = data.frame(matrix(0,nrow=ncells, ncol=nraster))
  dfwd = data.frame(matrix(0,nrow=ncells, ncol=nraster))
  names(dfws)<-1:nraster
  names(dfwd)<-1:nraster
  cnt = 0
  for(j in 1:length(speedClasses)) {
    sc = speedClasses[j]
    for(i in 1:length(directionClasses)) {
      wd = directionClasses[i]
      cnt = cnt+1
      wsfilename = paste(filebase,wd,"_",sc,"_",resolution,"_vel.asc",sep="")
      wdfilename = paste(filebase,wd,"_",sc,"_",resolution,"_ang.asc",sep="")
      dfws[,cnt] = read.asciigrid(wsfilename)@data[,1]
      dfwd[,cnt] = read.asciigrid(wdfilename)@data[,1]
    }
  }
  windSpeed = SpatialGridDataFrame(gt, dfws, proj4string)
  windDirection = SpatialGridDataFrame(gt, dfwd, proj4string)
  l = list(directionClasses = directionClasses,
           speedClasses = speedClasses,
           indexTable = indexTable,
           windSpeed = windSpeed,
           windDirection = windDirection)
  class(l)<-c("WindFields","list")
  return(l)
}
