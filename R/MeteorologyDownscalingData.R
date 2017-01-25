MeteorologyDownscalingData<-function(points, historicdata, futuredata, dates,
                                     params = defaultDownscalingParams()) {
  if(!inherits(points, "SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'")
  if(!inherits(historicdata,"data.frame") && !inherits(historicdata,"list")) stop("'historicdata' has to be of class 'data.frame' or 'list'.")
  if(!inherits(futuredata,"data.frame") && !inherits(futuredata,"list")) stop("'futuredata' has to be of class 'data.frame' or 'list'.")
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  ndates = length(dates)
  spm = new("MeteorologyDownscalingData",
            coords = points@coords,
            bbox = points@bbox,
            proj4string = points@proj4string,
            historicdata = historicdata,
            futuredata = futuredata,
            dates = dates,
            params = params)
  return(spm)
}
