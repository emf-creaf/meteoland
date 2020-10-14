MeteorologyUncorrectedData<-function(points, reference_data, projection_data, dates,
                                     params = defaultCorrectionParams()) {
  if(!inherits(points, "SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'")
  if(!inherits(reference_data,"data.frame") && !inherits(reference_data,"list") && !inherits(reference_data,"character")) 
    stop("'reference_data' has to be of class 'data.frame, 'character' or 'list'.")
  if(!inherits(projection_data,"data.frame") && !inherits(projection_data,"list") && !inherits(projection_data,"character")) 
    stop("'projection_data' has to be of class 'data.frame', 'character' or 'list'.")
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  ndates = length(dates)
  spm = new("MeteorologyUncorrectedData",
            coords = points@coords,
            bbox = points@bbox,
            proj4string = points@proj4string,
            reference_data = reference_data,
            projection_data = projection_data,
            dates = dates,
            params = params)
  return(spm)
}
