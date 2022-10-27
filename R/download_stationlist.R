
### AEMET
downloadAEMETstationlist <- function(api){
  # deprecation warning
  lifecycle::deprecate_warn(
    when = "1.1.0", what = "downloadAEMETstationlist()", with = "meteospain::get_stations_info_from()",
    details = "This function is deprecated in favour of the meteospain package"
  )
  opt = meteospain::aemet_options(api_key = api)
  data_sf = meteospain::get_stations_info_from("aemet", opt)
  data_sp = as(data_sf, "Spatial")
  data_sp@data = data_sp@data[,c("altitude", "station_id","station_name")]
  names(data_sp@data) = c("elevation", "ID", "name")
  row.names(data_sp@data) <- data_sp@data$ID
  colnames(data_sp@coords)<-c("long", "lat")
  rownames(data_sp@coords) <- data_sp@data$ID
  return(data_sp)
}


### SMC
downloadSMCstationlist <- function(api, date = NULL){
  # deprecation warning
  lifecycle::deprecate_warn(
    when = "1.1.0", what = "downloadSMCstationlist()", with = "meteospain::get_stations_info_from()",
    details = "This function is deprecated in favour of the meteospain package"
  )

  if(is.null(date)) date = Sys.Date()
  opt = meteospain::meteocat_options(api_key = api, start_date = as.Date(date))
  data_sf = meteospain::get_stations_info_from("meteocat", opt)
  data_sp = as(data_sf, "Spatial")
  data_sp@data = data_sp@data[,c("altitude", "station_id","station_province","station_name")]
  names(data_sp@data) = c("elevation", "ID","province", "name")
  row.names(data_sp@data) <- data_sp@data$ID
  colnames(data_sp@coords)<-c("long", "lat")
  rownames(data_sp@coords) <- data_sp@data$ID
  return(data_sp)
}


### MeteoGalicia
downloadMGstationlist <- function() {
  # deprecation warning
  lifecycle::deprecate_warn(
    when = "1.1.0", what = "downloadMGstationlist()", with = "meteospain::get_stations_info_from()",
    details = "This function is deprecated in favour of the meteospain package"
  )
  opt = meteospain::meteogalicia_options()
  data_sf = meteospain::get_stations_info_from("meteogalicia", opt)
  data_sp = as(data_sf, "Spatial")
  data_sp@data = data_sp@data[,c("altitude", "station_id","station_province","station_name")]
  names(data_sp@data) = c("elevation", "ID","province", "name")
  row.names(data_sp@data) <- data_sp@data$ID
  colnames(data_sp@coords)<-c("long", "lat")
  rownames(data_sp@coords) <- data_sp@data$ID
  return(data_sp)
}

#### Meteoclimatic
downloadMETEOCLIMATICstationlist <- function(station_id = 'ESCAT') {
  # deprecation warning
  lifecycle::deprecate_warn(
    when = "1.1.0", what = "downloadMETEOCLIMATICstationlist()", with = "meteospain::get_stations_info_from()",
    details = "This function is deprecated in favour of the meteospain package"
  )

  opt <- meteospain::meteoclimatic_options(stations = station_id)
  data_sf <- meteospain::get_stations_info_from("meteoclimatic", opt)
  data_sp <- as(data_sf, "Spatial")
  data_sp@data <- data_sp@data[,c("station_id","station_name")]
  names(data_sp@data) <- c("ID", "name")
  row.names(data_sp@data) <- data_sp@data$ID
  colnames(data_sp@coords)<-c("long", "lat")
  rownames(data_sp@coords) <- data_sp@data$ID
  return(data_sp)
}
