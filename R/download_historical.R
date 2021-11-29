### AEMET
downloadAEMEThistorical <- function(api, dates, station_id = NULL, export = FALSE, exportDir = getwd(),
                                exportFormat = "meteoland/txt",
                                metadataFile = "MP.txt", verbose=TRUE){
  opt <- meteospain::aemet_options("daily", 
                      start_date = dates[1], end_date=dates[length(dates)],
                      stations = station_id,
                      api_key = api)
  sf_data <- meteospain::get_meteo_from("aemet", opt)
  spm <- reshapemeteospain(sf_data, verbose = verbose)
  if(export){
    writemeteorologypointfiles(spm, dir =  exportDir, 
                               format = exportFormat,
                               metadataFile = metadataFile)
  } else {
    return(spm)
  }
}


#### SMC
downloadSMChistorical <- function(api, date, station_id=NULL,
                                  export = FALSE, exportDir = getwd(), exportFormat = "meteoland/txt", metadataFile = "MP.txt",
                                  verbose=TRUE){
  opt <- meteospain::meteocat_options("daily", 
                                      start_date = date[1],
                                      stations = station_id,
                                      api_key = api)
  sf_data <- meteospain::get_meteo_from("meteocat", opt)
  spm <- reshapemeteospain(sf_data, verbose = verbose)
  if(export){
    writemeteorologypointfiles(spm, dir =  exportDir, 
                               format = exportFormat,
                               metadataFile = metadataFile)
  } else {
    return(spm)
  }
}

#### MeteoGalicia
downloadMGhistorical <- function(date_from, date_to, station_id = NULL, verbose=TRUE) {
  opt <- meteospain::meteogalicia_options("daily", 
                       start_date = as.Date(date_from), 
                       end_date=as.Date(date_to),
                       stations = station_id)
  sf_data <- meteospain::get_meteo_from("meteogalicia", opt)
  spm <- reshapemeteospain(sf_data, verbose = verbose)
  return(spm)
}
