
### AEMET
downloadAEMEThistoricalstationlist <- function(api){
  # nonUTF8 = "\u00D1\u00C0\u00C1\u00C8\u00C9\u00D2\u00D3\u00CC\u00CD\u00DC\u00CF"
  # 
  # value.func <- function(x){
  #   value <- regmatches(x,gregexpr('(?<=\\:\\s)[[:print:]]*(?=\\n)', x, perl = T))[[1]]
  #   value <- gsub('\\",', "", value)
  #   value <- gsub('\\"', "", value)
  #   value <- gsub(',', ".", value)
  # }
  # 
  # # set options
  # h = new_handle()
  # handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  # handle_setopt(h, ssl_verifypeer=FALSE)
  # handle_setopt(h, customrequest="GET")
  # 
  # url <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/todasestaciones"
  # # get data url
  # urldata_raw <- curl_fetch_memory(url, h)$content
  # urldata_string <- value.func(rawToChar(urldata_raw))
  # 
  # if(urldata_string[2]=="401"){
  #   stop("Invalid API key. (API keys are valid for 3 months.)")
  # }
  # 
  # urldata <- urldata_string[3]
  # 
  # # get data and format
  # data_raw <- curl_fetch_memory(urldata, h)$content
  # data_string <- rawToChar(data_raw)
  # # Add local encoding information to data_string
  # Encoding(data_string) <-"latin1"
  # # enclocal <- l10n_info()
  # # if(enclocal[[2]]) Encoding(data_string) <-"UTF-8"
  # # else if(enclocal[[3]]) Encoding(data_string) <-"latin1"
  # # print(data_string)
  # cname <- regmatches(data_string,gregexpr('(?<=\\\n\\s{2}\\\")[[:print:]]*(?=\\\"\\s\\:)', data_string, perl = T))[[1]]
  # value <- regmatches(data_string,gregexpr(paste0('(?<=\\:\\s\\\")([[:print:]]|[',nonUTF8,'])*(?=\\\")'), data_string, perl = T))[[1]]
  # # print(head(value))
  # unique_cname <- unique(cname)
  # data_df <- as.data.frame(sapply(unique_cname,FUN = function(x){value[cname == x]}), stringsAsFactors = F)
  apidest = "/api/valores/climatologicos/inventarioestaciones/todasestaciones"
  data_df = .get_data_aemet(apidest, api)
  data_df$W <- grepl('W',data_df$longitud)
  data_df$latitud <- unlist(regmatches(data_df$latitud,gregexpr('\\d+',data_df$latitud)))
  data_df$longitud <- unlist(regmatches(data_df$longitud,gregexpr('\\d+',data_df$longitud)))
  data_df$altitud <- as.numeric(data_df$altitud)
  colnames(data_df) <- c("lat", "province", "elevation", "ID", "name", "zip", "long", "W")
  
  # calculate coordinates
  longString = data_df[,"long"]
  latString = data_df[,"lat"]
  deg = as.numeric(substr(longString, 0, nchar(longString)-4))
  min = as.numeric(substr(longString, nchar(longString)-3, nchar(longString)-2))
  sec = as.numeric(substr(longString, nchar(longString)-1, nchar(longString)))
  data_df$long = deg+min/60+sec/3600
  data_df$long[data_df$W] <- -data_df$long[data_df$W]
  deg = as.numeric(substr(latString, 0, nchar(latString)-4))
  min = as.numeric(substr(latString, nchar(latString)-3, nchar(latString)-2))
  sec = as.numeric(substr(latString, nchar(latString)-1, nchar(latString)))
  data_df$lat = deg+min/60+sec/3600
  
  # create sp object
  data_sp <- SpatialPointsDataFrame(data = data_df[,c("province", "elevation", "ID", "name", "zip")], 
                                    coords = data_df[,c("long", "lat")], proj4string = CRS("+proj=longlat"))
  row.names(data_sp) <- data_sp@data$ID
  return(data_sp)
}


### SMC
downloadSMCstationlist <- function(api, date = NULL){
  
  apidest = "/estacions/metadades"
  if(!is.null(date)){apidest=paste0(apidest, "?estat=ope&data=", date, "Z")}
  data = .get_data_smc(apidest, api)
  
  # Format data
  data_df <- data.frame(ID = data$codi, name = data$nom, municipality = data$municipi$nom, zip = data$municipi$codi,
                        county = data$comarca$nom, province = data$provincia$nom, 
                        long = data$coordenades$long, lat = data$coordenades$latitud, elevation = data$altitud)
  
  # create sp object
  data_sp <- SpatialPointsDataFrame(data = data_df[,c("ID", "name", "municipality", "zip", "county", "province", "elevation")], 
                                    coords = data_df[,c("long", "lat")], proj4string = CRS("+proj=longlat"))
  row.names(data_sp) <- data_sp@data$ID
  return(data_sp)
}


### MeteoGalicia
downloadMGstationlist <- function() {
  url_base <- "http://servizos.meteogalicia.es/rss/observacion/listaEstacionsMeteo.action"
  data_df <- jsonlite::fromJSON(txt=url_base)[[1]]
  cc = cbind(as.numeric(data_df$utmx), as.numeric(data_df$utmy))
  colnames(cc) <- c("utmx", "utmy")
  data_sp <- SpatialPointsDataFrame(data = data.frame(ID = data_df$idEstacion,
                                                      name = data_df$estacion,
                                                      municipality = data_df$concello,
                                                      province = data_df$provincia,
                                                      lon = data_df$lon,
                                                      lat = data_df$lat,
                                                      elevation = data_df$altitude),
                                    coords = cc, 
                                    proj4string = CRS("+init=epsg:25829"))
  row.names(data_sp) <- data_sp@data$ID
  return(data_sp)
}

#### Meteoclimatic
downloadMETEOCLIMATICstationlist <- function(station_id = 'ESCAT', spatial = TRUE) {
  # Stations coords extraction ----------------------------------------------------------------------------
  # We need to access the rss feed of meteoclimatic that contains all active stations. The meteoclimatic
  # API allows for selecting provinces or autonomous communities, with the corresponding code. We can use
  # this to obtain all catalonian stations with the "ESCAT" code. In case of station_id > 1 we need to iterate
  # and store the results in a final dataframe
  coords_xml_source <- paste0("http://meteoclimatic.com/feed/rss/",station_id)
  meteoclimatic_stations <- data.frame()
  
  for (station in coords_xml_source) {
    coords_xml_body <- xml2::read_xml(station)
    station_info <- data.frame(
      # station_id is special, we need obtain it from the link, so we need to remove all the link previous to the code
      station_id = substr(xml2::xml_text(xml2::xml_find_all(coords_xml_body, '//item/link')), start = 37, stop = 100),
      # name, lat and long are elements in each node/item, so we extract them
      name = xml2::xml_text(xml2::xml_find_all(coords_xml_body, '//item/title')),
      lat = xml2::xml_double(xml2::xml_find_all(coords_xml_body, '//item/geo:Point/geo:lat')),
      long = xml2::xml_double(xml2::xml_find_all(coords_xml_body, '//item/geo:Point/geo:long'))
    )
    meteoclimatic_stations <- rbind(meteoclimatic_stations, station_info)
  }
  
  # We need to check if the code is correct (there has to be at least one station)
  if (nrow(meteoclimatic_stations) < 1) {
    stop(paste0('No stations found for area code "', station_id, '"'))
  }
  
  # It seems, as the original code (https://github.com/lemuscanovas/meteoclimaticR/blob/master/R/downloadMeteoclimaticDaily.R)
  # indicates, that some stations have the points coordinates wrong:
  # 
  #       mutate(lat2 = ifelse(lat < 10, lon, lat ),
  #              lon2 = ifelse(lon >10, lat,lon)) %>%
  #         select(-c(lat:lon)) %>%
  #         rename(lat = lat2,
  #                lon = lon2) %>%
  #         mutate(lon = ifelse(lon >2 & lat<38,-lon,lon),
  #                lon = ifelse(lon >1 & lat>43,-lon,lon),
  #                lon = ifelse(lon >4 & lat > 40.5,-lon,lon))
  #                
  # So we need to fix this also
  meteoclimatic_stations$lat_original <- meteoclimatic_stations$lat
  meteoclimatic_stations$long_original <- meteoclimatic_stations$long
  # lat long changed
  meteoclimatic_stations[meteoclimatic_stations$lat_original < 10, ]$lat <- 
    meteoclimatic_stations[meteoclimatic_stations$lat_original < 10, ]$long_original
  meteoclimatic_stations[meteoclimatic_stations$lat_original < 10, ]$long <- 
    meteoclimatic_stations[meteoclimatic_stations$lat_original < 10, ]$lat_original
  meteoclimatic_stations$lat_original <- NULL
  meteoclimatic_stations$long_original <- NULL
  rownames(meteoclimatic_stations) <- meteoclimatic_stations$station_id
  
  message("Coordinates for the stations only have been corrected for Catalonia (station_id = 'ESCAT').")
  
  # sp object ---------------------------------------------------------------------------------------------
  if (isTRUE(spatial)) {
    sp::coordinates(meteoclimatic_stations) <- ~long+lat
    sp::proj4string(meteoclimatic_stations) <- sp::CRS("+proj=longlat")
  }
  
  return(meteoclimatic_stations)
  
}