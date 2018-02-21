downloadAEMEThistoricalstationlist <- function(api){
  
  value.func <- function(x){
    value <- regmatches(x,gregexpr('(?<=\\:\\s)[[:print:]]*(?=\\n)', x, perl = T))[[1]]
    value <- gsub('\\",', "", value)
    value <- gsub('\\"', "", value)
    value <- gsub(',', ".", value)
  }
  
  # set options
  h = new_handle()
  handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  handle_setopt(h, ssl_verifypeer=FALSE)
  handle_setopt(h, customrequest="GET")
  
  url <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/todasestaciones"
  # get data url
  urldata_raw <- curl_fetch_memory(url, h)$content
  urldata_string <- value.func(rawToChar(urldata_raw))
  
  if(urldata_string[2]=="401"){
    stop("Invalid API key. (API keys are valid for 3 months.)")
  }
  
  urldata <- urldata_string[3]
  
  # get data and format
  data_raw <- curl_fetch_memory(urldata, h)$content
  data_string <- rawToChar(data_raw)
  cname <- regmatches(data_string,gregexpr('(?<=\\\n\\s{2}\\\")[[:print:]]*(?=\\\"\\s\\:)', data_string, perl = T))[[1]]
  value <- regmatches(data_string,gregexpr('(?<=\\:\\s\\\")[[:print:]]*(?=\\\")', data_string, perl = T))[[1]]
  unique_cname <- unique(cname)
  
  data_df <- as.data.frame(sapply(unique_cname,FUN = function(x){value[cname == x]}), stringsAsFactors = F)
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
