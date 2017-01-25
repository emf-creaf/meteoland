downloadAEMETstationdata <- function(api, station_id){
  # set options
  h = new_handle()
  handle_setheaders(h, "Cache-Control" = "no-cache", api_key=api)
  handle_setopt(h, ssl_verifypeer=FALSE)
  handle_setopt(h, customrequest="GET")
  
  ## Metadata
  url <- paste("https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/estaciones/",
           paste(station_id,collapse=","), sep="")

  md_raw <- curl_fetch_memory(url, h)
  urldatos <- strsplit(rawToChar(md_raw$content), split="\"")[[1]][10]
  md_raw <- curl_fetch_memory(urldatos, h)
  md_raw <- md_raw$content[-which(md_raw$content %in% c("5b", "20", "7b", "20", "20", "22", "5d", "2c"))]
  md_raw <- strsplit(rawToChar(md_raw), split = "}")
  md_raw <- strsplit(md_raw[[1]], split = "\n")
  md_raw <- lapply(md_raw, FUN = function(x)x[-1])
  
  md <- lapply(md_raw, FUN = function(x){strsplit(x, split = ":")})
  md_names <- lapply(md, FUN = function(x){unlist(lapply(x, FUN = function(x){x[1]}))})[[1]]
  md_value <- lapply(md, FUN = function(x){unlist(lapply(x, FUN = function(x){x[2]}))})
  md_matrix <- matrix(unlist(md_value), byrow = T, ncol = length(md_names), dimnames = list(NULL, md_names))
  md_matrix[,"longitud"] <- unlist(lapply(md_matrix[,"longitud"], FUN = function(x){
    if(substr(x, start = nchar(x), stop = nchar(x)) == "W"){
      x <- paste("-", substr(x, start = 1, stop = nchar(x)-1), sep = "")
    }else{
      x <- substr(x, start = 1, stop = nchar(x)-1)
    }
  }))
  md_matrix[,"latitud"] <- unlist(lapply(md_matrix[,"latitud"], FUN = function(x){x <- substr(x, start = 1, stop = nchar(x)-1)}))
  
  station_id = md_matrix[,"indicativo"]
  longString = as.character(md_matrix[,"longitud"])
  latString = as.character(md_matrix[,"latitud"])
  deg = as.numeric(substr(longString, 0, nchar(longString)-4))
  min = as.numeric(substr(longString, nchar(longString)-3, nchar(longString)-2))
  sec = as.numeric(substr(longString, nchar(longString)-1, nchar(longString)))
  x = deg+min/60+sec/3600
  deg = as.numeric(substr(latString, 0, nchar(latString)-4))
  min = as.numeric(substr(latString, nchar(latString)-3, nchar(latString)-2))
  sec = as.numeric(substr(latString, nchar(latString)-1, nchar(latString)))
  y = deg+min/60+sec/3600
  points <- SpatialPoints(coords = cbind(x,y), CRS("+proj=longlat"))
  colnames(points@coords)<-c("longitude","latitude")
  dfout <- data.frame(station = md_matrix[,"nombre"], 
                      province = md_matrix[,"provincia"], 
                      elevation = as.numeric(md_matrix[,"altitud"]))
  rownames(dfout) <- station_id
  return(SpatialPointsDataFrame(points, dfout))
}

