#Functions to switch from relative humidity to specific humidity
humidity_specific2relative<-function(Tc, HS, allowSaturated = FALSE){


  #-------------------------------------------------------------
  #D?claration des constantes pour le calcul de l'HR et de la T en ?C
  Mas=28.966 # masse molaire air sec (g/mol)
  Mh2o=18 # masse molaire H2O(g/mol)
  Rgz=8.314472 # %J/mol/K cste gaz parfait
  p_air=101325 #  %en Pa
  #-------------------------------------------------------------

  #Calcul de l'Humidit? relative bas? sur Hs et Tk
  Tk=Tc+273.15
  Dair=((p_air)/(Rgz*(Tk)))*Mas # Calcul de la masse volumique de l'air sec en g.m-3
  masshum=HS*Dair # Masse d'eau dans l'air g.m-3
  nhum=masshum/Mh2o # en mol.m-3
  ea=nhum*(Rgz*(Tk)) # Pression de vapeur r?elle (en Pa)
  es=6.108*exp(17.27*Tc/(237.2+Tc))*100 # Pression de vapeur saturante ? la Temp?rature T (en Pa)
  HR=ea/es*100 #Calcul de l'HR
  if(!allowSaturated) HR[HR>100]=100 # On ne passe pas audessus de 100
  return(HR)
}
humidity_relative2specific<-function(Tc, HR){

  #-------------------------------------------------------------
  #D?claration des constantes pour le calcul de l'HR et de la T en ?C
  Mas=28.966 # masse molaire air sec (g/mol)
  Mh2o=18 # masse molaire H2O(g/mol)
  Rgz=8.314472 # %J/mol/K cste gaz parfait
  p_air=101325 #  %en Pa
  #-------------------------------------------------------------

  #Calcul de l'Humidit? relative bas? sur Hs et Tk
  Tk=Tc+273.15

  Dair=((p_air)/(Rgz*(Tk)))*Mas # Calcul de la masse volumique de l'air sec en g.m-3
  es=6.108*exp(17.27*Tc/(237.2+Tc))*100 # Pression de vapeur saturante ? la Temp?rature T (en Pa)
  ea=HR*es/100 #Calcul de l'ea
  nhum=ea/(Rgz*(Tk)) # Pression de vapeur r?elle (en Pa)
  masshum=nhum*Mh2o # en mol.m-3
  HS=masshum/Dair
  return(HS)
}

# Code modified from project https://github.com/SevillaR/aemet
.get_data_aemet <- function(apidest, apikey, verbose = FALSE) {
  
  url.base <- "https://opendata.aemet.es/opendata"
  
  url1 <- paste0(url.base, apidest)
  
  path1 <- httr::GET(url1, add_headers(api_key = apikey))
  
  urls.text <- httr::content(path1, as = "text")

  if(is.na(urls.text)) {
    cat("\n  The number of downloads per minute on the AEMET server is limited. Please wait.")
    for(t in 1:10){(Sys.sleep(6));cat(".");if(t == 10)cat("\n")}
    path1 <- httr::GET(url1, add_headers(api_key = apikey))
    urls.text <- httr::content(path1, as = "text")
  }
  urls <- jsonlite::fromJSON(urls.text)

  if(verbose) print(urls)
  if (urls$estado==401) {
    stop("Invalid API key. (API keys are valid for 3 months.)")
  } else if (urls$estado==404) { #"No hay datos que satisfagan esos criterios"
    return(NULL)
  } else if (urls$estado==429) {
    cat("\n  The number of downloads per minute on the AEMET server is limited. Please wait.")
    for(t in 1:10){(Sys.sleep(6));cat(".");if(t == 10)cat("\n")}
    path1 <- httr::GET(url1, add_headers(api_key = apikey))
    urls.text <- httr::content(path1, as = "text")
    urls <- jsonlite::fromJSON(urls.text)
  }
  if(urls$estado==200) {
    path2 <- httr::GET(urls$datos)
    data.json <- httr::content(path2, as = "text")
    if(is.na(data.json)) {
      cat("\n  The number of downloads per minute on the AEMET server is limited. Please wait.")
      for(t in 1:10){(Sys.sleep(6));cat(".");if(t == 10)cat("\n")}
      path2 <- httr::GET(urls$datos)
      data.json <- httr::content(path2, as = "text")
    }
    datos <- jsonlite::fromJSON(data.json)
    return(datos)
  }
}

.get_data_smc <- function(apidest, apikey) {
  
  url.base <- "https://api.meteo.cat/xema/v1"
  
  url1 <- paste0(url.base, apidest)
  
  path1 <- httr::GET(url1, httr::add_headers(`x-api-key`= apikey))
  
  ans.text <- httr::content(path1, as = "text")
  ans <- jsonlite::fromJSON(ans.text)
  
  if(path1$status_code != 200) {
    error.df <- data.frame(code = c(400, 403, 429, 500),
                           type = c("Bad request","Forbidden","Too many requests", "Internal server error"),
                           cause = c("The parameters of the request are incorrect",
                                     "If message=Forbidden, the request is not allowed.\nIf message=Missing authentification Token, the data does not exist",
                                     "The limit of requests per second or per month has been reached",
                                     "Internal server error (e.g. the requested data is not available)\nFurther information about the cause of the error is given in message"))
    sel <- error.df$code == path1$status_code  
    stop(paste(error.df[sel,1], error.df[sel,2], error.df[sel,3], paste0("\nMessage:",ans$message), sep = ". "))
  }else{
    return(ans)
  }
}
