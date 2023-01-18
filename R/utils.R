#' Humidity conversion tools
#'
#' @description
#' Functions to transform relative humidity to specific humidity or dew point
#' temperature and viceversa.
#'
#' @param Tc A numeric vector of temperature in degrees Celsius.
#' @param HR A numeric vector of relative Humidity (in %).
#' @param Td A numeric vector of dew temperature in degrees Celsius.
#' @param HS A numeric vector of specific humidity (unitless).
#' @param allowSaturated Logical flag to allow values over 100%
#' @return A numeric vector with specific or relative humidity.
#' @author Nicholas Martin-StPaul, INRA
#'
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{meteocomplete}}
#' @rdname humidity-conversion-tools
#' @export
humidity_relative2dewtemperature <- function(Tc, HR) {

  # assertions
  assertthat::assert_that(
    is.numeric(Tc), is.numeric(HR),
    msg = "Tc and HR must be numeric vectors"
  )
  assertthat::assert_that(
    identical(length(Tc), length(HR)), msg = "Tc and HR must be the same length"
  )

  if(is.data.frame(Tc)) Tc = as.matrix(Tc)
  if(is.data.frame(HR)) HR = as.matrix(HR)
  if(is.matrix(Tc) && is.matrix(HR)) {
    Td = .dewpointTemperatureFromRH(Tc,HR)
    dimnames(Td) = dimnames(Tc)
  } else {
    Td = as.vector(.dewpointTemperatureFromRH(as.matrix(Tc),as.matrix(HR)))
  }
  return(Td)
}
#' @rdname humidity-conversion-tools
#' @export
humidity_dewtemperature2relative<-function(Tc, Td, allowSaturated = FALSE) {

  # assertions
  assertthat::assert_that(
    is.numeric(Tc), is.numeric(Td),
    msg = "Tc and Td must be numeric vectors"
  )
  assertthat::assert_that(
    assertthat::is.flag(allowSaturated), msg = "allowSaturated must be TRUE or FALSE"
  )
  assertthat::assert_that(
    identical(length(Tc), length(Td)), msg = "Tc and Td must be the same length"
  )

  HR= as.vector(.relativeHumidityFromDewpointTemp(as.numeric(Tc),as.numeric(Td)))
  if(!allowSaturated) HR[HR>100]=100 # On ne passe pas audessus de 100
  return(HR)
}

#Functions to switch from relative humidity to specific humidity
#' @rdname humidity-conversion-tools
#' @export
humidity_specific2relative <- function(Tc, HS, allowSaturated = FALSE) {

  # assertions
  assertthat::assert_that(
    is.numeric(Tc), is.numeric(HS),
    msg = "Tc and HS must be numeric vectors"
  )
  assertthat::assert_that(
    assertthat::is.flag(allowSaturated), msg = "allowSaturated must be TRUE or FALSE"
  )
  assertthat::assert_that(
    identical(length(Tc), length(HS)), msg = "Tc and HS must be the same length"
  )

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
#' @rdname humidity-conversion-tools
#' @export
humidity_relative2specific<-function(Tc, HR){

  # assertions
  assertthat::assert_that(
    is.numeric(Tc), is.numeric(HR),
    msg = "Tc and HR must be numeric vectors"
  )
  assertthat::assert_that(
    identical(length(Tc), length(HR)), msg = "Tc and HR must be the same length"
  )

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
