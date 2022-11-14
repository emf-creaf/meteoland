#' Example data set for statistical correction of RCM predictions
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Example data set including the predictions of Regional Climate Model
#' (CCLM4-8-17; driving global model CNRM-CERFACS-CNRM-CM5) for 3 model cells
#' in a small area in Catalonia (NE Spain). Meteorological data covers an
#' historical (reference) period (2000-2003) and a future (projection) period
#' (2020-2023), the latter simulated under rcp4.5 scenario.
#' 
#' 
#' @name examplecorrectiondata
#' @docType data
#' @format Formal class '\code{\link{MeteorologyUncorrectedData-class}}'
#' @source ESFG web site (http://esgf.llnl.gov/) that centralizes climate data
#' from GCM and RCM uploaded in the frame of different international
#' consortium, including the EURO-CORDEX regionalisation project.
#' @keywords datasets
#' @examples
#' 
#' data(examplecorrectiondata)
#' 
NULL

#' Example spatial grid topography
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' 'SpatialGridTopography' object describing topographic features for a grid of
#' 5 km x 5 km and cell size of 100 m in Catalonia (NE Spain).
#' 
#' 
#' @name examplegridtopography
#' @docType data
#' @format Formal class 'SpatialGridTopography'
#' @source 'Institut Cartogràfic de Catalunya' (ICC)
#' @keywords datasets
#' @examples
#' 
#' data(examplegridtopography)
#' 
NULL

#' Example data set for interpolation from weather stations
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Example data set of spatial location, topography and daily meteorological
#' records from 38 weather stations in Catalonia (NE Spain) corresponding to
#' years 2000-2003.
#' 
#' 
#' @name exampleinterpolationdata
#' @docType data
#' @format Formal class '\code{\link{MeteorologyInterpolationData-class}}'
#' @source 'Servei Meteorològic de Catalunya' (SMC) and 'Agencia Española de
#' Meteorología' (AEMET)
#' @keywords datasets
#' @examples
#' 
#' data(exampleinterpolationdata)
#' 
NULL
