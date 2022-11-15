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

#' Example data set for meteo data from weather stations
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Example data set of spatial location, topography and daily meteorological
#' records from 189 weather stations in Catalonia (NE Spain) corresponding to
#' April 2022.
#'
#'
#' @name meteoland_meteo_example
#' @docType data
#' @format sf object
#' @source 'Servei Meteorològic de Catalunya' (SMC)
#' @keywords datasets
#' @examples
#'
#' data(meteoland_meteo_example)
#'
NULL

#' Example data set for meteo data from weather stations, without topography
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Example data set of spatial location and daily meteorological
#' records from 189 weather stations in Catalonia (NE Spain) corresponding to
#' April 2022.
#'
#'
#' @name meteoland_meteo_no_topo_example
#' @docType data
#' @format sf object
#' @source 'Servei Meteorològic de Catalunya' (SMC)
#' @keywords datasets
#' @examples
#'
#' data(meteoland_meteo_no_topo_example)
#'
NULL

#' Example data set for topography data from weather stations, without meteo
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Example data set of spatial location and topography
#' records from 189 weather stations in Catalonia (NE Spain).
#'
#'
#' @name meteoland_topo_example
#' @docType data
#' @format sf object
#' @source 'Servei Meteorològic de Catalunya' (SMC)
#' @keywords datasets
#' @examples
#'
#' data(meteoland_topo_example)
#'
NULL

#' Example data set of points for interpolation of weather variables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Example data set of spatial location and topography
#' records from 15 experimental plots in Catalonia (NE Spain).
#'
#'
#' @name points_to_interpolate_example
#' @docType data
#' @format sf object
#' @source Spanish National Forest Inventory
#' @keywords datasets
#' @examples
#'
#' data(points_to_interpolate_example)
#'
NULL

#' Example raster data set for interpolation of weather variables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Example raster data set of spatial location and topography
#' records from Catalonia (NE Spain). Cell size is 1km x 1km and raster size is 10x10 cells.
#'
#'
#' @name raster_to_interpolate_example
#' @docType data
#' @format stars object
#' @source ICGC
#' @keywords datasets
#' @examples
#'
#' data(raster_to_interpolate_example)
#'
NULL

#' Example interpolator object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Example interpolator with daily meteorological
#' records from 189 weather stations in Catalonia (NE Spain) corresponding to
#' April 2022.
#'
#'
#' @name meteoland_interpolator_example
#' @docType data
#' @format stars data cube object
#' @source Spanish National Forest Inventory
#' @keywords datasets
#' @examples
#'
#' data(meteoland_interpolator_example)
#'
NULL
