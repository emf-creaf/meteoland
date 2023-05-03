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
