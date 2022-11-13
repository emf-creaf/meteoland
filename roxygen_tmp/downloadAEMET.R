#' Download data from AEMET
#' 
#' DEPRECATED. Download data from the Spanish National Meterology Agency
#' (AEMET)
#' 
#' API key needs to be acquired from AEMET (https://opendata.aemet.es/)
#' 
#' @aliases downloadAEMETstationlist downloadAEMEThistorical
#' downloadAEMETcurrentday
#' @param api String with the AEMET API key (see https://opendata.aemet.es/).
#' @param dates An object of class \code{\link{Date}}.
#' @param station_id A string vector containing station ids (the list of
#' stations for which historical climatic series are available is given by
#' \code{downloadAEMETstationlist}).
#' @param export If \code{export = FALSE} the downloaded data is stored in
#' memory. Otherwise the result is written on the disk (using the format
#' specified in \code{exportFormat}).
#' @param exportDir Output directory for downloaded meteorology.
#' @param exportFormat Format of meteorological data. Current accepted formats
#' are \code{"castanea"} and \code{"meteoland"}.
#' @param metadataFile The name of the file that will store the meta data
#' describing all written files.
#' @param verbose Boolean flag to print process information.
#' @param daily Boolean flag. Are data to be returned at a daily or hourly
#' scale?
#' @return Function \code{downloadAEMETstationlist} returns a
#' \code{\link{SpatialPointsDataFrame-class}} object containing the list of
#' AEMET weather stations for which historical climatic series are available
#' and can be retrieved using \code{downloadAEMEThistorical}.
#' 
#' Function \code{downloadAEMEThistorical} downloads data for the specified
#' AEMET stations and dates. Data are availables for dates up to 4 days before
#' current date. If \code{export = FALSE}, function
#' \code{downloadAEMEThistorical} returns a
#' \code{\link{SpatialPointsMeteorology-class}} object with the downloaded
#' meteorology for each station (point). Otherwise the function writes on the
#' disk at the location specified by \code{exportDir} and solelely returns a
#' \code{\link{SpatialPointsDataFrame-class}} object containing the files
#' metadata.
#' 
#' Function \code{downloadAEMETcurrentday} downloads recent weather (the last
#' 24h) from all currently available stations and returns data frame if
#' \code{daily = FALSE} or a \code{\link{SpatialPointsDataFrame-class}} object
#' with observations aggregated at the daily scale if else.
#' @note Since ver. 1.0.1, weather data download functions included in
#' \code{meteoland} make internal calls to functions in package
#' \code{meteospain}. For an enhanced flexibility, users are recommended to
#' call functions in \code{meteospain} themselves, and then to use function
#' \code{\link{reshapemeteospain}} to generate data suitable for
#' \code{meteoland}.
#' 
#' The list of stations available in \code{downloadAEMETcurrentday} (current
#' observations) may be different from the list given by
#' \code{downloadAEMETstationlist} and available in
#' \code{downloadAEMEThistorical} (stations with historical climate series).
#' @author Antoine Cabon, CTFC
#' 
#' Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
#' @references AEMET should be acknowledged as author of information when using
#' this data.