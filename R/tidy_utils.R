# .fix_station_geometries <- function(meteo) {
#   distinct_rows <- meteo |>
#     dplyr::ungroup() |>
#     dplyr::arrange(timestamp) |>
#     dplyr::select(station_id, geometry) |>
#     dplyr::distinct()
# 
#   # res <- meteo |>
#   #   dplyr::select(timestamp, station_id, geometry) |>
#   #   dplyr::distinct()
# 
#   # If more geometries than station IDs, issue a warning and filter by the last
#   # geometry to remove duplicates
#   if (nrow(distinct_rows) != length(unique(distinct_rows[["station_id"]]))) {
#     usethis::ui_warn(c(
#       "Some stations have different metadata (elevation, coordinates...) for ",
#       "different dates. Choosing the most recent metadata"
#     ))
# 
#     distinct_rows <- distinct_rows |>
#       dplyr::group_by(station_id) |>
#       dplyr::filter(as.character(geometry) == dplyr::last(as.character(geometry)))
# 
#     meteo <- meteo |>
#       dplyr::as_tibble() |>
#       dplyr::select(-geometry) |>
#       dplyr::left_join(distinct_rows, by = 'station_id') |>
#       sf::st_as_sf()
#   }
# 
#   return(meteo)
# 
# }

.station_indexes_converter <- function(stations, interpolator) {

  res <- stations

  if (is.null(stations)) {
    res <- 1:length(stars::st_get_dimension_values(interpolator, "station"))
  }

  if (is.character(stations)) {
    res <- which(colnames(interpolator[[1]]) %in% stations)
    if (length(res) < 1) {
      usethis::ui_stop("Station names not found in interpolator")
    }
  }

  return(res)
}
