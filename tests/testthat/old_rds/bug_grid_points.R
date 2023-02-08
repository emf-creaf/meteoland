library(meteoland)
library(testthat)
# vamos a usar examplegridtopography como punto de partida
grid_to_test <- examplegridtopography

# y sacamos los puntos de esa topografía para comprobar que con las mismas
# coordenadas da resultados distintos
points_to_test <- as(grid_to_test, "SpatialPointsTopography")

# sacamos los resultados para febrero 2001
grid_results <- interpolationgrid(
  exampleinterpolationdata, grid_to_test, seq(as.Date("2001-02-01"), as.Date("2001-02-28"), 1)
)

points_results <- interpolationpoints(
  exampleinterpolationdata, points_to_test, seq(as.Date("2001-02-01"), as.Date("2001-02-28"), 1)
)

# Y ahora comprobamos que los resultados para temperatura son los mismos, pero
# para humedad no
c(MeanTemperature = "MeanTemperature", MeanRelativeHumidity = "MeanRelativeHumidity") |>
  purrr::walk(.f = function(variable) {
    names(grid_results@data) |>
      purrr::set_names(names(grid_results@data)) |>
      purrr::walk(
        .f = \(date_index) {
          expect_true(
            abs(mean(
              grid_results@data[[date_index]][[variable]] -
                (purrr::map_dbl(1:length(points_results@data), .f = \(point) {points_results@data[[point]][date_index, variable]})),
              na.rm = TRUE
            )) < testthat_tolerance()
          )
          expect_true(
            sd(
              grid_results@data[[date_index]][[variable]] -
                (purrr::map_dbl(1:length(points_results@data), .f = \(point) {points_results@data[[point]][date_index, variable]})),
              na.rm = TRUE
            ) < testthat_tolerance()
          )
        }
      )
  })

# Vemos que falla en relative humidity para la primera fecha:
date_index <- "2001-02-01"
variable <- "MeanRelativeHumidity"

# resultados para esa fecha y variable para todas las coordenadas del grid
rh_20010201_grid <- grid_results@data[[date_index]][[variable]]
# resultados para esa fecha y variable para todos los puntos del spatialpoints
rh_20010201_points <- purrr::map_dbl(
  1:length(points_results@data),
  .f = \(point) {points_results@data[[point]][date_index, variable]}
)

mean(rh_20010201_grid - rh_20010201_points, na.rm = TRUE)
sd(rh_20010201_grid - rh_20010201_points, na.rm = TRUE)

# esto pasa para otras fechas y otras variables (minRH, maxRH y Radiación), pero no
# para Temperaturas o precipitación

# otras fechas
date_index <- "2001-02-05"
variable <- "MeanRelativeHumidity"

# resultados para esa fecha y variable para todas las coordenadas del grid
rh_20010205_grid <- grid_results@data[[date_index]][[variable]]
# resultados para esa fecha y variable para todos los puntos del spatialpoints
rh_20010205_points <- purrr::map_dbl(
  1:length(points_results@data),
  .f = \(point) {points_results@data[[point]][date_index, variable]}
)

mean(rh_20010205_grid - rh_20010205_points, na.rm = TRUE)
sd(rh_20010205_grid - rh_20010205_points, na.rm = TRUE)

# radiación
date_index <- "2001-02-01"
variable <- "Radiation"

# resultados para esa fecha y variable para todas las coordenadas del grid
rad_20010201_grid <- grid_results@data[[date_index]][[variable]]
# resultados para esa fecha y variable para todos los puntos del spatialpoints
rad_20010201_points <- purrr::map_dbl(
  1:length(points_results@data),
  .f = \(point) {points_results@data[[point]][date_index, variable]}
)

mean(rad_20010201_grid - rad_20010201_points, na.rm = TRUE)
sd(rad_20010201_grid - rad_20010201_points, na.rm = TRUE)

# pero no temperaturas
date_index <- "2001-02-01"
variable <- "MeanTemperature"

# resultados para esa fecha y variable para todas las coordenadas del grid
temp_20010201_grid <- grid_results@data[[date_index]][[variable]]
# resultados para esa fecha y variable para todos los puntos del spatialpoints
temp_20010201_points <- purrr::map_dbl(
  1:length(points_results@data),
  .f = \(point) {points_results@data[[point]][date_index, variable]}
)

mean(temp_20010201_grid - temp_20010201_points, na.rm = TRUE)
sd(temp_20010201_grid - temp_20010201_points, na.rm = TRUE)
