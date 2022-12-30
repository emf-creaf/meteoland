library(meteospain)
library(sf)
library(stars)
library(meteoland)
library(dplyr)
library(lfcdata)

# raw meteospain april 2022 data
meteocat_april_2022 <- meteospain::get_meteo_from(
  "meteocat",
  meteospain::meteocat_options(
    "daily", as.Date("2022-04-01"),
    api_key = keyring::key_get('meteocat', keyring = "malditobarbudo")
  )
)

# transform to meteoland meteo
meteoland_meteo_example <- meteoland::meteospain2meteoland(
  meteocat_april_2022, complete = TRUE
)

# obtain topo
meteoland_topo_example <- meteoland_meteo_example |>
  dplyr::select(stationID, elevation) |>
  dplyr::distinct()

# meteo without topo example
meteoland_meteo_no_topo_example <- meteoland_meteo_example |>
  dplyr::select(-elevation, -aspect, -slope)

# interpolation data (points)
set.seed(2525) # for sample returning always the same
points_to_interpolate_example <-
  lfcdata::nfi()$get_data('plots', spatial = TRUE) |>
  dplyr::filter(presence_NFI_4) |>
  dplyr::filter(plot_id %in% sample(plot_id, 15)) |>
  dplyr::select(
    plot_id,
    elevation = topo_altitude_asl,
    slope = topo_fdm_slope_degrees,
    aspect = topo_fdm_aspect_degrees
  )

# interpolation data (stars)
temp_topo <-
  stars::read_ncdf('data-raw/Topology_grid.nc')
st_crs(temp_topo) <- st_crs(3043)
# we reduce to 1000m pixel, warp to 4326 and select 10x10 raster (100 cells)
raster_to_interpolate_example <- temp_topo |>
  st_warp(cellsize = 1000, crs = st_crs(temp_topo)) |>
  st_warp(crs = st_crs(4326)) |>
  dplyr::rename(elevation = Elevation, aspect = Aspect, slope = Slope)
raster_to_interpolate_example[["aspect"]] <-
  units::set_units(raster_to_interpolate_example[["aspect"]], "degrees")
raster_to_interpolate_example <-
  raster_to_interpolate_example[ , 155:165, 110:120]

# interpolator
meteoland_interpolator_example <-
  meteoland::with_meteo(meteoland_meteo_example) |>
  meteoland::create_meteo_interpolator()

# usedata
usethis::use_data(meteoland_meteo_example, overwrite = TRUE)
usethis::use_data(meteoland_topo_example, overwrite = TRUE)
usethis::use_data(meteoland_meteo_no_topo_example, overwrite = TRUE)
usethis::use_data(points_to_interpolate_example, overwrite = TRUE)
usethis::use_data(raster_to_interpolate_example, overwrite = TRUE)
usethis::use_data(meteoland_interpolator_example, overwrite = TRUE)
