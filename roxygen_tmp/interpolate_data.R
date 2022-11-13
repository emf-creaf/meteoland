#' Interpolation process for spatial data
#' 
#' Interpolate spatial data to obtain downscaled meteorologic variables
#' 
#' This function takes a spatial data object (sf or stars raster), an
#' interpolator object (\code{\link{create_meteoland_interpolator}}) and a
#' vector of dates to perform the interpolation of the meteorologic variables
#' for the spatial locations present in the \code{spatial_data} object.
#' 
#' @param spatial_data An sf or stars raster object to interpolate
#' @param interpolator A meteoland interpolator object, as created by
#' \code{\link{create_meteoland_interpolator}}
#' @param dates vector with dates to interpolate (must be within the
#' interpolator date range). Default to NULL (all dates present in the
#' interpolator object)
#' @return an object with the same class and structure as the provided spatial
#' data with the results of the interpolation joined. In the case of spatial
#' data being an sf, the results are added as a list-type column that can be
#' unnested with \code{\link[tidyr]{unnest}}. In the case of a stars raster
#' object, interpolation results are added as attributes (variables)
#' @section Spatial data:
#' 
#' The spatial data provided must be of two types. (I) A sf object containing
#' POINT for each location to interpolate or (II) a stars raster object for
#' which the interpolation should be done.  Independently of the class of
#' \code{spatial_data} it has to have some mandatory variables, namely
#' \code{elevation}. It should also contain \code{aspect} and \code{slope} for
#' a better interpolation process, though this two variables are not mandatory.