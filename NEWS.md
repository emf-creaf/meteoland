# meteoland 2.2.5
- Swap interpolated MinTemperature and MaxTemperature values when MinTemperature > MaxTemperature

# meteoland 2.2.4
- Warning raised for interpolation weights that would generate NaN in temperature interpolation (NA is returned), added tests, fix #33

# meteoland 2.2.3.9000
- Implemented verbose in write interpolator method, fix #32

# meteoland 2.2.3
- Completing PET in complete_meteo(), implementing issue #23

# meteoland 2.2.2
- Avoid spatial operations when unnecessary when creating interpolator

# meteoland 2.2.1
- Correction of error caused by Rcpp bug

# meteoland 2.2.0
- New interpolation parameters for PET calculation (defaults compatible with previous versions)

# meteoland 2.1.0
- Cleaning deprecated classes

# meteoland 2.0.2
- Modification of default variables for interpolator_calibration
- Avoiding interpolation in cells with missing elevation
- Fixed #21. Now `meteo_complete` works with missing Precipitation
- Option 'ignore_convex_hull_check' allows ignoring when target point is outside the convex hull

# meteoland 2.0.1
- Hard deprecation of "sp", "raster" and "rgdal" depedencies, including functions and classes using
  them.
- Removed old vignettes and old example data
- meteospain dependency moved to SUGGESTS

# meteoland 2.0.0
- Added the new workflows to work without `sp`, `raster`, `rgdal` packages.
**This is a breaking change**, meaning it affects all the package:
  - New functions for interpolation, calibration, cross-validation and
    summarising workflows has been added. See
    `vignette("tidy-meteoland", package = "meteoland")` for more info.
  - Added deprecation notices for all functions that use `sp`, `raster`, `rgdal`
    packages internally.
  - All functions using `sp`, `raster`, `rgdal` packages enter in *maintenance*
    mode. Starting in June, they will stop working in newer versions of
    `meteoland`, as these packages also enter in *maintenance* mode
  - Added tests for new workflows
  - Added tests for results equivalences between new and current workflows
  - Updated documentation (functions) to use roxygen2
  - Added vignetes for the new workflows and the rationale behind it.
- Maintenance of the old workflows:
  - Bug correction in the interpolation.cv
  - Bug correction in interpolategrid

# Version 1.0.3
- Minor C code corrections (clang check)
- Elimination of sysdata (SMCvarcodes not needed)
- Bug correction in the construction of MeteorologyInterpolationData from
  SpatialPointsMeteorology
- Minor corrections to interpolation of wind speed and relative humidity, and to
  PET estimation when wind speed is missing

# Version 1.0.2
- Remove non-necessary dependencies (xml2, httr, jsonlite)
- Documentation bug

# Version 1.0.1
- New function 'reshapemeteospain' to reshape weather data obtained using package 'meteospain'
- Functions 'downloadXXXstation', 'downloadXXXcurrentday' and 'downloadXXXhistorical' now call functions from package 'meteospain'

# Version 1.0.0
- 'debug' option in interpolation params to force additional output
- Download functions DEPRECATED. User is advised to use package 'meteospain'
- CRS specification changed to SRS_string

# Version 0.9.9
- New functions 'downloadMETEOCLIMATICcurrentday' and 'downloadMETEOCLIMATICstationlist' by MalditoBarbudo, inspired by lemuscanovas
- Bug correction in 'downloadAEMEThistorical'

# Version 0.9.8
- Bug correction in '.openwritegridNetCDF'

# Version 0.9.7
- Bug correction in 'mergegrids'
- Bug correction in '.putgridvardataday'
- Flexible variable definition for 'writemeteorologygrid' and 'writemeteorologypoints'
- MeteorologyUncorrectedData accepts filenames as input to avoid loading large files in memory when calling 'correctionpoints'

# Version 0.9.6
- Allow missing Precipitation when building MeteorologyInterpolationData
- New functions for conversion between relative humidity and dew temperature 

# Version 0.9.5
- Update of reshapeworldmet to adapt to changes in package worldmet v.0.9.0
- Update of function downloadSMCcurrentday

# Version 0.9.4
- Update of CRS in package data.
- Modification of error handling in .get_data_aemet.
- Bug correction in functions as(,"STFDF") for dates of length one.

# Version 0.9.3
- New functions 'readNetCDFpoints','readNetCDFproj4string', 'readNetCDFgridtopology' and 'readNetCDFdates'.
- Bug correction byPixels in interpolationpixels/interpolationgrid.

# Version 0.9.2
- Clean version

# Version 0.9.0
- New functions 'extractdates' and 'extractvars' replacing and generalizing 'extractpointdates'.
- Function 'summarypoints' can summarize all variables at once.
- Unlimited NetCDF time dimension for grids and station dimension for points.
- Grids/pixels with no dates allowed
- Time (unlimited) is written as last dimension in grids, but can be read as first or last dimension
- New functions as() to transform grids/pixels meteorology into point meteorology
- New function writeemptymeteorologygrid()

# Version 0.8.9
- New functions to download data from 'meteogalicia' (MG)
- Improvements in function 'reshapeworldmet'
- New (public) functions 'humidity_relative2specific' and 'humidity_specific2relative'

# Version 0.8.8
- New functions 'writemeteorologypoints' and 'readmeteorologypoints' for storing/reading point data as/from netCDF
- Functions 'interpolationpoints' and 'correctionpoints' can now write their result to netCDF
- Function 'summarypoints' can now produce summaries reading from netCDF
- Function 'extractpointdates' can now extract data reading from netCDF

# Version 0.8.7
- New function 'readmeteorologygridpoints'
- New function 'writemeteorologygridpixel'
- New functions 'mergegrids' and 'mergepoints'
- Reading multiple grid/pixels files

# Version 0.8.6
- New function 'averagearea'
- New functions 'defaultGenerationParams' and 'weathergeneration' implementing a multisite and multivariate weather generator
- Modification of readmeteorologygrid/pixels to facilitate reading data from netCDF not created by meteoland

# Version 0.8.5
- New functions to convert SpatialPointsMeteorology, SpatialPixelsMeteorology and SpatialGridMeteorology into objects of packages 'spacetime' and 'stars'
- Functions to read/write NetCDF files reprogrammed and improved.
- Functions extractgridindex and extractgridpoints reprogrammed

# Version 0.8.4
- New function to calculate rainfall erosivity 

# Version 0.8.3
- Remove dependencies of ncdf4.helpers

# Version 0.8.2
- Documentation converted to bookdown reference book.
- Bug correction in function 'doQmapDeque'

# Version 0.8.1
- Modification of 'radiation_directDiffuseInstant' and 'radiation_directDiffuseDay' to account for topographic effects
- New function 'summarypoint'

# Version 0.8.0
- New functions 'downloadSMCstationlist', 'downloadSMCcurrentday', 'downloadSMChistorical' and 'downloadSMCvarmetadata'.
- Update of vignette 'Meteorology'.

# Version 0.7.9
- New function reshapeworldmet to reshape data downloaded using package 'worldmet'
- New function reshapeweathercan to reshape data downloaded using package 'weathercan'
- Transform point coordinate system in 'interpolationpoints', 'interpolationpixels' and 'interpolationgrid' if necessary
- Check for infinite values in 'MeteorologyInterpolationData'
- New function 'correctionpoint'.
- Bug corrected in 'meteocomplete'. 
- Added conversion from specific humidity to relative humidity in meteocomplete.

# Version 0.7.8
- Corrected bug MeteorologyInterpolationData with SpatialPointsTopography as input
- Updated user guide documentation

# Version 0.7.7
- Empirical quantile mapping according to original method (Déqué)
- Dependency from package 'qmap' removed

# Version 0.7.6
- Methods 'print/head/tail' updated for objects SpatialPointsTopography and SpatialPointsMeteorology
- Subsetting for objects SpatialPointsMeteorology, SpatialGridMeteorology and SpatialPixelsMeteorology.
- Coercing objects of Spatial...Topography
- New vignette 'user guide' (old one renamed)
- Adapt to Rcpp changes

# Version 0.7.5
- AEMET download using packages httr and jsonlite (code adapted from https://github.com/SevillaR/aemet)
- Methods 'print/show' added for objects of class SpatialPointsMeteorology, SpatialGridMeteorology and SpatialPixelsMeteorology.
- Methods 'print/show' added for objects of class SpatialPointsTopography, SpatialGridTopography and SpatialPixelsTopography.
- Methods 'head/tail' added for objects SpatialPointsTopography and SpatialPointsMeteorology
- Bug correction: Historical download of AEMET data returns SunshineHours

# Version 0.7.4
- Added reference to publication
- Added citation reference.
- Bug correction: Over-dimensioned vectors for temperature differences (same for precipitation).
- Low-level interpolation routines made accessible to the user
- Bug correction: builder for MeteorologyInterpolationData
- Bug correction: Missing values in downloadAEMEThistorical

# Version 0.7.3
- New function 'meteocomplete'.
- Improvement of function 'meteoplot' to accept data frames with daily meteorological data as input

# Version 0.7.2
- Update of functions to download data from AEMET to deal with encoding
- Update of function 'SpatialPointsMeteorology' to build objects from sets of data frames, one per date
- Update of function 'MeteorologyInterpolationData' to build objects from different data structures
- New function 'summaryinterpolationdata' to summarize objects of class 'MeteorologyInterpolationData'

# Version 0.7.1
- Bug correction in interpolationpixels (with export=TRUE)
- Bug correction in correctionpoints.errors (with rds input)

# Version 0.7.0
- New S4 structures: SpatialPixelsTopography and SpatialPixelsMeteorology
- New function interpolationpixels
- Update of functions to download data from the Spanish Agencia Estatal de Meteorologia (AEMET)
- New function writemeteorologypixels
- Function correctiongrid removed (correction of weather series makes sense for point data)
- New function summarypixels

# Version 0.6.9
- Allow saturated (> 100% values) in relative humidity when extracting from NetCDF and when performing bias correction

# Version 0.6.8

- Modification of the precipitation interpolation (kernels)
- Modification of radiation calculation (diffuse light for slopes where direct light is too low)
- Modification of partitioning between direct and diffuse radiation

# Version 0.6.7

- Improvements in 'subsample' function
- New function 'radiation_skyLongwaveRadiation'

# Version 0.6.6

- Export format txt/rds
- Changed description

# Version 0.6.5

- Default correction method for wind switched to 'quantmap'.
- Unbias method for Tmin and Tmax applies bias from Tmean (if also unbiasing).
- Quantile mapping method for Tmin and Tmax is applied to (Tmin-Tmean) and (Tmax-Tmean).

# Version 0.6.4

- Corrected a bug in the correction of relative humidity.
- New function 'correctionpoints.errors' to evaluate correction errors for the reference period.

