#### TODO  list
### 1. ~transpose interpolator, dates must be rows, stations must be cols~ DONE
###
### 1. implement ncdfgeom write method to save the interpolator as nc with attributes DONE
###     - units DONE
###
### 1. implement ncdfgeom read method to read interpolators saved to file
###     - write method has to be changed to create stationID as last attribute DONE
###     - create interpolator method must be changed to attribute arrays to be
###       col named with station ID. this will get rid of the need to create
###       an attribute in the stars with the stationID. write interpolator method
###       must be changed accordingly. DONE
###     - review process, as stars has a as_stars.ncdfgeom method, maybe is more
###       efficient to use that. DONE: Not implemented, it seems that I need to
###       review how i write the interpolator. For the moment it stays as it is
### 1. helper to transform meteospain output to meteo input DONE
###
### 1. BUG!!!
###     - In .interpolator_arrays_creator, when completing the cases, some needed
###       variables, mostly elevation creates NA for the missing cases. This
###       generates problems later on. Fix!!! DONE
### 1. add crs conversion in the interpolation point method.
###     - Implemented a check in .interpolation_point but a conversion must be done
###     before calling this method. DONE
### 1. interpolation process
###     - .interpolation_points method DONE
###     - messages (user informed user happy) DONE
###     - ensure!!! that the points or whatever the user has supplied as input
###       has all the needed (elevation ie) DONE
###     - ensure that lat long and alt are present in the interpolator creation,
###       filter any station without those values DONE
###     - interpolation dispatcher (different spatial formats as inputs)
###       - crs conversion DONE
###     - interpolation general function DONE
###     - transform from raster(stars) to sf: .stars2sf DONE
### 1. Fixes
###     - mean_* variables not getting retrieved in meteospain2meteoland DONE
###     - distance calculation is done with repeated points, causing allocation
###       of big vectors and time wasting. Fix it. DONE
### 1. Fixes
###     - add topo with sf DONE
###     - time spend in joining topo when big meteo. I think is fixed now as i was
###       multiplying the topo as not taking careof repeated rows DONE
###     - ensure joining topo get the correct joining DONE
###     - check for correct params (what happens if user calls debig instead of debug...)
###     - ensure topo is needed with a warning if meteo has topo and topo is added DONE
###     - fix NA logical which causes error in interpolator creation. If a variable
###       is present and is logical (all NA not NA_integer_ nor NA_real_) then
###       transform to NA_real_ DONE
###     - Precip and Rad not interpolated correctly. Possible causes
###         - ~~transposition of smoothed vars~~ Not this
###         - creation of smoothed vars DONE
###         - differences of 1e-05 in correct vars is due to differences in Rp parameter
###         - Precip, some values interpolate to 0 instead of the value in Miquels method.
###             - This is SOLVED. I was doing the precip interpolation incorrectly
###               (some params were missing). DONE
###     - In raster method, aspect and slope, if they exist, are not copied to
###       results object DONE
###     - What happens when trying to read an nc object that is not an interpolator??
###       informative error please. DONE
###     - Informative warning when no aspect or slope is registered, as the results
###       can be really different DONE
### 1. Add complete_meteo method
###     - add complete_meteo method DONE
###     - update meteospain2meteoland method
###       - catch all variables DONE
###       - if is subdaily, aggregate method must be trigger DONE
###     - add tests DONE
###     - check results with old method DONE
###     - fix error in aggregating when variables are not present (only aemet for example) DONE
### 1. BUG in create_meteo_interpolator
###     - When station metadata changes (specifically the geometry) it can result in two lines
###       for the same station in the arranged data (one for each geometry), but with the same data.
###       In the specific case of the discovery of the bug, it comes from the aggregating test suite,
###       where one of the aemet stations changes the geometry and the elevation in the middle of the
###       day.
###     - Two rows for the same station with different geometries mess with the dimensions of the
###       arrays. There are more geometries than geometries names :(
###     - Fix should be done maybe in meteospain2meteoland, as for others, user must ensure
###       by themselves the quality of the data. For this last point, a check in with_meteo
###       must be added to check that station IDs are unique with only one geometry.
###       - new check for meteo objects DONE
###       - remove duplicated maintaining the latest metadata (geometry) DONE
###       - add new tests for this
###       - This is all for subdaily, check what happens when this happen in daily DONE
### 1. Tests round
###     - add tests for unique id checks DONE
###     - add tests for subdaily (aggregation) in meteospain DONE
###     - add tests for complete meteospain
###       - daily DONE
###       - subdaily DONE
###     - add tests for fix not unique id checks in meteospain DONE
### 1. Add interpolation cross validation and calibration routines
###    (maybe temporal resolution also) DONE
###     - calibration process.
###       - Fix bug in interpolator_calibration when station numeric vector is provided DONE
###       - If we are on it, then improvements:
###           - switch argument to return the interpolator with the parameters updated, instead of the
###             calibration results DONE
###           - accept also a vector of stations names. When numeric, use indexes, when character, use
###             the name of the station to get the index (check for wrong names) DONE
###           - add colnames and rownames to predicted matrix (rownames (dates) also in the
###             observed matrix) DONE
###           - perform all variables calibration (or at least more than one) NOTFIX
###       - check results with old method DONE
###     - cross validation
###       - logic DONE
###       - messaging (remove interpolation messages and add custom ones for the
###         cross validation routine) DONE
###       - accept also a vector of stations names. When numeric, use indexes, when character, use
###         the name of the station to get the index (check for wrong names) DONE
###       - convert the station index in the results in the station name by the interpolator. Added as
###       stationID, maintaining also station index. DONE
###       - add tests DONE
###       - check results with old method
###         - radiation, radiation biases are different.
###           This is happening because in my method the interpolator is created with NAs for
###           the aspect and slope vars if they are not present in the meteo. In Miquel's they are
###           filled with 0s which makes the difference in the Radiation calculation.
###           To fix this I have to create the aspect and the slope with zeros DONE
### 1. aggregating in meteospain2meteoland: DONE
###       - truncate radiation values to 0 (no negative values) DONE
### 1. Add wind logic to interpolation process
###     - Add logic DONE
###     - Check results with old method DONE
### 1. Fixes
###     - Humidity interpolation is wrong when no humidity in meteo is supplied.
###       I can't really test this because bug in old method. .interpolationPointSeries
###       only check if interpolator@RelativeHumidity is NULL, but it can't be NULL ever,
###       as the class check forces it to be a NA's matrix :(
###       Fixed in the old one, checked with the new and same results. DONE
###     - Check with Miquel which are the mandatory variables for meteo (only temperatures?
###       or also precipitation) NOTFIX
### 1. Generalize complete method (not only meteospain) and tests DONE
###       - The method is already generalize. We start from a meteo sf based on the with_meteo
###         specifications, so is very straightforward. complete_meteo can be used in any
###         meteo object complying.
###       - Test for this are already in the meteospain2meteoland tests so no need to add more
###         for the moment
### 1. Add a params setter for interpolators DONE

#### Road to meteoland 2.0.0 ####
#### TODO list
### 1. Add deprecation notices to deprecated functions DONE
### 1. Move docs to roxygen2, also C functions DONE
###     - NAMESPACE DONE
###     - Build and doc with roxygen DONE
###     - Roxygenize functions DONE
###     - Check all exported functions are still exported (check with old namespace) DONE
###     - Classes methods also exported DONE
###     - Fix bad formatting of items in the Rd to roxygen conversion DONE
### 1. Clean and organize the new functions in files DONE
### 1. Create example data for new logic and workflows
###     - interpolator DONE
###     - meteo data DONE
###     - topo data
###         - points DONE
###         - raster (stars) DONE
### 1. Create and test examples for docs DONE
### 1. Implement worldmet2meteoland method
###     - create a reshaping general framework DONE
###     - document reshaping framework DONE
###     - test reshaping framework DONE
###     - create dictionary for meteospain DONE
###     - update meteospain2meteoland DONE
###     - create dictionary for worldmet DONE
###     - update/create worldmet2meteoland DONE
###     - test worldmet2meteoland DONE
###     - clean and organize the reshape framework function in files DONE
### 1. Clean and organize tests in files
###     - reorganize tests in files DONE
###     - data for testing DONE
###         - meteospain data for reshape framework tests DONE
###         - worldmet data for reshape framework tests DONE
###         - mixed data for meteo and topo, interpolator and interpolation tests DONE
###     - test infra for testing both mixed and individual origin data
###         - infra for both mixed DONE
###     - add dates tests to interpolation DONE
### 1. Add verbosity control to functions DONE
###     - create helper function to check verbosity DONE
###     - accept session option "meteoland_verbosity" DONE
###     - add verbosity checks to all usethis::ui_info, usethis::ui_todo... (NOT warnings or errors) DONE
###     - add tests for the verbosity control and workflows DONE
### 1. Update dependencies (usethis::use_package) DONE (more will come with R CMD CHECK)
### 1. Ask Miquel
###     - if interpolator.coverage must be implemented NOPE
###     - if precipitation concentration is ok and then check the results with him
###         - precipitation_concentration deprecated
###         - precipitation_rainfallErosivity maintain
###     - if weathergeneration utils (dot knn and others) should be exported, NO, so is ok
###     - if humidity_dewtemperature2relative doesnt throw error in non numeric arguments by design TO CHECK
###     - which c exported methods must be maintained, which ones must be deprecated and which tested
### 1. Create new methods for summarypoint and related due to it being need for precipitation_rainfallErosivity
###     - summarypoint, summarypoints DONE
###     - summaryraster DONE
###     - summaryinterpolator DONE
###     - add tests DONE
###     - add function names in old summary functions deprecation notices DONE
### 1. Consistency between utils.R functions (all same errors and whatnot) DONE
### 1. Ask Miquel:
###     - interpolate_* c exports doesn't make sense as it is, as expect vectors that user dont have
###       anymore. Let's see options:
###         - don't export them:
###           - interpolate_data can be adapted to accept a variable argument that allows selecting only one
###           - create wrapper functions and export this ones. The wrapper functions will adapt an sf and a
###             interpolator object to build the inputs for the c function. I don't like this as is extra
###             work as it is done already in interpolate_data
###         - export them:
###           - as is. The user will fight for build the inputs from its own data (ufff)
###           - as is, but create helper functions to extract the inputs (meh, adapting
###             interpolate_data is still better)
###     ANSWER: Implement variable argument in interpolate_data, but maintain lowlevel exported with a
###     big warning or message
### 1. Explore terra instead of stars/ncdfgeom/ncmeta for interpolators NOPE
### 1. Implement variable argument in interpolate_data
###     - logic DONE
###     - tests DONE
### 1. Add tests
###     - New functions (check existent tests) DONE
###     - Old functions no deprecated
###         - utils DONE
###         - C functions
###           - utils_* DONE
###           - radiation_* DONE
###           - interpolation_* DONE
###               - tests for exported functions individually DONE
###               - tests for consistency between interpolation process and individual functions DONE
### 1. Update docs for interpolation_* lowlevel functions DONE
###     - deprecation notice and details DONE
###     - examples with the new system DONE
### 1. precipitation_rainfallErosivity
###     - Separate docs from precipitation_concentration DONE
###     - Deprecate precipitation_concentration DONE
###     - Maintain precipitation_rainfallErosivity and add tests
###         - Create methods for summarypoint and related functions (see own point) DONE
###         - Tests for summarising multiyear data (and also related functions) DONE
###         - Add recursion for using it in tests dONE
###     - in summarise methods, asserting for meteo names can generate problems when
###       using summarise functions inside others like precipitation_rainfall_erosivity.
###       It is better to check for names of variables to summarise in the data I think. Look at it!! DONE
### 1. summarise_* methods DONE
###     - Add multiyear tests (or at least check) NOPE, already done in rainfall
###     - Add recursion to be able to use the methods in mutate NOPE, is better do it in the way is done now
###     - Add tests for recursion NOPE see above (and tests already done for the actual logic)
### 1. Names of generated columns in summarise_* functions DONE
###     - it should create names based on the frequency and the function, that way we can pipe more than one
###       summarise step and create different summaries DONE
### 1. Units in example raster attributes DONE
# 1. Add vignette with the new logic and workflows
#     - vignette with equivalences old-new DONE
#     - vignette with workflows DONE
#     - vignette with meteo acquisition, reshaping and completing
# 1. Ask Miquel
#     - The big question, topology or topography????
