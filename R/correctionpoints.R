#' Low-level correction functions
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' Low-level function to perform bias correction.
#'
#'
#' @aliases correction_series
#' @param obs Observed series for the reference (historical) period.
#' @param mod Modelled series for the reference (historical) period.
#' @param proj Modelled series for the projected period. If missing, the
#' reference (historical) period is corrected.
#' @param method Correction method, either \code{"unbias"}, \code{"scaling"},
#' \code{"quantmap"}
#' @param isPrec A flag to indicate that variable is precipitation (only
#' relevant for quantile mapping).
#' @param qstep Probability step for quantile mapping (see
#' \code{\link{defaultCorrectionParams}}).
#' @return Returns a vector with corrected values.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{correctionpoints}},
#' \code{\link{defaultCorrectionParams}}
#' @references De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018)
#' Estimating daily meteorological data and downscaling climate models over
#' landscapes. Environmental Modelling and Software 108: 186-196.
#' @export
correction_series<-function(obs, mod, proj = NULL, method = "unbias", isPrec=TRUE, qstep=0.01) {
  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "correction_series()", with = NULL,
    details = "Correction methods and data classes are deprecated.
    Better bias correction methods are provided by other packages (see package `MBC` for example)"
  )

  # if(method=="unbias") {
  #   corr<-mean(obs-mod, na.rm=TRUE)
  # } else if(method=="scaling") {
  #   corr<- as.numeric(lm(obs~mod-1)$coefficients) #slope of a regression through the origin
  # } else if(method=="quantmap") {
  #   corr<-fitQmapDeque(obs,mod, isPrec, qstep)
  # }
  # if(!is.null(proj)) return(.corrApply(proj, corr, method))
  # return(.corrApply(mod, corr, method))
}
# Bias-correction of all variables for a single point
#' @describeIn correctionpoints `r lifecycle::badge('deprecated')`
#' @export
correctionpoint<-function(obs, mod, proj, dates = NULL, params = defaultCorrectionParams(), verbose=TRUE){

  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "correctionpoint()", with = NULL,
    details = "Correction methods and data classes are deprecated.
    Better bias correction methods are provided by other packages (see package `MBC` for example)"
  )

  # #Calculate mean temperature if absent
  # if(!("MeanTemperature" %in% names(obs))) {
  #   obs$MeanTemperature = 0.606*obs$MaxTemperature+0.394*obs$MinTemperature
  # }
  #
  # #Call statistical correction routine
  # mbias = .monthbiasonepoint(obs,mod,
  #                            params$varmethods,
  #                            qstep = params$qstep,
  #                            verbose = verbose)
  #
  # # if(verbose) print(mbias)
  # df = .correctiononepoint(mbias,proj, dates,
  #                          fill_wind = params$fill_wind,
  #                          allow_saturated =params$allow_saturated,
  #                          verbose = verbose)
  # return(list(monthlyBias = mbias, correctedProj = df))
}
# Bias-correction of all variables for multiple points


#' Statistical correction of meteorological variables for a set of points
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' Functions \code{correctionpoint} and \code{correctionpoints} perform
#' correction of predicted climatic data by applying statistical correction
#' methods (unbiasing, scaling, or quantile mapping) to meteorological
#' variables. Function \code{correctionpoints.errors} allows evaluating, for
#' each point, the bias and mean absolute error (MAE) obtained before and after
#' correcting the climate model for the historical period.
#'
#' @details
#' Function \code{correctionpoints} performs statistical correction of
#' predicted climatic data for all points supplied in \code{points} whereas
#' \code{correctionpoint} performs statistical correction of one single point.
#' Observed meteorological data for each point typically comes from a nearby
#' meteorological station, but they can be the result of interpolating the
#' meteorology of several stations (see
#' \code{\link{MeteorologyInterpolationData}}) or they can be extracted from
#' reanalyzed meteorology (e.g. EU-WATCH) (see \code{\link{extractNetCDF}}).
#'
#' For each target point, \code{correctionpoints} function first determines the
#' predicted cell where the point falls according to the euclidean distance in
#' the geographic space of \code{object}. Then it calls \code{correctionpoint}.
#' In turn, \code{correctionpoint} determines the dates that are shared in
#' observed and predicted data for the historical period. These meteorological
#' data of dates are used to conduct the correction of predicted climatic data
#' for the future period. Corrections biases are calculated and applied for the
#' twelve months separately. The user can control the methods used for
#' correction of each meteorological variable by changing the slot
#' \code{params} in \code{object} (see class
#' \code{\link{MeteorologyUncorrectedData-class}}) or the parameter
#' \code{params} to \code{correctionpoint}. Three options are allowed (see
#' \code{\link{defaultCorrectionParams}}): (a) 'unbias' for shifting the mean;
#' (b) 'scaling' for multiplication by a factor; and (c) 'quantmap' for
#' empirical quantile mapping between observed and modelled data
#' (\enc{Déqué}{Deque} 2007).
#'
#' A difficulty arises for quantile mapping when the variables bounded by zero,
#' such as precipitation. As the models tend to drizzle (or may have lower
#' frequency of precipitation events), the probability of precipitation in the
#' model may be greater or lower than that observed. To correct this, when
#' model precipitation is zero an observed value is randomly chosen in the
#' interval where the observed cumulative frequency is less than or equal to
#' the probability of no precipitation in the model. This procedure ensures
#' that the probability of precipitation after correction is equal to that
#' observed (\enc{Boé}{Boe} 2007).
#'
#' @aliases correctionpoint correctionpoints correctionpoints.errors
#' @param obs A data frame with observed meteorology.
#' @param mod,proj Data frame with predicted meteorology for the reference and
#' projection periods, respectively.
#' @param params A list with correction params (see
#' \code{\link{defaultCorrectionParams}}).
#' @param object An object of class
#' \code{\link{MeteorologyUncorrectedData-class}} containing the meteorology of
#' more than one point.
#' @param points An object of class
#' \code{\link{SpatialPointsMeteorology-class}} with the coordinates and
#' historical meteorological data of the locations for which correction of
#' predicted climatic data has to be done. Alternatively, an object of class
#' \code{SpatialPointsDataFrame} containing the meta data (columns
#' \code{dir}, \code{filename} and possibly \code{format}) of meteorological
#' files that will be read from the disk.
#' @param topodata A data frame with topographic data for each point (i.e.
#' three columns named \code{elevation}, \code{slope} and \code{aspect}). If
#' \code{topodata = NULL} then Penman's potential evapotranspiration is not
#' calculated.
#' @param dates An object of class \code{\link{Date}} with a subset of dates of
#' the projection period to be corrected. If \code{dates = NULL} then all dates
#' in \code{proj} or the projection data of \code{object} are processed.
#' @param export If \code{export = FALSE} the result of correction is stored in
#' memory. Otherwise the result is written in the disk (using the format
#' specified in \code{exportFormat}).
#' @param exportDir Output directory for corrected meteorology data (txt/rds
#' format).
#' @param exportFile Output file for corrected meteorology data (netCDF
#' format).
#' @param metadataFile The name of the file that will store the meta data
#' describing all written files.
#' @param exportFormat Export format for meteorological data (see
#' \code{\link{writemeteorologypoint}}).  If format is \code{"meteoland/txt"},
#' \code{"meteoland/rds"}, \code{"castanea/txt"} or \code{"castanea/rds"} the
#' function tries to write one file per point in \code{exportDir}. If format is
#' \code{"netCDF"} the function will write data to a single file specified by
#' \code{exportFile}.
#' @param corrOut Boolean flag to indicate that correction parameters (i.e.
#' calculated biases) should be included with the output. Setting \code{corrOut
#' = TRUE} changes the returned value.
#' @param verbose Boolean flag to print process information.
#' @param error.type String to specify the error to be evaluated, either
#' \code{"before"} (before correction), \code{"residual"} (after correction) or
#' \code{"residual.cv"} (after correction, but using cross-validation).
#' @param keep.data Boolean flag to return the uncorrected/corrected data for
#' the historical period.
#' @return \itemize{ \item{Function \code{correctionpoint} returns a data frame.}
#' \item{If \code{export = FALSE}, the function \code{correctionpoints} returns
#' an object of class \code{\link{SpatialPointsMeteorology-class}} with the
#' bias-corrected meteorology for each point. If \code{export=TRUE} then
#' bias-corrected data is written into the disk. For txt/rds export formats,
#' the function returns an object of class
#' \code{SpatialPointsDataFrame} containing the meta data of the
#' files written in the disk. For netCDF export format the function returns
#' \code{NULL}. If \code{corrOut = TRUE} the function returns a list which
#' contains any previous output and an object with the calculated correction
#' factors (biases, mappings) for each point and month.} \item{Function
#' \code{correctionpoints.errors} (\code{keep.data = FALSE}) returns a data
#' frame with the mean absolute error (MAE) and bias for each variable and
#' point. If \code{keep.data = TRUE} then the function also returns a list of
#' data frames with the uncorrected/corrected series used in the comparisons
#' with observations.} }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#'
#' Nicolas Martin, INRA-Avignon
#' @seealso \code{\link{penman}}, \code{\link{SpatialPointsMeteorology-class}},
#' \code{\link{writemeteorologypointfiles}},
#' \code{\link{MeteorologyUncorrectedData}},
#' \code{\link{MeteorologyInterpolationData}}
#' @references \enc{Boé}{Boe} J, Terray L, Habets F, Martin E (2007) Statistical
#' and dynamical downscaling of the Seine basin climate for
#' hydro-meteorological studies. Int J Climatol 27:1643–1655. doi:
#' 10.1002/joc.1602
#'
#' De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating
#' daily meteorological data and downscaling climate models over landscapes.
#' Environmental Modelling and Software 108: 186-196.
#'
#' \enc{Déqué}{Deque} M (2007) Frequency of precipitation and temperature
#' extremes over France in an anthropogenic scenario: Model results and
#' statistical correction according to observed values. Glob Planet Change
#' 57:16–26. doi: 10.1016/j.gloplacha.2006.11.030
#'
#' @export
correctionpoints<-function(object, points, topodata = NULL, dates = NULL, export = FALSE,
                           exportDir = getwd(), exportFile = NULL, exportFormat = "meteoland/txt",
                           metadataFile = "MP.txt", corrOut = FALSE, verbose=TRUE) {

  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "correctionpoints()", with = NULL,
    details = "Correction methods and data classes are deprecated.
    Better bias correction methods are provided by other packages (see package `MBC` for example)"
  )

  # if(export) exportFormat = match.arg(exportFormat, c("meteoland/txt", "meteoland/rds", "castanea/txt", "castanea/rds", "netCDF"))
  #
  # #Check input classes
  # if(!inherits(object,"MeteorologyUncorrectedData")) stop("'object' has to be of class 'MeteorologyUncorrectedData'.")
  # if(!inherits(points,"SpatialPointsMeteorology") && !inherits(points,"SpatialPointsDataFrame")) stop("'points' has to be of class 'SpatialPointsMeteorology' or 'SpatialPointsDataFrame'.")
  #
  # mPar = object@params
  #
  # npoints = length(points)
  #
  # if(verbose) cat(paste("Points to correct: ", npoints,"\n", sep=""))
  #
  # #Project points into long/lat coordinates to check if they are inside the boundary box
  # cchist = spTransform(points, object@proj4string)
  # sel = (cchist@coords[,1] >= object@bbox[1,1] & cchist@coords[,1] <=object@bbox[1,2]) &
  #   (cchist@coords[,2] >= object@bbox[2,1] & cchist@coords[,2] <=object@bbox[2,2])
  # if(sum(sel)<npoints) {
  #   warning("At least one target point is outside the boundary box of 'object'.\n", call. = FALSE, immediate.=TRUE)
  # } else if(verbose) cat(paste("All points inside boundary box.\n", sep=""))
  #
  # longlat = spTransform(points,CRS(SRS_string = "EPSG:4326"))
  # latitude = longlat@coords[,2]
  #
  # #Project long/lat coordinates of predicted climatic objects into the projection of points
  # xypred = spTransform(SpatialPoints(object@coords,object@proj4string,object@bbox), points@proj4string)
  # colnames(xypred@coords)<-c("x","y")
  # if(!is.null(topodata)) {
  #   latrad = latitude*(pi/180)
  #   elevation = topodata$elevation
  #   slorad = topodata$slope*(pi/180)
  #   asprad = topodata$aspect*(pi/180)
  # }
  # # Define vector of data frames
  # mbiasvec = vector("list", npoints)
  #
  # if(inherits(points,"SpatialPointsMeteorology")) {
  #   if(!is.null(names(points@data))) ids = names(points@data)
  #   else ids = 1:npoints
  # } else {
  #   if(!is.null(rownames(points@data))) ids = rownames(points@data)
  #   else ids = 1:npoints
  # }
  #
  # if(exportFormat %in% c("meteoland/txt","castanea/txt")) formatType = "txt"
  # else if (exportFormat %in% c("meteoland/rds","castanea/rds")) formatType = "rds"
  # else if (exportFormat %in% c("netCDF")) formatType = "netCDF"
  #
  #
  # if(export & exportFormat %in% c("meteoland/txt","castanea/txt", "meteoland/rds","castanea/rds")) {
  #   dfout = data.frame(dir = rep(exportDir, npoints), filename=paste0(ids,".", formatType))
  #   dfout$dir = as.character(dfout$dir)
  #   dfout$filename = as.character(dfout$filename)
  #   dfout$format = exportFormat
  #   rownames(dfout) = ids
  #   spdf = SpatialPointsDataFrame(as(points,"SpatialPoints"), dfout)
  #   colnames(spdf@coords)<-c("x","y")
  # }
  # else if(export & exportFormat=="netCDF") {
  #   if(is.null(exportFile)) stop("File 'exportFile' cannot be null when exporting to netCDF!")
  #   ncfile = exportFile
  #   if(is.null(dates)) dates = object@dates
  #   nc <-.openwritepointNetCDF(coordinates(points), proj4string(points), dates = dates, vars = NULL,
  #                              file = ncfile, overwrite = TRUE, verbose = verbose)
  # } else {
  #   dfvec = vector("list",npoints)
  # }
  #
  # #Loop over all points
  # for(i in 1:npoints) {
  #   if(verbose) cat(paste("Correcting point '",ids[i],"' (",i,"/",npoints,") -",sep=""))
  #   xy = points@coords[i,]
  #   #observed data frame
  #   if(inherits(points,"SpatialPointsMeteorology")) {
  #     obs = points@data[[i]]
  #   } else {
  #     f = paste(points@data$dir[i], points@data$filename[i],sep="/")
  #     if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
  #     if("format" %in% names(points@data)) { ##Format specified
  #       obs = readmeteorologypoint(f, format=points@data$format[i])
  #     } else {
  #       obs = readmeteorologypoint(f)
  #     }
  #   }
  #   #Find closest predicted climatic cell for reference/projection periods (ideally the same)
  #   d = sqrt(rowSums(sweep(xypred@coords,2,xy,"-")^2))
  #   ipred = which.min(d)[1]
  #   if(verbose) cat(paste(" ipred = ",ipred, sep=""))
  #   #predicted climatic data frames
  #   if(inherits(object@reference_data,"list")) {
  #     rcmhist = object@reference_data[[ipred]]
  #   } else if(inherits(object@reference_data,"character")) {
  #     rcmhist = readmeteorologypoints(object@reference_data, stations = ipred)@data[[1]]
  #   } else {
  #     if(("dir" %in% names(object@reference_data))&&("filename" %in% names(object@reference_data))) {
  #       f = paste(object@reference_data$dir[ipred], object@reference_data$filename[ipred],sep="/")
  #       if(!file.exists(f)) stop(paste("Reference meteorology file '", f,"' does not exist!", sep=""))
  #       if("format" %in% names(object@reference_data)) { ##Format specified
  #         rcmhist = readmeteorologypoint(f, format=object@reference_data$format[ipred])
  #       } else {
  #         rcmhist = readmeteorologypoint(f)
  #       }
  #     } else if(nrow(object@coords)==1) {
  #       rcmhist = object@reference_data
  #     } else {
  #       stop("Cannot access reference meteorology data")
  #     }
  #   }
  #   if(inherits(object@projection_data,"list")) {
  #     rcmfut = object@projection_data[[ipred]]
  #   } else if(inherits(object@projection_data,"character")) {
  #     rcmfut = readmeteorologypoints(object@projection_data, stations = ipred)@data[[1]]
  #   } else {
  #     if(("dir" %in% names(object@projection_data))&&("filename" %in% names(object@projection_data))) {
  #       f = paste(object@projection_data$dir[ipred], object@projection_data$filename[ipred],sep="/")
  #       if(!file.exists(f)) stop(paste("Projection meteorology file '", f,"' does not exist!",sep=""))
  #       if("format" %in% names(object@projection_data)) { ##Format specified
  #         rcmfut = readmeteorologypoint(f, format=object@projection_data$format[ipred])
  #       } else {
  #         rcmfut = readmeteorologypoint(f)
  #       }
  #     } else if(nrow(object@coords)==1) {
  #       rcmfut = object@projection_data
  #     } else {
  #       stop("Cannot access projection meteorology data")
  #     }
  #   }
  #
  #   #Call statistical correction routine
  #   res = correctionpoint(obs, rcmhist, rcmfut, dates, mPar, verbose)
  #   df = res$correctedProj
  #   mbiasvec[[i]] = res$monthlyBias
  #
  #   if(is.null(dates)) dates = as.Date(rownames(df))
  #   #Calculate PET
  #   if(!is.null(topodata)) {
  #     J = radiation_dateStringToJulianDays(as.character(dates))
  #     df$PET = .penmanpoint(latrad[i], elevation[i],slorad[i], asprad[i], J,
  #                        df$MinTemperature, df$MaxTemperature,
  #                        df$MinRelativeHumidity, df$MaxRelativeHumidity, df$Radiation,
  #                        df$WindSpeed, mPar$wind_height,
  #                        0.001, 0.25);
  #   }
  #
  #   #Write file
  #   if(!export) {
  #     dfvec[[i]] =df
  #     if(verbose) cat(" done")
  #   } else {
  #     if(exportFormat!="netCDF") {
  #       if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
  #       else f = dfout$filename[i]
  #       writemeteorologypoint(df, f, dfout$format[i])
  #       if(verbose) cat(paste(" written to ",f, sep=""))
  #       if(exportDir!="") f = paste(exportDir,metadataFile, sep="/")
  #       else f = metadataFile
  #       write.table(as.data.frame(spdf),file= f,sep="\t", quote=FALSE)
  #     } else {
  #       if(verbose) cat(paste0(" written to netCDF"))
  #       .writemeteorologypointNetCDF(df,nc,i)
  #     }
  #   }
  #   if(verbose) cat(".\n")
  # }
  # if(!export) {
  #   if(!corrOut) return(SpatialPointsMeteorology(points = points, data = dfvec, dates = dates))
  #   else {
  #     return(list(SpatialPointsMeteorology(points = points, data = dfvec, dates = dates),
  #                 mbiasvec))
  #   }
  # } else {
  #   if(exportFormat!="netCDF") {
  #     if(!corrOut) invisible(spdf)
  #     else invisible(list(spdf,mbiasvec))
  #   } else {
  #     .closeNetCDF(ncfile, nc)
  #     if(corrOut) invisible(mbiasvec)
  #   }
  # }
}

