#' Calibration and validation of interpolation procedures
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Function \code{interpolation.calibration} determines optimal interpolation
#' parameters 'N' and 'alpha' for a given meteorological variable. Optimization
#' is done by minimizing mean absolute error (MAE) (Thornton et al. 1997).
#' Function \code{interpolation.cv} calculates average mean absolute errors
#' (MAE) for the prediction period of an object of class
#' '\code{MeteorologyInterpolationData}'.  Function
#' \code{summary} returns a data.frame with cross-validation
#' summaries and \code{plot} plots cross-validation results.
#' In both calibration and validation procedures, predictions for each weather
#' station are made using a leave-one-out procedure (i.e. after excluding the
#' station from the predictive set).
#'
#'
#' @param object In the case of function \code{interpolation.cv}, an object of
#' class \code{\link{MeteorologyInterpolationData-class}}. In the case of
#' function \code{summary}, an object of class \code{\link{interpolation.cv}}
#' @param stations A numeric vector containing the indices of stations to be
#' used to calculate mean absolute errors (MAE) in the calibration or
#' cross-validation analysis. All the stations with data are included in the
#' training set but predictive MAE are calculated for the 'stations' subset
#' only.
#' @param variable A string indicating the meteorological variable for which
#' interpolation parameters 'N' and 'alpha' will be calibrated. Accepted values
#' are 'Tmin' (for minimum temperature), 'Tmax' (for maximum temperature),
#' 'Tdew' (for dew-point temperature), 'PrecEvent' (for precipitation
#' events),'PrecAmount' (for regression of precipitation amounts),'Prec' (for
#' precipitation with the same values for precipitation events and regression
#' of precipitation amounts).
#' @param N_seq Set of average number of points to be tested.
#' @param alpha_seq Set of alpha values to be tested.
#' @param fmax_seq Set of f_max values to be tested.
#' @param verbose A logical flag to generate additional console output.
#' @param x A S3 object of class \code{interpolation.cv} with cross-validation
#' results.
#' @param type A string of the plot type to be produced (either "stations" or
#' "dates").
#' @param ... Additional parameters passed to summary and plot functions.
#' @return Function \code{interpolation.calibration} returns an object of class
#' \code{'interpolation.calibration'} with the following items: \itemize{
#' \item\code{MAE}: A numeric matrix with the mean absolute error values
#' (averaged across stations) for each combination of parameters 'N' and
#' 'alpha'. \item\code{minMAE}: Minimum MAE value. \item\code{N}: Value of
#' parameter 'N' corresponding to the minimum MAE. \item\code{alpha}: Value of
#' parameter 'alpha' corresponding to the minimum MAE. \item\code{Observed}: A
#' matrix with observed values. \item\code{Predicted}: A matrix with predicted
#' values for the optimum parameter combination. } Function
#' \code{interpolation.cv} returns a list of class \code{'interpolation.cv'}
#' with the following items: \itemize{ \item\code{stations}: A data frame with
#' as many rows as weather stations and the following columns: \itemize{
#' \item\code{MinTemperature-Bias}: Bias (in degrees), calculated over the
#' prediction period, of minimum temperature estimations in weather stations.
#' \item\code{MinTemperature-MAE}: Mean absolute errors (in degrees), averaged
#' over the prediction period, of minimum temperature estimations in weather
#' stations. \item\code{MaxTemperature-Bias}: Bias (in degrees), calculated
#' over the prediction period, of maximum temperature estimations in weather
#' stations. \item\code{MaxTemperature-MAE}: Mean absolute errors (in degrees),
#' averaged over the prediction period, of maximum temperature estimations in
#' weather stations. \item\code{Precipitation-Total}: Difference in the total
#' precipitation of the studied period. \item\code{Precipitation-DPD}:
#' Difference in the proportion of days with precipitation.
#' \item\code{Precipitation-Bias}: Bias (in mm), calculated over the days with
#' precipitation, of precipitation amount estimations in weather stations.
#' \item\code{Precipitation-MAE}: Mean absolute errors (in mm), averaged over
#' the days with precipitation, of precipitation amount estimations in weather
#' stations. \item\code{RelativeHumidity-Bias}: Bias (in percent), calculated
#' over the prediction period, of relative humidity estimations in weather
#' stations. \item\code{RelativeHumidity-MAE}: Mean absolute errors (in
#' percent), averaged over the prediction period, of relative humidity
#' estimations in weather stations. \item\code{Radiation-Bias}: Bias (in
#' MJ/m2), calculated over the prediction period, of incoming radiation
#' estimations in weather stations. \item\code{Radiation-MAE}: Mean absolute
#' errors (in MJ/m2), averaged over the prediction period, of incoming
#' radiation estimations in weather stations. }
#'
#' \item\code{dates}: A data frame with as many rows as weather stations and
#' the following columns: \itemize{ \item\code{MinTemperature-Bias}: Daily bias
#' (in degrees), averaged over the stations, of minimum temperature
#' estimations. \item\code{MinTemperature-MAE}: Daily mean absolute error (in
#' degrees), averaged over the stations, of minimum temperature estimations.
#' \item\code{MaxTemperature-Bias}: Daily bias (in degrees), averaged over the
#' stations, of maximum temperature estimations.
#' \item\code{MaxTemperature-MAE}: Daily mean absolute error (in degrees),
#' averaged over the stations, of maximum temperature estimations.
#' \item\code{Precipitation-Bias}: Daily bias (in mm), averaged over the
#' stations, of precipitation amount estimations.
#' \item\code{Precipitation-MAE}: Daily mean absolute error (in mm), averaged
#' over the stations, of precipitation amount estimations.
#' \item\code{RelativeHumidity-Bias}: Daily bias (in percent), averaged over
#' the stations, of relative humidity estimations.
#' \item\code{RelativeHumidity-MAE}: Daily mean absolute error (in percent),
#' averaged over the stations, of relative humidity estimations.
#' \item\code{Radiation-Bias}: Daily bias (in MJ/m2), averaged over the
#' stations, of incoming radiation estimations. \item\code{Radiation-MAE}:
#' Daily mean absolute errors (in MJ/m2), averaged over the stations, of
#' incoming radiation estimations. }
#'
#' \item\code{MinTemperature}: A data frame with predicted minimum temperature
#' values. \item\code{MinTemperatureError}: A matrix with predicted minimum
#' temperature errors. \item\code{MaxTemperature}: A data frame with predicted
#' maximum temperature values. \item\code{MaxTemperatureError}: A matrix with
#' predicted maximum temperature errors. \item\code{Precipitation}: A data
#' frame with predicted precipitation values. \item\code{PrecipitationError}: A
#' matrix with predicted precipitation errors. \item\code{RelativeHumidity}: A
#' data frame with predicted relative humidity values.
#' \item\code{RelativeHumidityError}: A matrix with predicted relative humidity
#' errors. \item\code{Radiation}: A data frame with predicted radiation values.
#' \item\code{RadiationError}: A matrix with predicted radiation errors. }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyInterpolationData}}
#' @references Thornton, P.E., Running, S.W., 1999. An improved algorithm for
#' estimating incident daily solar radiation from measurements of temperature,
#' humidity, and precipitation. Agric. For. Meteorol. 93, 211–228.
#' doi:10.1016/S0168-1923(98)00126-9.
#'
#' De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating
#' daily meteorological data and downscaling climate models over landscapes.
#' Environmental Modelling and Software 108: 186-196.
#' @examples
#'
#' \donttest{
#' data(exampleinterpolationdata)
#'
#' #Calibration procedure
#' precEv_cal = interpolation.calibration(exampleinterpolationdata, variable="PrecEvent",
#'                                        stations = 1:5,
#'                                        N_seq=c(5,10,15), alpha_seq=seq(0.25,1.0, by=0.25),
#'                                        verbose = TRUE)
#'
#' precAm_cal = interpolation.calibration(exampleinterpolationdata, variable="PrecAmount",
#'                                        stations = 1:5,
#'                                        N_seq=c(5,10,15), alpha_seq=seq(0.25,1.0, by=0.25),
#'                                        verbose = TRUE)
#'
#' #Set 'alpha' and 'N' parameters to values found in calibration
#' exampleinterpolationdata@params$N_PrecipitationEvent = precEv_cal$N
#' exampleinterpolationdata@params$alpha_PrecipitationEvent = precEv_cal$alpha
#'
#' exampleinterpolationdata@params$N_PrecipitationAmount = precAm_cal$N
#' exampleinterpolationdata@params$alpha_PrecipitationAmount = precAm_cal$alpha
#'
#' #Run cross validation
#' cv = interpolation.cv(exampleinterpolationdata, stations = 1:5, verbose = TRUE)
#'
#' #Print cross validation summaries
#' summary(cv)
#'
#' #Plot results
#' plot(cv)
#' }
#'
#' @export
interpolation.cv<-function(object, stations = NULL, verbose = FALSE) {
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "interpolation.cv()", with = "interpolation_cross_validation()",
    details = "MeteorologyInterpolationData class is soft deprecated.
    Interpolator object should be created with create_meteo_interpolator() and the cross validation
    must be done with interpolation_cross_validation() "
  )

  if(!inherits(object, "MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'")
  if(is.null(stations)) stations = 1:length(object@elevation)
  points = SpatialPoints(object@coords, object@proj4string)
  codes = rownames(object@MinTemperature)[stations]
  ndates = length(object@dates)
  ndateres = matrix(0, nrow=ndates, ncol=5)
  MinTemperature = object@MinTemperature
  MinTemperature[] = NA
  MaxTemperature = object@MaxTemperature
  MaxTemperature[] = NA
  Precipitation = object@Precipitation
  Precipitation[] = NA
  RelativeHumidity = object@RelativeHumidity
  RelativeHumidity[] = NA
  Radiation = object@Radiation
  Radiation[] = NA
  if(!verbose) pb = txtProgressBar(0, stations[length(stations)], style=3)
  for(i in stations) {
    if(!verbose) setTxtProgressBar(pb, i)
    #Extract meteo values
    omint = as.numeric(object@MinTemperature[i,])
    object@MinTemperature[i,] = NA
    omaxt = as.numeric(object@MaxTemperature[i,])
    object@MaxTemperature[i,] = NA
    oprec = as.numeric(object@Precipitation[i,])
    osmprec = object@SmoothedPrecipitation[i,]
    object@Precipitation[i,] = NA
    object@SmoothedPrecipitation[i,] = NA
    orh = as.numeric(object@RelativeHumidity[i,])
    object@RelativeHumidity[i,] = NA
    orad = as.numeric(object@Radiation[i,])
    object@Radiation[i,] = NA
    if(sum(!is.na(c(oprec, orh, omaxt, omint)))==0) {
      if(verbose) {
        cat(paste("Station #",i," ", codes[i],": No observations.\n",sep=""))
      }
    } else {
      if(verbose) {
        cat(paste("Station #",i," ", codes[i],"\n",sep=""))
      }
      spt = SpatialPointsTopography(points[i],object@elevation[i],
                                    object@slope[i],
                                    object@aspect[i])
      mp = interpolationpoints(object, spt, verbose=FALSE)
      MinTemperature[i,] = mp@data[[1]]$MinTemperature
      MaxTemperature[i,] = mp@data[[1]]$MaxTemperature
      Precipitation[i,] = mp@data[[1]]$Precipitation
      RelativeHumidity[i,] = mp@data[[1]]$MeanRelativeHumidity
      Precipitation[i,] = mp@data[[1]]$Precipitation
      Radiation[i,] = mp@data[[1]]$Radiation
    }
    #replace meteo values
    object@MinTemperature[i,] = omint
    object@MaxTemperature[i,] = omaxt
    object@Precipitation[i,] = oprec
    object@SmoothedPrecipitation[i,] = osmprec
    object@RelativeHumidity[i,] = orh
    object@Radiation[i,] = orad
  }
  #Set to NA predictions where there are no observations
  MinTemperature[is.na(object@MinTemperature)] = NA
  MaxTemperature[is.na(object@MaxTemperature)] = NA
  TemperatureRange = MaxTemperature - MinTemperature
  Precipitation[is.na(object@Precipitation)] = NA
  Radiation[is.na(object@Radiation)] = NA
  RelativeHumidity[is.na(object@RelativeHumidity)] = NA

  #Process results
  nstations = length(codes)
  mintemp_station_mae =rep(NA,nstations)
  mintemp_station_bias = rep(NA,nstations)
  mintemp_day_mae = rep(NA,ndates)
  mintemp_day_bias = rep(NA,ndates)
  mintemp_obs = as.matrix(object@MinTemperature[stations,])
  mintemp_pred = as.matrix(MinTemperature[stations,])
  mintemp_pred[is.na(mintemp_obs)] = NA
  mintemp_error = mintemp_pred-mintemp_obs
  mintemp_r2 = NA
  if(sum(!is.na(as.vector(mintemp_pred)) & !is.na(as.vector(mintemp_obs)))>0) {
    mintemp_r2 = cor(as.vector(mintemp_pred), as.vector(mintemp_obs), use="complete.obs")
  }
  maxtemp_station_mae = rep(NA,nstations)
  maxtemp_station_bias = rep(NA,nstations)
  maxtemp_day_mae = rep(NA,ndates)
  maxtemp_day_bias = rep(NA,ndates)
  maxtemp_obs = as.matrix(object@MaxTemperature[stations,])
  maxtemp_pred = as.matrix(MaxTemperature[stations,])
  maxtemp_pred[is.na(maxtemp_obs)] = NA
  maxtemp_error = maxtemp_pred-maxtemp_obs
  maxtemp_r2 = NA
  if(sum(!is.na(as.vector(maxtemp_pred)) & !is.na(as.vector(maxtemp_obs)))>0) {
    maxtemp_r2 = cor(as.vector(maxtemp_pred), as.vector(maxtemp_obs), use="complete.obs")
  }
  temprange_station_mae = rep(NA,nstations)
  temprange_station_bias = rep(NA,nstations)
  temprange_day_mae = rep(NA,ndates)
  temprange_day_bias =rep(NA,ndates)
  temprange_obs = maxtemp_obs - mintemp_obs
  temprange_pred = maxtemp_pred - mintemp_pred
  temprange_error = temprange_pred - temprange_obs
  temprange_r2 = NA
  if(sum(!is.na(as.vector(temprange_pred)) & !is.na(as.vector(temprange_obs)))>0) {
    temprange_r2 = cor(as.vector(temprange_pred), as.vector(temprange_obs), use="complete.obs")
  }

  rh_station_mae = rep(NA,nstations)
  rh_station_bias = rep(NA,nstations)
  rh_day_mae = rep(NA,ndates)
  rh_day_bias = rep(NA,ndates)
  rh_obs = as.matrix(object@RelativeHumidity[stations,])
  rh_pred = as.matrix(RelativeHumidity[stations,])
  rh_pred[is.na(rh_obs)] = NA
  rh_error = rh_pred-rh_obs
  rh_r2 = NA
  if(sum(!is.na(as.vector(rh_pred)) & !is.na(as.vector(rh_obs)))>0) {
    rh_r2 = cor(as.vector(rh_pred), as.vector(rh_obs), use="complete.obs")
  }
  rad_station_mae = rep(NA,nstations)
  rad_station_bias = rep(NA,nstations)
  rad_day_mae = rep(NA,ndates)
  rad_day_bias = rep(NA,ndates)
  rad_obs = as.matrix(object@Radiation[stations,])
  rad_pred = as.matrix(Radiation[stations,])
  rad_pred[is.na(rad_obs)] = NA
  rad_error = rad_pred-rad_obs
  rad_r2 = NA
  if(sum(!is.na(as.vector(rad_pred)) & !is.na(as.vector(rad_obs)))>0) {
    rad_r2 = cor(as.vector(rad_pred), as.vector(rad_obs), use="complete.obs")
  }

  prec_obs = as.matrix(object@Precipitation[stations,])
  prec_pred = as.matrix(Precipitation[stations,])
  prec_pred[is.na(prec_obs)] = NA
  prec_error = prec_pred-prec_obs
  totalprec_station_relbias = rep(NA,nstations)
  totalprec_station_bias = rep(NA,nstations)
  precdays_station_bias =rep(NA,nstations)
  precdays_station_relbias = rep(NA,nstations)
  totalprec_day_relbias = rep(NA,ndates)
  totalprec_day_bias = rep(NA,ndates)
  precstations_day_bias =rep(NA,ndates)
  precstations_day_relbias = rep(NA,ndates)
  totalprec_stations_obs = rowSums(prec_obs,na.rm=TRUE)
  totalprec_stations_obs[totalprec_stations_obs==0] = NA
  totalprec_days_obs = colSums(prec_obs,na.rm=TRUE)
  totalprec_days_obs[totalprec_days_obs==0]= NA
  totalprec_stations_pred = rowSums(prec_pred,na.rm=TRUE)
  totalprec_stations_pred[totalprec_stations_pred==0] = NA
  totalprec_days_pred = colSums(prec_pred,na.rm=TRUE)
  totalprec_days_pred[totalprec_days_pred==0] = NA
  precfreq_stations_obs = rowMeans(prec_obs>0,na.rm=TRUE)
  precfreq_stations_obs[is.na(totalprec_stations_obs)] = NA
  precfreq_days_obs = colMeans(prec_obs>0,na.rm=TRUE)
  precfreq_days_obs[is.na(totalprec_days_obs)]=NA
  precfreq_stations_pred = rowMeans(prec_pred>0,na.rm=TRUE)
  precfreq_stations_pred[is.na(totalprec_stations_pred)] = NA
  precfreq_days_pred = colMeans(prec_pred>0,na.rm=TRUE)
  precfreq_days_pred[is.na(totalprec_days_pred)] = NA


  for(p in 1:nstations) {
    mintemp_station_bias[p] = mean(mintemp_error[p,],na.rm=TRUE)
    mintemp_station_mae[p] = mean(abs(mintemp_error[p,]),na.rm=TRUE)
    maxtemp_station_bias[p] = mean(maxtemp_error[p,],na.rm=TRUE)
    maxtemp_station_mae[p] = mean(abs(maxtemp_error[p,]),na.rm=TRUE)
    temprange_station_bias[p] = mean(temprange_error[p,],na.rm=TRUE)
    temprange_station_mae[p] = mean(abs(temprange_error[p,]),na.rm=TRUE)
    rh_station_bias[p] = mean(rh_error[p,],na.rm=TRUE)
    rh_station_mae[p] = mean(abs(rh_error[p,]),na.rm=TRUE)
    rad_station_bias[p] = mean(rad_error[p,],na.rm=TRUE)
    rad_station_mae[p] = mean(abs(rad_error[p,]),na.rm=TRUE)
    if(!is.na(totalprec_stations_pred[p])) {
      totalprec_station_bias[p] = sum(prec_pred[p,],na.rm=TRUE)-sum(prec_obs[p,],na.rm=TRUE)
      totalprec_station_relbias[p] = 100*(sum(prec_pred[p,],na.rm=TRUE)-sum(prec_obs[p,],na.rm=TRUE))/sum(prec_obs[p,],na.rm=TRUE)
      if(is.na(totalprec_station_relbias[p])) totalprec_station_relbias[p] = NA
      precdays_station_bias[p] =sum(prec_pred[p,]>0,na.rm=TRUE)-sum(prec_obs[p,]>0,na.rm=TRUE)
      precdays_station_relbias[p] = 100*(sum(prec_pred[p,]>0,na.rm=TRUE)-sum(prec_obs[p,]>0,na.rm=TRUE))/sum(prec_obs[p,]>0,na.rm=TRUE)
      if(is.na(precdays_station_relbias[p])) precdays_station_relbias[p] = NA
    }
  }
  for(d in 1:ndates) {
    mintemp_day_bias[d] = mean(mintemp_error[,d],na.rm=TRUE)
    mintemp_day_mae[d] = mean(abs(mintemp_error[,d]),na.rm=TRUE)
    maxtemp_day_bias[d] = mean(maxtemp_error[,d],na.rm=TRUE)
    maxtemp_day_mae[d] = mean(abs(maxtemp_error[,d]),na.rm=TRUE)
    temprange_day_bias[d] = mean(temprange_error[,d],na.rm=TRUE)
    temprange_day_mae[d] = mean(abs(temprange_error[,d]),na.rm=TRUE)
    rh_day_bias[d] = mean(rh_error[,d],na.rm=TRUE)
    rh_day_mae[d] = mean(abs(rh_error[,d]),na.rm=TRUE)
    rad_day_bias[d] = mean(rad_error[,d],na.rm=TRUE)
    rad_day_mae[d] = mean(abs(rad_error[,d]),na.rm=TRUE)
    if(!is.na(totalprec_days_pred[d])){
      totalprec_day_bias[d] = sum(prec_pred[,d],na.rm=TRUE)-sum(prec_obs[,d],na.rm=TRUE)
      totalprec_day_relbias[d] = 100*(sum(prec_pred[,d],na.rm=TRUE)-sum(prec_obs[,d],na.rm=TRUE))/sum(prec_obs[,d],na.rm=TRUE)
      if(is.na(totalprec_day_bias[d])) totalprec_day_relbias[d] = NA
      precstations_day_bias[d] =sum(prec_pred[,d]>0,na.rm=TRUE)-sum(prec_obs[,d]>0,na.rm=TRUE)
      precstations_day_relbias[d] = 100*(sum(prec_pred[,d]>0,na.rm=TRUE)-sum(prec_obs[,d]>0,na.rm=TRUE))/sum(prec_obs[,d]>0,na.rm=TRUE)
      if(is.na(precstations_day_bias[d])) precstations_day_relbias[d] = NA
    }
  }

  r2res = list("MinTemperature" = mintemp_r2,
               "MaxTemperature" = maxtemp_r2,
               "TemperatureRange" = temprange_r2,
               "RelativeHumidity" = rh_r2,
               "Radiation"= rad_r2)
  stationsres = data.frame(MinTemperature.Bias = mintemp_station_bias,
                        MinTemperature.MAE = mintemp_station_mae,
                        MaxTemperature.Bias = maxtemp_station_bias,
                        MaxTemperature.MAE = maxtemp_station_mae,
                        TemperatureRange.Bias = temprange_station_bias,
                        TemperatureRange.MAE = temprange_station_mae,
                        RelativeHumidity.Bias = rh_station_bias,
                        RelativeHumidity.MAE = rh_station_mae,
                        Radiation.Bias = rad_station_bias,
                        Radiation.MAE = rad_station_mae,
                        PrecFreq.Obs = precfreq_stations_obs,
                        PrecFreq.Pred = precfreq_stations_pred,
                        PrecDays.Bias = precdays_station_bias,
                        PrecDays.RelBias = precdays_station_relbias,
                        TotalPrec.Obs = totalprec_stations_obs,
                        TotalPrec.Pred = totalprec_stations_pred,
                        TotalPrec.Bias = totalprec_station_bias,
                        TotalPrec.RelBias = totalprec_station_relbias,
                        row.names = codes)
  datesres = data.frame(MinTemperature.Bias = mintemp_day_bias,
                     MinTemperature.MAE = mintemp_day_mae,
                     MaxTemperature.Bias = maxtemp_day_bias,
                     MaxTemperature.MAE = maxtemp_day_mae,
                     TemperatureRange.Bias = temprange_day_bias,
                     TemperatureRange.MAE = temprange_day_mae,
                     RelativeHumidity.Bias = rh_day_bias,
                     RelativeHumidity.MAE = rh_day_mae,
                     Radiation.Bias = rad_day_bias,
                     Radiation.MAE = rad_day_mae,
                     PrecFreq.Obs = precfreq_days_obs,
                     PrecFreq.Pred = precfreq_days_pred,
                     PrecStations.Bias = precstations_day_bias,
                     PrecStations.RelBias = precstations_day_relbias,
                     TotalPrec.Obs = totalprec_days_obs,
                     TotalPrec.Pred = totalprec_days_pred,
                     TotalPrec.Bias = totalprec_day_bias,
                     TotalPrec.RelBias = totalprec_day_relbias,
                     row.names=object@dates)

  l = list(r2 = r2res,stations = stationsres, dates = datesres,
           MinTemperature = MinTemperature[stations,],
           MinTemperatureError = mintemp_error,
           MaxTemperature = MaxTemperature[stations,],
           MaxTemperatureError = maxtemp_error,
           TemperatureRange = TemperatureRange[stations,],
           TemperatureRangeError = temprange_error,
           Precipitation = Precipitation[stations,],
           PrecipitationError = prec_error,
           RelativeHumidity = RelativeHumidity[stations,],
           RelativeHumidityError = rh_error,
           Radiation = Radiation[stations,],
           RadiationError = rad_error)
  class(l)<-c("interpolation.cv", "list")
  return(l)
}
