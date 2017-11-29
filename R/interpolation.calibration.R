interpolation.calibration<-function(object, stations = NULL, variable="Tmin", N_seq = seq(5,30, by=5), alpha_seq = seq(0.25,10, by=0.25), verbose = FALSE) {
  if(!inherits(object, "MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'")
  if(is.null(stations)) stations = 1:length(object@elevation)
  Osel = rep(FALSE, length(object@elevation))
  Osel[stations] = TRUE
  points = SpatialPoints(object@coords, object@proj4string)
  mPar = object@params
  MAE = matrix(0, nrow=length(N_seq), ncol = length(alpha_seq))
  rownames(MAE) = N_seq
  colnames(MAE) = alpha_seq
  if(variable=="Tmin") {
    Cmat = as.matrix(object@MinTemperature)
  }
  else if(variable=="Tmax") {
    Cmat = as.matrix(object@MaxTemperature)
  }
  else if(variable=="Tdew") {
    Cmat = .dewpointTemperatureFromRH(0.606*as.matrix(object@MaxTemperature)+0.394*as.matrix(object@MinTemperature),
                                      as.matrix(object@RelativeHumidity))
  }
  else if(variable=="Prec") {
    Cmat = as.matrix(object@Precipitation)
    Csmooth = object@SmoothedPrecipitation
  }
  else if(variable=="PrecAmount") {
    Cmat = as.matrix(object@Precipitation)
    Csmooth = object@SmoothedPrecipitation
  }
  else if(variable=="PrecEvent") {
    Cmat = as.matrix(object@Precipitation>0)
  }
  cat(paste("Total number of stations: ", nrow(Cmat),"\n",sep=""))
  Ccoords = object@coords
  Celevation = object@elevation
  Cnodatastations = (rowSums(is.na(Cmat))==ncol(Cmat))
  Cmat = Cmat[!Cnodatastations,]
  Ccoords = Ccoords[!Cnodatastations,]
  Celevation = Celevation[!Cnodatastations]
  Osel = Osel[!Cnodatastations]
  cat(paste("Number of stations with available data: ", nrow(Cmat),"\n",sep=""))
  stations = which(Osel)

  #Stations for the assessment of MAE
  Omat = Cmat[stations,]
  Ocoords = Ccoords[stations,]
  Oelevation = Celevation[stations]
  cat(paste("Number of stations used for MAE: ", length(stations),"\n",sep=""))
  ncomb = length(N_seq)*length(alpha_seq)
  cat(paste("Number of parameter combinations to test: ", ncomb,"\n\n",sep=""))
  #Initialize MAE to optimize
  minMAE = 9999999.0
  imin = NA
  jmin = NA
  cat("Evaluation of parameter combinations...\n")
  if(!verbose) pb = txtProgressBar(0, ncomb, style=3)
  for(i in 1:length(N_seq)) {
    for(j in 1:length(alpha_seq)) {
      if(!verbose) setTxtProgressBar(pb, (i-1)*length(alpha_seq) + j)
      Pmat = Omat
      Pmat[] = 0.0
      for(p in 1:length(stations)) {
        station = stations[p]
        if((variable=="Tmin") | (variable=="Tmax")) {
          mp= .interpolateTemperatureSeriesPoints( Xp = Ocoords[p,1],
                                                   Yp = Ocoords[p,2],
                                                   Zp = Oelevation[p],
                                                   X = Ccoords[-station,1],
                                                   Y = Ccoords[-station,2],
                                                   Z = Celevation[-station],
                                                   T = Cmat[-station,],
                                                   iniRp = mPar$initial_Rp,
                                                   alpha = alpha_seq[j],
                                                   N = N_seq[i],
                                                   iterations = mPar$iterations)
          Pmat[p,] = as.vector(mp)
        }else if(variable=="Tdew") {
          mp= .interpolateTdewSeriesPoints( Xp = Ocoords[p,1],
                                                   Yp = Ocoords[p,2],
                                                   Zp = Oelevation[p],
                                                   X = Ccoords[-station,1],
                                                   Y = Ccoords[-station,2],
                                                   Z = Celevation[-station],
                                                   T = Cmat[-station,],
                                                   iniRp = mPar$initial_Rp,
                                                   alpha = alpha_seq[j],
                                                   N = N_seq[i],
                                                   iterations = mPar$iterations)
          Pmat[p,] = as.vector(mp)
        } else if((variable=="Prec")){
          mp= .interpolatePrecipitationSeriesPoints( Xp = Ocoords[p,1],
                                                     Yp = Ocoords[p,2],
                                                     Zp = Oelevation[p],
                                                     X = Ccoords[-station,1],
                                                     Y = Ccoords[-station,2],
                                                     Z = Celevation[-station],
                                                     P = Cmat[-station,],
                                                     Psmooth = Csmooth[-station,],
                                                     iniRp = mPar$initial_Rp,
                                                     alpha_event =alpha_seq[j],
                                                     alpha_amount = alpha_seq[j],
                                                     N_event = N_seq[i],
                                                     N_amount = N_seq[i],
                                                     iterations = mPar$iterations,
                                                     popcrit = mPar$pop_crit,
                                                     fmax = mPar$f_max)
          Pmat[p,] = as.vector(mp)
        } else if((variable=="PrecAmount")){
          mp= .interpolatePrecipitationSeriesPoints( Xp = Ocoords[p,1],
                                                     Yp = Ocoords[p,2],
                                                     Zp = Oelevation[p],
                                                     X = Ccoords[-station,1],
                                                     Y = Ccoords[-station,2],
                                                     Z = Celevation[-station],
                                                     P = Cmat[-station,],
                                                     Psmooth = Csmooth[-station,],
                                                     iniRp = mPar$initial_Rp,
                                                     alpha_event = mPar$alpha_PrecipitationEvent,
                                                     alpha_amount = alpha_seq[j],
                                                     N_event = mPar$N_PrecipitationEvent,
                                                     N_amount = N_seq[i],
                                                     iterations = mPar$iterations,
                                                     popcrit = mPar$pop_crit,
                                                     fmax = mPar$f_max)
          Pmat[p,] = as.vector(mp)
        } else if((variable=="PrecEvent")){
          mp= .interpolatePrecipitationEventSeriesPoints( Xp = Ocoords[p,1],
                                                     Yp = Ocoords[p,2],
                                                     Zp = Oelevation[p],
                                                     X = Ccoords[-station,1],
                                                     Y = Ccoords[-station,2],
                                                     Z = Celevation[-station],
                                                     Pevent = Cmat[-station,],
                                                     iniRp = mPar$initial_Rp,
                                                     alpha = alpha_seq[j],
                                                     N = N_seq[i],
                                                     iterations = mPar$iterations,
                                                     popcrit = mPar$pop_crit)
          Pmat[p,] = as.vector(mp)
        }
      }
      if(sum(!is.na(Pmat))>0) {
        if((variable=="Tmin") || (variable=="Tmax") ||(variable=="Tdew")) {
          MAE[i,j] = mean(abs(Pmat-Omat), na.rm=TRUE)
        } else if((variable=="Prec") ||(variable=="PrecAmount")||(variable=="PrecEvent"))  {
          den1 = 0
          num1 = 0
          num2 = 0
          den2 = 0
          for(d in 1:ncol(Omat)) {
            oprec = Omat[,d]
            pprec = Pmat[!is.na(oprec),d]
            w = sum(!is.na(oprec))
            so = sum(oprec, na.rm=TRUE)
            sp = sum(pprec, na.rm=TRUE)
            if(w>0) {
              num1=num1+abs(sp/w - so/w)
              den1 = den1 + 1
            }
          }
          for(p in 1:nrow(Omat)) {
            oprec = Omat[p,]
            pprec = Pmat[p,!is.na(oprec)]
            w = sum(!is.na(oprec))
            so = sum(oprec, na.rm=TRUE)
            sp = sum(pprec, na.rm=TRUE)
            if(w>0) {
              num2=num2+abs(sp/w - so/w)
              den2 = den2 + 1
            }
          }

          # print(c(num,den))
          MAE[i,j] = sqrt((num1/den1)*(num2/den2))
          # MAE[i,j] = mean(abs(Pmat-Omat), na.rm=TRUE)
        }
        if(verbose) cat(paste("   N: ", N_seq[i], " alpha: ",alpha_seq[j], " MAE = ", MAE[i,j], "\n",sep=""))
        if(MAE[i,j]<minMAE) {
          minMAE = MAE[i,j]
          imin = i
          jmin = j
          OmatOpt = Omat
          PmatOpt = Pmat
        }
      }
    }
  }
  cat(paste("\nMinimum MAE value: ", minMAE, " N: ", N_seq[imin], " alpha: ", alpha_seq[jmin],"\n",sep=""))
  l = list(MAE = MAE, minMAE = minMAE, N = N_seq[imin], alpha = alpha_seq[jmin], Observed = OmatOpt, Predicted = PmatOpt)
  class(l)<-c("interpolation.calibration", "list")
  return(l)
}

interpolation.calibration.fmax<-function(object, stations = NULL, fmax_seq = seq(0.05,0.95, by=0.05), verbose = FALSE) {
  if(!inherits(object, "MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'")
  if(is.null(stations)) stations = 1:length(object@elevation)
  Osel = rep(FALSE, length(object@elevation))
  Osel[stations] = TRUE
  points = SpatialPoints(object@coords, object@proj4string)
  mPar = object@params
  MAE = numeric(length(fmax_seq))
  names(MAE) = fmax_seq
  Cmat = as.matrix(object@Precipitation)
  Csmooth = object@SmoothedPrecipitation
  cat(paste("Total number of stations: ", nrow(Cmat),"\n",sep=""))
  Ccoords = object@coords
  Celevation = object@elevation
  Cnodatastations = (rowSums(is.na(Cmat))==ncol(Cmat))
  Cmat = Cmat[!Cnodatastations,]
  Ccoords = Ccoords[!Cnodatastations,]
  Celevation = Celevation[!Cnodatastations]
  Osel = Osel[!Cnodatastations]
  cat(paste("Number of stations with available data: ", nrow(Cmat),"\n",sep=""))
  stations = which(Osel)
  
  #Stations for the assessment of MAE
  Omat = Cmat[stations,]
  Ocoords = Ccoords[stations,]
  Oelevation = Celevation[stations]
  cat(paste("Number of stations used for MAE: ", length(stations),"\n",sep=""))
  cat(paste("Number of parameter values to test: ", length(fmax_seq),"\n\n",sep=""))
  #Initialize MAE to optimize
  minMAE = 9999999.0
  imin = NA
  cat("Evaluation of fmax values\n")
  if(!verbose) pb = txtProgressBar(0, length(fmax_seq), style=3)
  for(i in 1:length(fmax_seq)) {
    if(!verbose) setTxtProgressBar(pb, i)
      Pmat = Omat
      Pmat[] = 0.0
      for(p in 1:length(stations)) {
        station = stations[p]
        mp= .interpolatePrecipitationSeriesPoints( Xp = Ocoords[p,1],
                                                   Yp = Ocoords[p,2],
                                                   Zp = Oelevation[p],
                                                   X = Ccoords[-station,1],
                                                   Y = Ccoords[-station,2],
                                                   Z = Celevation[-station],
                                                   P = Cmat[-station,],
                                                   Psmooth = Csmooth[-station,],
                                                   iniRp = mPar$initial_Rp,
                                                   alpha_event = mPar$alpha_PrecipitationEvent,
                                                   alpha_amount = mPar$alpha_PrecipitationAmount,
                                                   N_event = mPar$N_PrecipitationEvent,
                                                   N_amount = mPar$N_PrecipitationAmount,
                                                   iterations = mPar$iterations,
                                                   popcrit = mPar$pop_crit,
                                                   fmax = fmax_seq[i])
        Pmat[p,] = as.vector(mp)
      }
      if(sum(!is.na(Pmat))>0) {
        den1 = 0
        num1 = 0
        num2 = 0
        den2 = 0
        for(d in 1:ncol(Omat)) {
          oprec = Omat[,d]
          pprec = Pmat[!is.na(oprec),d]
          w = sum(!is.na(oprec))
          so = sum(oprec, na.rm=TRUE)
          sp = sum(pprec, na.rm=TRUE)
          if(w>0) {
            num1=num1+abs(sp/w - so/w)
            den1 = den1 + 1
          }
        }
        for(p in 1:nrow(Omat)) {
          oprec = Omat[p,]
          pprec = Pmat[p,!is.na(oprec)]
          w = sum(!is.na(oprec))
          so = sum(oprec, na.rm=TRUE)
          sp = sum(pprec, na.rm=TRUE)
          if(w>0) {
            num2=num2+abs(sp/w - so/w)
            den2 = den2 + 1
          }
        }
        
        # print(c(num,den))
        MAE[i] = sqrt((num1/den1)*(num2/den2))
        if(verbose) cat(paste("   fmax: ", fmax_seq[i], " MAE = ", MAE[i], "\n",sep=""))
        if(MAE[i]<minMAE) {
          minMAE = MAE[i]
          imin = i
          OmatOpt = Omat
          PmatOpt = Pmat
        }
      }
    }
  cat(paste("\nMinimum MAE value: ", minMAE, " fmax: ", fmax_seq[imin], "\n",sep=""))
  l = list(MAE = MAE, minMAE = minMAE, fmax = fmax_seq[imin], Observed = OmatOpt, Predicted = PmatOpt)
  class(l)<-c("interpolation.calibration.fmax", "list")
  return(l)
}