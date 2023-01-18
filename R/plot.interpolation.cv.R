#' @describeIn interpolation.cv `r lifecycle::badge("deprecated")`
#' @method plot interpolation.cv
#' @exportS3Method plot interpolation.cv
plot.interpolation.cv<-function(x, type="stations",...) {

  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "plot.interpolation.cv()", with = "interpolation_cross_validation()",
    details = "interpolation_cross_validation() returns a list with the cross validation results for
    dates and for stations. List elements are data frames that can be plotted in the usual ways (plot, ggplot2...)"
  )


  type = match.arg(type, c("stations", "dates"))
  if(type =="stations") {
    stationsdf = x$stations
    par(mfrow=c(3,4), mar=c(4,4,2,2))
    if(sum(!is.na(stationsdf$MinTemperature.Bias))>0) {
      hist(stationsdf$`MinTemperature.Bias`, xlab="Min. temp. bias (degrees C)", main="",...)
      abline(v=mean(stationsdf$`MinTemperature.Bias`, na.rm=TRUE), col="gray", lwd=2)
      abline(v=0, col="red", lwd=2)
    }
    if(sum(!is.na(stationsdf$MinTemperature.MAE))>0) {
      hist(stationsdf$`MinTemperature.MAE`, xlab="Min. temp. MAE (degrees C)", main="",...)
      abline(v=mean(stationsdf$`MinTemperature.MAE`, na.rm=TRUE), col="gray", lwd=2)
    }
    if(sum(!is.na(stationsdf$MaxTemperature.Bias))>0) {
      hist(stationsdf$`MaxTemperature.Bias`, xlab="Max. temp. bias (degrees C)", main="",...)
      abline(v=mean(stationsdf$`MaxTemperature.Bias`, na.rm=TRUE), col="gray", lwd=2)
      abline(v=0, col="red", lwd=2)
    }
    if(sum(!is.na(stationsdf$MaxTemperature.MAE))>0) {
      hist(stationsdf$`MaxTemperature.MAE`, xlab="Max. temp. MAE (degrees C)", main="")
      abline(v=mean(stationsdf$`MaxTemperature.MAE`, na.rm=TRUE), col="gray", lwd=2)
    }
    if(sum(!is.na(stationsdf$TotalPrec.Pred))>0) {
      hist(stationsdf$TotalPrec.Pred - stationsdf$TotalPrec.Obs, xlab="Error in total precipitation (mm)", main="",...)
      abline(v=mean(stationsdf$TotalPrec.Pred - stationsdf$TotalPrec.Obs, na.rm=TRUE), col="gray", lwd=2)
      abline(v=0, col="red", lwd=2)
    }
    if(sum(!is.na(stationsdf$PrecFreq.Pred))>0) {
      hist(stationsdf$PrecFreq.Pred - stationsdf$PrecFreq.Obs, xlab="Error in proportion of rainy days (%)", main="",...)
      abline(v=mean(stationsdf$PrecFreq.Pred - stationsdf$PrecFreq.Ob, na.rm=TRUE), col="gray", lwd=2)
      abline(v=0, col="red", lwd=2)
    }
    if(sum(!is.na(stationsdf$RelativeHumidity.Bias))>0) {
      hist(stationsdf$`RelativeHumidity.Bias`, xlab= "RelativeHumidity Bias (%)", main="",...)
      abline(v=mean(stationsdf$`RelativeHumidity.Bias`, na.rm=TRUE), col="gray", lwd=2)
      abline(v=0, col="red", lwd=2)
    }
    if(sum(!is.na(stationsdf$RelativeHumidity.MAE))>0) {
      hist(stationsdf$`RelativeHumidity.MAE`, xlab= "RelativeHumidity MAE (%)", main="",...)
      abline(v=mean(stationsdf$`RelativeHumidity.MAE`, na.rm=TRUE), col="gray", lwd=2)
    }
    if(sum(!is.na(stationsdf$Radiation.Bias))>0) {
      hist(stationsdf$`Radiation.Bias`, xlab= "Radiation Bias (MJ/m2)", main="",...)
      abline(v=mean(stationsdf$`Radiation.Bias`, na.rm=TRUE), col="gray", lwd=2)
    }
    if(sum(!is.na(stationsdf$Radiation.MAE))>0) {
      abline(v=0, col="red", lwd=2)
      hist(stationsdf$`Radiation.MAE`, xlab= "Radiation MAE (MJ/m2)", main="",...)
      abline(v=mean(stationsdf$`Radiation.MAE`, na.rm=TRUE), col="gray", lwd=2)
    }
  } else if(type=="dates") {
    datesdf = x$dates
    par(mfrow=c(5,2), mar=c(4,4,2,2))
    if(sum(!is.na(datesdf$`MinTemperature.Bias`))>0) {
      plot(datesdf$`MinTemperature.Bias`, type="l", ylab="Min. temp. bias (degrees C)", xlab="day", main="",...)
      abline(h=0, col="red", lwd=1.5)
    }
    if(sum(!is.na(datesdf$`MinTemperature.MAE`))>0) {
      plot(datesdf$`MinTemperature.MAE`, type="l", ylab="Min. temp. MAE (degrees C)", xlab="day", main="",...)
    }
    if(sum(!is.na(datesdf$`MaxTemperature.Bias`))>0) {
      plot(datesdf$`MaxTemperature.Bias`, type="l", ylab="Max. temp. bias (degrees C)", xlab="day", main="",...)
      abline(h=0, col="red", lwd=1.5)
    }
    if(sum(!is.na(datesdf$`MaxTemperature.MAE`))>0) {
      plot(datesdf$`MaxTemperature.MAE`, type="l", ylab="Max. temp. MAE (degrees C)", xlab="day", main="",...)
    }
    if(sum(!is.na(datesdf$`TotalPrec.Pred`))>0) {
      plot(datesdf$TotalPrec.Pred - datesdf$TotalPrec.Obs, type="l", ylab="Error in daily total precipitation (mm)", xlab="day", main="",...)
      abline(h=0, col="red", lwd=1.5)
    }
    if(sum(!is.na(datesdf$`PrecFreq.Pred`))>0) {
      plot(datesdf$PrecFreq.Pred - datesdf$PrecFreq.Obs, type="l", ylab="Error in frequency of wet stations (%)", xlab="day", main="",...)
      abline(h=0, col="red", lwd=1.5)
    }
    if(sum(!is.na(datesdf$`RelativeHumidity.Bias`))>0) {
      plot(datesdf$`RelativeHumidity.Bias`, type="l", ylab="RelativeHumidity bias (%)", xlab="day", main="",...)
      abline(h=0, col="red", lwd=1.5)
    }
    if(sum(!is.na(datesdf$`RelativeHumidity.MAE`))>0) {
      plot(datesdf$`RelativeHumidity.MAE`, type="l", ylab="RelativeHumidity MAE (%)", xlab="day", main="",...)
    }
    if(sum(!is.na(datesdf$`Radiation.Bias`))>0) {
      plot(datesdf$`Radiation.Bias`, type="l", ylab="Radiation bias (MJ/m2)", xlab="day", main="",...)
      abline(h=0, col="red", lwd=1.5)
    }
    if(sum(!is.na(datesdf$`Radiation.MAE`))>0) {
      plot(datesdf$`Radiation.MAE`, type="l", ylab="Radiation MAE (MJ/m2)", xlab="day", main="",...)
    }
  }
}
