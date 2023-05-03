#' Precipitation daily concentration
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Function \code{precipitation_concentration()} calculates daily precipitation
#' concentration (Martin-Vide et al. 2004).
#'
#' @aliases precipitation_concentration
#' @param p A numeric vector with daily precipitation values.
#' @return Function \code{precipitation_concentration()} returns a value
#' between 0 (equal distribution of rainfall) and 1 (one day concentrates all
#' rainfall).
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' @references Martin-Vide J (2004) Spatial distribution of a daily precipitation
#' concentration index in peninsular Spain. International Journal of
#' Climatology 24, 959–971. doi:10.1002/joc.1030.
#' @export
precipitation_concentration <- function(p) {

  # deprecation notice
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "precipitation_concentration()", with = NULL,
    details = "precipitation_concentration utility is deprecated"
  )

  # p_max = ceiling(max(p, na.rm=TRUE))
  # f = cut(p, breaks=c(0.1,1:p_max), include.lowest =TRUE, right=FALSE)
  # #Frequency distribution per class
  # ni = table(f)
  # mid = seq(0.5, p_max-0.5, by=1)
  # Pi =mid*ni
  # #Totals
  # P_tot = sum(Pi)
  # n_tot = sum(ni)
  # #Cumulative values
  # ni_cum = ni
  # Pi_cum = Pi
  # nclass = length(ni)
  # if(nclass>1) {
  #   for(i in 2:nclass) {
  #     ni_cum[i] = ni_cum[i]+ni_cum[i-1]
  #     Pi_cum[i] = Pi_cum[i]+Pi_cum[i-1]
  #   }
  # }
  # #Relative cumulative frequencies
  # X = as.vector(ni_cum/n_tot)
  # Y = as.vector(Pi_cum/P_tot)
  # X = X[ni>0]
  # Y = Y[ni>0]
  # # print(cbind(mid,ni,ni_cum,Pi, Pi_cum,X,Y))
  # # plot(X,Y, type="l")
  # X2 = X^2
  # Y2 = Y^2
  # SX = sum(X)
  # SY = sum(Y)
  # LX = log(X, base=exp(1))
  # LY = log(Y, base=exp(1))
  # SLX = sum(LX)
  # SLY = sum(LY)
  # SX2 = sum(X2)
  # SY2 = sum(Y2)
  # N = length(X)
  # den = (N*SX2)-(SX^2)
  # a_num = (SX2*SLY)+(SX*sum(X*LX))-(SX2*SLX)-(SX*sum(X*LY))
  # b_num = (N*sum(X*LY))+(SX*SLX)- (N*sum(X*LX))-(SX*SLY)
  # b =  b_num/den/100
  # if(den==0) b = 0
  # a = exp(a_num/den)
  # if(den==0) a = 1
  # A  = (a/b)*(exp(1)^(b*100))*(100-(1/b))
  # if(b==0) A = 0
  # S <- 5000 - A
  # ci =2*S/10000
  # return(ci)
}

#' Precipitation rainfall erosivity
#'
#' Function \code{precipitation_rainfallErosivity()} calculates a multi-year
#' average of monthly rainfall erosivity using the MedREM model proposed by
#' Diodato and Bellochi (2010) for the Mediterranean area (see also Guerra et
#' al. 2016).
#'
#' MedREM model is: Rm = b0·P·sqrt(d)·(alpha + b1*longitude), where P is
#' accumulated precipitation and d is maximum daily precipitation. Parameters
#' used for the MedREM model are b0 = 0.117, b1 = -0.015, alpha = 2. Note that
#' there is a mistake in Guerra et al. (2016) regarding parameters b1 and a.
#'
#' @aliases precipitation_rainfallErosivity
#' @param x A data frame of meteorological variables (dates as row names and
#' variables as columns).
#' @param long Longitude in degrees.
#' @param scale Either 'month' or 'year'.
#' @param average Boolean flag to calculate multi-year averages before applying
#' MedREM's formula.
#' @return Function \code{precipitation_rainfallErosivity()} returns a
#' vector of twelve values (one for each month) (in MJ·mm·ha-1·h-1·month-1) or
#' one value (in in MJ·mm·ha-1·h-1·yr-1) depending on the scale.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' @references Diodato, N., Bellocchi, G., 2010. MedREM, a rainfall erosivity
#' model for the Mediter-ranean region. J. Hydrol. 387, 119–127,
#' doi:10.1016/j.jhydrol.2010.04.003.
#'
#' Guerra CA, Maes J, Geijzendorffer I, Metzger MJ (2016) An assessment of soil
#' erosion prevention by vegetation in Mediterranean Europe: Current trends of
#' ecosystem service provision. Ecol Indic 60:213–222. doi:
#' 10.1016/j.ecolind.2015.06.043.
#' @export
precipitation_rainfallErosivity <- function(x, long, scale = "month", average = TRUE) {
  scale = match.arg(scale, c("month", "year"))
  b0 = 0.117
  b1 = -0.015
  a = 2

  if(scale == "month") {
    p_m = summarypoint(x, "Precipitation", fun="sum", freq="months", na.rm=T)
    d_m = summarypoint(x, "Precipitation", fun="max", freq= "months", na.rm=T)
    if(average) {
      months = substr(names(p_m),6,7)
      p_m = tapply(p_m, months, FUN="mean", na.rm=T)
      d_m = tapply(d_m, months, FUN="mean", na.rm=T)
    }
    R_m = b0*p_m*sqrt(d_m)*(a+b1*long)
    return(R_m)
  } else {
    p_y = summarypoint(x, "Precipitation", fun="sum", freq="years", na.rm=T)
    d_y = summarypoint(x, "Precipitation", fun="max", freq= "years", na.rm=T)
    if(average) {
      p_y = mean(p_y, na.rm=T)
      d_y = mean(d_y, na.rm=T)
    }
    R_y = b0*p_y*sqrt(d_y)*(a+b1*long)
    return(R_y)
  }
}
