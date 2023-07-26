#' Precipitation daily concentration
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
  
  p_max = ceiling(max(p, na.rm=TRUE))
  f = cut(p, breaks=c(0.1,1:p_max), include.lowest =TRUE, right=FALSE)
  #Frequency distribution per class
  ni = table(f)
  mid = seq(0.5, p_max-0.5, by=1)
  Pi =mid*ni
  #Totals
  P_tot = sum(Pi)
  n_tot = sum(ni)
  #Cumulative values
  ni_cum = ni
  Pi_cum = Pi
  nclass = length(ni)
  if(nclass>1) {
    for(i in 2:nclass) {
      ni_cum[i] = ni_cum[i]+ni_cum[i-1]
      Pi_cum[i] = Pi_cum[i]+Pi_cum[i-1]
    }
  }
  #Relative cumulative frequencies
  X = as.vector(ni_cum/n_tot)
  Y = as.vector(Pi_cum/P_tot)
  X = X[ni>0]
  Y = Y[ni>0]
  # print(cbind(mid,ni,ni_cum,Pi, Pi_cum,X,Y))
  # plot(X,Y, type="l")
  X2 = X^2
  Y2 = Y^2
  SX = sum(X)
  SY = sum(Y)
  LX = log(X, base=exp(1))
  LY = log(Y, base=exp(1))
  SLX = sum(LX)
  SLY = sum(LY)
  SX2 = sum(X2)
  SY2 = sum(Y2)
  N = length(X)
  den = (N*SX2)-(SX^2)
  a_num = (SX2*SLY)+(SX*sum(X*LX))-(SX2*SLX)-(SX*sum(X*LY))
  b_num = (N*sum(X*LY))+(SX*SLX)- (N*sum(X*LX))-(SX*SLY)
  b =  b_num/den/100
  if(den==0) b = 0
  a = exp(a_num/den)
  if(den==0) a = 1
  A  = (a/b)*(exp(1)^(b*100))*(100-(1/b))
  if(b==0) A = 0
  S <- 5000 - A
  ci =2*S/10000
  return(ci)
}