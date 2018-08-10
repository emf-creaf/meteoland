get("exampleinterpolationdata")
mxt100 = exampleinterpolationdata@MaxTemperature[,100]
Psmooth100 = exampleinterpolationdata@SmoothedPrecipitation[,100]
P100 = exampleinterpolationdata@Precipitation[,100]
mismxt = is.na(mxt100)
misP = is.na(P100)
Z = exampleinterpolationdata@elevation
X = exampleinterpolationdata@coords[,1]
Y = exampleinterpolationdata@coords[,2]
Zpv = seq(0,1000, by=100)
xp = 360000
yp = 4640000
xpv = rep(xp, 11)
ypv = rep(yp, 11)
Tv1 = interpolation_temperature(xpv, ypv, Zpv, X[!mismxt], Y[!mismxt], Z[!mismxt], mxt100[!mismxt])
Pv1 = interpolation_precipitation(xpv, ypv, Zpv, X[!misP], Y[!misP], Z[!misP], P100[!misP], Psmooth100[!misP])
