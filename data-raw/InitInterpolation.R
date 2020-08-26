library(meteoland)
setwd("~/Documents/Recerca/Datasets/Clima/")

#Target dates
dates = seq(as.Date("2000-01-01"), as.Date("2003-12-31"), by="day")

#Load SMC data
load("SMC/SMC_processed/rdata/SMC_MEDACC.rdata")
codes_SMC = stdata$CODI
coords_SMC = cbind(stdata$X_UTM, stdata$Y_UTM)

tmin_SMC = tmin[as.character(dates),]
tmax_SMC = tmax[as.character(dates),]
rh_SMC = rh[as.character(dates),]
p_SMC = p[as.character(dates),]
sr_SMC = sr[as.character(dates),]
topo_SMC = read.table("SMC/SMC_processed/txt/SMC_topo30m.txt",sep="\t",header=TRUE, row.names=1)
ws_SMC = tmax_SMC
ws_SMC[] = NA
wd_SMC = tmax_SMC
wd_SMC[] = NA

#Load AEMET data
load("AEMET/AEMET_processed/rdata/AEMET_MEDACC.rdata")
row.names(stdata) = stdata$INDICATIVO
topo_AEMET = read.table("AEMET/AEMET_processed/txt/AEMET_topo30m.txt",sep="\t",header=TRUE, row.names=1)
stdata = stdata[row.names(topo_AEMET),]
coords_AEMET = cbind(stdata$UTM31_X, stdata$UTM31_Y)

p_AEMET = p[as.character(dates),row.names(topo_AEMET)]
tmin_AEMET = tmin[as.character(dates),row.names(topo_AEMET)]
tmax_AEMET = tmax[as.character(dates),row.names(topo_AEMET)]
rh_AEMET = rhmean[as.character(dates),row.names(topo_AEMET)]
ws_AEMET = ws13h[as.character(dates),row.names(topo_AEMET)]
wd_AEMET = wd13h[as.character(dates),row.names(topo_AEMET)]
sr_AEMET = wd_AEMET
sr_AEMET[] = NA

#Merge SMC and AEMET data
MinTemperature = as.data.frame(t(cbind(tmin_SMC, tmin_AEMET)))
MaxTemperature = as.data.frame(t(cbind(tmax_SMC, tmax_AEMET)))
Precipitation = as.data.frame(t(cbind(p_SMC, p_AEMET)))
RelativeHumidity = as.data.frame(t(cbind(rh_SMC, rh_AEMET)))
WindSpeed = as.data.frame(t(cbind(ws_SMC, ws_AEMET)))
WindDirection = as.data.frame(t(cbind(wd_SMC, wd_AEMET)))
Radiation = as.data.frame(t(cbind(sr_SMC, sr_AEMET)))
topostations = rbind(topo_SMC, topo_AEMET)


#Remove outliers
MinTemperature[(!is.na(MinTemperature)) & (MinTemperature < (-50))] =NA
MinTemperature[(!is.na(MinTemperature)) & (MinTemperature > 50)] =NA
MaxTemperature[(!is.na(MaxTemperature)) & (MaxTemperature < (-50))] =NA
MaxTemperature[(!is.na(MaxTemperature)) & (MaxTemperature > 50)] =NA

#Select points near the example grid
data("examplegridtopography")
bbox = examplegridtopography@bbox
points = SpatialPoints(rbind(coords_SMC, coords_AEMET),CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m"))
cc = coordinates(points)
buffer = 20000 #20 km
sel = (cc[,1]>bbox[1,1]-buffer) & (cc[,1]<bbox[1,2]+buffer) & (cc[,2]>bbox[2,1]-buffer) & (cc[,2]<bbox[2,2]+buffer)
points = points[sel]
print(length(points))
topostations = topostations[sel,]
MinTemperature = MinTemperature[sel,]
MaxTemperature = MaxTemperature[sel,]
Precipitation = Precipitation[sel,]
RelativeHumidity = RelativeHumidity[sel,]
Radiation = Radiation[sel,]
WindSpeed = WindSpeed[sel,]
WindDirection = WindDirection[sel,]

exampleinterpolationdata<-MeteorologyInterpolationData(
  points = points,
  elevation = topostations$elevation,
  slope = topostations$slope,
  aspect = topostations$aspect,
  MinTemperature  = as.matrix(MinTemperature),
  MaxTemperature  = as.matrix(MaxTemperature),
  Precipitation = as.matrix(Precipitation),
  RelativeHumidity = as.matrix(RelativeHumidity),
  Radiation = as.matrix(Radiation),
  WindSpeed = as.matrix(WindSpeed),
  WindDirection = as.matrix(WindDirection))

#CALIBRATED PARAMETERIZATION FOR 2001
exampleinterpolationdata@params = defaultInterpolationParams()
exampleinterpolationdata@params$N_MinTemperature = 60
exampleinterpolationdata@params$alpha_MinTemperature = 10.0
exampleinterpolationdata@params$N_MaxTemperature = 60
exampleinterpolationdata@params$alpha_MaxTemperature = 7.5
exampleinterpolationdata@params$N_DewTemperature = 40
exampleinterpolationdata@params$alpha_DewTemperature = 9.5
exampleinterpolationdata@params$N_PrecipitationEvent = 4
exampleinterpolationdata@params$alpha_PrecipitationEvent = 11
exampleinterpolationdata@params$N_PrecipitationAmount = 30
exampleinterpolationdata@params$alpha_PrecipitationAmount = 4
exampleinterpolationdata@params$St_Precipitation=5
exampleinterpolationdata@params$pop_crit = 0.50
exampleinterpolationdata@params$f_max = 0.95

# save(exampleinterpolationdata, file="~/Documents/Rpackages/meteoland/meteoland/data/exampleinterpolationdata.rda")
#!REBUILD
