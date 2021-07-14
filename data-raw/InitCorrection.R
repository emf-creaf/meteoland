library(meteoland)

wd = getwd()
setwd("/media/miquel/Elements/INFORMED_backup/CaseStudies/Solsones")

#Read table containining RCM historical predictions
RCMHisMP = read.table("EUR-11/CCLM4-8-17/historical/MP_Solsones.txt", sep="\t", header=TRUE)
RCMHisMP$dir = "EUR-11/CCLM4-8-17/historical"
sp = SpatialPoints(RCMHisMP[,c("lon","lat")], CRS(SRS_string = "EPSG:4326"))

#Read table containing rcp4.5 future predictions
RCM45MP = read.table("EUR-11/CCLM4-8-17/rcp4.5/MP_Solsones.txt", sep="\t", header=TRUE)
RCM45MP$dir = "EUR-11/CCLM4-8-17/rcp4.5"

dateshist = seq(as.Date("2000-01-01"), as.Date("2003-12-31"), by="day")
datesfuture = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by="day")
sel = c(5,6,10)
hist = readmeteorologypointfiles(sp[sel],paste(RCMHisMP$dir[sel],RCMHisMP$filename[sel],sep="/"), dates = dateshist)
rcp4.5 = readmeteorologypointfiles(sp[sel],paste(RCM45MP$dir[sel],RCM45MP$filename[sel],sep="/"), dates = datesfuture)
examplecorrectiondata = MeteorologyUncorrectedData(sp[sel], hist@data, rcp4.5@data, dates=rcp4.5@dates)

setwd(wd)
usethis::use_data(examplecorrectiondata, overwrite = T)
#REBUILD!