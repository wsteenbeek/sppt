## ---- eval = TRUE, warning = FALSE, message = FALSE---------------------------
library("sppt")

## ---- fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
plot(areas.sp)
points(points1.sp, col="blue", pch = 19)
points(points2.sp, col="red", pch = 15)
text(coordinates(areas.sp), label = areas.sp$ID, font = 2)

## -----------------------------------------------------------------------------
points1 <- points1.sp[points1.sp$ID != 5, ]
points2 <- points2.sp[points2.sp$ID != 5, ]

## ---- fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
plot(areas.sp)
points(points1, col="grey", pch = 19)
points(points2, col="grey", pch = 19)
points(points1[areas.sp, ], col="blue", pch = 19)
points(points2[areas.sp, ], col="red", pch = 15)
text(coordinates(areas.sp), label = areas.sp$ID, font = 2)

## -----------------------------------------------------------------------------
set.seed(39346) # set seed for reproducibility
toy.sp <- sppt(points1, points2, areas.sp)

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(39346) # set seed for reproducibility
#  toy.sp <- sppt(points1, points2, areas.sp,
#                 nsamples=200, percpoints=85, conf_level=95)

## -----------------------------------------------------------------------------
toy.sp@data

## -----------------------------------------------------------------------------
summary_sppt(toy.sp)

## -----------------------------------------------------------------------------
if(!require(rgdal)) install.packages("rgdal", repos = "https://cloud.r-project.org/")

burglary2003 <- rgdal::readOGR(dsn = "../inst/extdata", layer = "Vancouver_Residential_Burglary_2003") # The shapefiles are found in the "inst/extdata" folder within the package
burglary2016 <- rgdal::readOGR(dsn = "../inst/extdata", layer = "Vancouver_Residential_Burglary_2016")
das <- rgdal::readOGR(dsn = "../inst/extdata", layer = "Vancouver_DAs_2011_Census_UTMz10")

## -----------------------------------------------------------------------------
proj4string(burglary2003)
proj4string(burglary2016)
proj4string(das)

## ---- fig.height = 6, fig.width = 6, fig.align = "center"---------------------
plot(das)
points(burglary2003, col="blue", pch=16, cex=.3)
points(burglary2016, col="red", pch=16, cex=.3)

## -----------------------------------------------------------------------------
myoutput <- sppt(burglary2003, burglary2016, das, nsamples = 200, percpoints = 85, conf_level = 95)

## -----------------------------------------------------------------------------
mean(myoutput$globalS) 
mean(myoutput$globalS.robust, na.rm=TRUE) 

## -----------------------------------------------------------------------------
head(myoutput@data, n=10) # the first 10 cases of data

## ---- fig.height = 6, fig.width = 6, fig.align = "center"---------------------
plot(myoutput)
plot(myoutput[which(myoutput$localS == -1),], col="#2c7bb6", add = TRUE)
plot(myoutput[which(myoutput$localS == 1),], col="#d7191c", add = TRUE)
plot(myoutput[which(myoutput$localS == 0),], col="#ffffbf", add = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  rgdal::writeOGR(obj = myoutput, dsn = "C:/My/File/Path/Here", layer = "Burglary_2003_2016_DAs_SPPT", driver = "ESRI Shapefile")

