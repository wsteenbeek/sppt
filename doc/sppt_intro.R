## ---- eval = TRUE, warning = FALSE, message = FALSE---------------------------
library("sppt")

## ---- fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
plot(areas.sp)
text(coordinates(areas.sp), label = areas.sp$ID)
points(points1.sp, col="blue", pch = 19)
points(points2.sp, col="red", pch = 15)

## ---- fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
# Keep only red points with ID == 1:5
points1 <- points1.sp[points1.sp$ID != 0, ]
points2 <- points2.sp[points2.sp$ID %in% 1:4, ]

# plot
plot(areas.sp)
text(coordinates(areas.sp), label = areas.sp$ID)
points(points1, col="blue", pch = 19)
points(points2, col="red", pch = 15)

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
set.seed(244)
myoutput <- sppt(burglary2003, burglary2016, das, nsamples = 200, percpoints = 85, conf_level = 95)

## -----------------------------------------------------------------------------
summary_sppt(myoutput)

## -----------------------------------------------------------------------------
head(myoutput@data, n=10) # the first 10 cases of data

## ---- fig.height = 6, fig.width = 6, fig.align = "center"---------------------
plot(myoutput)
plot(myoutput[which(myoutput$localS == -1),], col="#2c7bb6", add = TRUE)
plot(myoutput[which(myoutput$localS == 1),], col="#d7191c", add = TRUE)
plot(myoutput[which(myoutput$localS == 0),], col="#ffffbf", add = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  rgdal::writeOGR(obj = myoutput, dsn = "C:/My/File/Path/Here", layer = "Burglary_2003_2016_DAs_SPPT", driver = "ESRI Shapefile")

## ---- fig.height = 6, fig.width = 6, fig.align = "center"---------------------
plot(das, lwd = .5)
points(burglary2003[1:400, ], col="blue", pch=16, cex=.3)
points(burglary2016[1:150, ], col="red", pch=16, cex=.3)

## -----------------------------------------------------------------------------
set.seed(244)
myoutput <- sppt(burglary2003[1:400, ], burglary2016[1:150, ], das)

## -----------------------------------------------------------------------------
summary_sppt(myoutput)

