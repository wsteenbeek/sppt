## ---- eval = TRUE, warning = FALSE, message = FALSE, fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
library("sppt")
plot(areas.sp)
points1 <- points1.sp[areas.sp, ]
points2 <- points2.sp[areas.sp, ]
points(points1, col="blue", pch = 19)
points(points2, col="red", pch = 15)
text(coordinates(areas.sp), label = areas.sp$ID, font = 2)

## ------------------------------------------------------------------------
set.seed(39346) # set seed for reproducibility
toy.sp <- sppt(points1, points2, areas.sp)

## ------------------------------------------------------------------------
set.seed(39346) # set seed for reproducibility
toy2.sp <- sppt(points2, points1, areas.sp)

## ------------------------------------------------------------------------
toy.sp@data
toy2.sp@data

## ------------------------------------------------------------------------
if(!require(rgdal)) install.packages("rgdal", repos = "https://cloud.r-project.org/")

burglary2003 <- rgdal::readOGR(dsn = "../inst/extdata", layer = "Vancouver_Residential_Burglary_2003") # The shapefiles are found in the "inst/extdata" folder within the package
burglary2016 <- rgdal::readOGR(dsn = "../inst/extdata", layer = "Vancouver_Residential_Burglary_2016")
das <- rgdal::readOGR(dsn = "../inst/extdata", layer = "Vancouver_DAs_2011_Census_UTMz10")

## ---- fig.height = 6, fig.width = 6, fig.align = "center"----------------
plot(das)
points(burglary2003, col="blue", pch=16, cex=.3)
points(burglary2016, col="red", pch=16, cex=.3)

## ------------------------------------------------------------------------
set.seed(547347)
myoutput_sppt <- sppt(burglary2003, burglary2016, das)
set.seed(547347)
myoutput_boot <- sppt_boot(burglary2003, burglary2016, das)
set.seed(547347)
myoutput_diff <- sppt_diff(burglary2003, burglary2016, das, adj="none")

## ------------------------------------------------------------------------
mean(myoutput_sppt$globalS)
mean(myoutput_boot$globalS)
mean(myoutput_diff$globalS)

## ---- echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE--------
if(!require(maptools)) install.packages("maptools", repos = "https://cloud.r-project.org/")
if(!require(spatstat)) install.packages("spatstat", repos = "https://cloud.r-project.org/")

## ---- eval = TRUE, warning = FALSE, message = FALSE----------------------
library("maptools", quietly = TRUE)
library("spatstat", quietly = TRUE)

## ---- eval = TRUE--------------------------------------------------------
# simulate point patterns
das.owin <- as.owin(das)
west <- das.owin$xrange[1]
east <- das.owin$xrange[2]
# 100k points, so the next lines take a while
set.seed(67337)
pts1 <- spatstat::rpoint(100000, function(x, y, ...){ 100 * (east - x) }, win = das.owin)
pts2 <- spatstat::rpoint(100000, function(x, y, ...){ 100 * (east - x) }, win = das.owin)

# convert back to SpatialPoints
pts1 <- as(pts1, "SpatialPoints")
proj4string(pts1) <- proj4string(das)
pts2 <- as(pts2, "SpatialPoints")
proj4string(pts2) <- proj4string(das)

## ---- fig.height = 6, fig.width = 6, fig.align = "center"----------------
if(!require(scales)) install.packages("scales", repos = "https://cloud.r-project.org/")

plot(das)
points(pts1, col=scales::alpha("blue", .3), pch=16, cex=.1)
points(pts2, col=scales::alpha("red", .3), pch=16, cex=.1)

## ------------------------------------------------------------------------
if(!require(rgeos)) install.packages("rgeos", repos = "https://cloud.r-project.org/")

# Select the areal units containing 100 or more points (of both Base and Test)
das2 <- das[as.numeric(which(colSums(rgeos::gContains(das, pts1, byid = TRUE)) >= 100)), ]
das2 <- das2[as.numeric(which(colSums(rgeos::gContains(das2, pts2, byid = TRUE)) >= 100)), ]

# Select only points within these areal units
pts1.sp <- pts1[das2, ]
pts2.sp <- pts2[das2, ]

## ---- fig.height = 6, fig.width = 6, fig.align = "center"----------------
# Plot
plot(das2)
points(pts1.sp, col=scales::alpha("blue", .3), pch=16, cex=.1)
points(pts2.sp, col=scales::alpha("red", .3), pch=16, cex=.1)

## ------------------------------------------------------------------------
set.seed(547347)
myoutput_sppt <- sppt(pts1.sp, pts2.sp, das2)
set.seed(547347)
myoutput_boot <- sppt_boot(pts1.sp, pts2.sp, das2)
set.seed(547347)
myoutput_diff <- sppt_diff(pts1.sp, pts2.sp, das2, adj="none")

## ------------------------------------------------------------------------
mean(myoutput_sppt$globalS)
mean(myoutput_boot$globalS)
mean(myoutput_diff$globalS)

