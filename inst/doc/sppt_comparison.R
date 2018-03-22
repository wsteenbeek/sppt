## ---- eval = TRUE, warning = FALSE, message = FALSE, fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
plot(areas.sp)
points(points1.sp, col="blue", pch = 19)
points(points2.sp, col="red", pch = 15)
text(coordinates(areas.sp), label = areas.sp$ID, font = 2)

## ---- echo = FALSE-------------------------------------------------------
load("toy_java.rda")
toy_java@data

## ---- echo = FALSE-------------------------------------------------------
areas.sp$ID

## ---- echo = FALSE-------------------------------------------------------
toy_java_df <- toy_java@data[order(as.numeric(as.character(toy_java$ID))),]
toy_java_df

## ------------------------------------------------------------------------
set.seed(39346) # set seed for reproducibility
toy_r <- sppt(points1.sp, points2.sp, areas.sp)
toy_r@data

## ---- eval = TRUE, fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
if(!require(scales)) install.packages("scales", repos = "https://cloud.r-project.org/")

plot(areas.sp)
points(points1.sp, col="blue", pch = 19)
points(points2.sp, col="red", pch = 15)
set.seed(345)
points(x = jitter(rep(bbox(points1.sp)[3]+150, times=50), factor=0.008), 
       y = jitter(rep(bbox(points1.sp)[2]-50, times=50), factor=0.008), 
       col=scales::alpha("blue", 0.6), pch=16)
text(coordinates(areas.sp), label = areas.sp$ID, font = 2)

## ---- echo = FALSE-------------------------------------------------------
# remove points outside any areal unit
points1.sp.new <- points1.sp[areas.sp,]
points2.sp.new <- points2.sp[areas.sp,]

## ---- echo = FALSE, fig.show = 'hold', fig.height = 6, fig.width = 6, fig.align = "center"----
plot(areas.sp)
points(points1.sp.new, col="blue", pch = 19)
points(points2.sp.new, col="red", pch = 15)
text(coordinates(areas.sp), label = areas.sp$ID, font = 2)

## ---- echo = FALSE-------------------------------------------------------
load("toy_java_new.rda")
toy_java_df <- toy_java_new@data[order(as.numeric(as.character(toy_java_new$ID))),]
toy_java_df

## ------------------------------------------------------------------------
1 - mean(abs(toy_java_df$SIndex))

## ------------------------------------------------------------------------
set.seed(39346) # set seed for reproducibility
toy_r_new <- sppt(points1.sp.new, points2.sp.new, areas.sp, nsamples=200, percpoints=85, conf_level=95)

## ------------------------------------------------------------------------
toy_r_new@data

## ------------------------------------------------------------------------
summary.sppt(toy_r_new)

## ---- fig.height = 6, fig.width = 6, fig.align = "center", echo = FALSE, message=FALSE, fig.cap = "Vancouver Base points"----
plot(vancouver_areas.sp)
points(vancouver_points1.sp, col="blue", pch = 19, cex=.2)

## ---- fig.height = 6, fig.width = 6, fig.align = "center", echo = FALSE, message=FALSE, fig.cap = "Vancouver Test points"----
plot(vancouver_areas.sp)
points(vancouver_points2.sp, col="red", pch = 15, cex=.2)

## ---- echo = FALSE-------------------------------------------------------
# remove points outside any areal unit
vancouver_points1.sp <- vancouver_points1.sp[vancouver_areas.sp, ]
vancouver_points2.sp <- vancouver_points2.sp[vancouver_areas.sp, ]

## ---- eval = FALSE-------------------------------------------------------
#  if(!require(microbenchmark)) install.packages("microbenchmark", repos = "https://cloud.r-project.org/")
#  
#  microbenchmark::microbenchmark(sppt(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp, nsamples=200, percpoints=85, conf_level=95), times = 50L, unit = "s")

## ------------------------------------------------------------------------
set.seed(39346) # set seed for reproducibility
vancouver_r <- sppt(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp, nsamples=200, percpoints=85, conf_level=95)

## ---- echo = FALSE-------------------------------------------------------
vancouver_r_df <- vancouver_r@data

load("vancouver_java_new.rda")
vancouver_java_df <- vancouver_java_new@data
vancouver_java_df <- vancouver_java_df[order(as.numeric(as.character(vancouver_java_df$CTUID))),]

## ------------------------------------------------------------------------
# test if the ID numbers are in the same order:
table(vancouver_r_df$CTUID == vancouver_java_df$CTUID)
# test that vectors PctBsePts are the same:
table(vancouver_r_df$PctBsePts == vancouver_java_df$PctBsePts)
# test that vectors PctTstPts are the same:
table(vancouver_r_df$PctTstPts == vancouver_java_df$PctTstPts)

## ------------------------------------------------------------------------
summary.sppt(vancouver_r)

## ------------------------------------------------------------------------
table(vancouver_r_df$SIndex == vancouver_java_df$SIndex)

## ---- echo = FALSE-------------------------------------------------------
differences <- which(vancouver_r_df$SIndex != vancouver_java_df$SIndex)
vancouver_r_df[differences, c("CTUID", "PctBsePts","PctTstPts","ConfLowP","ConfUppP","SIndex")]

## ---- echo = FALSE-------------------------------------------------------
vancouver_java_df[differences, c("CTUID", "PctBsePts","PctTstPts","ConfLowP","ConfUppP","SIndex")]

## ------------------------------------------------------------------------
set.seed(85335)
reps.output <- replicate(10, sppt(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp))
reps.output <- lapply(c("ConfLowP", "ConfUppP"), function(y) do.call(cbind, sapply(reps.output, function(x) x@data[y])))

## ------------------------------------------------------------------------
as.numeric(reps.output[[1]][52,])

## ------------------------------------------------------------------------
vancouver_java_df$ConfLowP[1] %in% reps.output[[1]][1,]

## ------------------------------------------------------------------------
# ConfLowP
table(sapply(1:nrow(vancouver_java_df), function(x) {vancouver_java_df$ConfLowP[x] %in% reps.output[[1]][x,]} ))
# ConfUppP
table(sapply(1:nrow(vancouver_java_df), function(x) {vancouver_java_df$ConfUppP[x] %in% reps.output[[2]][x,]} ))

## ------------------------------------------------------------------------
set.seed(85335)
reps.output <- replicate(50, sppt(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)) # this will take some time
reps.output <- lapply(c("ConfLowP", "ConfUppP"), function(y) do.call(cbind, sapply(reps.output, function(x) x@data[y])))

## ------------------------------------------------------------------------
# ConfLowP
table(sapply(1:nrow(vancouver_java_df), function(x) {vancouver_java_df$ConfLowP[x] %in% reps.output[[1]][x,]} ))
# ConfUppP
table(sapply(1:nrow(vancouver_java_df), function(x) {vancouver_java_df$ConfUppP[x] %in% reps.output[[2]][x,]} ))

