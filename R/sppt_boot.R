#' @importFrom sp over
#' @importFrom dplyr group_by summarise
NULL

#' Performs a Spatial Point Pattern Test with resampling of both Base and Test
#' datasets.
#'
#' This function is different from the original sppt() in that it
#' does a (bootstrapped) resampling procedure on both Base and Test data,
#' instead of just on the Test data.
#'
#' After a (bootstrapped) resample, for each areal unit the difference between
#' the two percentages is calculated immediately (and this is done nsamples
#' times). If the 95% distribution of the differences between (resampled) Base
#' and Test percentages excludes 0, they are deemed statistically different.
#'
#' With the default settings of bootstrap = TRUE,
#' it's a real bootstrap procedure: (a) for each sampling loop (nsamples of
#' times) as many points are sampled as present in the data (so argument
#' 'percpoints' is ignored, or more precisely, set to 100%!); (b) points are
#' sampled WITH replacement.
#'
#' If one wants to mimic the behavior of standard sppt() but with the added
#' improvement that the choice of Base and Test data does not affect results,
#' set percpoints to 85 and bootstrap = FALSE.
#'
#' @param base_points.sp  the Base data of type SpatialPoints*
#' @param test_points.sp  the Test data of type SpatialPoints*
#' @param uoa.sp          the units of analysis of type SpatialPolygons*
#' @param nsamples        number of samples in simulations, default = 200
#' @param percpoints      percentage of points used in simulations if bootstrap = FALSE, default = 85
#' @param conf_level      confidence interval, default = 95
#' @param bootstrap       logical. should bootstrap sampling be done (i.e. with replacement?) default = TRUE
#' @return Returns the \code{uoa.sp} spatialobject including sppt outcomes.
#' @examples
#' # Plot areas, base points data, and test points data
#' plot(areas.sp)
#' text(coordinates(areas.sp), label = areas.sp$ID)
#' text(coordinates(points1.sp), label = points1.sp$ID, col="blue")
#' text(coordinates(points2.sp), label = points2.sp$ID, col="red")
#'
#' set.seed(76772)
#' myoutput <- sppt_boot(base_points.sp = points1.sp, test_points.sp = points2.sp, uoa.sp = areas.sp)
#' summary_sppt(myoutput)
#' myoutput@data
#'
#' # Vancouver data
#' set.seed(9866)
#' myoutput <- sppt_boot(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)
#' summary_sppt(myoutput)
#'
#' # Global-S may not be exactly the same when switching Base for Test
#' # due to random sampling procedure. Compare by doing the sppt_boot() 20 times:
#' # some global-s values should be the same
#' set.seed(85335)
#' reps.output <- replicate(20, mean(sppt_boot(vancouver_points1.sp,
#'        vancouver_points2.sp, vancouver_areas.sp)$globalS.robust, na.rm=TRUE))
#' set.seed(85335)
#' reps.output2 <- replicate(20, mean(sppt_boot(vancouver_points2.sp,
#'        vancouver_points1.sp, vancouver_areas.sp)$globalS.robust, na.rm=TRUE))
#' reps.output
#' reps.output2
#' reps.output %in% reps.output2
#'
#' # Mimic the original sppt procedure but with resampling of both Base and Test
#' set.seed(9866)
#' myoutput <- sppt_boot(points1.sp, points2.sp, areas.sp, bootstrap=FALSE)
#' summary_sppt(myoutput)
#' myoutput@data
#'
#' @export
sppt_boot <- function(base_points.sp, test_points.sp, uoa.sp, nsamples=200, percpoints=85, conf_level=95, bootstrap=TRUE){

  # Check validity of points objects:
  if (!is(base_points.sp, "SpatialPoints")) stop( paste("Your Base data is not a SpatialPoints* object.") )
  if (!is(test_points.sp, "SpatialPoints")) stop( paste("Your Test data is not a SpatialPoints* object.") )

  #################################
  # Read-in Units of Analysis
  #################################
  uoa <- uoa.sp

  # check if it's already a SpatialPolygonsDataFrame. If not, coerce to one.
  if (is(uoa, "SpatialPolygonsDataFrame")){
    uoa$uoa_id <- 1:length(uoa)
  } else if (is(uoa, "SpatialPolygons")){
    uoa <- SpatialPolygonsDataFrame(uoa, data = data.frame(uoa_id = 1:length(uoa)), match.ID = FALSE)
  } else {
    stop( paste("Your units of analysis are not a SpatialPolygons* object.") )
  }

  #################################
  # Base data set:
  #################################

  # keep only points that fall within a spatial unit of analysis
  base_points <- base_points.sp[uoa, ]

  # count number of base_points per areal unit
  # save the results of the over function in a dataframe for later reference
  basedata_over_results <- data.frame(point_id = 1:length(base_points), uoa_id = sp::over(base_points, uoa)$uoa_id)

  # number of points per unit (only for units that have any points)
  npoints_per_uoa <- dplyr::group_by(basedata_over_results, uoa_id)
  npoints_per_uoa <- dplyr::summarise(npoints_per_uoa, npoints = dplyr::n())
  npoints_per_uoa <- as.data.frame(npoints_per_uoa)

  # base data outcome data.frame
  baseoutcome <- data.frame(uoa_id = uoa$uoa_id)
  baseoutcome$nevents <- npoints_per_uoa[match(baseoutcome$uoa_id, npoints_per_uoa$uoa_id), "npoints"]
  baseoutcome$nevents[is.na(baseoutcome$nevents)] <- 0
  # add percentages:
  baseoutcome$perc <- with(baseoutcome, nevents/sum(nevents)*100)

  #################################
  # Test data set:
  #################################

  # keep only points that fall within a spatial unit of analysis
  test_points <- test_points.sp[uoa, ]

  # count number of test_points per areal unit
  # save the results of the over function in a dataframe for later reference
  testdata_over_results <- data.frame(point_id = 1:length(test_points), uoa_id = sp::over(test_points, uoa)$uoa_id)

  # number of points per unit (only for units that have any points)
  npoints_per_uoa <- dplyr::group_by(testdata_over_results, uoa_id)
  npoints_per_uoa <- dplyr::summarise(npoints_per_uoa, npoints = dplyr::n())
  npoints_per_uoa <- as.data.frame(npoints_per_uoa)

  # base data outcome data.frame
  testoutcome <- data.frame(uoa_id = uoa$uoa_id)
  testoutcome$nevents <- npoints_per_uoa[match(testoutcome$uoa_id, npoints_per_uoa$uoa_id), "npoints"]
  testoutcome$nevents[is.na(testoutcome$nevents)] <- 0
  # add percentages:
  testoutcome$perc <- with(testoutcome, nevents/sum(nevents)*100)


  #################################
  # Function: for a sample of points, calculate number of points in each unit of analysis
  #################################

  calculate.points.per.unit <- function(nsample, data_over_results){

    # how many points need to be sampled in each iteration?
    if(bootstrap){
      samplesize <- nrow(data_over_results)
    } else {
      samplesize <- round((percpoints/100) * nrow(data_over_results))
    }

    # select sample of points
    points_sample <- sample(data_over_results$point_id, size = samplesize, replace = bootstrap)

    # create temporary basedata_over_results based on sample
    data_over_results_sample <- data.frame(point_id = points_sample)
    data_over_results_sample$uoa_id <- data_over_results[match(data_over_results_sample$point_id, data_over_results$point_id), "uoa_id"]

    # number of points per unit (only for units that have any points)
    npoints_per_uoa <- dplyr::group_by(data_over_results_sample, uoa_id)
    npoints_per_uoa <- dplyr::summarise(npoints_per_uoa, npoints = dplyr::n())
    npoints_per_uoa <- as.data.frame(npoints_per_uoa)

    # sample outcome data.frame
    sampleoutcome <- data.frame(uoa_id = uoa$uoa_id)
    sampleoutcome$nevents <- npoints_per_uoa[match(sampleoutcome$uoa_id, npoints_per_uoa$uoa_id), "npoints"]
    sampleoutcome$nevents[is.na(sampleoutcome$nevents)] <- 0

    # add percentages:
    sampleoutcome$perc <- with(sampleoutcome, nevents/sum(nevents)*100)

    return(sampleoutcome)
  }

  #################################
  # Run function
  #################################

  # get all samples in one list
  allsamples <- lapply(1:nsamples, function(x, y) calculate.points.per.unit(x, basedata_over_results))
  # extract the percentage objects only, per areal unit
  allperc.base <- do.call(cbind, lapply(allsamples, function(x) x["perc"]))

  # get all samples in one list
  allsamples <- lapply(1:nsamples, function(x, y) calculate.points.per.unit(x, testdata_over_results))
  # extract the percentage objects only, per areal unit
  allperc.test <- do.call(cbind, lapply(allsamples, function(x) x["perc"]))

  ### Compare bootstrapped base and test percentages per unit, per sample
  allperc_diff <- allperc.test - allperc.base

  # sort each row(= unit) from lowest to highest percentage, then remove the bottom 5 and top 5
  allperc_diff <- as.data.frame(t(apply(as.matrix(allperc_diff), 1, sort)))

  # e.g. with 200 simulations, drop bottom 5 and top 5. So select column nr 6 and 195.
  #      with 100 simulations, drop bottom 2.5 and top 2.5. So select column 3 and 98.
  column_lower <- floor(((100-conf_level)/100/2) * nsamples)+1
  column_upper <- ceiling((1- (100-conf_level)/100/2) * nsamples)

  # save the 'lowest' and 'highest' percentage difference score for that unit
  conf <- data.frame(uniqueid = uoa$uoa_id, lower = allperc_diff[, column_lower], upper = allperc_diff[, column_upper])

  #################################
  # Calculate SIndex for unit of analysis by evaluating if confidence interval includes 0
  #################################

  conf$localS <- sapply(1:nrow(conf), function(x){
    if(conf$lower[x] > 0){
      return(1)
    } else if(conf$upper[x] < 0){
      return(-1)
    } else {
      return(0)
    }
  })

  outcome <- data.frame(uoa_id = uoa$uoa_id)
  outcome$NumBsePts <- baseoutcome$nevents
  outcome$NumTstPts <- testoutcome$nevents
  outcome$PctBsePts <- baseoutcome$perc
  outcome$PctTstPts <- testoutcome$perc
  outcome$SumBseTstPts <- outcome$NumBsePts + outcome$NumTstPts
  outcome <- cbind(outcome, conf[, -1])

  outcome$SIndex <- -1 * outcome$localS
  outcome$similarity <- ifelse(outcome$SIndex == 0, 1, 0)
  outcome$globalS <- mean(outcome$similarity)

  outcome$localS.robust <- outcome$localS
  outcome[outcome$SumBseTstPts == 0, "localS.robust"] <- NA
  outcome$SIndex.robust <- -1 * outcome$localS.robust
  outcome$similarity.robust <- ifelse(outcome$SIndex.robust == 0, 1, 0)
  outcome$globalS.robust <- mean(outcome$similarity.robust, na.rm = TRUE)

  #################################
  # Create outcome SpatialPolygon with data
  #################################

  # because outcome is in same order as shapefile, simply add variables to SpatialPolygonsDataFrame
  uoa@data <- cbind(uoa@data, outcome[, names(outcome)[-1]])

  #################################
  # Return outcome object
  #################################

  return(uoa)
}
