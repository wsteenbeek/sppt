#' @import sp
NULL

#' Performs a Spatial Point Pattern Test (SPPT) with resampling of
#' both Base and Test datasets
#'
#' This is a point pattern test that measures the degree of
#' similarity at the local level between two spatial point patterns
#' and is an area-based test. \cr
#' This function is different from the original sppt() in that it
#' does a resampling procedure on both Base and Test data, and then for each
#' unit of analysis the difference between the two percentages
#' is calculated immediately (and this is done nsamples times). If the
#' 95% distribution of the differences between (resampled) Base and Test
#' percentages excludes 0, they are deemed statistically different from each
#' other. \cr
#' With the standard settings of percpoints = 100 and replacement = TRUE,
#' it's a real bootstrap procedure: (a) for each sampling loop (nsamples of
#' times) as many points are sampled as present in the data (so no longer a 85%
#' selection!); (b) points are sampled WITH replacement.\cr
#' If one wants to mimic the behavior of standard sppt() but with the added
#' improvement that the choice of Base and Test data does not affect results,
#' set percpoints to 85 and replacement to FALSE.
#'
#' @param base_points.sp  the base points spatialobject
#' @param test_points.sp  the test points spatialobject
#' @param uoa.sp          the units of analysis spatial object
#' @param outputlist      should the output of the function be a list, including all simulation runs as a matrix? default = FALSE
#' @param nsamples        number of samples in simulations, default = 200
#' @param percpoints      percentage of points used in simulations, default = 100
#' @param conf_level      confidence interval, default = 95
#' @param replacement     should sampling be done with replacement? default = TRUE
#' @return Returns the \code{uoa.sp} spatialobject including SPPT outcomes.
#' @examples
#' # Plot areas, base points data, and test points data
#' plot(areas.sp)
#' text(coordinates(areas.sp), label = areas.sp$ID)
#' text(coordinates(points1.sp), label = points1.sp$ID, col="blue")
#' text(coordinates(points2.sp), label = points2.sp$ID, col="red")
#'
#' set.seed(76772)
#' myoutput <- sppt_boot(base_points.sp = points1.sp, test_points.sp = points2.sp, uoa.sp = areas.sp)
#' summary.sppt(myoutput)
#' myoutput@data
#'
#' # is exactly the same as:
#' set.seed(76772)
#' myoutput <- sppt_boot(points1.sp, points2.sp, areas.sp, nsamples=200, percpoints=100, conf_level=95, replacement=TRUE)
#' myoutput@data
#'
#' # Vancouver data
#' set.seed(9866)
#' myoutput <- sppt_boot(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)
#' summary.sppt(myoutput)
#'
#' # Global-S may not be exactly the same when switching Base for Test
#' # due to random sampling procedure. Compare by doing the sppt_boot() 20 times:
#' # some global-s values should be the same
#' set.seed(85335)
#' reps.output <- replicate(20, mean(sppt_boot(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)$globalS.robust, na.rm=TRUE))
#' set.seed(85335)
#' reps.output2 <- replicate(20, mean(sppt_boot(vancouver_points2.sp, vancouver_points1.sp, vancouver_areas.sp)$globalS.robust, na.rm=TRUE))
#' reps.output
#' reps.output2
#' reps.output %in% reps.output2
#'
#' # Mimic the original sppt procedure but with resampling of both Base and Test
#' set.seed(9866)
#' myoutput <- sppt_boot(base_points.sp = points1.sp, test_points.sp = points2.sp, uoa.sp = areas.sp, percpoints=85, replacement=FALSE)
#' summary.sppt(myoutput)
#' myoutput@data
#'
#' @export
sppt_boot <- function(base_points.sp, test_points.sp, uoa.sp, nsamples=200, percpoints=100, conf_level=95, replacement=TRUE){

  #################################
  # Read-in Units of Analysis
  #################################
  uoa <- uoa.sp

  uoa$uoa_id <- 1:length(uoa)

  #################################
  # Base data set:
  #################################

  # keep only points that fall within a spatial unit of analysis
  base_points <- base_points.sp[uoa.sp, ]

  # count number of base_points per areal unit
  # save the results of the over function in a dataframe for later reference
  basedata_over_results <- data.frame(point_id = 1:length(base_points), uoa_id = sp::over(base_points, uoa)$uoa_id)

  # number of points per unit (only for units that have any points)
  npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = basedata_over_results, FUN=length)

  # base data outcome data.frame
  baseoutcome <- data.frame(uoa_id = uoa$uoa_id)
  baseoutcome$nevents <- npoints_per_uoa[match(baseoutcome$uoa_id, npoints_per_uoa$uoa_id), "point_id"]
  baseoutcome$nevents[is.na(baseoutcome$nevents)] <- 0
  # add percentages:
  baseoutcome$perc <- with(baseoutcome, nevents/sum(nevents)*100)

  # how many points need to be sampled in each iteration?
  samplesize <- round((percpoints/100) * length(base_points))

  ## Function: for a sample of base points, calculate number of points in each unit of analysis
  calculate.basepoints.per.unit <- function(nsample){

    # select sample of base points
    base_points_sample <- sample(basedata_over_results$point_id, size = samplesize, replace = replacement)

    # create temporary basedata_over_results based on sample
    basedata_over_results_sample <- data.frame(point_id = base_points_sample)
    basedata_over_results_sample$uoa_id <- basedata_over_results[match(basedata_over_results_sample$point_id, basedata_over_results$point_id), "uoa_id"]

    # number of points per unit (only for units that have any points)
    npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = basedata_over_results_sample, FUN=length)

    # base data outcome data.frame
    sampleoutcome <- data.frame(uoa_id = uoa$uoa_id)
    sampleoutcome$nevents <- npoints_per_uoa[match(sampleoutcome$uoa_id, npoints_per_uoa$uoa_id), "point_id"]
    sampleoutcome$nevents[is.na(sampleoutcome$nevents)] <- 0
    # add percentages:
    sampleoutcome$perc <- with(sampleoutcome, nevents/sum(nevents)*100)

    return(sampleoutcome)
  }

  # get all samples in one list
  allsamples <- lapply(1:nsamples, calculate.basepoints.per.unit)

  # extract the percentage objects only, per areal unit
  allperc.base <- do.call(cbind, lapply(allsamples, function(x) x["perc"]))

  #################################
  # Test data set:
  #################################

  # keep only points that fall within a spatial unit of analysis
  test_points <- test_points.sp[uoa.sp, ]

  # count number of test_points per areal unit
  # save the results of the over function in a dataframe for later reference
  testdata_over_results <- data.frame(point_id = 1:length(test_points), uoa_id = sp::over(test_points, uoa)$uoa_id)

  # number of points per unit (only for units that have any points)
  npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = testdata_over_results, FUN=length)

  # test data outcome data.frame
  testoutcome <- data.frame(uoa_id = uoa$uoa_id)
  testoutcome$nevents <- npoints_per_uoa[match(testoutcome$uoa_id, npoints_per_uoa$uoa_id), "point_id"]
  testoutcome$nevents[is.na(testoutcome$nevents)] <- 0
  # add percentages:
  testoutcome$perc <- with(testoutcome, nevents/sum(nevents)*100)

  # how many points need to be sampled in each iteration?
  samplesize <- round((percpoints/100) * length(test_points))

  ## Function: for a sample of test points, calculate number of points in each unit of analysis
  calculate.testpoints.per.unit <- function(nsample){

    # select sample of test points
    test_points_sample <- sample(testdata_over_results$point_id, size = samplesize, replace = replacement)

    # create temporary testdata_over_results based on sample
    testdata_over_results_sample <- data.frame(point_id = test_points_sample)
    testdata_over_results_sample$uoa_id <- testdata_over_results[match(testdata_over_results_sample$point_id, testdata_over_results$point_id), "uoa_id"]

    # number of points per unit (only for units that have any points)
    npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = testdata_over_results_sample, FUN=length)

    # test data outcome data.frame
    sampleoutcome <- data.frame(uoa_id = uoa$uoa_id)
    sampleoutcome$nevents <- npoints_per_uoa[match(sampleoutcome$uoa_id, npoints_per_uoa$uoa_id), "point_id"]
    sampleoutcome$nevents[is.na(sampleoutcome$nevents)] <- 0
    # add percentages:
    sampleoutcome$perc <- with(sampleoutcome, nevents/sum(nevents)*100)

    return(sampleoutcome)
  }

  # get all samples in one list
  allsamples <- lapply(1:nsamples, calculate.testpoints.per.unit)

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
