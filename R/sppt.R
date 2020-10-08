#' @importFrom sp over
#' @importFrom dplyr group_by summarise
NULL

#' Performs Andresen's Spatial Point Pattern Test
#'
#' This is a point pattern test that measures the degree of
#' similarity at the local level between two spatial point patterns
#' and is an area-based test.
#'
#' In its default settings, the function is the original Andresen's sppt, so
#' only Test data is randomly resampled (without replacement, by default
#' using 85% of the Test points).
#'
#' By changing 'bootstrap' to TRUE, the 'percpoints' argument is ignored
#' and the Test points are a bootstrapped sample: within each simualed dataset,
#' as many points as the test set itself are sampled, but with replacement
#' within each sample.
#'
#' @param base_points.sp  the Base data of type SpatialPoints*
#' @param test_points.sp  the Test data of type SpatialPoints*
#' @param uoa.sp          the units of analysis of type SpatialPolygons*
#' @param outputlist      should the output of the function be a list, including all simulation runs as a matrix? default = FALSE
#' @param nsamples        number of samples in simulations, default = 200
#' @param percpoints      percentage of points used in simulations, default = 85
#' @param conf_level      confidence interval, default = 95
#' @param bootstrap       logical, default is FALSE
#' @return When \code{outputlist} is \code{FALSE} (the default), returns the
#'         \code{uoa.sp} spatialobject including SPPT outcomes. When
#'         \code{outputlist} is \code{TRUE} (the default), returns a list with
#'         the first list element the \code{uoa.sp} spatialobject including
#'         SPPT outcomes, and the second list element a matrix of all
#'         \code{nsamples} simulations.
#' @examples
#' # Plot areas, base points data, and test points data
#' plot(areas.sp)
#' text(coordinates(areas.sp), label = areas.sp$ID)
#' text(coordinates(points1.sp), label = points1.sp$ID, col="blue")
#' text(coordinates(points2.sp), label = points2.sp$ID, col="red")
#'
#' set.seed(76772)
#' myoutput <- sppt(base_points.sp = points1.sp, test_points.sp = points2.sp, uoa.sp = areas.sp)
#' summary_sppt(myoutput)
#' myoutput@data
#'
#' # is exactly the same as:
#' set.seed(76772)
#' myoutput <- sppt(points1.sp, points2.sp, areas.sp,
#'                  outputlist=FALSE, nsamples=200, percpoints=85, conf_level=95)
#' myoutput@data
#'
#' # now with a list being outputted, the 2nd element has all simulation outcomes
#' set.seed(76772)
#' myoutput <- sppt(points1.sp, points2.sp, areas.sp, outputlist = TRUE)
#' head(myoutput[[1]]@data) # same as previous
#' head(myoutput[[2]])
#'
#' # Vancouver data
#' set.seed(9866)
#' myoutput <- sppt(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)
#' summary_sppt(myoutput)
#'
#' # Vancouver data, using a bootstrap approach instead of Andresen's subsampling method
#' set.seed(9866)
#' myoutput <- sppt(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp, bootstrap = TRUE)
#' summary_sppt(myoutput)
#'
#' @export
sppt <- function(base_points.sp, test_points.sp, uoa.sp, outputlist = FALSE, nsamples = 200, percpoints = 85, conf_level = 95, bootstrap = FALSE){

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

  # how many points need to be sampled in each iteration?
  if(bootstrap){
    samplesize <- nrow(testdata_over_results)
  } else {
    samplesize <- round((percpoints/100) * nrow(testdata_over_results))
  }

  ## Function: for a sample of points, calculate number of points in each unit of analysis
  calculate.points.per.unit <- function(nsample, data_over_results){

    # select sample of base points
    points_sample <- sample(data_over_results$point_id, size = samplesize, replace = bootstrap)

    # create temporary basedata_over_results based on sample
    data_over_results_sample <- data.frame(point_id = points_sample)
    data_over_results_sample$uoa_id <- data_over_results[match(data_over_results_sample$point_id, data_over_results$point_id), "uoa_id"]

    # number of points per unit (only for units that have any points)
    npoints_per_uoa <- dplyr::group_by(data_over_results_sample, uoa_id)
    npoints_per_uoa <- dplyr::summarise(npoints_per_uoa, npoints = dplyr::n())
    npoints_per_uoa <- as.data.frame(npoints_per_uoa)

    # base data outcome data.frame
    sampleoutcome <- data.frame(uoa_id = uoa$uoa_id)
    sampleoutcome$nevents <- npoints_per_uoa[match(sampleoutcome$uoa_id, npoints_per_uoa$uoa_id), "npoints"]
    sampleoutcome$nevents[is.na(sampleoutcome$nevents)] <- 0

    # add percentages:
    sampleoutcome$perc <- with(sampleoutcome, nevents/sum(nevents)*100)

    return(sampleoutcome)
  }

  # get all samples in one list
  allsamples <- lapply(1:nsamples, function(x, y) calculate.points.per.unit(x, testdata_over_results))

  # extract the percentage objects only, per areal unit
  allperc <- do.call(cbind, lapply(allsamples, function(x) x["perc"]))

  # sort each row(= unit) from lowest to highest percentage, then remove the bottom 5 and top 5
  allperc <- as.data.frame(t(apply(as.matrix(allperc), 1, sort)))

  # e.g. with 200 simulations, drop bottom 5 and top 5. So select column nr 6 and 195.
  #      with 100 simulations, drop bottom 2.5 and top 2.5. So select column 3 and 98.
  column_lower <- floor(((100-conf_level)/100/2) * nsamples)+1
  column_upper <- ceiling((1- (100-conf_level)/100/2) * nsamples)

  # save the 'lowest' and 'highest' percentage score for that unit
  conf <- data.frame(uniqueid = uoa$uoa_id, lower = allperc[, column_lower], upper = allperc[, column_upper])

  #################################
  # Calculate SIndex for unit of analysis by evaluating if base percentage falls within confidence interval of test data
  #################################

  svalues <- data.frame(uoa_id = uoa$uoa_id)
  svalues$SIndex[baseoutcome$perc < conf$lower] <- -1
  svalues$SIndex[(baseoutcome$perc >= conf$lower) & (baseoutcome$perc <= conf$upper)] <- 0
  svalues$SIndex[baseoutcome$perc > conf$upper] <- 1

  #################################
  # Create outcome shapefile
  #################################

  outcome.sp <- uoa

  outcome.sp$SIndex <- svalues$SIndex
  outcome.sp$NumBsePts <- baseoutcome$nevents
  outcome.sp$NumTstPts <- testoutcome$nevents
  outcome.sp$PctBsePts <- baseoutcome$perc
  outcome.sp$PctTstPts <- testoutcome$perc
  outcome.sp$SumBseTstPts <- outcome.sp$NumBsePts + outcome.sp$NumTstPts
  outcome.sp$ConfLowP <- conf$lower
  outcome.sp$ConfUppP <- conf$upper

  # local S is the inverse of SIndex. Simple interpretation, with basedata == t0 and testdata == t1:
  # if % of events in a particular unit in t1 (testdata) is higher than in t0 (basedata),
  #         it becomes +1 (i.e. an INcrease over time)
  # if % of events in a particular unit in t1 (testdata) is indistinguishable from t0 (basedata),
  #         it becomes 0
  # if % of events in a particular unit in t1 (testdata) is lower than in t0 (basedata),
  #         it becomes -1 (i.e. a DEcrease over time)
  outcome.sp$localS <- -1 * outcome.sp$SIndex

  # Local Similarity: if % of crime in t0 (basedata) is indistinguishable from t1 (testdata),
  #         then get 1, otherwise 0.
  outcome.sp$similarity <- ifelse(outcome.sp$SIndex == 0, 1, 0)

  # standard global S index
  outcome.sp$globalS <- mean(outcome.sp$similarity)


  ### Robust measures of: SIndex; localS; similarity; globalS

  # vector of which units/rows to include in the calculation of robust measures
  include_for_robust <- outcome.sp$SumBseTstPts > 0

  # set-up NA's for all robust measures
  outcome.sp$SIndex.robust <- NA
  outcome.sp$localS.robust <- NA
  outcome.sp$similarity.robust <- NA
  outcome.sp$globalS.robust <- NA

  # calculate robust measures
  outcome.sp$SIndex.robust[include_for_robust] <- outcome.sp$SIndex[include_for_robust]
  outcome.sp$localS.robust[include_for_robust] <- outcome.sp$localS[include_for_robust]
  outcome.sp$similarity.robust[include_for_robust] <- outcome.sp$similarity[include_for_robust]
  outcome.sp$globalS.robust[include_for_robust] <- mean(outcome.sp$similarity.robust, na.rm=TRUE)


  #################################
  # Return outcome object
  #################################

  # If outputlist = FALSE, return the spatial object: units of analysis with
  #                         the information on number of points, percentage of points,
  #                          and SIndex and similarity
  # If outputlist = TRUE,  return a list of objects:
  #                        (1) the spatial object (see above)
  #                        (2) all percentages of simulations for test data

  if (!isTRUE(outputlist)){
    return(outcome.sp)
  } else {
    outcome <- list(sppt.sp = outcome.sp, simuls = allperc)
    # name of the first list element == the name of the basedata + "_" + name of the testdata
    names(outcome)[1] <- paste0(deparse(substitute(base_points)), "_", deparse(substitute(test_points)))
    return(outcome)
  }

}
