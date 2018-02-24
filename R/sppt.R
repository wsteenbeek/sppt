#' @import sp
NULL

#' Performs the Spatial Point Pattern Test (SPPT)
#'
#' This is a point pattern test that measures the degree of
#' similarity at the local level between two spatial point patterns
#' and is an area-based test.
#'
#' @param base_points.sp  the base points spatialobject
#' @param test_points.sp  the test points spatialobject
#' @param uoa.sp          the units of analysis spatial object
#' @param outputlist      should the output of the function be a list, including all simulation runs as a matrix? default = FALSE
#' @param nsamples        number of samples in simulations, default = 200
#' @param percpoints      percentage of points used in simulations, default = 85
#' @param conf_level      confidence interval, default = 95
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
#' summary.sppt(myoutput)
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
#' head(myoutput[[1]]@data) # similar as above
#' head(myoutput[[2]])
#'
#' # Vancouver data
#' set.seed(9866)
#' myoutput <- sppt(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)
#' summary.sppt(myoutput)
#'
#' @export
sppt <- function(base_points.sp, test_points.sp, uoa.sp, outputlist=FALSE, nsamples=200, percpoints=85, conf_level=95){

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
  # base_points_per_unit <- data.frame(uniqueid = uoa$uniqueid, nevents = colSums(rgeos::gContains(uoa, base_points, byid = TRUE)))

  # count number of base_points per areal unit
  # save the results of the over function in a dataframe for later reference
  basedata_over_results <- data.frame(point_id = 1:length(base_points), uoa_id = sp::over(base_points, uoa)$uoa_id)

  # number of points per unit (only for units that have any points)
  npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = basedata_over_results, FUN = length)

  # base data outcome data.frame
  baseoutcome <- data.frame(uoa_id = uoa$uoa_id)
  baseoutcome$nevents <- npoints_per_uoa[match(baseoutcome$uoa_id, npoints_per_uoa$uoa_id), "point_id"]
  baseoutcome$nevents[is.na(baseoutcome$nevents)] <- 0
  # add percentages:
  baseoutcome$perc <- with(baseoutcome, nevents/sum(nevents)*100)

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

  # base data outcome data.frame
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
    test_points_sample <- sample(testdata_over_results$point_id, size = samplesize)

    # create temporary testdata_over_results based on sample
    testdata_over_results_sample <- testdata_over_results[testdata_over_results$point_id %in% test_points_sample,]

    # number of points per unit (only for units that have any points)
    npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = testdata_over_results_sample, FUN=length)

    # base data outcome data.frame
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

  outcome.sp <- uoa.sp
  outcome.sp$uoa_id <- uoa$uoa_id

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


#' A convenience function to output the standard global S-value
#'
#' This function takes as input a spatial object generated by the \code{sppt} function
#' and returns the standard global S-value: the mean of all local \code{similarity}
#' values. The standard global S-value is the percentage of areas with a similar proportion
#' of test points as compared to the base points.
#'
#' @param sppt.sp  the spatialobject generated by the \code{sppt} function
#' @return         returns the standard global S-value
#' @examples
#' set.seed(76772)
#' myoutput <- sppt(base_points.sp = points1.sp, test_points.sp = points2.sp, uoa.sp = areas.sp)
#' summary.globalS.standard(myoutput)
#'
#' @keywords internal
.summary.globalS.standard <- function(sppt.sp){
  mean(sppt.sp$globalS, na.rm=TRUE)
}


#' A convenience function to output the robust global S-value
#'
#' This function takes as input a spatial object generated by the \code{sppt} function
#' and returns the robust global S-value: the mean of all robust local \code{similarity}
#' values. The robust global S-value is the percentage of areas with a similar proportion
#' of test points as compared to the base points, not counting areas in which no points
#' occur in the base data and the test data.
#'
#' @param sppt.sp  the spatialobject generated by the \code{sppt} function
#' @return         returns the robust global S-value
#' @examples
#' set.seed(76772)
#' myoutput <- sppt(base_points.sp = points1.sp, test_points.sp = points2.sp, uoa.sp = areas.sp)
#' summary.globalS.robust(myoutput)
#'
#' @keywords internal
.summary.globalS.robust <- function(sppt.sp){
  mean(sppt.sp$globalS.robust, na.rm=TRUE)
}


#' A convenience function to output S-values
#'
#' This function takes as input a spatial object generated by the \code{sppt} function
#' and returns the standard global S-value and the robust global S-value in a list. The standard
#' global S-value refers to the mean of all local \code{similarity} values (i.e.,
#' the percentage of areas with a similar proportion of test points as compared to the base points.)
#' The robust global S-value refers to the mean of all robust local \code{similarity}
#' values. The robust global S-value is the percentage of areas with a similar proportion
#' of test points as compared to the base points, not counting areas in which no points
#' occur in both the base data and the test data.
#'
#' @param sppt.sp  the spatialobject generated by the \code{sppt} function
#' @return         returns the standard and robust global S-values
#' @examples
#' set.seed(76772)
#' myoutput <- sppt(base_points.sp = points1.sp, test_points.sp = points2.sp, uoa.sp = areas.sp)
#' summary.sppt(myoutput)
#'
#' @export
summary.sppt <- function(sppt.sp){
  # print(paste0("Standard global S-value: ", .summary.globalS.standard(sppt.sp)))
  # print(paste0("  Robust global S-value: ", .summary.globalS.robust(sppt.sp)))
  return(list(globalS.standard = .summary.globalS.standard(sppt.sp),
              globalS.robust = .summary.globalS.robust(sppt.sp)
              )
         )
}
