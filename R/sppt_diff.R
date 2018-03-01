#' @import sp
NULL

#' @import exact2x2
NULL

#' Performs a Spatial Point Pattern Test (SPPT) following Wheeler et al. (2018)
#'
#' As opposed to the traditional SPPT, this test calculates the difference in
#' the proportions using either a Chi-square proportions test or using Fisher's
#' exact test, the p-values are then adjusted for multiple comparisons. See
#' https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3111822 for a description
#' and example. \cr
#' If test = 'Fisher', Fisher's exact test is used but with adjusted confidence
#' intervals (method = 'minlike'). See the R package exact2x2 for details
#' (https://cran.r-project.org/package=exact2x2)
#'
#' @param p1.sp           the first points spatialobject, order does not matter
#' @param p2.sp           the second points spatialobject, order does not matter
#' @param uoa.sp          the units of analysis spatial object
#' @param conf_level      confidence interval, default = 95
#' @param test            test to conduct, currently either 'Yates' or 'Fisher', default Yates
#' @param tsmethod        if test equals 'Fisher', then exact2x2:exact2x2() is used, with default 'minlike'.
#'                        With Yates's test, method is ignored.
#' @param adj             method to adjust p-values (as per p.adjust), default "BY"
#' @return Returns a spatial polygons data frame object that has fields related to the test
#'         calculated. When test equals \code{Fisher}, the lower and upper confidence intervals are
#'         for the odds ratio. When test equals \code{Yates}, the lower and upper confidence intervals
#'         are for the difference in proportions
#' @examples
#' # Plot areas, base points data, and test points data
#' plot(areas.sp)
#' text(coordinates(areas.sp), label = areas.sp$ID)
#' text(coordinates(points1.sp), label = points1.sp$ID, col="blue")
#' text(coordinates(points2.sp), label = points2.sp$ID, col="red")
#'
#' output <- sppt_diff(points1.sp, points2.sp, areas.sp, test="Yates")
#' summary.sppt(output)
#' output@data
#'
#' output <- sppt_diff(points1.sp, points2.sp, areas.sp, test="Fisher")
#' summary.sppt(output)
#' output@data
#'
#' sppt_diff(points1.sp, points2.sp, areas.sp, test="????") # should get error message
#'
#' # Vancouver data
#' set.seed(9866)
#' myoutput <- sppt_diff(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)
#' summary.sppt(myoutput)
#'
#' # The sppt_diff() outcomes without correction for multiple comparisons
#' # are close to sppt_boot() outcomes:
#' set.seed(9866)
#' myoutput1 <- sppt_diff(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp, test = "Fisher", adj = "none")
#' summary.sppt(myoutput1)
#'
#' set.seed(9866)
#' myoutput2 <- sppt_boot(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp, nsamples = 200)
#' summary.sppt(myoutput2)
#'
#' @export
sppt_diff <- function(p1.sp, p2.sp, uoa.sp, conf_level = 95, test = "Yates", tsmethod = "minlike", adj = "BY"){

  #################################
  # Read-in Units of Analysis
  #################################
  uoa <- uoa.sp

  uoa$uoa_id <- 1:length(uoa)

  #################################
  # p1 data set:
  #################################

  # keep only points that fall within a spatial unit of analysis
  p1 <- p1.sp[uoa.sp, ]

  # save the results of the over function in a dataframe for later reference
  basedata_over_results <- data.frame(point_id = 1:length(p1), uoa_id = sp::over(p1, uoa)$uoa_id)

  # number of points per unit (only for units that have any points)
  npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = basedata_over_results, FUN=length)

  # fill outcome data.frame
  outcome <- data.frame(uoa_id = uoa$uoa_id)
  outcome$nevents.b <- npoints_per_uoa[match(outcome$uoa_id, npoints_per_uoa$uoa_id), "point_id"]
  outcome$nevents.b[is.na(outcome$nevents.b)] <- 0
  # add percentages:
  outcome$perc.b <- with(outcome, nevents.b/sum(nevents.b)*100)
  # add totals
  outcome$tot.b <- with(outcome, sum(nevents.b))

  #################################
  # p2 data set:
  #################################

  # keep only points that fall within a spatial unit of analysis
  p2 <- p2.sp[uoa.sp, ]

  # save the results of the over function in a dataframe for later reference
  testdata_over_results <- data.frame(point_id = 1:length(p2), uoa_id = sp::over(p2, uoa)$uoa_id)

  # number of points per unit (only for units that have any points)
  npoints_per_uoa <- stats::aggregate(. ~ uoa_id, data = testdata_over_results, FUN=length)

  # base data outcome data.frame
  outcome$nevents.t <- npoints_per_uoa[match(outcome$uoa_id, npoints_per_uoa$uoa_id), "point_id"]
  outcome$nevents.t[is.na(outcome$nevents.t)] <- 0
  # add percentages:
  outcome$perc.t <- with(outcome, nevents.t/sum(nevents.t)*100)
  # add totals
  outcome$tot.t <- with(outcome, sum(nevents.t))

  #################################
  # calculating p-value and confidence interval of the differences
  #################################

  outcome$diff_perc <- with(outcome, perc.b - perc.t)
  outcome$p.value <- -1
  outcome$lower <- NA
  outcome$upper <- NA
  outcome$SIndex <- NA

  # calculating p-values and confidence intervals
  # can likely be improved via vectorization, also should add in Agresti-Coffo method
  if (test=="Yates"){
    for (i in outcome$uoa_id){
	    ro <- outcome[outcome$uoa_id == i,]
	    da <- matrix(c(ro$nevents.b, ro$nevents.t, ro$tot.b - ro$nevents.b, ro$tot.t - ro$nevents.t), ncol=2)
	    res <- suppressWarnings(prop.test(da, conf.level = conf_level/100, correct = TRUE))
	    outcome[outcome$uoa_id == i, c("p.value")] <- res$p.value
      outcome[outcome$uoa_id == i, c("lower")] <- res$conf.int[1]*100
      outcome[outcome$uoa_id == i, c("upper")] <- res$conf.int[2]*100

      # Calculate S Index within loop
      if (res$conf.int[1] > 0){
        outcome[outcome$uoa_id == i,c("SIndex")] <- 1
      } else if (res$conf.int[2] < 0){
        outcome[outcome$uoa_id == i,c("SIndex")] <- -1
      } else {
        outcome[outcome$uoa_id == i,c("SIndex")] <- 0
      }
	  }
  } else if (test=="Fisher"){
    for (i in outcome$uoa_id){
  	  ro <- outcome[outcome$uoa_id == i,]
  	  da <- matrix(c(ro$nevents.b, ro$nevents.t, ro$tot.b - ro$nevents.b, ro$tot.t - ro$nevents.t), ncol=2)
  	  res <- exact2x2::exact2x2(da, conf.level = conf_level/100, tsmethod = tsmethod)
  	  outcome[outcome$uoa_id == i, c("p.value")] <- res$p.value

  	  #These confidence intervals are for the odds ratio - not for the difference in percentages
      outcome[outcome$uoa_id == i,c("lower")] <- res$conf.int[1]
      outcome[outcome$uoa_id == i,c("upper")] <- res$conf.int[2]

      #Since it is odds ratio, check above/below 1, not 0
      if (res$conf.int[1] > 1){
        outcome[outcome$uoa_id == i,c("SIndex")] <- 1
      } else if (res$conf.int[2] < 1){
        outcome[outcome$uoa_id == i,c("SIndex")] <- -1
      } else {
        outcome[outcome$uoa_id == i,c("SIndex")] <- 0
      }
    }
  } else {
    stop( paste("Your test argument of", test,"is not valid, please specify either 'Yates' or 'Fisher'") )
  }

  # doing the multiple comparison adjustment to the p-values
  outcome$p.adjusted <- p.adjust(p = outcome$p.value, method = adj)
  outcome[which(outcome$p.adjusted >= 1-conf_level/100), "SIndex"] <- 0

  #################################
  # Calculating Robust SIndex
  #################################

  outcome$SIndex.robust <- outcome$SIndex
  outcome[outcome$nevents.b + outcome$nevents.t == 0, "SIndex.robust"] <- NA

  # Calculate the global values and add to the data frame
  # Local Similarity: if % of crime in t0 (basedata) is indistinguishable from t1 (testdata),
  #         then get 1, otherwise 0.
  outcome$similarity <- ifelse(outcome$SIndex == 0, 1, 0)

  outcome$globalS <- mean(outcome$similarity)

  outcome$similarity.robust <- ifelse(outcome$SIndex.robust == 0, 1, 0)
  outcome$globalS.robust <- mean(outcome$similarity.robust, na.rm = TRUE)

  #################################
  # Calculating localS
  #################################
  # local S is the inverse of SIndex. Simple interpretation, with basedata == t0 and testdata == t1:
  # if % of events in a particular unit in t1 (testdata) is higher than in t0 (basedata),
  #         it becomes +1 (i.e. an INcrease over time)
  # if % of events in a particular unit in t1 (testdata) is indistinguishable from t0 (basedata),
  #         it becomes 0
  # if % of events in a particular unit in t1 (testdata) is lower than in t0 (basedata),
  #         it becomes -1 (i.e. a DEcrease over time)

  outcome$localS <- -1 * outcome$SIndex
  outcome$localS.robust <- -1 * outcome$SIndex.robust

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
