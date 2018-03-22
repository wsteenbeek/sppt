#' @importFrom sp over
#' @importFrom dplyr group_by summarise
NULL

#' Performs a Spatial Point Pattern Test following Wheeler et al. (2018)
#'
#' As opposed to the traditional SPPT, this test calculates the difference in
#' the proportions using either a variety of Chi-square proportions tests or using
#' Fisher's exact test, and then the p-values are adjusted for multiple comparisons.
#' See https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3111822 for a description
#' and example.
#'
#' @param p1.sp           the first points spatialobject, order does not matter
#' @param p2.sp           the second points spatialobject, order does not matter
#' @param uoa.sp          the units of analysis spatial object
#' @param conf_level      confidence interval, default = 95
#' @param test            test to conduct, currently either 'Yates' or 'Fisher', default Yates
#' @param tsmethod        test to conduct, choice of "Chi2_Nmin1", "Chi2",
#'                        "Yates", or "Fisher", default = "Chi2_Nmin1"
#' @param adj             method to adjust p-values (as per p.adjust), default "BY". To turn off, set to "none".
#' @return Returns uoa.sp (a spatial polygons data frame) including sppt_diff outcomes.
#' @examples
#' # Plot areas, base points data, and test points data
#' plot(areas.sp)
#' text(coordinates(areas.sp), label = areas.sp$ID)
#' text(coordinates(points1.sp), label = points1.sp$ID, col="blue")
#' text(coordinates(points2.sp), label = points2.sp$ID, col="red")
#'
#' output <- sppt_diff(points1.sp, points2.sp, areas.sp, test="Chi2_Nmin1")
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
#' myoutput1 <- sppt_diff(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp, test = "Chi2_Nmin1", adj = "none")
#' summary.sppt(myoutput1)
#'
#' set.seed(9866)
#' myoutput2 <- sppt_boot(vancouver_points1.sp, vancouver_points2.sp, vancouver_areas.sp)
#' summary.sppt(myoutput2)
#'
#' @export
sppt_diff <- function(p1.sp, p2.sp, uoa.sp, conf_level = 95, test = "Chi2_Nmin1", adj = "BY"){

  # Check Test
  if(!test %in% c("Chi2_Nmin1", "Chi2", "Yates", "Fisher")) stop( paste("Your test argument of", test,"is not valid.") )

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
  npoints_per_uoa <- dplyr::group_by(basedata_over_results, uoa_id)
  npoints_per_uoa <- dplyr::summarize(npoints_per_uoa, npoints = n())
  npoints_per_uoa <- as.data.frame(npoints_per_uoa)

  # fill outcome data.frame
  outcome <- data.frame(uoa_id = uoa$uoa_id)
  outcome$nevents.b <- npoints_per_uoa[match(outcome$uoa_id, npoints_per_uoa$uoa_id), "npoints"]
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
  npoints_per_uoa <- dplyr::group_by(testdata_over_results, uoa_id)
  npoints_per_uoa <- dplyr::summarize(npoints_per_uoa, npoints = n())
  npoints_per_uoa <- as.data.frame(npoints_per_uoa)

  # base data outcome data.frame
  outcome$nevents.t <- npoints_per_uoa[match(outcome$uoa_id, npoints_per_uoa$uoa_id), "npoints"]
  outcome$nevents.t[is.na(outcome$nevents.t)] <- 0
  # add percentages:
  outcome$perc.t <- with(outcome, nevents.t/sum(nevents.t)*100)
  # add totals
  outcome$tot.t <- with(outcome, sum(nevents.t))

  #################################
  # calculating p-value of the differences
  #################################

  outcome$diff_perc <- with(outcome, perc.b - perc.t)
  outcome$p.value <- -1

  outcome$p.value <- sapply(outcome$uoa_id, function(x){
    ro <- outcome[outcome$uoa_id == x, ]
    da <- matrix(c(ro$nevents.b, ro$nevents.t, ro$tot.b - ro$nevents.b, ro$tot.t - ro$nevents.t), ncol = 2)

    if (test == "Chi2_Nmin1"){
      r <- suppressWarnings(as.numeric(chisq.test(da, correct = FALSE)$statistic))
      N <- sum(da)
      pval <- pchisq(r*(N-1)/N, 1, lower.tail = FALSE)
    } else if (test %in% c("Chi2", "Yates")){
      res <- suppressWarnings(prop.test(da, conf.level = conf_level/100, correct = ifelse(test == "Yates", TRUE, FALSE)))
      pval <- res$p.value
    } else if (test == "Fisher") {
      res <- suppressWarnings(fisher.test(da, conf.level = conf_level/100))
      pval <- res$p.value
    }

    return(pval)
  })

  outcome$SIndex <- 0
  outcome$SIndex[(outcome$p.value < 1 - conf_level/100) & (outcome$perc.b > outcome$perc.t)] <- 1
  outcome$SIndex[(outcome$p.value < 1 - conf_level/100) & (outcome$perc.b < outcome$perc.t)] <- -1

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
