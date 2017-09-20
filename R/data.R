#' Six spatial units of analysis
#'
#' A SpatialPolygons object of 6 areas
#'
#' @format A SpatialPoylgonsDataFrame with 6 rows and 1 variable:
#' \describe{
#'   \item{ID}{ID number of the area}
#' }
#' @source \url{https://github.com/sppt/sppt/tree/master/java/}
"areas.sp"

#' Base Points dataset: 9 points
#'
#' A SpatialPointsDataFrame object with 9 points
#'
#' @format A SpatialPointsDataFrame with 9 rows and 4 variables:
#' \describe{
#'   \item{TYPE}{Type of transportation node}
#'   \item{ROUTENUM}{Associated Route number}
#'   \item{STATNUM}{Unimportant variable}
#'   \item{ID}{ID number of the spatial point}
#' }
#' @source \url{https://github.com/sppt/sppt/tree/master/java/}
"points1.sp"

#' Test Points dataset: 10 points
#'
#' A SpatialPointsDataFrame object with 10 points
#'
#' @format A SpatialPointsDataFrame with 10 rows and 4 variables:
#' \describe{
#'   \item{TYPE}{Type of transportation node}
#'   \item{ROUTENUM}{Associated Route number}
#'   \item{STATNUM}{Unimportant variable}
#'   \item{ID}{ID number of the spatial point}
#' }
#' @source \url{https://github.com/sppt/sppt/tree/master/java/}
"points2.sp"

#' Vanvouver data: 108 spatial units of analysis
#'
#' A SpatialPolygons object of 108 areas
#'
#' @format A SpatialPoylgonsDataFrame with 108 rows and 3 variables:
#' \describe{
#'   \item{CTUID}{ID number of the area}
#'   \item{CMAUID}{Unimportant variable}
#'   \item{PRUID}{Unimportant variable}
#' }
"vancouver_areas.sp"

#' Base Points dataset: 14585 crimes occuring in Vancouver
#'
#' A SpatialPointsDataFrame object with 14585 crimes
#'
#' @format A SpatialPointsDataFrame with 14585 rows and 32 variables:
#' \describe{
#'   \item{OBJECTID}{ID number of the spatial point}
#'   \item{YEAR}{year of crime occurrence: 2012}
#'   ...
#' }
"vancouver_points1.sp"

#' Test Points dataset: 14784 crimes occuring in Vancouver
#'
#' A SpatialPointsDataFrame object with 14784 crimes
#'
#' @format A SpatialPointsDataFrame with 14784 rows and 34 variables:
#' \describe{
#'   \item{OBJECTID_1}{ID number of the spatial point}
#'   \item{YEAR}{year of crime occurrence: 2013}
#'   ...
#' }
"vancouver_points2.sp"
