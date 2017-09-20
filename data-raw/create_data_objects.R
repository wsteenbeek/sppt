# load packages for opening the units shapefile
library(rgdal)


####################################################################################
#
#
# Toy Daya
#
#
####################################################################################

areas.sp <- readOGR("data-raw/basic_test", "areas")
points1.sp <- readOGR("data-raw/basic_test", "points1")
points2.sp <- readOGR("data-raw/basic_test", "points2")

devtools::use_data(areas.sp, points1.sp, points2.sp, overwrite = TRUE)


####################################################################################
#
#
# Vancouver data
#
#
####################################################################################

vancouver_areas.sp <- readOGR("data-raw/vancouver_all", "areas")
vancouver_points1.sp <- readOGR("data-raw/vancouver_all", "points1")
vancouver_points2.sp <- readOGR("data-raw/vancouver_all", "points2")

devtools::use_data(vancouver_areas.sp, vancouver_points1.sp, vancouver_points2.sp, overwrite = TRUE)
