# load packages for opening the units shapefile
library(rgdal)

### Toy data

# read in to_data java output and save in Vignettes folder
toy_java <- readOGR("data-raw/basic_test", "basic_test_output")
save(toy_java, file = "vignettes/toy_java.rda")

# remove points outside any areal unit
points1.sp.new <- points1.sp[areas.sp, ]
points2.sp.new <- points2.sp[areas.sp, ]

writeOGR(points1.sp.new, "data-raw/basic_test_new", "points1.new", driver = "ESRI Shapefile")
writeOGR(points2.sp.new, "data-raw/basic_test_new", "points2.new", driver = "ESRI Shapefile")

# run Java app. Then load and save in Vignettes folder
toy_java_new <- readOGR("data-raw/basic_test_new", "basic_test_new_output")
save(toy_java_new, file = "vignettes/toy_java_new.rda")


### Vancouver data

# remove points outside any areal unit
vancouver_points1.sp.new <- vancouver_points1.sp[vancouver_areas.sp, ]
vancouver_points1.sp.new <- vancouver_points2.sp[vancouver_areas.sp, ]

writeOGR(vancouver_points1.sp.new, "data-raw/vancouver_all_new", "points1_vancouver.sp.new", driver = "ESRI Shapefile")
writeOGR(vancouver_points1.sp.new, "data-raw/vancouver_all_new", "points2_vancouver.sp.new", driver = "ESRI Shapefile")

# run Java app. Then load and save in Vignettes folder
vancouver_java_new <- readOGR("data-raw/vancouver_all_new", "vancouver_all_new_output")
save(vancouver_java_new, file = "vignettes/vancouver_java_new.rda")

