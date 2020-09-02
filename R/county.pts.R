#' approx lat lon of each US County by FIPS
#'
#' A dataset containing latitude and longitude approx of each US county,
#'  created as average lat and average lon of all block group internal points.
#'  CREATING county.pts from bg.pts to get lat lon of middle roughly of county from block group lat lon dataset: \cr
#'  # countylat <- aggregate(bg.pts$lat, by = list(substr(bg.pts$FIPS, 1, 5)), FUN = mean) \cr
#'  # countylon <- aggregate(bg.pts$lon, by = list(substr(bg.pts$FIPS, 1, 5)), FUN = mean) \cr
#'  # countyfips <- countylat$Group.1 \cr
#'  # countylat <- countylat$x \cr
#'  # countylon <- countylon$x \cr
#'  # county.pts <- data.frame(FIPS.COUNTY = countyfips, lat = countylat, lon = countylon, stringsAsFactors = FALSE) \cr
#'  # plot(county.pts$lon, county.pts$lat, pch = '.', xlim = c(-170,-60), ylim = c(18, 75)) \cr
#'
#' @format A data frame with one row per county and FIPS.COUNTY, lat, lon as colnames
#'
#'
"county.pts"

