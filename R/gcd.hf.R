#' @title Distance based on Haversine formula
#' @description Calculates the geodesic distance between two points (or multiple pairs of points) specified by radian latitude/longitude 
#' using the Haversine formula (hf)
#' Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#' but use pmin instead of min to vectorize it.
#' @param long1 longitude(s) in radians
#' @param lat1 latitude(s) in radians
#' @param long2 longitude(s) in radians
#' @param lat2 latitude(s) in radians
#' @return Distance in kilometers
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(pmin(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}
