#' @title Distance based on spherical law of cosines
#' @description Calculates the geodesic distance between two points specified by radian latitude/longitude 
#' using the Spherical Law of Cosines (slc). 
#' Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#' @param long1 longitude(s) in radians
#' @param lat1 latitude(s) in radians
#' @param long2 longitude(s) in radians
#' @param lat2 latitude(s) in radians
#' @return Distance in km
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}