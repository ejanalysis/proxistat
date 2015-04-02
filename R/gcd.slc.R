#' @title Distance based on spherical law of cosines
#' @description Calculates the geodesic distance between two points 
#'   (or multiple pairs of points) specified by radian latitude/longitude.
#' @details long1 and lat1 must be same length. long2 and lat1 must be same length.
#'   All four must be the same length, defining pairs of points. 
#'   Alternatively long1 and lat1 can define a single point while long2 and lat2 define a series of points, or vice versa.
#'   Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#'   but use \code{\link{pmin}} instead of \code{\link{min}} to vectorize it to handle at least pairs.
#' @param long1 longitude(s) in radians, vector of one or more numbers
#' @param lat1 latitude(s) in radians, vector of one or more numbers
#' @param long2 longitude(s) in radians, vector of one or more numbers
#' @param lat2 latitude(s) in radians, vector of one or more numbers
#' @return Distance in kilometers
#' @seealso \code{\link{convert}}, \code{\link{gcd}}, \code{\link{get.distances}}, \code{\link{get.distances.all}}
#' @export
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}
