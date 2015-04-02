#' @title Distance between two points by Haversine, Spherical Law of Cosines, or Vincenty inverse formula
#' @description Calculates the geodesic distance between two points (or multiple pairs of points) specified by degrees (DD) latitude/longitude using
#' Haversine formula (hf), Spherical Law of Cosines (slc) and Vincenty inverse formula for ellipsoids (vif)
#' Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#' Note these are not the most accurate method for long distances (e.g., >1000 km) nor the fastest. 
#' @details long1 and lat1 must be same length. long2 and lat1 must be same length.
#'   All four must be the same length, defining pairs of points. 
#'   Alternatively long1 and lat1 can define a single point while long2 and lat2 define a series of points, or vice versa.
#'   Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#'   but use \code{\link{pmin}} instead of \code{\link{min}} to vectorize it to handle at least pairs.
#' @param algorithm Character string "hf" for Haversine by default, or "slc" for Spherical Law of Cosines 
#'   (and may add "vif" for Vincenty inverse formula but that is not implemented here)
#' @param long1 longitude(s) in radians, vector of one or more numbers
#' @param lat1 latitude(s) in radians, vector of one or more numbers
#' @param long2 longitude(s) in radians, vector of one or more numbers
#' @param lat2 latitude(s) in radians, vector of one or more numbers
#' @return Distance in kilometers
#' @seealso \code{\link{convert}}, \code{\link{gcd}}, \code{\link{get.distances}}, \code{\link{get.distances.all}}
#' @export
gcd <- function(long1, lat1, long2, lat2, algorithm='hf') {
  
  # Convert degrees to radians
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)

  if (!missing(algorithm)) {
    #if (!(algorithm %in% c('hf', 'slc', 'vif'))) {
    if (!(algorithm %in% c('hf', 'slc'))) {
      #stop('must specify algorithm as hf, slc, or vif')
      stop('must specify algorithm as hf or slc')
      }
  }
  
  d = switch(algorithm,
             'hf'  = gcd.hf( long1, lat1, long2, lat2),
             'slc' = gcd.slc(long1, lat1, long2, lat2) #,
             # 'vif' = gcd.vif(long1, lat1, long2, lat2)
  )
  return(d)
}
