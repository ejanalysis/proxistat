#' @title Distance between two points by Haversine, Spherical Law of Cosines, or Vincenty inverse formula
#' @description Calculates the geodesic distance between two points specified by degrees (DD) latitude/longitude using
#' Haversine formula (hf), Spherical Law of Cosines (slc) and Vincenty inverse formula for ellipsoids (vif)
#' Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#' Note these are not the most accurate method for long distances (e.g., >1000 km) nor the fastest. 
#' @param algorithm Character string "hf" for Haversine by default, or "slc" for Spherical Law of Cosines, or "vif" for Vincenty inverse formula
#' @param long1 longitude in decimal degrees
#' @param lat1 latitude in decimal degrees
#' @param long2 longitude in decimal degrees
#' @param lat2 latitude in decimal degrees
#' @return Distance in kilometers
#' @export
gcd <- function(long1, lat1, long2, lat2, algorithm='haversine') {
  
  # Convert degrees to radians
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)

  if (!missing(algorithm)) {
    if (!(algorithm %in% c('hf', 'slc', 'vif'))) {stop('must specify algorithm as hf, slc, or vif')}
  }
  
  d = switch(algorithm,
             'hf'  = gcd.hf( long1, lat1, long2, lat2),
             'slc' = gcd.slc(long1, lat1, long2, lat2),
             'vif' = gcd.vif(long1, lat1, long2, lat2)
  )
  return(d)
}
