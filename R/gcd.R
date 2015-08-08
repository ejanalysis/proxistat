#' @title Distance between two points by Haversine, Spherical Law of Cosines, or Vincenty inverse formula
#' @description Calculates the geodesic distance between two points (or multiple pairs of points) specified by degrees (DD) latitude/longitude using
#' Haversine formula (hf), Spherical Law of Cosines (slc) and Vincenty inverse formula for ellipsoids (vif) \cr
#' Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/} \cr
#' Note these are not the most accurate method for long distances (e.g., >1000 km) nor the fastest. 
#' @details *** frompoints and topoints must have same number of rows, defining all pairs of points.
#'   Alternatively can define a single point while topoints defines a series of points, or vice versa.
#'   Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#'   but use \code{\link{pmin}} instead of \code{\link{min}} to vectorize it to handle at least pairs,
#'   and parameters changed to keep lat/lon as a matrix or data.frame rather than 2 separate vectors.
#' @param dfunc Character string "hf" for Haversine by default, or "slc" for Spherical Law of Cosines 
#'   (and may add "vif" for Vincenty inverse formula but that is not implemented here). 
#'   For "sp" algorithm (\pkg{sp}), see \code{\link{get.distances}}
#' @param frompoints Required matrix or data.frame of 2 columns, named lat and lon, with latitude and longitude(s) in degrees, one row per point.
#' @param topoints Required matrix or data.frame of 2 columns, named lat and lon, with latitude and longitude(s) in degrees, one row per point.
#' @param units Optional character variable specifying 'km' or 'miles', 'km' by default.
#' @return Distance in kilometers by default, or in miles if units='miles'
#' @seealso \code{\link{convert}}, \code{\link{gcd}}, \code{\link{get.distances}}, \code{\link{get.distances.all}}
#' @export
gcd <- function(frompoints, topoints, dfunc='hf', units='km') {
  
  # handle cases where an input is only one row (one point)
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow=1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(topoints)) {mycols <- names(topoints); topoints <- matrix(topoints, nrow=1); dimnames(topoints)[[2]] = mycols }
  
  colnames(frompoints) <- latlon.colnames.check(frompoints)
  colnames(topoints)   <- latlon.colnames.check(topoints)
  
  # Work with 4 vectors of equal length, 
  # and Convert degrees to radians
  frompoints <- deg2rad(frompoints)
  topoints   <- deg2rad(  topoints)
  long1 <- frompoints[ , 'lon']
  long2 <-   topoints[ , 'lon']
  lat1  <- frompoints[ , 'lat']
  lat2  <-   topoints[ , 'lat']

  if (!(units %in% c('km', 'miles'))) {stop('units must be "miles" or "km" ')}
  
  if (!missing(dfunc)) {
    #if (!(dfunc %in% c('hf', 'slc', 'vif'))) {
    if (!(dfunc %in% c('hf', 'slc'))) {
      #stop('must specify dfunc as hf, slc, or vif')
      stop('must specify dfunc as hf or slc')
      }
  }
  
  d = switch(dfunc,
             'hf'  = gcd.hf( long1, lat1, long2, lat2),
             'slc' = gcd.slc(long1, lat1, long2, lat2) #,
             # 'vif' = gcd.vif(long1, lat1, long2, lat2)
  )
  if (units=='miles') {d <- convert(d, 'km', 'miles')}
  return(d)
}
