#' @title Generate a number of randomly placed points, as latitude/longitude values.
#'
#' @description
#' Generate a number of randomly placed points, as latitude/longitude values.
#' 
#' @details
#' This function returns n points at random locations using uniform distributions of latitude and longitude values, with specified ranges.
#' Points are specified using latitude and longitude in decimal degrees.
#' #' \cr\cr
#' @param n Numeric value, required, TRUE by default. Specifies how many testpoints to return. Must be an integer between zero and 50 million, and not NA.
#' @param minlat Default 40. A number that is the minimum latitude in decimal degrees to use for generating random points within some range.
#' @param maxlat Default 42. A number that is the maximum latitude in decimal degrees to use for generating random points within some range.
#' @param minlon Default -125. A number that is the minimum longitude in decimal degrees to use for generating random points within some range.
#' @param maxlon Default -70. A number that is the maximum longitude in decimal degrees to use for generating random points within some range.
#' @param as.df Logical, default is TRUE, in which case returns a data.frame, otherwise a matrix.
#' 
#' @return By default, returns a data.frame (or matrix if as.df=FALSE) that has 2 columns: lat and lon, in decimal degrees, with 1 row per point.
#' @seealso \code{\link{get.distances.all}} which allows you to get distances between all points,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit based on distances to nearby points.
#' @concept proximity
#' @examples
#' testpoints(19,minlat=47,maxlat=48)
#' get.distances(testpoints(1000),testpoints[10],radius=999,return.rownums=TRUE,return.latlons=TRUE)
#'     
#' @export
testpoints <- function(n, minlat=40,maxlat=42, minlon=-125,maxlon=-70, as.df=TRUE) { 
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if (missing(n) || !is.numeric(n) || is.na(n) || !is.wholenumber(n) || is.infinite(n) || n < 0 || n > 5e7) {
    if (n == 1e6) {warning('a million is a lot')}
    if (n > 1e6) {warning('more than a million is more than a lot')}
    stop('n must be specified, and must be a whole number, where <0 is too hard to fathom and >50 million is too hard to keep track of')
  }
  if ( minlat < -90 || maxlat < -90 || minlat > 90 || maxlat > 90 )     {stop('minlat and maxlat must be from -90 through 90 decimal degrees')}
  if ( minlon < -180 || maxlon < -180 || minlon > 180 || maxlon > 180 ) {stop('minlon and maxlon must be from -180 through 180 decimal degrees')}
  
  x=structure( c(runif(n, min=minlat, max=maxlat), runif(n, min=minlon, max=maxlon) ),
    .Dim = c(n, 2L), .Dimnames = list( NULL, c("lat", "lon")))
  if (as.df) {return(as.data.frame(x))} else {return(x)}
}
