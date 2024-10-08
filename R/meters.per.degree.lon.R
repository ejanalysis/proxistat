#' @title Convert degrees longitude to meters East-West.
#' @description
#' \code{meters.per.degree.lon} returns meters traveled East-West per decimal degree longitude, given latitude (Northern Hemisphere).
#' @details
#' This function returns the meters traveled East-West per decimal degree longitude, at a given latitude (Northern Hemisphere).
#' This is an approximation and is less accurate further from the given latitude.
#' Based on 
#' \url{http://en.wikipedia.org/wiki/Latitude#Length_of_a_degree_of_latitude} and \cr
#' \url{http://en.wikipedia.org/wiki/Longitude#Length_of_a_degree_of_longitude} \cr \cr
#' Input theta is latitude on WGS84.
#' Function is as follows:  \cr
#'   theta.r <- 0.01745329 * theta \cr
#'   ecc2 <- 0.00669438 \cr
#'   return( 20037508 * cos(theta.r) / ( 180 * sqrt(1- ecc2 * (sin(theta.r))^2 ) ) ) \cr
#' Based on the following calculations: \cr
#' The equatorial.radius used is 6378137.0 in meters \cr
#' 2 pi / 360 = 0.01745329 \cr
#' for the WGS84 ellipsoid with a = 6,378,137.0 m and b = 6,356,752.3142 m. \cr
#' equatorial.radius <-  6378137.0  # a in meters \cr
#' b=6356752.3142 \cr
#' ecc2 <- (equatorial.radius^2 - b^2)/equatorial.radius^2  \cr
#' ecc2 <- (6378137.0^2 - 6356752.3142^2) / 6378137.0^2 \cr
#' pi * equatorial.radius = 20037508 \cr \cr
#' Also see \url{http://en.wikipedia.org/wiki/Longitude} and \url{http://en.wikipedia.org/wiki/Decimal_degrees}
#' @param theta The decimal degrees of latitude of the Northern Hemisphere location(s) of interest, as number or vector of numbers.
#' @return Returns meters traveled East-West per decimal degree longitude, as a number or vector of numbers the same length as the input.
#' @seealso \code{\link{meters.per.degree.lat}} for a similar function but for travel North-South,
#'   and \code{\link{deltalon.per.km}} for the inverse of this function (other than a factor of 1000),
#'   and \code{\link{get.distances.all}} and also \code{\link{get.distances}} to get distances between points, and related functions.
#' @concept proximity
#' @examples
#' meters.per.degree.lon(32)
#' meters.per.degree.lon(c(0,45,72))
#' @export
meters.per.degree.lon <- function(theta) { 
  theta.r <- 0.01745329 * theta
  ecc2 <- 0.00669438
  return( 20037508 * cos(theta.r) / ( 180 * sqrt(1- ecc2 * (sin(theta.r))^2 ) ) )
}
