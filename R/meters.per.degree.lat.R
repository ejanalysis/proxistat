#' @title Convert degrees latitude to meters North-South.
#'
#' @description
#' \code{meters.per.degree.lat} returns meters traveled North-South per decimal degrees latitude, given latitude (Northern Hemisphere).
#' 
#' @details
#' This function returns the meters traveled North-South per decimal degree latitude, at a given latitude (Northern Hemisphere).
#' This is an approximation and is less accurate further from the given latitude.
#' Based on 
#' \url{http://en.wikipedia.org/wiki/Latitude#Length_of_a_degree_of_latitude} and \cr 
#' \url{http://en.wikipedia.org/wiki/Longitude#Length_of_a_degree_of_longitude} \cr
#' Input theta is latitude on WGS84. \cr
#' Also see \url{http://en.wikipedia.org/wiki/Longitude} and \url{http://en.wikipedia.org/wiki/Decimal_degrees} \cr
#' @param theta The decimal degrees of latitude of the Northern Hemisphere location(s) of interest, as number or vector of numbers.
#' @return Returns meters traveled N-S, as a number or vector of numbers the same length as the input.
#' @seealso \code{\link{meters.per.degree.lon}} for a similar function but for travel East-West, with more detailed explanation/help,
#'   and \code{\link{get.distances.all}} and \code{\link{get.distances}} for distances between points, and related functions.
#' @concept proximity
#' @examples
#' meters.per.degree.lat(32)
#' meters.per.degree.lat(c(0,45,72))
#' @export
meters.per.degree.lat <- function(theta) { 
  theta.r <- 0.01745329 * theta
  return( 111132.954 - 559.822 * cos(2 * theta.r) + 1.175 * cos(4 * theta.r))
}
