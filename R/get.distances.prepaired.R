#' @title Find distances between points, for pairs of points already organized as pairs.
#'
#' @description
#' \code{get.distances.prepaired} returns all the distances between each specified pair of points.
#' 
#' @details
#' May need to fix cases where only a single row is input.
#' This function returns a matrix or vector of distances, 
#' between points specified as pairs of lat/lon values.
#' Points are specified using latitude and longitude in decimal degrees.
#' Relies on the \pkg{sp} package for the \code{\link[sp]{spDistsN1}} and \code{\link[sp]{SpatialPoints}} functions.
#' 
#' @param pts A matrix or data.frame that has columns names 'fromlon', 'fromlat', 'tolon', 'tolat' with datum=WGS84 assumed.
#' @return Returns a vector of distances as numbers, in kilometers. Each element corresponds to one row in pts.
#' @seealso \code{\link{get.distances.all}} for a useful general function, \code{\link{get.distances}} for get.distances() which allows you to specify a search radius and 
#'   get distances only within that radius which can be faster,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit
#'   based on distances to nearby points.
#' @concept proximity
#' @examples
#' test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428, 38.0), 
#'  fromlon = c(-77.0896572305, -77.0896199948, -77.0)), 
#'  .Names = c("lat", "lon"), row.names = c("one", "two", "three"), class = "data.frame")
#' test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435), 
#'  tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), 
#'  .Names = c("lat", "lon"), class = "data.frame", row.names = c("a", "b", "c"))
#' get.distances.prepaired(data.frame(
#'   fromlat=test.from$lat, fromlon=test.from$lon, tolat=test.to$lat, tolon=test.to$lon)
#' )
#' @export
get.distances.prepaired <- function(pts) {

  #require(sp)

  n <- length(pts[,1])
  frompoints <- sp::SpatialPoints(coords = data.frame(
    x = pts[,'fromlon'], y = pts[,'fromlat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84")
  )
  topoints   <- sp::SpatialPoints(coords = data.frame(
    x = pts[,'tolon'], y = pts[,'tolat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84")
  )
  distances <- vector(length=n)
  for (i in 1:n) {
    distances[i] <- sp::spDistsN1(frompoints[i], topoints[i], longlat=TRUE) 
    # result is in kilometers. this is not vectorized in both from and to, so need to use loop or get all pairs as matrix
  }
  return(distances)
}
