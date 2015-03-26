#' @title Find distance from each point in a set to the nearest of a second set of points (by lat/lon).
#'
#' @description
#' \code{get.nearest} returns the distance from each point in a set to the nearest of a second set of points (by lat/lon).
#' 
#' @details
#' This function returns a vector of distances, 
#' which are the distances from one set of points to the nearest single member (if any) of another set of points.
#' Points are specified using latitude and longitude in decimal degrees.
#' Relies on the \pkg{sp} package for the \code{\link[sp]{spDists}} and \code{\link[sp]{SpatialPoints}} functions.
#' \cr\cr
#' A future version may use get.distances.all() but for performance only use it for distance pairs (pairs of points) that have been initially 
#' quickly filtered using lat/lon to be not too far, in an attempt to go much faster in an initial pass.
#' *** get.nearest with loops takes 42 seconds vs 3 seconds for this version, for 100k frompoints and 100 topoints:  Sys.time(); x=get.nearest2(t100k, t100); Sys.time()
#' > Sys.time(); x=get.nearest2(testpoints(1e6), testpoints(100)); Sys.time()
#' [1] 14:33:05 EDT
#' [1] 14:33:33 EDT  <30 seconds from 1 mill to 100 points, as in finding nearest of 100 sites for 9% of the US Census blocks.
#' But R hung/crashed on 11mill frompoints -- Probably out of memory. *** Need to break it up into batches of maybe 1 to 100 million distances at a time? 
#' There are 11,078,297 blocks according to \url{http://www.census.gov/geo/maps-data/data/tallies/national_geo_tallies.html}
#' 
#' @param frompoints A matrix or data.frame with two cols, 'lat' and 'lon' (or only 2 cols that are lat and lon in that order) with datum=WGS84 assumed.
#' @param topoints A matrix or data.frame with two cols, 'lat' and 'lon' (or only 2 cols that are lat and lon in that order) with datum=WGS84 assumed.
#' @param ignore0 A logical, default is FALSE, specifying whether to ignore distances that are zero and report only the minimum nonzero distance. 
#'   Useful if nearest point other than self, where frompoints=topoints, for example.
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes these 2 columns:
#'   a col named fromrow of index numbers starting at 1 specifying the frompoint and a similar col named n specifying the row of the nearest topoint.
#' @param return.latlons Logical value, FALSE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#' @param units A string that is 'miles' by default, or 'km' for kilometers, specifying units for distances returned.
#' @return By default, returns a vector of distances, but can return a matrix of numbers, with columns that can include fromrow and torow indexing 
#'   which is nearest (first if >1 match) of topoints, fromlat, fromlon, tolat, tolon, and d (distance).
#'   Distance returned is in miles by default, but with option to set units='km' to get kilometers.
#'   See parameters for details on other formats that may be returned if specified.
#' @seealso \code{\link{get.distances}} which gets distances between all points (within an optional search radius),
#'   \code{\link{get.distances.all}} which allows you to get distances between all points,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points, and
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit based on distances to nearby points.
#' @concept proximity
#' @import sp
#' @examples
#' test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428), 
#'  fromlon = c(-77.0896572305, -77.0896199948)), .Names = c("lat", "lon"), 
#'  row.names = c("6054762", "6054764"), class = "data.frame")
#' test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435), 
#'  tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), .Names = c("lat", "lon"),
#'  class = "data.frame", row.names = c("6054762", "6054763", "6054764"))
#' get.nearest2(test.from, test.to)
#' get.nearest2(testpoints(10), testpoints(30))
#' @export
get.nearest2 <- function(frompoints, topoints, units='miles', ignore0=FALSE, 
                         return.rownums=FALSE, return.latlons=FALSE, radius=Inf) {
  
  n.from <- length(frompoints[,1])
  n.to <- length(topoints[,1])
  #frompoints and topoints each must be a matrix with 'lat' and 'lon' colnames (or only 2 cols that are lat and lon in that order)
  
  # ***** Notes on performance/ speed: ******
  # It should be easy to speed this up (for very large numbers of topoints) by using a box to search within (as get.distances does) and only enlarge the search box if no topoints are found in it.
  # One could assume topoints are uniformly distributed in extent defined by range(c(frompoints$lat, topoints$lat)) and range(c(frompoints$lon, topoints$lon))
  # Then do initial search for nearest by using get.distances(frompoints, topoints, max.miles=x) 
  # where max.miles is chosen so there is a 95% chance that at least one topoint will be within max.miles ***
  # *** BUT doesn't that assume frompoint is in the center of all the topoints? and that box is close to square not long rectangle?
  # So full extent of A sqkm for N topoints means avg of N/A topoints per sqkm, or A/N sqkms have 1 topoint on avg, and 
  # Each of N topoints has box/A chance of being in box, and 
  # chance of at least one being in box is 1-(1-box/A)^N = 0.95, so box/A= 1 - (1-0.95)^(1/N)
  #
  #   searchRadiuskm <- function(frompoints, topoints, prob) {
  #     meanlat = mean(topoints$lat)
  #     latrangekm = meters.per.degree.lat(meanlat ) * abs(diff(range(c(frompoints$lat, topoints$lat))))  / 1000
  #     lonrangekm = meters.per.degree.lon(meanlat ) * abs(diff(range(c(frompoints$lon, topoints$lon))))  / 1000
  #     allPointsSqkm = latrangekm * lonrangekm 
  #     n.to=length(topoints$lat)
  #     searchboxarea = allPointsSqkm * (1-(1-prob)^(1/n.to)) 
  #     searchRadius1 = sqrt(searchboxarea) / 2
  #     return(searchRadius1)
  #   }
  #   searchRadius1 <- searchRadiuskm(frompoints, topoints, 0.95)
  #   searchRadiusLat <- searchRadius1 / ( meters.per.degree.lat(mean(topoints$lat))/1000 )
  #   searchRadiusLon <- searchRadius1 / ( meters.per.degree.lon(mean(topoints$lat))/1000 )
  #   # There is a high probability that within ± this search radius you will find the nearest of topoints (at least one of the topoints)
  #   if (usebox) {
  #     # would want to limit search for a given frompoint, using box, but that is slower than just getting ALL distances in sp package.
  #   }
  
  frompoints <- sp::SpatialPoints(coords = data.frame(x = frompoints[,'lon'], y = frompoints[,'lat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  topoints   <- sp::SpatialPoints(coords = data.frame(x = topoints[  ,'lon'], y = topoints[  ,'lat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  
  distances <- sp::spDists(frompoints, topoints, longlat=TRUE)
  
  if (ignore0) {
    # This could be improved to consider machine precision to handle zero vs nearly zero values correctly.
    distances[distances==0] <- Inf
  }
  
  d = rowMins( distances, na.rm = TRUE)
  
  if (units=='miles') {
    miles.per.km <- convert( 1, 'km', 'miles')
    d <- d * miles.per.km
  }
  
  d[d > radius] <- NA
  
  if (return.rownums || return.latlons) {
    #torow <- apply( distances, 1, which.min ) # this was slightly slower than line below approach
    torow <- max.col(distances)
  }
  
  if (return.rownums & return.latlons) { 
    nearest <- matrix(nrow=n.from, ncol=7)
    nearest[ , ] <- c(d, 1:n.from, torow, as.vector(frompoints@coords[ , c('y','x')]), as.vector(topoints@coords[ torow, c('y','x')]))
    colnames(nearest) <- c('d', 'fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon')
  }
  
  if (!return.rownums & return.latlons) { 
    nearest <- matrix(nrow=n.from, ncol=5)
    nearest[ , ] <- c(d, as.vector(frompoints@coords[ , c('y','x')]), as.vector(topoints@coords[ torow, c('y','x')]))
    colnames(nearest) <- c('d', 'fromlat', 'fromlon', 'tolat', 'tolon')
  }
  
  if (return.rownums & !return.latlons) { 
    nearest <- matrix(nrow=n.from, ncol=3)
    nearest[ , ] <- c(d, 1:n.from, torow)
    colnames(nearest) <- c('d', 'fromrow', 'torow')
  }
  
  if (!return.rownums & !return.latlons) { 
    nearest <- matrix(d, ncol = 1)
    colnames(nearest) <- 'd'
  }
  
  return(nearest)
}
