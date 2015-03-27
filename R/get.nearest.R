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
#' NOTE: *** This is OK for <100k frompoints and 100 topoints, but starts to get slow. 
#' This method is MUCH slower than just using other version of get.nearest that gets full distance matrix via spDists() and then gets minima.
#' A future version may use get.distances.all() but for performance only use it for distance pairs (pairs of points) that have been initially 
#' quickly filtered using lat/lon to be not too far, in an attempt to go much faster in an initial pass.
#'
#' @param frompoints A matrix or data.frame with two cols, 'lat' and 'lon' (or only 2 cols that are lat and lon in that order) with datum=WGS84 assumed.
#' @param topoints A matrix or data.frame with two cols, 'lat' and 'lon' (or only 2 cols that are lat and lon in that order) with datum=WGS84 assumed.
#' @param radius Optional number, default is Inf. Distance within which search should be limited, or max distance that will be returned.
#' @param ignore0 A logical, default is FALSE, specifying whether to ignore distances that are zero and report only the minimum nonzero distance. 
#'   Useful if nearest point other than self, where frompoints=topoints, for example.
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes these 2 columns:
#'   a col named fromrow of index numbers starting at 1 specifying the frompoint and a similar col named n specifying the row of the nearest topoint.
#' @param return.latlons Logical value, FALSE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#' @param units A string that is 'miles' by default, or 'km' for kilometers, specifying units for distances returned.
#' @return By default, returns a matrix of numbers, with columns fromrow, torow indexing which is nearest of topoints, and d (distance).
#'   Distance returned is in miles by default, but with option to set units='km' to get kilometers.
#'   See parameters for details on other formats that may be returned if specified.
#' @seealso \code{\link{get.distances}} which gets distances between all points (within an optional search radius),
#'   \code{\link{get.distances.all}} which allows you to get distances between all points,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points, and
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit based on distances to nearby points.
#' @concept proximity
#' @import sp
#' @examples
#' set.seed(999)
#' t1=testpoints(1)
#' t10=testpoints(10)
#' t100=testpoints(100)
#' t1k=testpoints(1e3)
#' t10k=testpoints(1e4)
#' t100k=testpoints(1e5)
#' t1m=testpoints(1e6)
#' #t10m=testpoints(1e7)
#' 
#' get.nearest(t1, t1)
#' get.nearest(t1, t10[2, ,drop=FALSE])
#' get.nearest(t10, t1k)
#' get.nearest(t10, t1k, radius=500, units='km')
#' get.nearest(t10, t1k, radius=10, units='km')
#' 
#' test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428), 
#'  fromlon = c(-77.0896572305, -77.0896199948)), .Names = c("lat", "lon"), 
#'  row.names = c("6054762", "6054764"), class = "data.frame")
#' test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435), 
#'  tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), .Names = c("lat", "lon"),
#'  class = "data.frame", row.names = c("6054762", "6054763", "6054764"))
#' get.nearest(test.from, test.to)
#' 
#' @export
get.nearest <- function(frompoints, topoints, units='miles', ignore0=FALSE, 
                        return.rownums=TRUE, return.latlons=FALSE, radius=Inf) {

  # ***** Notes on performance/ speed: ******
  # It should be easy to speed this up (for very large numbers of topoints) by using a box to search within (as get.distances does) and only enlarge the search box if no topoints are found in it.
  # One could assume topoints are uniformly distributed in extent defined by range(topoints$lat) and range(topoints$lat)
  # Then do initial search for nearest by using get.distances(frompoints, topoints, max.miles=x) 
  # where max.miles is chosen so there is a 95% chance that at least one topoint will be within max.miles ***
  # So full extent of A sqkm for N topoints means avg of N/A topoints per sqkm, or A/N sqkms have 1 topoint on avg, and 
  # Each of N topoints has box/A chance of being in box, and 
  # chance of at least one being in box is 1-(1-box/A)^N = 0.95, so box/A= 1 - (1-0.95)^(1/N)
  # e.g., 
  # search box area should be roughly ... 

  #frompoints and topoints each must be a matrix with 'lat' and 'lon' colnames (or only 2 cols that are lat and lon in that order)
  
  # This formula (using SpatialPoints() & spDists() ) is the best method I've tried so far.
  # [DIRECTLY USING MY OWN DISTANCE FORMULA WITH sin cos etc. was even faster, but results appeared to be wrong - not sure if units or formula were the problem.]

  if (!(units %in% c('miles', 'km'))) {stop('units must be unspecified (i.e., miles) or miles or km')}
  if (units=='miles' & !missing(radius) ) {radius <- convert( radius, 'miles', 'km') }
  
  # Here, May need to fix cases where only a single row is in frompoints or topoints ( get.distances() handles that well.)
    
  # This check of colnames is copied from get.distances()
  latlon.colnames.check <- function(mypoints) {
    if (!( ('lat' %in% colnames(mypoints)) & ('lon' %in% colnames(mypoints)) )) {
      if (length(colnames(mypoints))==2) {
        warning('assuming the first column is latitude and second is longitude')
        return(c('lat', 'lon'))
      } else {
        stop('frompoints must have columns named lat and lon, or at least have only 2 columns so they can be interpreted as lat and lon')
      }
    } else {
      return(colnames(mypoints))
    }
  }
  
  if(is.vector(frompoints) & length(frompoints)==2) {frompoints <- matrix(frompoints, ncol=2) }
  if(is.vector(topoints)   & length(topoints)==2)   {topoints   <- matrix(topoints,   ncol=2) }
  
  colnames(frompoints) <- latlon.colnames.check(frompoints)
  colnames(topoints)   <- latlon.colnames.check(topoints)
  
  #require(sp)
  
  n.from <- NROW(frompoints[,1])
  n.to   <- NROW(topoints[,1])
  
  if (!return.latlons) {nearest <- matrix(nrow=n.from, ncol=2) } # will have 1 or 3, but 2 for now
  if (return.latlons)  {nearest <- matrix(nrow=n.from, ncol=6)} # will have 5 or 7, but 6 for now. 
  
  frompoints <- sp::SpatialPoints(coords = data.frame(x = frompoints[,'lon'], y = frompoints[,'lat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  topoints   <- sp::SpatialPoints(coords = data.frame(x = topoints[  ,'lon'], y = topoints[  ,'lat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  
  ###########################################################
  
  for (i in 1:n.from) {
    
    distances <- sp::spDists(frompoints[i], topoints, longlat=TRUE)
    # result is a matrix    
    if (ignore0) {distances[distances==0] <- Inf}
    
#cat('str distances is \n'); print(str(distances))
#cat('\n str nearest is \n');print(str(nearest))
    
    # distances is just a 1 row and 1+column matrix now
    distances[distances > radius] <- Inf
    
    nearest[i, 1] <- which.min(distances)
    nearest[i, 2] <- distances[ nearest[i, 1] ]

    if (return.latlons) {
      #need fromlat, fromlon, tolat, tolon
# cat('str nearest is \n');print(str(nearest))
# cat('str frompoints is \n');print(str(frompoints))
      
      nearest[i, 3:4] <- frompoints@coords[i, c('y','x')]
      nearest[i, 5:6] <- topoints@coords[ nearest[i,1], c('y','x')]
# cat('str nearest is \n');print(str(nearest))
    }
  }
  ###########################################################
  
  if (return.latlons) {
    colnames(nearest) <- c('torow', 'd', 'fromlat', 'fromlon', 'tolat', 'tolon')
  } else {
    colnames(nearest) <- c('torow', 'd')
  }
  
  if (units=='miles') {
    nearest[ , 'd'] <- convert( nearest[ , 'd'], 'km', 'miles')
  }
  
  if (return.rownums) {
    mynames <- colnames(nearest)
    nearest <- matrix( c(fromrow=1:n.from, nearest), ncol=1+NCOL(nearest))
    colnames(nearest) <- c('fromrow' , mynames)
  } else {
    if (return.latlons) {
      nearest <- nearest[ , colnames(nearest)!='torow', drop=FALSE]
    } else {
      # make it a vector if it shouldn't have any cols for fromrow torow lat lon
      
#      print(str(nearest))
#       > get.nearest(testpoints(9), testpoints(20), return.rownums = FALSE, return.latlons=FALSE)
#       Error in nearest$d : $ operator is invalid for atomic vectors
#       
      
      as.vector( nearest <- nearest[ , 'd'] )
    }
  }
  # nearest <- as.data.frame(nearest) # prior version
  return(nearest)
}
