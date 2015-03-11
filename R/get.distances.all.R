#' @title Find distances between points.
#'
#' @description
#' \code{get.distances.all} returns all the distances from one set of points to another set of points.
#' 
#' @details
#' This function returns a matrix or vector of distances, 
#' from each of the first set of points to each of the second set of points, 
#' where points are specified using latitude and longitude.
#' Relies on the \pkg{sp} package for the \code{\link[sp]{spDists}} and \code{\link[sp]{SpatialPoints}} functions.
#'
#' @param frompoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param topoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param units A string that is 'miles' by default, or 'km' for kilometers, 
#'   specifying units for distances returned.
#' @param return.crosstab Logical value, FALSE by default. If TRUE, value returned is a matrix of the distances, 
#'   with a row per frompoint and col per topoint.
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes two extra columns:
#'   a col of index numbers starting at 1 specifying the frompoint and a similar col specifying the topoint.
#' @param return.latlons Logical value, TRUE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#' @return By default, returns a dataframe that has 3 columns: fromrow, torow, distance 
#'   (where fromrow or torow is the row number of the corresponding input, starting at 1).
#' @seealso \code{\link{get.distances}} which allows you to specify a search radius and 
#'   get distances only within that radius which can be faster,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit
#'   based on distances to nearby points.
#' @concept proximity
#' @examples
#' test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428), 
#'  fromlon = c(-77.0896572305, -77.0896199948)), 
#'  .Names = c("lat", "lon"), row.names = c("6054762", "6054764"), class = "data.frame")
#' test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435), 
#'  tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), 
#'  .Names = c("lat", "lon"), class = "data.frame", row.names = c("6054762", "6054763", "6054764"))
#' get.distances.all(test.from, test.to)
#' get.distances.all(test.from, test.to, return.crosstab=TRUE)
#' get.distances.all(test.from, test.to, return.rownums=FALSE)
#' get.distances.all(test.from, test.to, return.latlons=FALSE)
# 'get.distances.all(test.from, test.to, return.latlons=FALSE, return.rownums=FALSE)
#' @export
get.distances.all <- function(frompoints, topoints, units='miles', return.crosstab=FALSE, return.rownums=TRUE, return.latlons=TRUE) {

  #require(sp)
  if (!(units %in% c('km', 'miles'))) {stop('units must be "miles" or "km" ')}
  km.per.mile <- convert(1, 'miles', 'km') # about 1.60934
  
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow=1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(topoints))   {mycols <- names(topoints);   topoints   <- matrix(  topoints, nrow=1); dimnames(  topoints)[[2]] = mycols }

  if (paste(colnames(frompoints),collapse='')!='latlon')  {
    warning('frompoints colnames being changed to lat and lon, in that order')
    colnames(frompoints) <- c('lat', 'lon')
  }

  if (paste(colnames(topoints),collapse='')!='latlon' ) {
    warning('topoints colnames being changed to lat and lon, in that order')
    colnames(topoints) <- c('lat', 'lon')
  }

  frompoints.sp <- sp::SpatialPoints(coords = data.frame(x = frompoints[,'lon'], y = frompoints[,'lat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  topoints.sp   <- sp::SpatialPoints(coords = data.frame(x = topoints[,'lon'], y = topoints[,'lat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  results.matrix <- sp::spDists(frompoints.sp, topoints.sp, longlat=TRUE) # result is in kilometers so far
  rm(frompoints.sp, topoints.sp)
  
  if (units=='miles') {results.matrix <- results.matrix / km.per.mile }
  
  if (return.crosstab) { 
    # if crosstab=TRUE, ignore return.rownums and return.latlons
    return(results.matrix)
  } # this will return crosstab, i.e., tall matrix of fromrow, torow, distance (3 columns, one row per from-to pair)

  if (!return.rownums & !return.latlons) { return( as.vector( t(results.matrix) )  ) }

  if (!return.rownums & return.latlons) { 
    # return tall matrix with 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'
    
    # **** BUT expand.grid creates a data.frame, which if very large is likely to be very slow vs a matrix...
    
    results <- cbind( expand.grid(topoints[,'lat'], frompoints[,'lat']), expand.grid(topoints[,'lon'], frompoints[,'lon']) , as.vector( t(results.matrix) ) )
    colnames(results) <- c('tolat', 'fromlat', 'tolon', 'fromlon', 'd')
    return( results[ , c('fromlat', 'fromlon', 'tolat', 'tolon', 'd')] )
  }

  if (return.rownums & !return.latlons) { 
    # return tall matrix with fromrow, torow, d

    # **** BUT expand.grid creates a data.frame, which if very large is likely to be very slow vs a matrix...
    
    results=cbind( expand.grid(1:length(topoints[,'lat']), 1:length(frompoints[,'lat'])), as.vector( t(results.matrix) )  )
    colnames(results) <- c('torow', 'fromrow', 'd')
    return( results[ , c('fromrow', 'torow', 'd')] )
  }
  
  if (return.rownums & return.latlons) { 
    # return tall matrix with 'fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'

    # **** BUT expand.grid creates a data.frame, which if very large is likely to be very slow vs a matrix...
    
    results=cbind( expand.grid(1:length(topoints[,'lat']), 1:length(frompoints[,'lat'])), as.vector( t(results.matrix) )  )
    colnames(results) <- c('torow', 'fromrow', 'd')

    # **** BUT expand.grid creates a data.frame, which if very large is likely to be very slow vs a matrix...
    
    results <- cbind( expand.grid(topoints[,'lat'], frompoints[,'lat']), expand.grid(topoints[,'lon'], frompoints[,'lon']) , results)
    colnames(results) <- c('tolat', 'fromlat', 'tolon', 'fromlon', 'torow', 'fromrow', 'd')
    return( results[ , c('fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd')] )
  }
}
