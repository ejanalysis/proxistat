#' @title Find distances between nearby points, just searching within a specified maximum distance.
#'
#' @description
#' \code{get.distances} returns the distances from one set of points to nearby members of another set of points.
#' 
#' @details
#' This function returns a matrix or vector of distances, 
#' which are the distances from one set of points to the nearby members of another set of points.
#' It searches within a circle (of radius max.miles or max.km, defining what is considered nearby), 
#' to calculate distance (in miles or km) from each of frompoints to each of topoints that is within the specified radius.
#' Points are specified using latitude and longitude in decimal degrees.
#' Relies on the sp package for the spDists() and SpatialPoints() functions.
#' Uses get.distances.all() but for performance it only uses it for distance pairs (pairs of points) that have been initially 
#' quickly filtered using lat/lon to be not much more than max.km or max.miles, in an attempt to go 
#' much faster than finding every distance pair and then dropping all outside the search radius.
#' Finding distance to all of the 11 million census blocks in usa within 5 km, for 100 points, can take a while... maybe >1 minute?
#' May wish to investigate using data.table to index and more quickly subset the (potentially 11 million Census blocks of) topoints
#' (or pre-index that block point dataset and allow this function to accept a data.table as input).
#'
#' @param frompoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param topoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param max.miles A single number defining nearby, the maximum distance searched for or recorded. Default is 5 miles unless max.km specified.
#' @param max.km A single number defining nearby, the maximum distance searched for or recorded. Default is 8.0467 kilometers unless max.miles specified.
#' @param return.units A string that is 'miles' by default, or 'km' for kilometers, specifying units for distances returned.
#' @param return.crosstab Logical value, FALSE by default. If TRUE, value returned is a matrix of the distances, 
#'   with a row per frompoint and col per topoint. (Distances larger than max search radius are not provided, even in this format).
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes two extra columns:
#'   a col of index numbers starting at 1 specifying the frompoint and a similar col specifying the topoint.
#' @param return.latlons Logical value, FALSE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#' @param tailored.deltalon Logical value, TRUE by default. Defines size of initially searched area as function of lat, for each frompoint,
#' rather than initially searching a conservatively large box. The large box is big enough for even the Northernmost US (AK) but perhaps not for further north!
#' Taking time to scale the box according to latitude makes it work for anywhere in the N. Hemisphere (Southern not tested), 
#' and speeds up the distance calculations closer to the equator, but takes a bit of time to define a custom box for each frompoint 
#' so it might be slower overall in just very northern locations? Comprehensive speed tests have not been performed.
#' @return By default, returns a matrix that has 3 columns: fromrow, torow, distance (where fromrow or torow is the row number of the corresponding input, starting at 1).
#'   Distance returned is in miles by default, but with option to set return.units='km' to get kilometers.
#'   See parameters for details on other formats that may be returned if specified.
#' @seealso \code{\link{get.distances.all}} for get.distances.all() which allows you to get distances between all points,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} for get.nearest() which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit based on distances to nearby points.
#' @concept proximity
#' @examples
#'    test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428), 
#'     fromlon = c(-77.0896572305, -77.0896199948)), 
#'     .Names = c("lat", "lon"), row.names = c("6054762", "6054764"), class = "data.frame")
#'     test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435), 
#'     tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), 
#'     .Names = c("lat", "lon"), class = "data.frame", 
#'     row.names = c("6054762", "6054763", "6054764"))
#'     
#'     n=100
#'     test.from2 <- structure(list(fromlat = runif(n, min=25,max=48), 
#'      fromlon = runif(n,min=-125,max=-70) ), 
#'      .Names = c("lat", "lon"), row.names = 1:n, class = "data.frame")
#'     n=1000
#'     test.to2 <- structure(list(tolat = runif(n, min=25,max=48), 
#'      tolon = runif(n,min=-125,max=-70) ), 
#'      .Names = c("lat", "lon"), row.names = 1:n, class = "data.frame")
#'     
#'     # see as map of many points
#'     #*** Can fail if max.miles=50... Error in rbind() numbers of
#'     #  columns of arguments do not match !
#'     #big = get.distances(test.from2, test.to2, max.miles=100, return.latlons=TRUE)
#'     big = get.distances(test.from2, test.to2, max.miles=100, return.latlons=TRUE)
#'     plot(big$fromlon, big$fromlat,main='from black circles... 
#'      closest is red, others nearby are green ')
#'     points(test.to2$lon, test.to2$lat, col='blue',pch='.')
#'     points(big$tolon, big$tolat, col='green')
#'     junk=as.data.frame( get.nearest(test.from2, test.to2) )
#'     points(test.to2$lon[junk$n],test.to2$lat[junk$n],col='red')
#'     
#'     # test cases
#'     
#'get.distances(test.from[1,],test.to[1,],max.miles=999,return.rownums=FALSE,
#'return.latlons=FALSE)
#'get.distances(test.from[1,],test.to[1,],max.miles=999,return.rownums=FALSE,return.latlons=TRUE)
#'get.distances(test.from[1,],test.to[1,],max.miles=999,return.rownums=TRUE,return.latlons=FALSE)
#'get.distances(test.from[1,],test.to[1,],max.miles=999,return.rownums=TRUE,return.latlons=TRUE)
#' 
#'get.distances(test.from[1,],test.to[1:3,],max.miles=999,return.rownums=FALSE,
#'return.latlons=FALSE)
#'get.distances(test.from[1,],test.to[1:3,],max.miles=999,return.rownums=FALSE,return.latlons=TRUE)
#'get.distances(test.from[1,],test.to[1:3,],max.miles=999,return.rownums=TRUE,return.latlons=FALSE)
#'get.distances(test.from[1,],test.to[1:3,],max.miles=999,return.rownums=TRUE,return.latlons=TRUE)
#' 
#'get.distances(test.from[1:2,],test.to[1,],max.miles=999,return.rownums=FALSE,
#'return.latlons=FALSE)
#'get.distances(test.from[1:2,],test.to[1,],max.miles=999,return.rownums=FALSE,return.latlons=TRUE)
#'get.distances(test.from[1:2,],test.to[1,],max.miles=999,return.rownums=TRUE,return.latlons=FALSE)
#'get.distances(test.from[1:2,],test.to[1,],max.miles=999,return.rownums=TRUE,return.latlons=TRUE)
#' 
#'get.distances(test.from[1:2,],test.to[1:3,],max.miles=999,return.rownums=FALSE,
#'return.latlons=FALSE)
#'get.distances(test.from[1:2,],test.to[1:3,],max.miles=999,return.rownums=FALSE,
#'return.latlons=TRUE)
#'get.distances(test.from[1:2,],test.to[1:3,],max.miles=999,return.rownums=TRUE,
#'return.latlons=FALSE)
#'get.distances(test.from[1:2,],test.to[1:3,],max.miles=999,return.rownums=TRUE,
#'return.latlons=TRUE)
#'get.distances(test.from[1:2,],test.to[1:3,],max.km=0.7,  return.rownums=TRUE,
#'return.latlons=TRUE)
#' 
#' get.distances(test.from[1,],test.to[1:3, ], return.crosstab=TRUE)
#' get.distances(test.from[1:2,],test.to[1, ], return.crosstab=TRUE)
#' get.distances(test.from[1:2,],test.to[1:3, ], return.crosstab=TRUE)
#' get.distances(test.from[1:2,],test.to[1:3, ], max.km=0.7, return.crosstab=TRUE)
#'   # Warning message:
#'   # In get.distances(test.from[1:2, ], test.to[1:3, ], max.km = 0.7,  :
#'   # Ignoring return.crosstab because max.miles was specified
#' @export
get.distances <- function(frompoints, topoints, max.miles=5, max.km=8.0467, return.units='miles', 
                          return.rownums=TRUE, return.latlons=FALSE, return.crosstab=FALSE, tailored.deltalon=TRUE) {
  # Specify way to define size of rectangular box to use to search within as quick filter rather than finding full matrix of distances between all points.
  #   (hopefully faster to iterate over modest # of sites & highly (&rapidly?) filtered set of blocks, than over all 10m+ blocks)
  #   (speed boost may be >100x if only 1% of all blocks are in that window - )
  #   (3400 miles at widest point, 5km is 3.1 miles, 6.2 mile wide window/3400= 1/548 of the width, and 10x10 km box is extremely tiny % of US area.)
  #  Make FIPS columns factors for speed when rollup to block groups? 
  #  Index blocks on longitude and latitude. Use data.table for speed?
  
  # Can't really do max.miles and return.crosstab simultaneously -- either the entire matrix is filled in or doesn't make sense / not easy to use spDists() to get matrix for only some combos
  # unless could do vector version of distances and remove those over distance limit (waste of time to calculate all pairs then), and report in matrix format. 
  # Loses the time-savings benefits of setting max.miles unless done right.

  if (!(return.units %in% c('km', 'miles'))) {stop('return.units must be "miles" or "km" ')}
  km.per.mile <- 1.60934  
  if (missing(max.miles) & !missing(max.km)) { max.miles <- max.km / km.per.mile}
  if (missing(max.km) & !missing(max.miles)) { max.km <- max.miles * km.per.mile}
  if (missing(max.km) & missing(max.miles))  { max.miles <- max.km / km.per.mile} # not really needed if defaults are equivalent
  if (!missing(max.miles) & !missing(max.km)) { if ( max.km != max.miles * km.per.mile ) stop('cannot specify both max.km and max.miles')}
  
  maxlat <- 72
  # For most northern point of USA http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States, where 
  # max degrees per mile, latitude is under 72
  
  deltalat <- 1.01 * max.miles * (km.per.mile) * 1 / (meters.per.degree.lat(maxlat)/1000 )
  # added in 1.01 * to search over 2% wider and taller box just to avoid problems from rounding or 72 degree max assumed lat.
  # old approximation wasn't quite good enough: deltalat <- max.miles * ( max.lat.per.mile <- 1/68 )   # 0.07352941 degrees for 5 miles

  if (!tailored.deltalon) {
    deltalon <- 1.01 * max.miles * (km.per.mile) * 1 / ( meters.per.degree.lon(maxlat)/1000 )
  }
  # This should calc deltalon as function of lat, for each frompoint, using lat of northern edge of box as input to meters.per.degree.lon(lat)
  # making it up to ~2-3x as wide, so maybe 2x-3x as fast if search smaller box for more southern (20-30 degrees) vs northmost points (72 degrees)
  # But checking that using # maybe would just slow it down on net? Seems worth trying.
  # Old approximation: deltalon <- max.miles * ( max.lon.per.mile <- 1/21 )     # 0.2380952 degrees for 5 miles
  # Each degree at the equator represents 111,319.9 metres or approximately 111.32 km.
  
  if (return.crosstab) { if ( !missing(max.km) | !missing(max.miles) ) {warning('Ignoring return.crosstab because max.miles or max.km was specified'); return.crosstab <- FALSE} }
  
  # handle cases where an input is only one row (one point)
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow=1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(topoints)) {mycols <- names(topoints); topoints <- matrix(topoints, nrow=1); dimnames(topoints)[[2]] = mycols }

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
  colnames(frompoints) <- latlon.colnames.check(frompoints)
  colnames(topoints)   <- latlon.colnames.check(topoints)
  
  fromcount <- length(frompoints[,1])
  tocount <- length(topoints[,1])
  results.full <- matrix() # can't preallocate since don't know size yet due to not getting dist for any except within box defined by max.miles, and then removing others based on max.miles
  
  if (return.crosstab) {
    # don't use loop below if need full crosstab. Loop speeds it up if willing to limit to a search radius, but not clear if it might speed it up if using Inf search radius ...
    # Is the loop plus using box to limit distances checked faster than just doing full matrix directly in distances function?
    results.full <- get.distances.all(frompoints, topoints, return.crosstab=TRUE)
    return(results.full)
  }
  
  # For each row in frompoints, 
  for (rownum.frompoints in 1:fromcount) {
    # for performance, may want to use data.table package here?

    # Filter topoints using a box that is based on max.miles, to limit to tobox, then use get.distances.all() for just that subset
    #cat('max.miles',max.miles,'\n')
    #cat('deltalon and lat', deltalon, deltalat, '\n')
    #cat('rownum ',rownum.frompoints,' out of', fromcount,'\n')
    
    fromlat <- frompoints[rownum.frompoints, 'lat']
    fromlon <- frompoints[rownum.frompoints, 'lon']

    # ****  check speed impact of using this:
    if (tailored.deltalon) {
      deltalon <- 1.01 * max.miles * (km.per.mile) * 1 / ( meters.per.degree.lon( fromlat + deltalat ) / 1000 )
    }
    # This should calc deltalon as function of lat, for each frompoint, using lat of northern edge of box as input to meters.per.degree.lon(lat)
    # making it up to ~2-3x as wide, so maybe 2x-3x as fast if search smaller box for more southern (20-30 degrees) vs northmost points (72 degrees)
    # But checking that using # maybe would just slow it down on net? Seems worth trying.

    # not sure if faster to say tolat<-topoints[ , 'lat'] and same for tolon, or just look up each vector twice below
    rownum.topoints <- (topoints[ , 'lat'] > fromlat - deltalat) &
      (topoints[ , 'lat'] < fromlat + deltalat) & 
      (topoints[ , 'lon'] > fromlon - deltalon) & 
      (topoints[ , 'lon'] < fromlon + deltalon) 
    #cat('topoints total:',length(topoints[,'lat']), '  topoints in box: ', sum(rownum.topoints),'\n')
    tobox <- topoints[rownum.topoints, ]
    rownum.topoints <- (1:tocount)[rownum.topoints]
    #cat('rownum.topoints',rownum.topoints,'\n')
    #cat('rownum.frompoints',rownum.frompoints,'\n')
    
    if (sum(as.numeric(rownum.topoints)) > 0) {
      #print(frompoints[rownum.frompoints,])
      #print(tobox)
      
      results <- get.distances.all(frompoints[rownum.frompoints, ], tobox, return.units=return.units, return.rownums=return.rownums, return.latlons=return.latlons, return.crosstab=return.crosstab)
      
      if (!return.rownums & !return.latlons & !return.crosstab) {wantvector <- TRUE} else {wantvector <- FALSE}
      if (length(rownum.topoints)==1) {just1topoint <- TRUE} else {just1topoint <- FALSE}
      
      # testing:
      #cat('results: \n'); print(results)
      #cat('wantvector:', wantvector, 'just1topoint:', just1topoint, '\n')
      #cat('is.matrix(results):', is.matrix(results),  '  is.vector(results): ', is.vector(results), '  is.data.frame(results):', is.data.frame(results), '  class(results):', class(results), '\n')
      #cat('str(results): \n');   print(str(results))
      #cat('dim(results): \n'); print(dim(results))
      #cat('dimnames(results):\n'); print( dimnames(results) )

      # is.data.frame(results)==TRUE  when  !wantvector, whether just1topoint or !just1topoint
      # 
      
      if (return.rownums) {
        # fix the torow numbers to be just those requested, and fromrow should be the one being used right now in this loop
        if (!just1topoint) {
          results[ , 'fromrow'] <- rownum.frompoints
          results[ , 'torow'] <- rownum.topoints
        } else {
          # need to test this:
          results['fromrow'] <- rownum.frompoints
          results['torow'] <- rownum.topoints
        }
      }
      
      # remove those outside search radius, noting that results may be in km or miles depending on return.units
      max.dist.in.return.units <- ifelse(return.units=='km', max.km, max.miles)
      if (!wantvector) {
        results <- results[ results[ , 'd'] <= max.dist.in.return.units, ]
      } else {
        results <- results[ results <= max.dist.in.return.units]
      }
      
      #print('results now: '); print(results)
      
      # append results to results.full
      if (!wantvector) {
        # test that this works if just1topoint
        if (rownum.frompoints==1) {results.full <- results} else { results.full <- rbind(results.full, results) } 
      } else {
        # test that this works if just1topoint and if !just1topoint
        #print('WANTVECTOR SO USE c()')
        #print('rownum.frompoints');print(rownum.frompoints)
        if (rownum.frompoints==1) {results.full <- results} else { results.full <- c(results.full, results) }
      }
            
      # Note that if wantvector, results are hard to interpret since length returned is not same as length of input and can't tell which was <= max.miles
    }    
  }

  #  if (is.data.frame(results) && min(dim(results))>1) { results.full <- results.full[-1,] } else { results.full <- results.full[-1] }
  
  return(results.full)  
}
