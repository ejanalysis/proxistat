#' @title Find distances between nearby points, just within specified radius. **work in progress
#'
#' @description Returns the distances from one set of points to nearby members of another set of points.
#' 
#' @details ** note- provides incorrect search box if a point is within radius of max or min lat or lon allowed or where it is zero **
#'   This function returns a matrix or vector of distances, 
#'  which are the distances from one set of points to the nearby members of another set of points.
#'  It searches within a circle (of radius = radius, defining what is considered nearby), 
#'  to calculate distance (in miles or km) from each of frompoints to each of topoints that is within the specified radius.
#'  Points are specified using latitude and longitude in decimal degrees.
#'  \cr\cr
#'  Uses \code{\link{get.distances.all}}. Relies on the \pkg{sp} package for the \code{\link[sp]{spDists}} and \code{\link[sp]{SpatialPoints}} functions. 
#'  \cr\cr
#'  ** Note this is work in progress - see notes on parameter \code{tailored.deltalon}. 
#'  For performance it only uses it for distance pairs (pairs of points) that have been initially 
#'  quickly filtered using lat/lon to be not much more than radius, in an attempt to go 
#'  much faster than finding every distance pair and then dropping all outside the search radius.
#'  But it might be faster just to get all distances even outside box and then return only those within radius.
#'  \cr\cr
#'  Regarding distance calculation, also see \url{http://en.wikipedia.org/wiki/Vincenty\%27s_formulae}, 
#'  \url{http://williams.best.vwh.net/avform.htm#Dist}, \url{http://sourceforge.net/projects/geographiclib/}, 
#'  and \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}.
#'  \cr\cr
#'  Finding distance to all of the 11 million census blocks in usa within 5 km, for 100 points, can take a while... maybe >1 minute?
#'  May need to switch to just use a js library like turf, or investigate using data.table to index and more quickly subset the (potentially 11 million Census blocks of) topoints
#'  (or pre-index that block point dataset and allow this function to accept a data.table as input).
#' @param frompoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param topoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param radius A single number defining nearby, the maximum distance searched for or recorded. Default is 5 miles, which is 8.0467 km, unless units or radius is specified. Must be <5,000 km (about 3106 miles)
#' @param units A string that is 'miles' by default, or 'km' for kilometers, specifying units for radius and distances returned.
#' @param ignore0 A logical, default is FALSE, specifying whether to ignore distances that are zero and report only nonzero distances.
#'   Useful if want distance to points other than self, where frompoints=topoints, for example. Ignored if return.crosstab = TRUE.
#' @param return.crosstab Logical value, FALSE by default. If TRUE, value returned is a matrix of the distances, 
#'   with a row per frompoint and col per topoint. (Distances larger than max search radius are not provided, even in this format).
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes two extra columns:
#'   a col of index numbers starting at 1 specifying the frompoint and a similar col specifying the topoint.
#' @param return.latlons Logical value, FALSE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#' @param as.df Optional logical, default is TRUE
#' @param tailored.deltalon Logical value, TRUE by default. Defines size of initially searched area as function of lat, for each frompoint,
#' rather than initially searching a conservatively large box. The large box is big enough for even the Northernmost US (AK) but perhaps not for further north!
#' Taking time to scale the box according to latitude makes it work for anywhere in the N. Hemisphere (Southern not tested), 
#' and speeds up the distance calculations closer to the equator, but takes a bit of time to define a custom box for each frompoint 
#' so it might be slower overall in just very northern locations? Comprehensive speed tests have not been performed.
#' Just using get.distances.all is reasonably fast? (30-40 seconds for 100 million distances, but slow working with results so large), 
#'   and could remove those outside the radius after that, skipping the searchbox approach: \cr
#'  (or just use spDists) \cr
#' Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), return.crosstab=TRUE); Sys.time() \cr
#' [1] "2015-03-10 18:59:08 EDT" \cr
#' [1] "2015-03-10 18:59:31 EDT"  23 SECONDS  for 100 million distances IF NO PROCESSING OTHER THAN CROSSTAB \cr
#' Sys.time(); x=get.distances.all(testpoints(1e6), testpoints(100), return.crosstab=TRUE); Sys.time() \cr
#' [1] "2015-03-10 21:54:11 EDT" \cr
#' [1] "2015-03-10 21:54:34 EDT"  23 SECONDS for 100 million distances (1m x 100, or 100k x 1000) \cr
#' Sys.time(); x=get.distances.all(testpoints(1e6), testpoints(300), return.crosstab=TRUE); Sys.time() \cr
#' [1] "2015-03-10 21:56:11 EDT" \cr
#' [1] "2015-03-10 21:57:18 EDT"  67-74 seconds for 300 million pairs.  \cr
#'  plus 20 seconds or so for x[x>100] <- Inf  \cr
#'            #' so 11m blocks to 1k points could take >40 minutes!  \cr
#'            >3 minutes per 100 sites? \cr
#'            About 2.6 seconds per site for 11m blocks?  \cr
#'             \cr
#' > Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), units='miles',return.rownums=TRUE); Sys.time() \cr
#' [1] "2015-03-09 21:23:04 EDT" \cr
#' [1] "2015-03-09 21:23:40 EDT"  36 SECONDS IF DATA.FRAME ETC. DONE TO FORMAT RESULTS AND GET ROWNUMS \cr
#' > Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), units='miles',return.rownums=TRUE)$d; Sys.time() \cr
#' [1] "2015-03-09 21:18:47 EDT" \cr
#' [1] "2015-03-09 21:19:26 EDT" 49 SECONDS IF DATA.FRAME ETC. DONE TO FORMAT RESULTS AND GET ROWNUMS IN get.distances.all \cr
#'  \cr
#' get.distances using loop and searchbox is Too slow:  \cr
#'  \cr
#' On a quadcore i7 MacBookPro10,1 2012 with 16GB RAM, it takes 2.5 minutes for 100k frompoints (like block groups not blocks) to 1k sites/topoints.
#' > Sys.time(); x=get.distances(testpoints(1e5), testpoints(1000), units='miles',radius=5,return.rownums=TRUE)$d; Sys.time()
#' [1] "2015-03-09 21:13:08 EDT"
#' [1] "2015-03-09 21:15:40 EDT"  152 SECONDS IF USE SEARCH BOX & THEN RADIUS **** 
#' > Sys.time(); x=get.distances(testpoints(1e5), testpoints(100), units='miles',radius=5,return.rownums=TRUE)$d; Sys.time()
#' [1] "2015-03-09 21:11:55 EDT"
#' [1] "2015-03-09 21:12:24 EDT"  29 SEC FOR SMALLER JOB
#' > Sys.time(); x=get.distances(testpoints(1e4), testpoints(1000), units='miles',radius=5,return.rownums=TRUE)$d; Sys.time()
#' [1] "2015-03-09 21:12:40 EDT"
#' [1] "2015-03-09 21:12:53 EDT" 13 SEC FOR SMALLER JOB of frompoints but same total points
#' 
#' @return By default, returns a dataframe that has 3 columns: fromrow, torow, distance (where fromrow or torow is the row number of the corresponding input, starting at 1).
#'   Distance returned is in miles by default, but with option to set units='km' to get kilometers.
#'   See parameters for details on other formats that may be returned if specified.
#' @seealso \code{\link{get.distances.all}} which allows you to get distances between all points,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit based on distances to nearby points.
#' @concept proximity
#' @examples #
#' set.seed(999)
#' t1=testpoints(1)
#' t10=testpoints(10)
#' t100=testpoints(100,  minlat = 25, maxlat = 45, minlon = -100, maxlon = -60)
#' t1k=testpoints(1e3,   minlat = 25, maxlat = 45, minlon = -100, maxlon = -60)
#' t10k=testpoints(1e4)
#' t100k=testpoints(1e5)
#' t1m=testpoints(1e6)
#' #t10m=testpoints(1e7)
#'    test.from <- structure(list(fromlat = c(38.9567309094, 45), 
#'      fromlon = c(-77.0896572305, -100)), .Names = c("lat", "lon"), 
#'      row.names = c("1", "2"), class = "data.frame")
#'     
#'    test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 45), 
#'     tolon = c(-77.0892818598, -77.2, -90)), 
#'     .Names = c("lat", "lon"), class = "data.frame", 
#'     row.names = c("1", "2", "3"))
#'     
#'    #*** Can fail if radius=50 miles? ... Error in rbind() numbers of
#'    #  columns of arguments do not match !
#'    #big = get.distances(t100, t1k, radius=100, units='miles', return.latlons=TRUE); head(big); summary(big$d)
#'    big = get.distances(t100, t1k, radius=100, units='miles', return.latlons=TRUE); head(big); summary(big$d)
#'    
#'    # see as map of many points
#'     plot(big$fromlon, big$fromlat,main='from black circles... 
#'       closest is red, others nearby are green ')
#'     points(t1k$lon, t1k$lat, col='blue',pch='.')
#'     points(big$tolon, big$tolat, col='green')
#'    junk=as.data.frame( get.nearest(t100, t1k, return.latlons=TRUE) )
#'    points(junk$tolon, junk$tolat, col='red')
#'    # Draw lines from frompoint to nearest:
#'    with(junk,linesegments(fromlon, fromlat, tolon, tolat) )
#'    
#'     # more test cases
#'  length(get.distances(t10,t10,radius=4999,ignore0 = TRUE, units='km')$d)
#'  get.distances(t10,t10,radius=4999,ignore0 = TRUE, units='km')
#' get.distances(test.from[1,],test.to[1,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances(test.from[1,],test.to[1,],radius=3000,return.rownums=FALSE,return.latlons=TRUE)
#' get.distances(test.from[1,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=FALSE)
#' get.distances(test.from[1,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#'  
#' get.distances(test.from[1,],test.to[1:3,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances(test.from[1,],test.to[1:3,],radius=3000,return.rownums=FALSE,return.latlons=TRUE)
#' get.distances(test.from[1,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=FALSE)
#' get.distances(test.from[1,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#'  
#' get.distances(test.from[1:2,],test.to[1,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances(test.from[1:2,],test.to[1,],radius=3000,return.rownums=FALSE,return.latlons=TRUE)
#' get.distances(test.from[1:2,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=FALSE)
#' get.distances(test.from[1:2,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#'  
#' get.distances(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=FALSE,return.latlons=T)
#' get.distances(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=F)
#' get.distances(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#' get.distances(test.from[1:2,],test.to[1:3,], radius=0.7,return.rownums=TRUE,
#'   return.latlons=TRUE, units='km')
#' get.distances(test.from[1:2,],test.to[1:3,], radius=0.7,return.rownums=TRUE,
#'   return.latlons=TRUE, units='miles')
#' 
#'   # Warning messages:
#'   # Ignoring return.crosstab because radius was specified
#' get.distances(test.from[1,],test.to[1:3, ], return.crosstab=TRUE)
#' get.distances(test.from[1:2,],test.to[1, ], return.crosstab=TRUE)
#' get.distances(test.from[1:2,],test.to[1:3, ], return.crosstab=TRUE)
#' get.distances(test.from[1:2,],test.to[1:3, ], radius=0.7, return.crosstab=TRUE)
#' @export
get.distances <- function(frompoints, topoints, radius=5, units='miles', ignore0=FALSE, 
                          return.rownums=TRUE, return.latlons=FALSE, return.crosstab=FALSE, tailored.deltalon=TRUE, as.df=FALSE) {
  # Specify way to define size of rectangular box to use to search within as quick filter rather than finding full matrix of distances between all points.
  #   (hopefully faster to iterate over modest # of sites & highly (&rapidly?) filtered set of blocks, than over all 10m+ blocks)
  #   (speed boost may be >100x if only 1% of all blocks are in that window - )
  #   (3400 miles at widest point, 5km is 3.1 miles, 6.2 mile wide window/3400= 1/548 of the width, and 10x10 km box is extremely tiny % of US area.)
  #  Make FIPS columns factors for speed when rollup to block groups? 
  #  Index blocks on longitude and latitude. Use data.table for speed?
  
  # Can't really do radius and return.crosstab simultaneously -- either the entire matrix is filled in or doesn't make sense / not easy to use spDists() to get matrix for only some combos
  # unless could do vector version of distances and remove those over distance limit (waste of time to calculate all pairs then), and report in matrix format. 
  # Loses the time-savings benefits of setting radius unless done right.

  if (!(units %in% c('km', 'miles'))) {stop('units must be "miles" (default) or "km" to specify units for values returned, and for radius if specified')}
  #km.per.mile <- 1.609344  # default km is 8.04672
  km.per.mile <- convert(1, 'miles', 'km')
  # WORK IN KM, THEN CONVERT TO units FOR RETURNED VALUES
  missingradius <- missing(radius) # converting default to new units makes missing(radius) FALSE !
  if (units=='miles' ) { radius <- radius * km.per.mile }
  if (radius > 5000) {stop('radius must be less than 5,000 kilometers')}
  
  maxlat <- 72
  # For most northern point of USA http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States, where 
  # max degrees per mile, latitude is under 72
  
  deltalat <- 1.01 * radius * 1 / (meters.per.degree.lat(maxlat) / 1000 )
  # added in 1.01 * to search over 2% wider and taller box just to avoid problems from rounding or 72 degree max assumed lat.
  # old approximation wasn't quite good enough: deltalat <- radius * ( max.lat.per.mile <- 1/68 )   # 0.07352941 degrees for 5 miles

  if (!tailored.deltalon) {
    deltalon <- 1.01 * radius * 1 / ( meters.per.degree.lon(maxlat) / 1000 )
  }
  # This should calc deltalon as function of lat, for each frompoint, using lat of northern edge of box as input to meters.per.degree.lon(lat)
  # making it up to ~2-3x as wide, so maybe 2x-3x as fast if search smaller box for more southern (20-30 degrees) vs northmost points (72 degrees)
  # But checking that using # maybe would just slow it down on net? Seems worth trying.
  # Each degree at the equator represents 111,319.9 metres or approximately 111.32 km.
  
  if (return.crosstab) { if ( !missingradius ) {warning('Ignoring return.crosstab because radius was specified'); return.crosstab <- FALSE} }
  
  # handle cases where an input is only one row (one point)
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow=1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(topoints)) {mycols <- names(topoints); topoints <- matrix(topoints, nrow=1); dimnames(topoints)[[2]] = mycols }

  colnames(frompoints) <- latlon.colnames.check(frompoints)
  colnames(topoints)   <- latlon.colnames.check(topoints)
  
  fromcount <- length(frompoints[,1])
  tocount   <- length(  topoints[,1])

  if (return.crosstab) {
    # don't use loop below if need full crosstab. Loop speeds it up if willing to limit to a search radius, but not clear if it might speed it up if using Inf search radius ...
    # Is the loop plus using box to limit distances checked faster than just doing full matrix directly in distances function?
    results.full <- get.distances.all(frompoints, topoints, return.crosstab=TRUE, as.df=as.df)
    return(results.full)
  }
  
  if (!return.rownums & !return.latlons & !return.crosstab) {wantvector <- TRUE} else {wantvector <- FALSE}
  
  results.empty=get.distances.all(data.frame(lat=0,lon=0), data.frame(lat=0,lon=0), return.rownums=return.rownums, return.latlons=return.latlons, return.crosstab=return.crosstab)
  colcount=dim(results.empty)[2]
  if (is.null(colcount)) {colcount <- 1}
  results.full <- matrix(ncol=colcount) # can't preallocate since don't know size yet due to not getting dist for any except within box defined by radius, and then removing others based on radius

  ########################################################################
  # For each row in frompoints, 
  ########################################################################
  
  firstvalidresults <- TRUE
  
  for (rownum.frompoints in 1:fromcount) {
    
    # for performance, may want to use data.table package here?
    
    # Filter topoints using a box that is based on radius, to limit to tobox, then use get.distances.all() for just that subset
    # cat('\n\n')    
    # if (exists('deltalon')) {cat('deltalon and lat', deltalon, deltalat, '\n')}
    # cat('rownum ',rownum.frompoints,' out of', fromcount,'\n')
    #     
    fromlat <- frompoints[rownum.frompoints, 'lat']
    fromlon <- frompoints[rownum.frompoints, 'lon']
    
    # ****  check speed impact of using this:
    if (tailored.deltalon) {
      deltalon <- 1.01 * radius * 1 / ( meters.per.degree.lon( fromlat + deltalat ) / 1000 )
    }
    # This should calc deltalon as function of lat, for each frompoint, using lat of northern edge of box as input to meters.per.degree.lon(lat)
    # making it up to ~2-3x as wide, so maybe 2x-3x as fast if search smaller box for more southern (20-30 degrees) vs northmost points (72 degrees)
    # But checking that using # maybe would just slow it down on net? Seems worth trying.
    # not sure if faster to say tolat<-topoints[ , 'lat'] and same for tolon, or just look up each vector twice below
    rownum.topoints <- (topoints[ , 'lat'] > fromlat - deltalat) &
      (topoints[ , 'lat'] < fromlat + deltalat) & 
      (topoints[ , 'lon'] > fromlon - deltalon) & 
      (topoints[ , 'lon'] < fromlon + deltalon) 
    # 
    # cat('topoints total:',length(topoints[,'lat']), '  topoints in box: ', sum(rownum.topoints),'\n')
    tobox <- topoints[rownum.topoints, ]
    rownum.topoints <- (1:tocount)[rownum.topoints]
    # cat('rownum.topoints',rownum.topoints,'\n')
    # cat('rownum.frompoints',rownum.frompoints,'\n')
    
    if (sum(as.numeric(rownum.topoints)) > 0) {
      # print(frompoints[rownum.frompoints,])
      # print(tobox)
      
      results <- get.distances.all(frompoints[rownum.frompoints, ], tobox, units='km', return.rownums=return.rownums, return.latlons=return.latlons, return.crosstab=FALSE)
      
      if (length(rownum.topoints)==1) {just1topoint <- TRUE} else {just1topoint <- FALSE}
      
      # # testing:
      # cat('results: \n'); print(results)
      # cat('wantvector:', wantvector, '\njust1topoint:', just1topoint, '\n')
      # cat('is.matrix(results):', is.matrix(results),  '\nis.vector(results): ', is.vector(results), '\nis.data.frame(results):', is.data.frame(results), '\nclass(results):', class(results), '\n')
      # cat('str(results): \n');   print(str(results)); cat('\n')
      # cat('dim(results): \n'); print(dim(results));cat('\n')
      # cat('dimnames(results):\n'); print( dimnames(results) ); cat('\n')
      # 
      
      # remove those outside search radius, and note right now results and radius are in km
      if (!wantvector) {
        if (return.rownums) {
          # fix the torow numbers to be just those requested, and fromrow should be the one being used right now in this loop
          if (!just1topoint) {
            results[ , 'fromrow'] <- rownum.frompoints
            results[ , 'torow']   <- rownum.topoints
          } else {
            # test this:
            results['fromrow'] <- rownum.frompoints
            results['torow']   <- rownum.topoints
          }
        }
        results <- results[ results[ , 'd'] <= radius, ]
        # append results to results.full
        
        if (ignore0) { results <- results[ results[,'d'] != 0,  ] }
        
        #*** need to handle case here where ignore0 caused there to be zero valid nonzero results:
        #*****
        # if (length(results[,'d']) != 0) { }
        
        
        
        if (rownum.frompoints==1 | firstvalidresults ) {results.full <- results} else { results.full <- rbind(results.full, results) } 
        firstvalidresults <- FALSE
        
      } else {
        # JUST A VECTOR OF DISTANCES
        results <- results[ results <= radius]
        if (ignore0) { results <- results[ results != 0 ] }
        # test that this works if 0,1,>1 topoints
        # print('rownum.frompoints');print(rownum.frompoints); cat('\n')
        # append results to results.full
        
        #*** need to handle case here where ignore0 caused there to be zero valid nonzero results:
        #*****
        # if (length(results) == 0) { }
        
        
        
        
        if (rownum.frompoints==1 | firstvalidresults ) {results.full <- results} else { results.full <- c(results.full, results) }
        firstvalidresults <- FALSE
        
      }
      
      # print('results.full now: '); print(results.full); cat('\n')
      # print('results now: '); print(results); cat('\n')
      
      # Note that if wantvector, results are hard to interpret since length returned is not same as length of input and can't tell which was <= radius
    }
  }

  #  if (is.data.frame(results) && min(dim(results))>1) { results.full <- results.full[-1,] } else { results.full <- results.full[-1] }
  
  # CONVERT TO CORRECT UNITS NOW ****
  if (!wantvector) {
    if (units=='miles') { results.full[,'d'] <- results.full[,'d'] / km.per.mile }
  } else {
    if (units=='miles') { results.full <- results.full / km.per.mile }
  }
  if (as.df) {results.full <- as.data.frame(results.full)}
  return(results.full)  
}
