#' @title Alt method MUCH faster/no loop & simpler distance formula approx - to Find distances between nearby points, just within specified radius. **work in progress
#'
#' @description Returns the distances from one set of points to nearby members of another set of points.
#'   ** Documentation copied from get.distances, not edited to be relevant to this alt version.
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
#' @param dfunc Optional character element "hf"(default) or "slc" to specify distance function Haversine or spherical law of cosines.
#'   If "sp", it uses the \pkg{sp} package to find distances more accurately but more slowly. 
#' @param return.crosstab Logical value, FALSE by default. If TRUE, value returned is a matrix of the distances, 
#'   with a row per frompoint and col per topoint. (Distances larger than max search radius are not provided, even in this format).
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes two extra columns:
#'   a col of index numbers starting at 1 specifying the frompoint and a similar col specifying the topoint.
#' @param return.latlons Logical value, FALSE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#' @param as.df Optional logical, default is TRUE
#' @param tailored.deltalon Logical value, FALSE by default, but ignored. Leftover from older get.distances function. Defined size of initially searched area as function of lat, for each frompoint,
#' rather than initially searching a conservatively large box. 
#'   Just using get.distances.all is reasonably fast? (30-40 seconds for 100 million distances, but slow working with results so large), 
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
#' **** get.distances.all scales linearly, more or less: *****
#' IT TAKES ALMOST 30 (27) SECONDS FOR 10 POINTS and every Census Block in the US (11m).
#' > Sys.time(); x=get.distances.all(testpoints(11e6), testpoints(10), return.crosstab=TRUE); Sys.time()
#' [1] "2015-04-18 12:02:31 EDT"
#' [1] "2015-04-18 12:02:58 EDT"
#' and doesnt matter much if fewer blocks and more sites, vs more blocks and fewer sites, at a time.
#' > Sys.time(); x=get.distances.all(testpoints((1.1e6)), testpoints(100), return.crosstab=TRUE); Sys.time()
#' [1] "2015-04-18 12:06:51 EDT"
#' [1] "2015-04-18 12:07:18 EDT"
#' and 131 seconds for 50 sites to all blocks: so 5x27sec=135 expected, close to actual 131.
#' Sys.time(); x=get.distances.all(testpoints(11e6), testpoints(50), return.crosstab=TRUE); Sys.time()
#' [1] "2015-04-18 12:10:10 EDT"
#' [1] "2015-04-18 12:12:21 EDT"
#' **** So a typical request of 100 to 1000 to 10k sites could take
#'   262 to 2620 to 26200 seconds, or 3 minutes to 45 minutes to 7.3 hours!! That is too slow for common use/webapp.
#' ******
#' > Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), units='miles',return.rownums=TRUE); Sys.time() \cr
#' [1] "2015-03-09 21:23:04 EDT" \cr
#' [1] "2015-03-09 21:23:40 EDT"  36 SECONDS IF DATA.FRAME ETC. DONE TO FORMAT RESULTS AND GET ROWNUMS \cr
#' > Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), units='miles',return.rownums=TRUE)$d; Sys.time() \cr
#' [1] "2015-03-09 21:18:47 EDT" \cr
#' [1] "2015-03-09 21:19:26 EDT" 49 SECONDS IF DATA.FRAME ETC. DONE TO FORMAT RESULTS AND GET ROWNUMS IN get.distances.all \cr
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
#'    #big = get.distances3(t100, t1k, radius=100, units='miles', return.latlons=TRUE); head(big); summary(big$d)
#'    big = get.distances3(t100, t1k, radius=100, units='miles', return.latlons=TRUE); head(big); summary(big$d)
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
#'  length(get.distances3(t10,t10,radius=4999,ignore0 = TRUE, units='km')$d)
#'  get.distances3(t10,t10,radius=4999,ignore0 = TRUE, units='km')
#' get.distances3(test.from[1,],test.to[1,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances3(test.from[1,],test.to[1,],radius=3000,return.rownums=FALSE,return.latlons=TRUE)
#' get.distances3(test.from[1,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=FALSE)
#' get.distances3(test.from[1,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#'  
#' get.distances3(test.from[1,],test.to[1:3,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances3(test.from[1,],test.to[1:3,],radius=3000,return.rownums=FALSE,return.latlons=TRUE)
#' get.distances3(test.from[1,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=FALSE)
#' get.distances3(test.from[1,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#'  
#' get.distances3(test.from[1:2,],test.to[1,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances3(test.from[1:2,],test.to[1,],radius=3000,return.rownums=FALSE,return.latlons=TRUE)
#' get.distances3(test.from[1:2,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=FALSE)
#' get.distances3(test.from[1:2,],test.to[1,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#'  
#' get.distances3(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=F,return.latlons=F)
#' get.distances3(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=FALSE,return.latlons=T)
#' get.distances3(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=F)
#' get.distances3(test.from[1:2,],test.to[1:3,],radius=3000,return.rownums=TRUE,return.latlons=TRUE)
#' get.distances3(test.from[1:2,],test.to[1:3,], radius=0.7,return.rownums=TRUE,
#'   return.latlons=TRUE, units='km')
#' get.distances3(test.from[1:2,],test.to[1:3,], radius=0.7,return.rownums=TRUE,
#'   return.latlons=TRUE, units='miles')
#' 
#'   # Warning messages:
#'   # Ignoring return.crosstab because radius was specified
#' get.distances3(test.from[1,],test.to[1:3, ], return.crosstab=TRUE)
#' get.distances3(test.from[1:2,],test.to[1, ], return.crosstab=TRUE)
#' get.distances3(test.from[1:2,],test.to[1:3, ], return.crosstab=TRUE)
#' get.distances3(test.from[1:2,],test.to[1:3, ], radius=0.7, return.crosstab=TRUE)
#' @export
get.distances3 <- function(frompoints, topoints, radius=5, units='miles', ignore0=FALSE, dfunc='hf',
                    return.rownums=TRUE, return.latlons=FALSE, return.crosstab=FALSE, tailored.deltalon=FALSE, as.df=FALSE) {
  # notes:
  #  Make FIPS columns factors for speed when rollup to block groups? 
  #  Index blocks on longitude and latitude. Use data.table for speed?
  
  # Can't really do radius and return.crosstab simultaneously -- either the entire matrix is filled in or doesn't make sense / not easy to use spDists() to get matrix for only some combos
  # unless could do vector version of distances and remove those over distance limit (waste of time to calculate all pairs then), and report in matrix format. 
  # Loses the time-savings benefits of setting radius unless done right.
  
  # Error check radkus and units (km or miles)

  if (!(units %in% c('km', 'miles'))) {stop('units must be "miles" (default) or "km" to specify units for values returned, and for radius if specified')}
  #km.per.mile <- 1.609344  # default km is 8.04672
  km.per.mile <- convert(1, 'miles', 'km')
  missingradius <- missing(radius)
  if (units=='miles' ) { radius.km <- radius * km.per.mile } else {radius.km <- radius}
  if (radius.km > 5000) {stop('radius must be less than 5,000 kilometers')}
  
  if (return.crosstab) { if ( !missingradius ) {warning('Ignoring return.crosstab because radius was specified'); return.crosstab <- FALSE} }
  
  # handle cases where an input is only one row (one point)
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow=1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(  topoints)) {mycols <- names(  topoints);   topoints <- matrix(  topoints, nrow=1); dimnames(  topoints)[[2]] = mycols }

  colnames(frompoints) <- latlon.colnames.check(frompoints)
  colnames(  topoints) <- latlon.colnames.check(  topoints)
  
  fromcount <- length(frompoints[ , 1])
  tocount   <- length(  topoints[ , 1])
  
  if (!return.rownums & !return.latlons & !return.crosstab) {wantvector <- TRUE} else {wantvector <- FALSE}
  
  # reformat frompoints and topoints to have 1 row per combo (per from-to pair, i.e., per distance)
  latlonpairs = cbind(expand.gridMatrix(frompoints$lat, topoints$lat), expand.gridMatrix(frompoints$lon, topoints$lon))
  colnames(latlonpairs) <- c('fromlat', 'tolat', 'fromlon', 'tolon')

  ############# KEY FUNCTION -- gets distance for every pair of from-to: ################
  
  # *** ideally to avoid warning HAVE TO NAME THE COLS lat lon lat lon here:
  
  results <- gcd(
    frompoints= latlonpairs[ , c('fromlat', 'fromlon')],
    topoints= latlonpairs[ , c(  'tolat',   'tolon')],
    units=units,
    dfunc=dfunc
  )
  results <- cbind(d=results)
  
  if (return.crosstab) {
    # reformat to be 1 row per frompoints and 1 col per topoints
    results <- matrix(results, nrow=length(frompoints[,1]), ncol=length(topoints[,1]), byrow=FALSE)
    if (as.df) {results=as.data.frame(results)}
    return(results)
  }
  
  if (return.rownums) {
    # Create rownums in correct format... cycle through frompoints first, then topoints 
    rownumpairs <- cbind(expand.gridMatrix(1:length(frompoints[,1]), 1:length(topoints[,1])))
    results <- cbind(rownumpairs, results)
    colnames(results) <- c('fromrow','torow', colnames(results))
  }

  if (return.latlons) {
    results <- cbind(results, latlonpairs[ , c('fromlat' ,'fromlon', 'tolat', 'tolon')])
  }

  # units should already be same for results and radius, as defined by units param
  #if (units=='miles') { results <- convert(results, from='km', towhat = 'mi')}

  if (as.df) {results <- as.data.frame(results)}
  
  # Remove those outside radius, and remove zero distances if ignore0=TRUE (now radius and results are both in same units)
  
  if (!wantvector) {
    results <- results[ results[ , 'd'] <= radius, ]
    if (ignore0) { results <- results[ results[,'d'] != 0,  ] }
  } else {
    results <- results[ results <= radius]
    if (ignore0) { results <- results[ results != 0 ] }
  }
  
  return(results)
}
