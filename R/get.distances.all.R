#' @title Find all distances between two sets of points (based on lat/lon)
#'
#' @description Returns all the distances from one set of geographic points to another set of points.
#' Can return a matrix of distances (m x n points) or vector or data.frame with one row per pair.
#' Lets you specify units and whether you need lat/lon etc, but essentially just a wrapper for
#' the \pkg{sp} package for the \code{\link[sp]{spDistsN1}} and \code{\link[sp]{SpatialPoints}} functions.
#' @details
#' *** Probably slower than it needs to be partly by using data.frame instead of matrix class? Roughly 10-20% faster if as.df=FALSE than if TRUE.
#' Just using get.distances.all is reasonably fast? (30-40 seconds for 100 million distances, but slow working with results so large),
#' Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), return.crosstab=TRUE); Sys.time() \cr
#' [1] "2015-03-10 18:59:08 EDT" \cr
#' [1] "2015-03-10 18:59:31 EDT"  23 SECONDS  for 100 million distances IF NO PROCESSING OTHER THAN CROSSTAB \cr
#' Sys.time(); x=get.distances.all(testpoints(1e6), testpoints(100), return.crosstab=TRUE); Sys.time() \cr
#' [1] "2015-03-10 21:54:11 EDT" \cr
#' [1] "2015-03-10 21:54:34 EDT"  23 SECONDS for 100 million distances (1m x 100, or 100k x 1000) \cr
#' Sys.time(); x=get.distances.all(testpoints(1e6), testpoints(300), return.crosstab=TRUE); Sys.time() \cr
#' [1] "2015-03-10 21:56:11 EDT" \cr
#' [1] "2015-03-10 21:57:18 EDT"  67 seconds for 300 million pairs.  \cr
#'  plus 20 seconds or so for x[x>100] <- Inf  \cr
#'            #' so 11m blocks to 1k points could take >40 minutes! (you would want to more quickly remove the ones outside some radius)  \cr
#'            >3 minutes per 100 sites? \cr
#'            About 2.6 seconds per site for 11m blocks?  \cr \cr
#' > Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), units='miles',return.rownums=TRUE); Sys.time() \cr
#' [1] "2015-03-09 21:23:04 EDT" \cr
#' [1] "2015-03-09 21:23:40 EDT"  36 SECONDS IF DATA.FRAME ETC. DONE TO FORMAT RESULTS AND GET ROWNUMS \cr
#' > Sys.time(); x=get.distances.all(testpoints(1e5), testpoints(1000), units='miles',return.rownums=TRUE)$d; Sys.time() \cr
#' [1] "2015-03-09 21:18:47 EDT" \cr
#' [1] "2015-03-09 21:19:26 EDT" 49 SECONDS IF DATA.FRAME ETC. DONE TO FORMAT RESULTS AND GET ROWNUMS IN get.distances.all \cr
#' @param frompoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param topoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param units A string that is 'miles' by default, or 'km' for kilometers,
#'   specifying units for distances returned.
#' @param return.crosstab Logical value, FALSE by default. If TRUE, value returned is a matrix of the distances,
#'   with a row per frompoint and col per topoint.
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes two extra columns:
#'   a col of index numbers starting at 1 specifying the frompoint and a similar col specifying the topoint.
#'   If crosstab=TRUE, ignores return.rownums and return.latlons
#' @param return.latlons Logical value, TRUE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#'   If crosstab=TRUE, ignores return.rownums and return.latlons
#' @param as.df Logical, default is TRUE, in which case returns a data.frame (unless vector), otherwise a matrix (unless vector).
#' @return By default, returns a dataframe that has 3 columns: fromrow, torow, distance
#'   (where fromrow or torow is the row number of the corresponding input, starting at 1).
#'   If return.crosstab=FALSE, which is default, and return.rownums and/or return.latlons is TRUE,
#'   returns a row per from-to pair, and columns depending on parameters, sorted first cycling through all topoints for first frompoint, and so on.
#'   If return.crosstab=FALSE and return.rownums and return.latlons are FALSE, returns a vector of distances in same order as rows described above.
#'   If return.crosstab=TRUE, returns a matrix of distances, with one row per frompoint and one column per topoint.
#' @seealso \code{\link{get.distances}} which allows you to specify a search radius and
#'   get distances only within that radius which can be faster,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point
#'   within a specified search radius instead of all topoints, and
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit
#'   based on distances to nearby points.
#' @concept proximity
#' @examples
#' set.seed(999)
#' t1=testpoints(1)
#' t10=testpoints(10)
#' t100=testpoints(100, minlat=25,maxlat=48)
#' t1k=testpoints(1e3)
#' t10k=testpoints(1e4)
#' t100k=testpoints(1e5)
#' t1m=testpoints(1e6)
#' #t10m=testpoints(1e7)
#'
#' get.distances.all(t1, t1)
#' get.distances.all(t1, t10[2, ,drop=FALSE])
#' x=get.distances.all(t10, t100[1:20 , ], units='km')
#'  plot(x$tolon, x$tolat,pch='.')
#'  points(x$fromlon, x$fromlat)
#'  with(x, linesegments(fromlon, fromlat, tolon, tolat ))
#'  with(x[x$d<500, ], linesegments(fromlon, fromlat, tolon, tolat ,col='red'))
#' x=get.distances.all(t10, t1k); head(x);summary(x$d)
#' x=get.distances.all(t10, t1k, units='km'); head(x);summary(x$d)
#' x=get.distances.all(t10, t1k, units='km'); head(x);summary(x$d)
#'
#'\dontrun{
#' require(UScensus2010blocks) # for the get.blocks() function and dataset
#' blocks <- get.blocks(fields=c('fips','lat','lon'),charfips = FALSE)
#' 
#'}
#' 
#'    test.from <- structure(list(fromlat = c(38.9567309094, 45),
#'      fromlon = c(-77.0896572305, -100)), .Names = c("lat", "lon"),
#'      row.names = c("1", "2"), class = "data.frame")
#'
#'    test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 45),
#'     tolon = c(-77.0892818598, -77.2, -90)),
#'     .Names = c("lat", "lon"), class = "data.frame",
#'     row.names = c("1", "2", "3"))
#'  test.to.NA = rbind(c(NA,NA), test.to[2:3,])
#'  test.from.NA = rbind(test.from[1,], c(NA,NA))
#'
#' get.distances.all(test.from, test.to)
#' get.distances.all(test.from, test.to, return.crosstab=TRUE)
#' get.distances.all(test.from, test.to, return.rownums=FALSE)
#' get.distances.all(test.from, test.to, return.latlons=FALSE)
#' get.distances.all(test.from, test.to, return.latlons=FALSE, return.rownums=FALSE)
#'
#'      # test cases
#'
#' get.distances.all(test.from,    test.to.NA)
#' get.distances.all(test.from.NA, test.to)
#' get.distances.all(test.from.NA, test.to.NA)
#' get.distances.all(test.from[1,],test.to[1,],return.rownums=F,return.latlons=F)
#' get.distances.all(test.from[1,],test.to[1,],return.rownums=FALSE,return.latlons=TRUE)
#' get.distances.all(test.from[1,],test.to[1,],return.rownums=TRUE,return.latlons=FALSE)
#' get.distances.all(test.from[1,],test.to[1,],return.rownums=TRUE,return.latlons=TRUE)
#'
#' get.distances.all(test.from[1,],test.to[1:3,],return.rownums=F,return.latlons=F)
#' get.distances.all(test.from[1,],test.to[1:3,],return.rownums=FALSE,return.latlons=TRUE)
#' get.distances.all(test.from[1,],test.to[1:3,],return.rownums=TRUE,return.latlons=FALSE)
#' get.distances.all(test.from[1,],test.to[1:3,],return.rownums=TRUE,return.latlons=TRUE)
#'
#' get.distances.all(test.from[1:2,],test.to[1,],return.rownums=F,return.latlons=F)
#' get.distances.all(test.from[1:2,],test.to[1,],return.rownums=FALSE,return.latlons=TRUE)
#' get.distances.all(test.from[1:2,],test.to[1,],return.rownums=TRUE,return.latlons=FALSE)
#' get.distances.all(test.from[1:2,],test.to[1,],return.rownums=TRUE,return.latlons=TRUE)
#'
#' round(get.distances.all(test.from[1:2,],test.to[1:3,],return.rownums=F,return.latlons=F),1)
#' get.distances.all(test.from[1:2,],test.to[1:3,],return.rownums=FALSE,return.latlons=T)
#' get.distances.all(test.from[1:2,],test.to[1:3,],return.rownums=TRUE,return.latlons=F)
#' get.distances.all(test.from[1:2,],test.to[1:3,],return.rownums=TRUE,return.latlons=TRUE)
#' get.distances.all(test.from[1:2,],test.to[1:3,], return.rownums=TRUE,
#'   return.latlons=TRUE, units='km')
#' get.distances.all(test.from[1:2,],test.to[1:3,], return.rownums=TRUE,
#'   return.latlons=TRUE, units='miles')
#'
#' get.distances.all(test.from[1,],test.to[1:3, ], return.crosstab=TRUE)
#' get.distances.all(test.from[1:2,],test.to[1, ], return.crosstab=TRUE)
#' round(get.distances.all(test.from[1:2,],test.to[1:3, ],return.crosstab=TRUE, units='miles'),2)
#' round(get.distances.all(test.from[1:2,],test.to[1:3, ],return.crosstab=TRUE, units='km'),2)
#' @export
get.distances.all <- function(frompoints, topoints, units='miles', return.crosstab=FALSE, return.rownums=TRUE, return.latlons=TRUE, as.df=TRUE) {

  #require(sp)
  if (!(units %in% c('km', 'miles'))) {stop('units must be "miles" or "km" ')}
  km.per.mile <- convert(1, 'miles', 'km') # about 1.60934

  #   if (paste(colnames(frompoints),collapse='')!='latlon')  {
  #     warning('frompoints colnames being changed to lat and lon, in that order')
  #     colnames(frompoints) <- c('lat', 'lon')
  #   }

  # handle cases where an input is only one row (one point)
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow=1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(topoints))   {mycols <- names(topoints);   topoints   <- matrix(topoints,   nrow=1); dimnames(topoints)[[2]]   = mycols }

  colnames(frompoints) <- latlon.colnames.check(frompoints)
  colnames(topoints)   <- latlon.colnames.check(topoints)

  # ADD NA HANDLING 
  from_na <- is.na(frompoints[ , 'lon']) | is.na(frompoints[ , 'lat'])
  to_na   <- is.na(topoints[ , 'lon'])   | is.na(topoints[ , 'lat'])
  originalfrom <- frompoints
  originalto <- topoints
  frompoints[from_na, ] <- c(0,0) # replace NA with 0 so that spatialpoints will not stop with error
  topoints[to_na, ]     <- c(0,0)
  
  frompoints.sp <- sp::SpatialPoints(coords = data.frame(x = frompoints[,'lon'], y = frompoints[,'lat']), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  topoints.sp   <- sp::SpatialPoints(coords = data.frame(x = topoints[,'lon'],   y = topoints[,'lat']),   proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  results.matrix <- sp::spDists(frompoints.sp, topoints.sp, longlat=TRUE) # result is in kilometers so far
  rm(frompoints.sp, topoints.sp)

  # NA HANDLING 
  frompoints <- originalfrom
  topoints    <- originalto
  results.matrix[from_na, ] <- NA
  results.matrix[ , to_na]  <- NA
  rm(originalfrom, originalto)

  if (units=='miles') {results.matrix <- results.matrix / km.per.mile }

  if (return.crosstab) {
    # if crosstab=TRUE, ignore return.rownums and return.latlons
    if (as.df) {return(as.data.frame(results.matrix)  )  } else {return(results.matrix)  }
  } # this will return crosstab, i.e., tall matrix of fromrow, torow, distance (3 columns, one row per from-to pair)

  if (!return.rownums & !return.latlons) { return( as.vector( t(results.matrix) )  ) }

  if (!return.rownums & return.latlons) {
    # return tall matrix with 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'

    results <- cbind( analyze.stuff::expand.gridMatrix(topoints[,'lat'], frompoints[,'lat']), analyze.stuff::expand.gridMatrix(topoints[,'lon'], frompoints[,'lon']) , as.vector( t(results.matrix) ) )
    colnames(results) <- c('tolat', 'fromlat', 'tolon', 'fromlon', 'd')
    if (as.df) {
      return(as.data.frame(results[ , c('fromlat', 'fromlon', 'tolat', 'tolon', 'd'), drop=FALSE] )  )
    } else {
      return( results[ , c('fromlat', 'fromlon', 'tolat', 'tolon', 'd')] )
    }
  }

  if (return.rownums & !return.latlons) {
    # return tall matrix with fromrow, torow, d
    # **** BUT THIS MAY BE TOO SLOW OR FAILS FOR LARGE NUMBERS LIKE 100k frompoints x 10K topoints

    results=cbind( analyze.stuff::expand.gridMatrix(1:length(topoints[,'lat']), 1:length(frompoints[,'lat'])), as.vector( t(results.matrix) )  )
    colnames(results) <- c('torow', 'fromrow', 'd')
    if (as.df) {
      return(as.data.frame(results[ , c('fromrow', 'torow', 'd'), drop=FALSE] )  )
    } else {
      return( results[ , c('fromrow', 'torow', 'd')] )
    }

  }

  if (return.rownums & return.latlons) {
    # return tall matrix with 'fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'

    results=cbind( analyze.stuff::expand.gridMatrix(1:length(topoints[,'lat']), 1:length(frompoints[,'lat'])), as.vector( t(results.matrix) )  )
    colnames(results) <- c('torow', 'fromrow', 'd')

    results <- cbind( analyze.stuff::expand.gridMatrix(topoints[,'lat'], frompoints[,'lat']), analyze.stuff::expand.gridMatrix(topoints[,'lon'], frompoints[,'lon']) , results)
    colnames(results) <- c('tolat', 'fromlat', 'tolon', 'fromlon', 'torow', 'fromrow', 'd')
    if (as.df) {
      return(as.data.frame(results[ , c('fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'), drop=FALSE]  )  )
    } else {
      return( results[ , c('fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd')]  )
    }
  }
}
