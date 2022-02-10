#' @title Generate a number of randomly placed points, as latitude/longitude values.
#'
#' @description
#' Generate a number of randomly placed points, as latitude/longitude values.
#' 
#' @details
#' This function returns n points at random location latitude and longitude values, 
#' weighted by area or by population count in block group, to be roughly representative of 
#' either a random location (like throwing a dart, equal weight per square mile of land area - does not include water area in weighting but can return a point in water if bg has internal point in water?!)
#' or where a randomly selected US resident lives (population weighted).
#' Points are returned as latitude and longitude in decimal degrees. 
#' For people weighted, it picks from the Census blockgroup internal points (2010), (or could change to use blocks?)
#'  which are much more representative of where people live than a random square mile would be.
#' @param n Numeric value, 1 by default. Specifies how many testpoints to return. Must be an integer between zero and 50 million, and not NA.
#' @param weighting area or people or geo or degrees   text indicating what type of weighting, by area or people or geo (each block or block group has equal chance).
#'   Default is area - a random square mile not a random residence or random Census geometry. 
#' @param useblocks logical, whether to use the approx 11 mill. Census blocks if TRUE (from github package UScensus2010blocks), 
#'   or just the approx 220k block groups (from github package ejscreen) if FALSE which is faster
#' @param ST optional vector of 2 letter state abbreviations, to limit it to some state(s). Default is all USA (including PR I think).
#' @param minlat the minimum latitude in decimal degrees to use for generating random points within some range.
#' @param maxlat the maximum latitude in decimal degrees to use for generating random points within some range.
#' @param minlon the minimum longitude in decimal degrees to use for generating random points within some range.
#' @param maxlon the maximum longitude in decimal degrees to use for generating random points within some range.
#' @param as.df Logical, default is TRUE, in which case returns a data.frame, otherwise a matrix.
#' 
#' @return By default, returns a data.frame (or matrix if as.df=FALSE) that has 2 columns: lat and lon, in decimal degrees, with 1 row per point.
#' @seealso \code{\link{get.distances.all}} which allows you to get distances between all points,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit based on distances to nearby points.
#' @concept proximity
#' @examples
#' testpoints(19,minlat=47,maxlat=48)
#' get.distances(testpoints(100),testpoints[10],radius=999,return.rownums=TRUE,return.latlons=TRUE)
#' 
#' @export
testpoints <- function(n=1, weighting='area', ST, as.df=TRUE, minlat=17.9, maxlat=71.25, minlon=-175.89, maxlon=178.34, useblocks=FALSE) { 
  # box for default bounds is 
  # summary(bg20[,c('lat', 'lon')])
  # lat             lon         
  # Min.   :17.90   Min.   :-175.86  
  # Max.   :71.25   Max.   : 178.34  
  # NA's   :13      NA's   :13      
  
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if ( NROW(n) > 1  || !is.numeric(n) || is.na(n) || !is.wholenumber(n) || is.infinite(n) || n < 0 || n > 5e7) {
    if (n == 1e6) {warning('a million is a lot')}
    if (n > 1e6) {warning('more than a million is more than a lot')}
    if (n > 1e7) {warning('that is ridiculous')}
    if (n == 1) {warning('you just want one point? whatever.')}
    if (n == 0) {warning('you want zero points. seriously?')}
    stop('n must be a single whole number, where <0 is too hard to fathom and >50 million is too hard to keep track of')
  }
  if ( minlat < -90 || maxlat < -90 || minlat > 90 || maxlat > 90 )     {stop('minlat and maxlat must be from -90 through 90 decimal degrees')}
  if ( minlon < -180 || maxlon < -180 || minlon > 180 || maxlon > 180 ) {stop('minlon and maxlon must be from -180 through 180 decimal degrees')}
  if (!(weighting %in% c('area', 'people', 'geo', 'degrees'))) {stop('weighting must be a valid option. see help.')}
  
  colsneeded <- c('lat', 'lon')
  if (weighting == 'people') {
    colsneeded <- c(colsneeded, 'pop')
  }
  if (weighting == 'area') {
    colsneeded <- c(colsneeded, 'area')
  }
  if (weighting == 'degrees') {
    
  }
  
  if (useblocks) {
    # use about 11 million Census block points
    
    if (!missing(ST)) {
      colsneeded <- c(colsneeded, 'FIPS')
    }
    places <- UScensus2010blocks::get.blocks(fields = colsneeded)
    if (!missing(ST)) {
      if ('ST' %in% colnames(places)) {
        places$ST <- get.state.info( get.fips.st(places$FIPS), fields = 'ST')[,'ST']
      }
      places$FIPS <- NULL
    }
    
  } else {
    # DO NOT USE BLOCKS - USE BLOCK GROUPS (UNLESS weighting is degrees)
    # use about 220,000 Census block GROUP points
    # summary(bg20[,c('lat', 'lon')])
    # lat             lon         
    # Min.   :17.90   Min.   :-175.86  
    # Max.   :71.25   Max.   : 178.34  
    # NA's   :13      NA's   :13      
    if (!missing(ST)) {
      colsneeded <- c(colsneeded, 'ST')
    }
    if (isTRUE(require('ejscreen'))) {
      # if you have bg20, use it
      places <- ejscreen::bg20[ , colsneeded]
      # places <- bg.pts # did not have population counts, only areas. 
    } else {
      # bg20 not available so warn and treat as if weighting is degrees, so use box min/max lat/lon
      warning('blockgroups dataset not found so picking from rectangle via min and max lat and lon')
      weighting <- 'degrees'
    }
  }
  
  if (missing(ST)) {
    # all states in US are selected from
  } else {
    # limit to specified states assuming that column is not available
    if ('ST' %in% colnames(places)) {
      places <- places[places$ST %in% ST, ]
    } else {
      warning('you specified states but there is no column called ST found')
    }
  }
  if (weighting == 'degrees') {
    rownum <- 1:n
    places <- data.frame(lat=runif(min = minlat, max = maxlat), lon=runif(min = minlon, max = maxlon))
  }
  
  # it is slower, but drop invalid lat lon BEFORE draw sample to ensure we get n not n minus invalid ones.
  # places <- places[!is.na(rowSums(places)), ]
  valid_lat_lon <- function(lat, lon) {
    # TRUE only if both lat and lon seem valid
    if( any(badlat <- is.na(lat)) || any(badlon <- is.na(lon)) ) {
      bad <- is.na(lat) | is.na(lon)
    } else {
      bad <- rep(FALSE, length(lat))
    }
    bad <- bad | lat < -90 | lat > 90 | lon < -180 | lon > 180
    return(!bad)
  }
  places <- places[ valid_lat_lon(lat = places$lat, lon = places$lon), ] # this validates lat lon values, not only for NA values.
  
  if (weighting == 'geo') {
    rownum <- sample(x=1:NROW(places), size = n, replace = FALSE) 
  }
  if (weighting == 'area') {
    rownum <- sample(x=1:NROW(places), size = n, replace = FALSE, prob = places$area)
  }
  if (weighting == 'pop') {
    rownum <- sample(x=1:NROW(places), size = n, replace = FALSE, prob = places$pop)
  }
  
  # MIGHT WANT TO RETURN FIPS ALSO? 
  
  x <- structure( c(places[rownum, 'lat'], places[rownum, 'lon']),
                  .Dim = c(n, 2L), .Dimnames = list( NULL, c("lat", "lon")))
  
  # x <- x[ which(!is.na(rowSums(df))), ] 
  
  if (as.df) {
    return(as.data.frame(x))
  } else {
    return(x)
  }
}
