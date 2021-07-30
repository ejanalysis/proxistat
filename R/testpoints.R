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
#' @param n Numeric value, required, TRUE by default. Specifies how many testpoints to return. Must be an integer between zero and 50 million, and not NA.
#' @param weighting area or people or geo ... text indicating what type of weighting, by area or people or geo (each block or block group has equal chance).
#'   Default is area - a random square mile not a random residence or random Census geometry. 
#' @param useblocks logical, whether to use the approx 11 mill. Census blocks if TRUE (from github package UScensus2010blocks), 
#'   or just the approx 220k block groups (from github package ejscreen) if FALSE which is faster
#' @param ST optional vector of 2 letter state abbreviations, to limit it to some state(s). Default is all USA (including PR I think).
#' @param minlat not used anymore. Default 40. A number that is the minimum latitude in decimal degrees to use for generating random points within some range.
#' @param maxlat not used anymore.Default 42. A number that is the maximum latitude in decimal degrees to use for generating random points within some range.
#' @param minlon not used anymore.Default -125. A number that is the minimum longitude in decimal degrees to use for generating random points within some range.
#' @param maxlon not used anymore.Default -70. A number that is the maximum longitude in decimal degrees to use for generating random points within some range.
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
#' get.distances(testpoints(1000),testpoints[10],radius=999,return.rownums=TRUE,return.latlons=TRUE)
#'     
#' @export
testpoints <- function(n, weighting='area', ST, as.df=TRUE, minlat=40, maxlat=42, minlon=-125, maxlon=-70) { 
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if (missing(n) || !is.numeric(n) || is.na(n) || !is.wholenumber(n) || is.infinite(n) || n < 0 || n > 5e7) {
    if (n == 1e6) {warning('a million is a lot')}
    if (n > 1e6) {warning('more than a million is more than a lot')}
    if (n > 1e7) {warning('that is ridiculous')}
    stop('n must be specified, and must be a whole number, where <0 is too hard to fathom and >50 million is too hard to keep track of')
  }
  if ( minlat < -90 || maxlat < -90 || minlat > 90 || maxlat > 90 )     {stop('minlat and maxlat must be from -90 through 90 decimal degrees')}
  if ( minlon < -180 || maxlon < -180 || minlon > 180 || maxlon > 180 ) {stop('minlon and maxlon must be from -180 through 180 decimal degrees')}
  if (!(weighting %in% c('area', 'people'))) {stop('weighting must be area or people')}
  
  colsneeded <- c('lat', 'lon')
  if (weighting == 'people') {
    colsneeded <- c(colsneeded, 'pop')
  }
  if (weighting == 'area') {
    colsneeded <- c(colsneeded, 'area')
  }
  
  if (useblocks) {
    # use about 11 million Census block points
    if (!missing(ST)) {
      colsneeded <- c(colsneeded, 'FIPS')
    }
    places <- UScensus2010blocks::get.blocks(fields = colsneeded)
    if (!missing(ST)) {
      places$ST <- get.state.info( get.fips.st(places$FIPS), fields = 'ST')[,'ST']
      places$FIPS <- NULL
    }
  } else {
    # use about 220,000 Census block GROUP points
    # use block groups not blocks
    if (!missing(ST)) {
      colsneeded <- c(colsneeded, 'ST')
    }
    places <- ejscreen::bg20[ , colsneeded]
    # places <- bg.pts # did not have population counts, only areas. 
  }
  
  if (missing(ST)) {
    # all states in US are selected from
  } else {
    places <- places[places$ST %in% ST, ]
  }
  
  if (weighting == 'geo') {
    rownum <- sample(x=1:NROW(places), size = n, replace = FALSE) 
  }
  if (weighting == 'area') {
    rownum <- sample(x=1:NROW(places), size = n, replace = FALSE, prob = places$area)
  }
  if (weighting == 'pop') {
    rownum <- sample(x=1:NROW(places), size = n, replace = FALSE, prob = places$pop)
  }
  
  # MIGHT WANT TO RETURN FIPS ALSO... SOMETIMES WOULD WANT THAT... 
  x <- structure( c(places[rownum, 'lat'], places[rownum, 'lon']),
                  .Dim = c(n, 2L), .Dimnames = list( NULL, c("lat", "lon")))
  
  if (as.df) {return(as.data.frame(x))} else {return(x)}
}
