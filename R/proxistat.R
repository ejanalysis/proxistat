#' @title Proximity Statistic for Each Location and Nearby Points
#' 
#' @description Calculate proximity statistic for each location,
#'   quantifying number of and proximities of nearby points.
#'   \code{proxistat} returns a proximity statistic (score) for each location (e.g., Census block).
#' @details  This uses \code{\link{get.distances}} with return.crosstab=TRUE.
#' This function returns a vector of proximity scores, one for each location such as a Census block.
#' For example, the proximity score may be used to represent how many hazardous waste sites are near any given neighborhood and how close they are.
#' A proximity score quantifies the proximity and count of nearby points using a specified formula. \cr
#' Proximity Score = distance-weighted count of points nearby (within search radius) (or with another optional weight for each topoint) \cr
#' (or weighted distance to nearest single point if there are none within the radius). \cr
#' This is the sum of 1/d or 1/d^2 or 1/1, depending on the decay weighting, (or with another optional weight for each topoint instead of the number 1)
#' where d is the distance from census unit's internal point to user-defined point.
#' The default proximity score, using 1/d, is the count of nearby points divided by the harmonic mean of their distances (n/harmean), 
#' (but adjusted when distance is very small, and using the nearest single one if none are nearby). 
#' This is the same as the sum of inverse distances. 
#' The harmonic mean distance (see \code{\link[analyze.stuff]{harmean}}) is the inverse of the arithmetic mean of the inverses, or n / (sum of inverses). \cr \cr
#' Nearby is defined as a user-specified parameter, so only points within the specified distance are counted, except if none are nearby,
#' the single nearest point (at any distance) is used. \cr\cr
#' Default relies on the \pkg{sp} package for the \code{\link[sp]{spDists}} and \code{\link[sp]{SpatialPoints}} functions.
#' Other values of dfunc parameter are slower. \cr\cr
#' IMPORTANT: \cr\cr
#' To create a proximity score for a block group, one can find the score for each block in the block group
#' and then find the population-weighted average of those block scores, for a single block group. \cr
#' FIPS for blocks can be used to find FIPS for block groups. FIPS for block groups can be used to find FIPS for tracts. \cr\cr
#' ADJUSTMENT FOR SMALL DISTANCES:  \cr\cr
#' The adjustment for small distances ensure that each distance represents roughly the distance to the average resident within a spatial unit like a block,
#' rather than just the distance to the center or internal point. 
#' The adjustment uses the area of the spatial unit and assumes residents are evenly spread across the unit.
#' Distance is adjusted in each place if area of each spatial unit is specified, to ensure it represents roughly distance to average resident in the unit:
#' The distance is capped to be no less than 0.9 x radius of a circle of area equal to census unit's area.
#' This approximation treats unit as if it were a circle and assumes pop is evenly distributed within that circle's area, since \cr
#'   0.9r = 0.9 x sqrt(area/pi) = approx solution to dist from avg point (resident) in circle
#' to a random point in the circle (facility or point of interest).
#' The use of a minimum distance per areal unit is intended to help approximate the distance from the average resident 
#' rather than from the internal point or center of the areal unit. The approximation assumes distance to the average resident can be estimated 
#' as if homes and facilities were on average uniformly distributed within blocks (or whatever units are used) that were roughly circular on average.
#' It relies on the fact that the average distance between two random points in a circle of radius R is 90 percent of R 
#' (Weisstein, Eric W. Disk Line Picking. From MathWorld--A Wolfram Web Resource. \url{http://mathworld.wolfram.com/DiskLinePicking.html} ). 
#' This means that if a population is randomly spread over a roughly circular block, a facility inside the block (i.e., very close to the internal point)
#' typically would be 0.9R from the average person. The same math shows that the average point in the circle is 0.67R from the center, 
#' and 1.13R from the edge of the circle. We can describe this relationship using an equation that is a portion of the formula for the 
#' distance between two random points in a circle of radius = 1. The formula uses b = the distance of the facility from the center as a fraction of the radius, 
#' and the integral over a represents distances of residences from the center. 
#' We can solve the equation using \url{http://WolframAlpha.com}, for b = 0, 0.5, or 1, representing points at the center, halfway to the edge, 
#' and at the edge of the circle. For example, we can use this equation for b = 0.5 to find that the average person, if randomly located in a circle of radius R, 
#' is a distance of about 0.8 R from a facility that is halfway between the center and edge of the circle. 
#' Note this is not the same as the expected location of a randomly placed facility, which would use b = sqrt(0.5) instead and gives a distance of about 0.9R. 
#' The following would be used as the input to WolframAlpha to derive the 0.9 approximation: 
#'   Integrate((1/Pi) Sqrt(a + (Sqrt(0.5))^2 - 2(Sqrt(0.5)) Sqrt(a) cos(t)), {a,  0,  1},  {t, 0, pi})
#'  \url{http://bit.ly/1GJ9UID} 
#' @param testing Logical during work in progress
#' @param frompoints Locations of internal points of Census subunits. 
#'   A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed. Decimal degrees. Required.
#' @param topoints Locations of nearby points of interest, proximity to which is the basis of each Census unit's score. 
#'   A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed. Decimal degrees. Required.
#' @param area A number or vector of numbers giving size of each spatial unit with FIPS.pop, 
#'   in square miles or square kilometers depending on the \code{units} parameter. Optional. 
#'     Default is 0, in which case no adjustment is made for small or even zero distance, 
#'     which can cause unrealistically large or even infinite/undefined scores. For zero distance if area=0, Inf will be returned for the score.
#' @param radius NOTE: This default is not the same as the default in \code{\link{get.distances}}! 
#'   Optional, a number giving distance defining nearby, i.e. the search radius, 
#'   in km or miles depending on the code{units} parameter. Default is 5 (km if units='km'). Max is 5200 miles (roughly the distance from Hawaii to Maine).
#' @param units A string that is 'miles' or 'km' for kilometers (default is 'km'), specifying units for distances returned and for radius input.
#' @param FIPS NOT USED CURRENTLY - COULD BE USED LATER TO AGGREGATE (rollup) TO BLOCK GROUPS FROM BLOCKS, FOR EXAMPLE. 
#'   A vector of strings designating places that will be assigned scores where each is the Census FIPS code or other ID. Optional.
#'   Might want to have this be a factor not string to be faster, or ensure it is indexed on fips, or have separate FIPS.BG passed to this function.
#' @param pop NOT USED CURRENTLY - COULD BE USED LATER TO AGGREGATE (rollup) TO BLOCK GROUPS FROM BLOCKS, FOR EXAMPLE. 
#'   A number or vector of numbers giving population count of each spatial unit. 
#'   Default is 1, which would give the unweighted average.
#' @param wts Optional vector of numbers same length as number of topoints. 
#'   If wts is specified, the score for each of the frompoints will be the weighted sum of influences of topoints. 
#'   For example, if decay='1/d' (default), proximity score = sum(wts/d) for all the topoints nearby. 
#'   If decay='1/1', proximity score = sum(wts) for all the topoints nearby.
#' @param return.count Optional, logical, defaults to FALSE, specifies if results returned should include a column with the count of topoints that were within radius, for each of the frompoints
#' @param return.nearest Optional, logical, defaults to FALSE, specifies if results returned should include a column with the distance to the nearest single of the topoints, for each of the frompoints
#' @param decay A string specifying type of function to use when weighting by distance. The Default is '1/d'
#'   For '1/d' decay weighting (default), score is count of points within radius, divided by harmonic mean of distances (when count>0).
#'   Decay weighting also can be '1/d^2' or '1/1' to represent decay by inverse of squared distance, or no decay (equal weighting for all points).
#' @param dfunc Optional character element hf or slc to specify distance function Haversine or spherical law of cosines.
#'   If sp (default, fastest), it uses the \pkg{sp} package to find distances more accurately and more quickly.
#' @return By default, returns a vector of numbers, the proximity scores, one for each of the frompoints (or if testing, a matrix with 2 columns: fromrow and d for distance).
#'   Based on miles by default, or km depending on units. 
#'   Returns +Inf for a unit if that area's area and distance are both zero.
#' @seealso \code{\link{get.distances}} and \code{\link{get.distances.all}} for distances between points, and
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point
#'   within a specified search radius instead of all topoints.
#' @concept proximity
#' @examples
#' test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428),
#'   fromlon = c(-77.0896572305, -77.0896199948)), .Names = c("lat", "lon"),
#'   row.names = c("6054762", "6054764"), class = "data.frame")
#' test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435),
#'   tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), .Names = c("lat", "lon"),
#'   class = "data.frame", row.names = c("6054762", "6054763", "6054764"))
#'
#' set.seed(999)
#' t1=testpoints(1)
#' t10=testpoints(10)
#' t100=testpoints(100)
#' t1k=testpoints(1e3)
#' t10k=testpoints(1e4)
#' t100k=testpoints(1e5)
#' t1m=testpoints(1e6)
#'
#' proxistat(t1, t10k, radius=1, units='km')
#'
#' proxistat(t10, t10k)
#'
#' subunitscores = proxistat(frompoints=test.from, topoints=test.to,
#'   area=rep(0.2, length(test.from[,1])), radius=1, units='km')
#' print(subunitscores)
#' subunitpop = rep(1000, length(test.from$lat))
#' subunits = data.frame(FIPS=substr(rownames(test.from), 1, 5), 
#'   pop=subunitpop, stringsAsFactors=FALSE )
#' unitscores = aggregate(subunits,
#'   by=list(subunits$FIPS), FUN=function(x) {Hmisc::wtd.mean(x$score, wts=x$pop, na.rm=TRUE)}
#' )
#' print(unitscores)
#' \dontrun{
#' output = proxistat.chunked(blocks[ , c('lon','lat')], topoints=rmp, fromchunksize=10000, area=blocks$area / 1e6,
#'    return.count=TRUE, return.nearest=TRUE )
#' output=as.data.frame(output)
#' if (class(blocks$fips)!='character') {blocks$fips <- lead.zeroes(blocks$fips, 15)}
#' blocks$FIPS.BG <- get.fips.bg(blocks$fips)
#' bg.proxi <- data.frame()
#' bg.proxi$scores  <-  aggregate( cbind(d=output$scores, pop=blocks$pop), by=list(blocks$FIPS.BG), function(x) Hmisc::wtd.mean(1/x[,'d'], x[,'pop']))
#' if ('nearestone.d' %in% colnames(output)) { bg.proxi$nearestone.d <- aggregate( output$d, by=list(blocks$FIPS.BG), min) }
#' if ('count.near' %in% colnames(output))   { bg.proxi$count.near   <- aggregate( cbind(d=output$count.near, pop=blocks$pop), by=list(blocks$FIPS.BG), function(x) Hmisc::wtd.mean(1/x[,'d'], x[,'pop'])) }
#' }
#' 
#' @export
proxistat <- function(frompoints, topoints, area=0, radius=5, units='km', decay='1/d', wts, return.count=FALSE, return.nearest=FALSE, FIPS, pop, testing=FALSE, dfunc='sp') {
  
  maxradius.miles <- 5200

  # notes :   Make FIPS columns factors for speed when rollup to block groups? 
  
   warning('NOT NECESSARILY WORKING YET- THIS IS A WORK IN PROGRESS')

  # Error checking -- also uses the error checking that get.distances() does
  
  if (missing(frompoints) | missing(topoints)) {stop('frompoints and topoints must be specified')}
  if (!(units %in% c('km', 'miles'))) {stop('units must be "km" or "miles" ')  }
  radius.miles <- convert(radius, units, 'mi')
  if (radius.miles > maxradius.miles) {
    stop(paste(
      'radius must be less than about', 
      round(convert(maxradius.miles, 'mi', 'km'), 1),
      'kilometers (actually ',
      maxradius.miles,
      ' miles, or the distance from Hawaii to Maine)'
    )) 
  }
  if (is.na(radius) | !is.numeric(radius) | radius < 0 | is.infinite(radius)  ) {stop('invalid radius')}
  if (length(area) == 1) {if (area == 0) {area <- rep(0, length(frompoints[,1]))}}
	if (!missing(area) ) {
    if (!is.vector(area) | !is.numeric(area) | any(is.na(area)) | any(is.infinite(area)) | length(area) != length(frompoints[,1])) {
      stop('area will not be recycled - if supplied, it must be a numeric vector of same length as number of points with no NA or Inf values')
    }
	  if (any(area < 0)  ) {stop('area must be >= 0 ')}
	}
  if (!(decay %in% c('1/d', '1/d^2', '1'))) {stop('invalid decay parameter')}
  if (!missing(wts) & any( !is.numeric(wts) | !is.vector(wts) | length(wts) != length(topoints[,1]) )) {stop('If specified, wts must be a numeric vector of same length as number of topoints')} 

  # handle cases where an input is only one row (one point)
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow = 1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(  topoints)) {mycols <- names(  topoints);   topoints <- matrix(  topoints, nrow = 1); dimnames(  topoints)[[2]] = mycols }
  
  colnames(frompoints) <- latlon.colnames.check(frompoints)
  colnames(topoints)   <- latlon.colnames.check(  topoints)
  
  decayfunction <- switch(decay,
    '1/d'   = function(d) rowSums(1/d,     na.rm = TRUE),
    '1/d^2' = function(d) rowSums(1/(d^2), na.rm = TRUE),
    '1/1'   = function(d) rowSums(d > 0,   na.rm = TRUE)
  )
  # For 1/d,  sum(1/d) is the same as n/harmean(d)
  
  n <- length(frompoints[,1])
  
  ######################################## #
  # Sequence of steps in finding d value(s):
  ######################################## #
  #
  # 1) get distances that are <=radius using get.distances()
  # 2) where d < min.dist, set d <- min.dist to adjust it upwards
  # 3)     and for those, check again to see if new d is still <= radius. keep only if d<=radius now.
  # 4) for each frompoints, if no distances were found, get nearest single d at any radius,
  #       originally thought perhaps by expanding outwards step by step until at least one is found (but not worth the overhead vs just finding ALL d and picking min)

  ######################################## #
  # 1) get distances <= radius
  ######################################## #
  
  ddf <- get.distances(frompoints, topoints, units = units, dfunc = dfunc, return.crosstab = TRUE)
  # NOTE: DO NOT SPECIFY radius IN get.distances here, so it will use default radius of 5200 miles, so that
  # this function can use distances greater than radius that was passed to proxistat, 
  # in case there are none within radius and it needs nearest single one outside radius.
  
  if (testing) {cat('\n\n ddf before fix min dist: \n\n');print(ddf);cat('\n\n')}

  ######################################## #
  # 2) Set distance to minimum allowed distance or true distance, whichever is greater.
  ######################################## #

  # use d or min.dist, whichever is greater
  if (length(area) == 1) {area <- rep(area, n) }
  min.dist <- 0.9 * sqrt( area / pi )  # one per frompoints, which now that crosstab used is one per ddf row
  # takes a few seconds for 10k x 10k matrix, for example
  ddf <- apply(ddf, 2, FUN = function(x) pmax(x, min.dist))
  
  if (testing) {cat('ddf with d adjusted up if d<min.dist: \n\n'); print(ddf); cat('\n\n')}
  
  ######################################## #
  #  RETAIN SINGLE NEAREST IN CASE NEED THAT!! 
  ######################################## #
  
  # which topoint was the nearest?
  # Accounts for maybe 10% of all time in this function
  #nearestone.d <-  rowMins(ddf) # this was a bit slower than method below
  nearestone.colnum <- apply(ddf, 1, which.min)  # about 4 seconds for 10k x 10k matrix, for example
  # how far away was that one nearest to each frompoint?
  nearestone.d <- ddf[ cbind(1:NROW(ddf), nearestone.colnum) ]
  
  ######################################## #
  # 3) keep only if new adjusted d <=radius  (or if it is the minimum of all for the given fromrow)
  ######################################## #
  
  # set to NA any cell of matrix where distance is > search radius
  # Accounts for maybe 10% of all time in this function
  ddf[ddf > radius] <- NA
  
  if (return.count) {
    # record how many are within the radius (<<2% of all time is spent here)
    count.near <- rowSums(!is.na(ddf))
  }

  if (testing) {cat('ddf with final d adjusted up where was <min.dist, but dropped if adjusted to > radius: \n\n');print(ddf);cat('\n\n')}

  #### *** May want to retain info on which fromrows had a score that was based on any distances that were adjusted upwards based on min.dist? ****
  # min.dist.adjustment.used <- which()
  
  ######################################## #
  # 4) where None within radius.... 
  # For any frompoint that had no topoint within radius,
  # use distance to nearest single topoint
  ######################################## #
  
  # note which frompoints had zero within the radius (negligible time)
  fromrow.0near <- which(rowSums(ddf, na.rm = TRUE) == 0)
  
  # put back in the distance that was the nearest one, but only for rows where none were within radius
  # fast
  ddf[ cbind(1:NROW(ddf), nearestone.colnum) ][fromrow.0near ] <- nearestone.d[fromrow.0near]
  
  if (testing) {cat('fromrow.0near = '); print(fromrow.0near)}
  if (testing) {cat('\nlength of fromrow.0near = ');print(length(fromrow.0near));cat('\n\n')}
  if (length(fromrow.0near) > 0) {
    if (testing) {cat(' some fromrows were not in results of get.distances in ddf \n')}
  }
  
  ##################################### #
  # AGGREGATE SCORES ACROSS ALL TOPOINTS NEAR A GIVEN FROMPOINT
  ##################################### #
  
  if (!missing(wts)) {
    # wts is as long as a row of ddf (one for each of the topoints), so use t(t(ddf)*wts) to multiply by wts correctly
    results <- cbind(scores = decayfunction(t(t(ddf) * wts)))
  } else {
    results <- cbind(scores = decayfunction(ddf))  # fast if unwtd
  }
  
  if (return.count) {
    #    or get multiple metrics per block group:
    results <- cbind( scores = results, count.near = count.near)
  }
  
  if (return.nearest) {
    results <- cbind(results, nearestone.d = nearestone.d)
  }

  return(results)

}


if (1 == 0) {
  ########################################################################################################################## #
  
  # ANOTHER APPROACH to get.distances?:
  # might be more efficient to LOOP OVER SITES, NOT BLOCKS, TO CALC DISTANCES, 
  #  running get.distances() on just all sites as frompoints, and then get.nearest as needed.
  
  ######################################## #
  # Could treat each site as a frompoint, so create one row per facility/site rather than per block???:
  ######################################## #

  distances.to.blocks <- get.distances(frompoints = sites[,c('lat','lon')] , topoints = blocks[, c('lat', 'lon')] , radius = 5, units = 'km', return.latlons = FALSE, return.rownums = TRUE)
  distances.to.blocks$FIPS.BG <- blocks$FIPS.BG[ distances.to.blocks$torow ] # not sure about this. CAN we get torownum back??

  ######################################## #
  # ROLLUP TO BLOCK GROUPS. For each BG:
  ######################################## #

  # Using aggregate for 11m blocks aggregated into 220k block groups might take something like 2 minutes! 
  # #rollup to blockgroups is slow using aggregate:
  # system.time({x = aggregate(pop ~ fips.bg, data=blocks, FUN=sum)})   TAKES ABOUT 40 SECONDS
  
  count.near      <- aggregate(distances.to.blocks$FACILITYID, by = list(distances.to.blocks$FIPS.BG), function(x) length(unique(x)))
  nearest.pts     <- aggregate(distances.to.blocks$d, by = list(distances.to.blocks$FIPS.BG), min)
  proximity.score <- aggregate(cbind(d = distances.to.blocks$d, pop = distances.to.blocks$pop), by = list(distances$FIPS.BG), function(x) Hmisc::wtd.mean(1/x[ ,'d'], x[ ,'pop']))

  # IF NO SITE NEARBY: (THIS WILL BE TRUE FOR THE VAST MAJORITY OF ALL BLOCK GROUPS IN THE US TYPICALLY!!)
  # AGAIN, LOOP THROUGH SITES, NOT BLOCK/BG LIST, AND CALC DISTANCES ONLY FOR A WINDOW (WIDER WINDOW NOW), UNTIL FIND ONE.
  zero.near <- 1
  # starting search radius is original radius?
  current.max.miles <-  5 * 0.621371  # 5 kilometers = 3.10686 miles

  while (length(zero.near) > 0) {
    current.max.miles <- current.max.miles * 2
    zero.near <- which(nearest.pts == 0)
    nearest.pts[zero.near] <- min(get.distances(bg[zero.near, c("lat", "lon")], radius = current.max.miles, units = 'miles'))
  }


}


if (1 == 0)  {
  
  ###################################################################################### #
  # notes on how to calc distances
  ###################################################################################### #

  # Formula for distance between two lat/lon points
  # see http://stackoverflow.com/questions/27928/how-do-i-calculate-distance-between-two-latitude-longitude-points
  # or many formulas at http://www.movable-type.co.uk/scripts/latlong.html
  #
  # or maybe just
  #=ACOS(COS(RADIANS(90-Lat1)) *COS(RADIANS(90-Lat2)) +SIN(RADIANS(90-Lat1)) *SIN(RADIANS(90-Lat2)) *COS(RADIANS(Long1-Long2))) *6371
  #PS. To calculate distances in miles, substitute R (6371) with 3958.756 (and for nautical miles, use 3440.065).
  # http://bluemm.blogspot.com/2007/01/excel-formula-to-calculate-distance.html
  
  # For the US 48 states plus DC not PR/VI/etc.,
  # furthest east is roughly -66.949778
  # furthest south is roughly 24.554
  #
  # furthest north is roughly 48.99 (1 mile buffer is a bit into Canada), OR 49
  # furthest west is roughly  -124.771694
  #   but AK/HI are further.
}
