#' @title Proximity Statistic for Each Location and Nearby Points
#' 
#' @description Calculate proximity statistic for each location, 
#' quantifying number of and proximities of nearby points.
#' \code{proxistat} returns a proximity statistic (score) for each location (e.g., Census block).
#' 
#' @details
#' This function returns a vector of proximity scores, one for each location such as a Census block. 
#' For example, the proximity score may be used to represent how many hazardous waste sites are near any given neighborhood and how close they are.
#' A proximity score quantifies the proximity and count of nearby points using a specified formula. 
#' \cr
#' Proximity Score = distance-weighted count of points nearby (within search radius)
#' \cr
#' (or weighted distance to nearest single point if there are none within the radius).
#' \cr
#' This is the sum of 1/d or 1/d^2 or 1/1, depending on the decay weighting, 
#' where d is the distance from census unit's internal point to user-defined point.
#' The default proximity score, using 1/d, is the count of nearby points divided by the harmonic mean of their distances
#' (but adjusted when distance is very small, and using the nearest single one if none are nearby). This is the same as the sum of inverse distances.
#' Nearby is defined as a user-specified parameter, so only points within the specified distance are counted, except if none are nearby, 
#' the single nearest point (at any distance) is used.
#' \cr\cr
#' The adjustment for small distances ensure that each distance represents roughly the distance to the average resident within a spatial unit like a block, 
#' rather than just the distance to the center or internal point. The adjustment uses the area of the spatial unit and assumes residents are evenly spread across the unit.
#' Distance is adjusted in each place if area of each spatial unit is specified, to ensure it represents roughly distance to average resident in the unit: 
#' The distance is capped to be no less than 0.9 * radius of a circle of area equal to census unit's area. 
#' This approximation treats unit as if it were a circle and assumes pop is evenly distributed within that circle's area, since 
#' \cr
#'   0.9r = 0.9 * sqrt(area/pi()) = approx solution to dist from avg point (resident) in circle 
#' to a random point in the circle (facility or point of interest).
#' \cr\cr
#' Relies on the \pkg{sp} package for the \code{\link{sp}{spDists}} and \code{\link{sp}{SpatialPoints}} functions.
#' \cr\cr
#' IMPORTANT:
#' \cr
#' To create a proximity score for a block group, one can find the score for each block in the block group
#' and then find the population-weighted average of those block scores, for a single block group.
#' \cr
#' FIPS for blocks can be used to find FIPS for block groups. FIPS for block groups can be used to find FIPS for tracts. 
#' 
#' @param testing Logical during work in progress
#' @param frompoints Locations of internal points of Census subunits. A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed. Decimal degrees. Required.
#' @param topoints Locations of nearby points of interest, proximity to which is the basis of each Census unit's score. A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed. Decimal degrees. Required.
#' @param area A number or vector of numbers giving size of each spatial unit with FIPS.pop, in square miles by default (or square kilometers if units is 'km'). Optional. Default is 0, in which case no adjustment is made for small or even zero distance, which can cause unrealistically large or even infinite/undefined scores. For zero distance if area=0, Inf will be returned for the score.
#' @param radius A number giving distance defining nearby, i.e. the search radius, in miles by default (or kilometers if units is 'km'). Default is 5.
#' @param units A string that is 'miles' by default, or 'km' for kilometers, specifying units for distances returned and for radius input.
#' @param FIPS NOT USED CURRENTLY - COULD BE USED LATER TO AGGREGATE (rollup) TO BLOCK GROUPS FROM BLOCKS, FOR EXAMPLE. A vector of strings designating places that will be assigned scores where each is the Census FIPS code or other ID. Optional.
#' @param pop NOT USED CURRENTLY - COULD BE USED LATER TO AGGREGATE (rollup) TO BLOCK GROUPS FROM BLOCKS, FOR EXAMPLE. A number or vector of numbers giving population count of each spatial unit. Default is 1, which would give the unweighted average.
#' @param decay A string specifying type of function to use when weighting by distance. Default is '1/d'
#'   For '1/d' decay weighting (default), score is count of points within radius, divided by harmonic mean of distances (when count>0).
#'   Decay weighting also can be '1/d^2' or '1/1' to represent decay by inverse of squared distance, or no decay (equal weighting for all points).
#' @return By default, returns a vector of numbers, the proximity scores, one for each of the frompoints. Based on miles by default, or km depending on units. Returns +Inf for a unit if that area's area and distance are both zero.
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
#' subunits = data.frame(FIPS=substr(rownames(test.from), 1, 5), pop=subunitpop, stringsAsFactors=FALSE )
#' unitscores = aggregate(subunits, 
#'   by=list(subunits$FIPS), FUN=function(x) {wtd.mean(x$score, wts=x$pop, na.rm=TRUE)} 
#' )
#' print(unitscores)
#' 
#' @export
proxistat <- function(frompoints, topoints, area=0, radius=5, units='miles', decay='1/d', FIPS, pop, testing=FALSE) {
  
  warning('THIS IS A WORK IN PROGRESS')

	# Value returned also could include count of points nearby (within radius)?
  # One way to get that is specify decay='1/1'

  # Error checking -- also uses the error checking that get.distances() does
  if (missing(frompoints) | missing(topoints)) {stop('frompoints and topoints must be specified')}
  if (is.na(radius) || !is.numeric(radius) || radius<0 || is.infinite(radius) || radius> 40000 ) {stop('invalid radius')}
  if (!(units %in% c('km', 'miles'))) {stop('units must be "km" or "miles" ')  }
	if (!missing(area)) {
    if (!is.vector(area) || !is.numeric(area) || any(is.na(area)) || any(is.infinite(area)) || length(area)!=length(frompoints[,1])) {
      stop('area will not be recycled - if supplied, it must be a numeric vector of same length as number of points with no NA or Inf values')
    }
	  if (any(area < 0) | any(area > 1e6) ) {stop('area must be between 0 and 1 million')}
	}
  if (!(decay %in% c('1/d', '1/d^2', '1'))) {stop('invalid decay parameter')}

  decayfunction <- switch(decay, 
    '1/d'   = function(d) sum(1/d, na.rm=TRUE),
    '1/d^2' = function(d) sum(1/(d^2), na.rm=TRUE),
    '1/1'   = function(d) length(d)
  )
  # For 1/d,  sum(1/d) is the same as n/harmean(d)

  n <- length(frompoints[,1])
  
  #########################################
  # Sequence of steps in finding d value(s):
  #########################################
  #
  # 1) get distances that are <=radius using get.distances()
  # 2) where d < min.dist, set d <- min.dist to adjust it upwards
  # 3)     and for those, check again to see if new d is still <= radius. keep only if d<=radius now.
  # 4) for each frompoints, if no distances were found, get nearest single d at any radius,
  #       perhaps by expanding outwards step by step until at least one is found (if worth the overhead vs just finding ALL d and picking min)
  
  #########################################
  # 1) get distances <= radius
  #########################################
  
  ddf <- get.distances(frompoints, topoints, radius=radius, units=units, )

  if (testing) {cat('\n\n ddf before fix min dist: \n\n');print(ddf);cat('\n\n')}

  #########################################
  # 2) Set distance to minimum allowed distance or true distance, whichever is greater.
  #########################################
  
  if (length(area)==1) {area <- rep(area, n) }
  min.dist <- 0.9 * sqrt( area / pi )  # one per frompoints, not one per ddf row
  ddf.min.dist <- min.dist[ddf$fromrow] # min.dist[match(ddf$fromrow , 1:length(area))]
  ddf$d <- pmax(ddf$d, ddf.min.dist) # use d or min.dist, whichever is greater
  
  # *** May want to retain info on which distances were adjusted upwards based on min.dist?

  if (testing) {cat('ddf with d adjusted up if d<min.dist: \n\n');print(ddf);cat('\n\n')}
  
  # WHERE area = 0 FOR ONE OR ALL UNITS, AND DISTANCE=0 from that unit to 1+ topoints, 
  # THIS RETURNS +Inf FOR THE SCORE FOR THAT frompoint
  #   (regardless of distances to any other topoints), unless no decay.
  # i.e., just keep d=0 for those frompoint-topoint pairs, 
  # and 1/d or 1/d^2 will return Inf as will score, which is sum of those for a frompoint.
  # ddf$d[ddf$d==0 & area==0] <- 0
  
  #########################################
  # 3) keep only if new adjusted d <=radius
  #########################################
  
  ddf <- ddf[ddf$d <= radius, ] 
  
  if (testing) {cat('ddf with final d adjusted up where was <min.dist, but dropped if adjusted to > radius: \n\n');print(ddf);cat('\n\n')}
  
  #########################################
  # 4) None within radius.... 
  # For any frompoint that had no topoint within radius,
  # use distance to nearest single topoint 
  #########################################
  
  fromrow.0near <- which(!(1:n %in% ddf$fromrow))
  
  if (testing) {cat('fromrow.0near = '); print(fromrow.0near)}
  if (testing) {cat('\nlength of fromrow.0near = ');print(length(fromrow.0near));cat('\n\n')}
  
  if (length(fromrow.0near) > 0) {
    
    if (testing) {cat(' some fromrows were not in results of get.distances in ddf \n')}
    
    d.nearest1 <- get.nearest(frompoints = frompoints[fromrow.0near, ], topoints, units = units, return.rownums = FALSE, return.latlons = FALSE)
    
    # Now have to check again to fix d < min.dist but radius is now irrelevant and even if adjusted d is no longer the nearest, it is the smallest d allowed.
    
    d.nearest1$d[d.nearest1$d < min.dist[fromrow.0near] ] <- min.dist[fromrow.0near] 
  }
  
  # now merge ddf with d.nearest1
  
  ##################################################
  # *** NEED TO HANDLE CASES WHERE ddf and/or d.nearest1 HAVE ZERO ROWS HERE, OR JUST USE c() to combine vectors instead of using rbind to combine all the vectors at once in a data.frame
  ##################################################
  
  if (length(ddf$d)==0 & exists('d.nearest1')) {
    ddf <- d.nearest1
  } else {
    if (exists('d.nearest1')) {
      colnames(d.nearest) <- colnames(ddf)
      if (testing) {cat('d.nearest1 = ', d.nearest1,'\n')}
      ddf <- rbind(ddf, d.nearest1)   # STILL CRASHES HERE IN CERTAIN CASES? ******
    } 
  }

  
  

  ######################################

  # **** CASES WITH CRASHES THAT NEED TO BE FIXED:
  
#   > proxistat(test.from, test.to,radius=0.002)
#   fromrow torow d
#   1       2     2 0
#   fromrow torow d min.dist
#   1       2     2 0        0
#   [1] 1
#   [1] 1
#   Error in rbind(deparse.level, ...) : 
#     numbers of columns of arguments do not match 
#   4 stop("numbers of columns of arguments do not match") 
#   3 rbind(deparse.level, ...) 
#   2 rbind(ddf, d.nearest1) at proxistat.R#148
#   1 proxistat(test.from, test.to, radius = 0.002) 
  # 
#     > proxistat(test.from, test.to,radius=0.00)
#   [,1] [,2] [,3]
#   [1,]   NA   NA   NA
#   Error in ddf$fromrow : $ operator is invalid for atomic vectors
#   
  
  
  scores <- aggregate(ddf$d, by=list(ddf$fromrow), FUN=decayfunction )
  
  return(scores)
}



if (1==0) {
  ###########################################################################################################################
  # ANOTHER APPROACH I TRIED : 
  # might be more efficient to LOOP OVER SITES, NOT BLOCKS, TO CALC DISTANCES, running get.distances() on just all sites as frompoints, and then get.nearest as needed.
  
  #########################################
  # Get blocks as "blocks" with FIPS.BLOCK, FIPS.BG, lat, lon, pop, area
  #########################################
  blocks$FIPS.BG <- substr(blocks$FIPS.BLOCK, 1, 12)
  
  # etc.
  
  #########################################
  # each site is treated as a frompoint, so create one row per facility/site:
  #########################################
  
  distances.to.blocks <- get.distances(frompoints=sites[,c('lat','lon')] , topoints=blocks[, c('lat', 'lon')] , radius=1.6, units='miles', return.latlons=FALSE, return.rownums=TRUE) 
  # CAN'T DO return.rownums=TRUE YET ??# use 5 kilometers, converted to miles
  distances.to.blocks$FIPS.BG <- blocks$FIPS.BG[ distances.to.blocks$torow ] # not sure about this. CAN we get torownum back??
  
  #########################################
  # ROLLUP TO BLOCK GROUPS. For each BG:
  #########################################
  
  count.near      <- aggregate(distances.to.blocks$FACILITYID, by=list(distances.to.blocks$FIPS.BG), function(x) length(unique(x)))
  nearest.pts     <- aggregate(distances.to.blocks$d, by=list(distances.to.blocks$FIPS.BG), min)
  proximity.score <- aggregate(cbind(d=distances.to.blocks$d, pop=distances.to.blocks$pop), by=list(distances$FIPS.BG), function(x) wtd.mean(1/x[,'d'], x[,'pop']))
  
  # IF NO SITE NEARBY: (THIS WILL BE TRUE FOR THE VAST MAJORITY OF ALL BLOCK GROUPS IN THE US TYPICALLY!!)
  # AGAIN, LOOP THROUGH SITES, NOT BLOCK/BG LIST, AND CALC DISTANCES ONLY FOR A WINDOW (WIDER WINDOW NOW), UNTIL FIND ONE.
  zero.near <- 1
  # starting search radius is original radius?
  current.max.miles <-  5 * 0.621371  # 5 kilometers = 3.10686 miles
  
  while (length(zero.near) > 0) {
    current.max.miles <- current.max.miles * 2
    zero.near <- which(nearest.pts==0)
    nearest.pts[zero.near] <- min( get.distances( bg[zero.near, c("lat","lon")]  , radius = current.max.miles, units='miles'  ) )
  }
  
  
}


if (1==0)  {

#######################################################################################
# NOTES ON R CODE TO CALCULATE 
#   PROXIMITY SCORES, 
#    DISTANCE TO NEAREST SITE, AND 
#   COUNT OF SITES NEARBY (WITHIN X MILES)
# FOR EVERY BLOCK OR BLOCK GROUP IN THE US, 
# FOR A USER-DEFINED SET OF SITES (E.G. REGULATED FACILITIES IN ONE SECTOR)
# AND SUMMARY STATISTICS ON DISTRIBUTIONS OF THESE BY DEMOGRAPHIC GROUP.
#
# Started work on this around 9/24/2013




# #######################################################################################
#
#	TOOL TO CALCULATE PROXIMITY SCORE FOR EVERY BLOCK GROUP IN THE US
#	FOR PROXIMITY AND COUNT OF USER-SPECIFIED SET OF FACILITIES OR POINTS
#	AND
#	THEN CALCULATE US SUMMARY STATS ON 
#
#	DISTRIBUTION OF PROXIMITY SCORES WITHIN EACH DEMOGRAPHIC GROUP INCLUDING US POP OVERALL, SUCH AS: 
#
# DISTANCE TO NEAREST SITE:
#
#	1A- DIST. FOR EACH %ILE OF POP.: DISTANCE TO CLOSEST SITE FOR EACH DEMOG GROUP (AVG/MEDIAN/DISTRIBUTION OVER PEOPLE)
#	1B- %ILE OF POP., FOR EACH DISTANCE: WHAT % OF EACH DEMOG GROUP IS WITHIN X MILES OF ANY ONE SITE? (%ILES OF # OF MILES)
#	1C- RR AS RATIO OF AVG PERSON'S DISTANCE TO NEAREST SITE, FOR DEMOG GROUP VS REST OF THE US POPULATION
#
# COUNT OF SITES NEARBY:
#
#	2A- # OF SITES NEARBY, FOR EACH %ILE OF POP.: # OF SITES WITHIN X MILES, FOR EACH DEMOG GROUP (AVG/MEDIAN/DISTRIB OVER PEOPLE)
#		- POSSIBLY FOR EACH OF SEVERAL DISTANCES X
#	2B- %ILE OF POP., FOR EACH # OF SITES NEARBY: WHAT % FOR EACH DEMOG GROUP HAS Y SITES NEARBY (WITHIN X MILES)?
#		- POSSIBLY FOR EACH OF SEVERAL DISTANCES X
#	2C- RR AS RATIO OF AVG PERSON'S COUNT OF SITES NEARBY, FOR DEMOG GROUP VS REST OF THE US POPULATION
#
# PROXIMITY SCORE (DISTANCE AND COUNT):
#
#	3A- PROXIMITY SCORE FOR A GIVEN %ILE OF POP.: AVG/MEDIAN/DISTRIBUTION OF PROXIMITY SCORES IN US, FOR JUST LOW-INCOME, ETC.
#	3B- %ILE OF POP., FOR EACH PROXIMITY SCORE: WHAT % OF EACH DEMOG GROUP HAS PROXIMITY SCORE OF Z?
#	3C- RR AS RATIO OF AVG PERSON'S PROXIMITY SCORE, FOR DEMOG GROUP VS REST OF THE US POPULATION
#
#
#
#######################################################################################


#######################################################################################
# notes on how to calc distances
#######################################################################################

# Formula for distance between two lat/lon points
# see http://stackoverflow.com/questions/27928/how-do-i-calculate-distance-between-two-latitude-longitude-points 
# or many formulas at http://www.movable-type.co.uk/scripts/latlong.html 
#
# or maybe just
#=ACOS(COS(RADIANS(90-Lat1)) *COS(RADIANS(90-Lat2)) +SIN(RADIANS(90-Lat1)) *SIN(RADIANS(90-Lat2)) *COS(RADIANS(Long1-Long2))) *6371
#and my final version that uses Excel cell references is:
#
#=ACOS(COS(RADIANS(90-A2)) *COS(RADIANS(90-A3)) +SIN(RADIANS(90-A2)) *SIN(RADIANS(90-A3)) *COS(RADIANS(B2-B3))) *6371
#PS. To calculate distances in miles, substitute R (6371) with 3958.756 (and for nautical miles, use 3440.065).
# http://bluemm.blogspot.com/2007/01/excel-formula-to-calculate-distance.html 

# For the US 48 states plus DC not PR/VI/etc., 
# furthest east is roughly -66.949778
# furthest south is roughly 24.554
#
# furthest north is roughly 48.99 (1 mile buffer is a bit into Canada), OR 49
# furthest west is roughly  -124.771694
#   but AK/HI are further.

#######################################################################################
# old examples of distance functions - but see get.distance() etc. now
#######################################################################################
# 
# get.distance <- function(lat1, lon1, lat2, lon2) { 
#   sqrt( (lat2-lat1)^2 + (lon2-lon1)^2) 
#   # this could return a single number or vectorized should return a vector, but 
#   # all 4 inputs have to be same length 
#   # or if one is a single point it will be recycled.
#   # *** Replace this with the more accurate formula for spheroid
# } 
# 
# get.distance.matrix <- function(frompoints, topoints) {
#   apply(frompoints, 1, FUN=function(x) get.distance(x$lat, x$lon, topoints$lat, topoints$lon) )
#   # This can return a matrix of all pairs, where it returns one row for each combo of frompoints & topoints.
#   # It loops over the rows in frompoints[,c('lat', 'lon')] and for each finds the distances to all the points in topoints[,c('lat','lon')]
#   # frompoints$lat1 & frompoints$lon1 can be different length than topoints$lat2, topoints$lon2
# }


################# ################# ################# ################# ################# ################# ################# 
}
