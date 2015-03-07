#' @title THIS FUNCTION IS NOT YET COMPLETE - 
#' #' Calculate proximity statistic for each location, 
#' quantifying number of and proximities of nearby points.
#' @description
#' \code{proxistat} returns a proximity statistic (score) for each location (e.g., Census block group), quantifying the number of and proximities of nearby points.
#' 
#' @details
#' This function returns a vector of proximity scores, one for each location such as a Census block group. 
#' For example, the proximity score may be used to represent how many hazardous waste sites are near any given neighborhood and how close they are.
#' A proximity score quantifies the proximity and count of nearby points using a specified formula. 
#' Proximity Score = distance-weighted count of points nearby (within search radius)
#' (or weighted distance to nearest single point if there are none within the radius).
#' This is the sum of 1/d or 1/d^2 or 1/1, depending on the decay weighting, 
#' where d is the distance from census unit's internal point to user-defined point,
#' The default proximity score is the count of nearby points divided by the harmonic mean of their distances
#' (but adjusted when distance is very small, and using the nearest single one if none are nearby). This is the same as the sum of inverse distances.
#' Nearby is defined as a user-specified parameter, so only points within the specified distance are counted, except if none are nearby, 
#' the single nearest point (at any distance) is used.
#' The adjustment for small distances ensure that each distance represents roughly the distance to the average resident within a spatial unit like a tract, 
#' rather than just the distance to the center or internal point. The adjustment uses the area of the spatial unit and assumes residents are evenly spread across the unit.
#' Distance is adjusted in each place if area of each spatial unit is specified, to ensure it represents roughly distance to average resident in the unit: 
#' The distance is capped to be no less than 0.9 * radius of a circle of area equal to census unit's area. 
#' This approximation treats unit as if it were a circle and assumes pop is evenly distributed within that circle's area, since 
#'   0.9r = 0.9 * sqrt(area/pi()) = approx solution to dist from avg point (resident) in circle 
#' to a random point in the circle (facility or point of interest).
#' Relies on the \pkg{sp} package for the \code{\link{sp}{spDists}} and \code{\link{sp}{SpatialPoints}} functions.
#' \cr\cr
#' IMPORTANT:
#' Each spatial unit with a FIPS.score is assigned a proximity score that is the population weighted arithmetic 
#' average of scores assigned to the subunits with FIPS.pop, each of which must have pop or use the default of 1 (unweighted).
#' To create scores for all block groups, with populations and points known for all blocks within each block group, 
#' FIPS.pop is for blocks and FIPS.score is for block groups. 
#' However, it is important that the function be able to know which blocks are in which block groups. 
#' This may for now assume bg and block FIPS conventions, but more flexibly 
#' later could have an input param that is the length of pop and area and censuspoints, with 2 cols, FIPS.pop and FIPS.score.
#' FIPS.score would have a lot of redundancy and perhaps could be input as a factor in a 2 col data.frame.

#' @param FIPS.score A vector of strings designating places that will be assigned scores where each is the Census FIPS code or ID for the larger spatial units that will be given proximity scores.
#' @param FIPS.pop A vector of strings, where each is the Census FIPS code or ID for the smaller spatial units that have censuspoints, area, & pop information.
#' @param pop A number or vector of numbers giving population count of each spatial unit. Default is 1.
#' @param censuspoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed. Decimal degrees. Required.
#' @param topoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed. Decimal degrees. Required.
#' @param radius A number giving distance defining nearby, in miles by default (or kilometers if units is 'km'). Default is 5.
#' @param units A string that is 'miles' by default, or 'km' for kilometers, specifying units for distances returned and for radius input.
#' @param decay A string specifying type of function to use when weighting by distance. Default is '1/d'
#'   For '1/d' decay weighting (default), score is count of points within radius, divided by harmonic mean of distances (when count>0).
#'   Decay weighting also can be '1/d^2' or '1/1' to represent decay by inverse of squared distance, or no decay (equal weighting for all points).
#' @param area A number or vector of numbers giving size of each spatial unit with FIPS.pop, in square miles by default (or square kilometers if units is 'km'). Default is 1.
#' @return By default, returns a vector of numbers, the proximity scores, one for each of the censuspoints. Based on miles by default, or km depending on units.
#' @seealso \code{\link{get.distances}} and \code{\link{get.distances.all}} for distances between points, and
#'   \code{\link{get.nearest}} for get.nearest() which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints.
#' @concept proximity
#' @examples
#' test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428), 
#'   fromlon = c(-77.0896572305, -77.0896199948)), .Names = c("lat", "lon"), 
#'   row.names = c("6054762", "6054764"), class = "data.frame")
#' test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435), 
#'   tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), .Names = c("lat", "lon"), 
#'   class = "data.frame", row.names = c("6054762", "6054763", "6054764"))
#' @note **** TO BE DONE: 
#' proxistat(censuspoints=test.from, topoints=test.to, 
#'  FIPS.score=c('012','013'), 
#'  FIPS.pop=c('01245','01346','01347'), 
#'  area=rep(0.2, length(testfrom[,1])), pop=rep(1000, length(testfrom[,1])))
#' @export
#' 
proxistat <- function(censuspoints, topoints, FIPS.pop, FIPS.score, area=1, pop=1, radius=5, units='miles', decay='1/d') {

  stop('THIS IS NOT WORKING YET -- WORK IN PROGRESS')

	# Value returned also could include count of points nearby (within radius).??
	#
	# pop can be used later to roll up results of small units (e.g. blocks) to larger (e.g. block group or tract) 
	# using population weighted mean of scores in larger unit.
  # but need to have 2 more parameters: FIPS.pop and FIPS.score

  frompoints <- censuspoints; rm(censuspoints)

  # Error checking
  # use the error checking that get.distances() does
  
	if (!missing(area)) {
    if (!is.vector(area) || length(area)!=length(frompoints[,1])) {
      stop('area must be vector of same length as number of points')
    }
	}
	if (!missing(pop))  {
    if (!is.vector(pop)  ||  length(pop)!=length(frompoints[,1])) {
      stop('pop must be vector of same length as number of points')
    }
	}

	n <- length(frompoints[,1])
	#n2 <- length(topoints[,1])

	# INSERT CODE HERE
	#  USE get.distances() etc. related functions I have defined.
	
  # topoints, FIPS.pop, FIPS.score, area=1, pop=1, radius=5, units='miles', decay='1/d'



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

#######################################################################################
# OVERALL OUTLINE:
#######################################################################################
#
#########################################
# Get list of user-defined sites "sites" with ID, lat, lon, wts
#########################################
#   Could error check lat/lon values, etc.

#########################################
# Get blocks as "blocks" with FIPS.BLOCK, FIPS.BG, lat, lon, pop, area
#########################################
blocks$FIPS.BG <- substr(blocks$FIPS.BLOCK, 1, 12)

#########################################
#  GET AREA of each block to use in defining M, minimum distance allowed.
#########################################

# check units are miles and square miles ***
min.dist <- 0.9 * sqrt(blocks$area/pi )

#########################################
# LOOP OVER SITES, NOT BLOCKS, CALC DISTANCES
#########################################

# create one row per facility/site:
distances.to.blocks <- get.distances(frompoints=sites[,c('lat','lon')] , topoints=blocks[, c('lat', 'lon')] , max.miles=1.6, return.latlons=FALSE, return.rownums=TRUE) # CAN'T DO return.rownums=TRUE YET ??# use 5 kilometers, converted to miles
distances.to.blocks$FIPS.BG <- blocks$FIPS.BG[ distances.to.blocks$torow ] # not sure about this. CAN we get torownum back??


#########################################
# ROLLUP TO BLOCK GROUPS. For each BG:
#########################################

    count.near <- aggregate(distances.to.blocks$FACILITYID, by=list(distances.to.blocks$FIPS.BG), function(x) length(unique(x)))

    nearest.pts <- aggregate(distances.to.blocks$d, by=list(distances.to.blocks$FIPS.BG), min)

    proximity.score <- aggregate(cbind(d=distances.to.blocks$d, pop=distances.to.blocks$pop), by=list(distances$FIPS.BG), function(x) wtd.mean(1/x[,'d'], x[,'pop']))

# IF NO SITE NEARBY: (THIS WILL BE TRUE FOR THE VAST MAJORITY OF ALL BLOCK GROUPS IN THE US TYPICALLY!!)
# AGAIN, LOOP THROUGH SITES, NOT BLOCK/BG LIST, AND CALC DISTANCES ONLY FOR A WINDOW (WIDER WINDOW NOW), UNTIL FIND ONE.

zero.near <- 1
# starting search radius is max.miles
current.max.miles <-  5 * 0.621371  # 5 kilometers = 3.10686 miles

while (length(zero.near) > 0) {
  current.max.miles <- current.max.miles * 2
  zero.near <- which(nearest.pts==0)
  nearest.pts[zero.near] <- min( get.distances( bg[zero.near, c("lat","lon")]  , max.miles=current.max.miles  ) )
}





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
