#' @title Find Distances between lat lon Points or Create Proximity Scores.
#' @docType package
#' @name proxistat-package
#' @import sp
#'
#' @description This package has functions helping to calculate distances between points,
#' such as the distances between all points,
#' distances to all points within some maximum distance,
#' distance to nearest single point, etc.
#' It also can create a proximity score for each spatial unit like a
#' Census block group, to quantify the distance-weighted count of nearby
#' points.
#'
#' @details This package has functions helping to calculate distances between
#' geographic points, such as the distances between all points, distances to
#' all points within some maximum distance, distance to nearest single point,
#' etc. It also can create a proximity score for each spatial unit like a
#' Census block group, to quantify the distance-weighted count of nearby
#' points. This proximity score can be used in environmental justice (EJ)
#' analysis, for example.
#'
#' This package relies on the \pkg{sp} package for the actual calculation of distance.
#'
#' Points are defined by decimal degrees of latitude and longitude.
#'
#' A vector of points can be specified using a data.frame of two columns, "lat" and "lon"
#' which specify latitude and longitude in decimal degrees.
#' It returns the distances from one or more \code{frompoints} to one or more \code{topoints}.
#' \cr\cr
#' Key functions include
#' \cr
#' \itemize{
#' \item \code{\link{get.nearest}} to find the one among \code{topoints} nearest each \code{frompoints}
#' \item \code{\link{get.distances}} to find distances quickly within an optional search radius
#' \item \code{\link{get.distances.all}} to find distances from all \code{frompoints} to all \code{topoints}
#' \item \code{\link{proxistat}} to create a proximity score that quantifies,
#' for each spatial unit like a Census block group,
#' how many \code{topoints} are nearby and how close they are.
#' }
#' @author info@@ejanalysis.com<info@@ejanalysis.com>
#'
#' @references \pkg{sp} package documentation for basic distance function.\cr
#'
#' [author] (2015). [...Technical Documentation 2015], \cr
#' \url{http://www.TBD.gov/environmentaljustice}\cr
#'
#' Wikipedia pages mentioned in help for \code{\link{deltalon.per.km}}, \code{\link{deltalon.per.km}}, \code{\link{meters.per.degree.lat}}, \code{\link{meters.per.degree.lon}}: \cr
#' \url{http://en.wikipedia.org/wiki/Longitude} and \url{http://en.wikipedia.org/wiki/Decimal_degrees}
#'
#' @concept distance
#' @concept proximity
#' @concept environmental justice
#' @concept EJ
#' @seealso \pkg{sp}
#' @examples
#' test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428),
#'   fromlon = c(-77.0896572305, -77.0896199948)),
#'   .Names = c("lat","lon"), row.names = c("6054762", "6054764"), class = "data.frame")
#' test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435),
#'   tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)),
#'   .Names = c("lat","lon"), class = "data.frame", row.names = c("6054762", "6054763", "6054764"))
#' t100=testpoints(100)
#' t10=testpoints(10)
#' t3=testpoints(3)
#' 
#' get.distances(
#'   test.from[1:2,], test.to[1:3, ], radius=0.7, units='km', return.rownums=TRUE, return.latlons=TRUE
#' )
#' get.nearest(test.from, test.to)
#' 
#' 
#' get.distances(  t3, t10, units='km', return.crosstab=TRUE)
#' get.distances(  t3, t10, units='km')
#' get.distances(  t3, t10, radius=300, units='km')
#' proxistat(  t3, t10, radius=300, units='km')
#' 1/get.nearest(   t3, t10, radius=300, units='km')
NULL
