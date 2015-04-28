#' @title Utility function to check for valid lat/lon columns
#'
#' @description Used by functions like \code{\link{get.distances}} to check input parameters frompoints and topoints
#' @param mypoints A matrix or data.frame
#' @return Returns a vector of colnames such as c('lat', 'lon') or stops if problem found
#' @seealso \code{\link{get.distances.all}} which allows you to get distances between all points,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit based on distances to nearby points.
#' @concept proximity
#' @examples #
#' @seealso \code{\link{get.distances}} which allows you to specify a search radius and 
#'   get distances only within that radius which can be faster,
#'   \code{\link{get.distances.prepaired}} for finding distances when data are already formatted as pairs of points,
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point 
#'   within a specified search radius instead of all topoints, and 
#'   \code{\link{proxistat}} which calculates a proximity score for each spatial unit
#' @export
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
