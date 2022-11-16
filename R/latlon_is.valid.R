#' Validate latitudes and longitudes
#' @description Check each latitude and longitude value to see if they are 
#'   NA or outside expected numeric ranges
#'
#' @param lat vector of latitudes in decimal degrees
#' @param lon numeric vector of longitudes in decimal degrees, same length
#'
#' @return logical vector, one element per lat lon pair (location)
#' @seealso latlon_df_clean() latlon_infer() latlon_is.valid() latlon_as.numeric()
#' @export
#'
latlon_is.valid <- function(lat, lon) {
  # TRUE only if both lat and lon seem valid
  if( any(is.na(lat)) || any(is.na(lon)) ) {
    bad <- is.na(lat) | is.na(lon)
  } else {
    bad <- rep(FALSE, length(lat))
  }
  bad <- bad | lat < -90 | lat > 90 | lon < -180 | lon > 180
  return(!bad)
}
