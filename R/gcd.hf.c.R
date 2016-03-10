#' @title Distance based on Haversine formula but using C
#' @description Calculates the geodesic distance between two points 
#'   (or multiple pairs of points) specified by radian latitude/longitude.
#' @details long1 and lat1 must be same length. long2 and lat1 must be same length.
#'   All four must be the same length, defining pairs of points. 
#'   Alternatively long1 and lat1 can define a single point while long2 and lat2 define a series of points, or vice versa.
#'   Taken from \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#'   but use \code{\link{pmin}} instead of \code{\link{min}} to vectorize it to handle at least pairs.
#' @param long1 longitude(s) in radians, vector of one or more numbers
#' @param lat1 latitude(s) in radians, vector of one or more numbers
#' @param long2 longitude(s) in radians, vector of one or more numbers
#' @param lat2 latitude(s) in radians, vector of one or more numbers
#' @return Distance in kilometers
#' @seealso \code{\link{convert}}, \code{\link{gcd}}, \code{\link{get.distances}}, \code{\link{get.distances.all}}
#' @export
gcd.hf <- function(long1, lat1, long2, lat2) {
  
  cat('NOT IMPLEMENTED YET- WORK IN PROGRESS... \n')
#   
# if (1==0) {
#   
#   
#   #include <cmath> 
#   #define pi 3.14159265358979323846
#   #define earthRadiusKm 6371.0
#   
#   // This function converts decimal degrees to radians
#   double deg2rad(double deg) {
#     return (deg * pi / 180);
#   }
#   
#   //  This function converts radians to decimal degrees
#   double rad2deg(double rad) {
#     return (rad * 180 / pi);
#   }
#   
#   /**
#     * Returns the distance between two points on the Earth.
#   * Direct translation from http://en.wikipedia.org/wiki/Haversine_formula
#   * @param lat1d Latitude of the first point in degrees
#   * @param lon1d Longitude of the first point in degrees
#   * @param lat2d Latitude of the second point in degrees
#   * @param lon2d Longitude of the second point in degrees
#   * @return The distance between the two points in kilometers
#   */
#     double distanceEarth(double lat1d, double lon1d, double lat2d, double lon2d) {
#       double lat1r, lon1r, lat2r, lon2r, u, v;
#       lat1r = deg2rad(lat1d);
#       lon1r = deg2rad(lon1d);
#       lat2r = deg2rad(lat2d);
#       lon2r = deg2rad(lon2d);
#       u = sin((lat2r - lat1r)/2);
#       v = sin((lon2r - lon1r)/2);
#       return 2.0 * earthRadiusKm * asin(sqrt(u * u + cos(lat1r) * cos(lat2r) * v * v));
#     }
#   
#   
#   
# }
# 
#     
#   R <- 6371 # Earth mean radius [km]
#   return( R * 2 * asin(pmin(1,sqrt( sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2))) )
#   #  return(d) # Distance in km
}
