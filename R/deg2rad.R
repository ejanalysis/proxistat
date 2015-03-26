#' @title Convert degrees to radians
#' @description From \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#' @param deg Degrees as decimal degrees
#' @return Radians
#' @export
deg2rad <- function(deg) return(deg*pi/180)