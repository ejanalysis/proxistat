#' @title Convert degrees to radians
#' @description From \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#' @param deg Degrees as decimal degrees, required numeric vector
#' @return Radians, as numeric vector same length as input
#' @export
deg2rad <- function(deg) return(deg*pi/180)
