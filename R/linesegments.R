#' @title Add line segments connecting pairs of points, to a plot
#'
#' @description Accepts vectors of x and y values of pairs of points (e.g., longitude and latitude), 
#'   reformats them and adds line segments to an existing plot. Each line segment drawn 
#'   connects one point its paired point. The two sets of points have to be equal in length, set up as pairs.
#' @details
#'   This function also silently returns a matrix of two columns. 
#'   Each column is a vector that has elements sequenced in groups of three -- 
#'   the from point, then the to point, then NA to signify a break in line drawing. See \code{\link{lines}}
#' @param xfrom required numeric vector of x values for starting points
#' @param yfrom required numeric vector of y values for starting points
#' @param xto required numeric vector of x values for ending points
#' @param yto required numeric vector of y values for ending points
#' @param ... optional additional parameters to pass to \code{\link{lines}}
#' @return Draws lines(), one line segment from each starting point to its corresponding ending point.
#' @seealso \code{\link{testpoints}} and \code{\link{get.nearest}} 
#' @examples
#'  t10  <- testpoints(10,  minlat = 25, maxlat = 45, minlon = -100, maxlon = -60)
#'  t100 <- testpoints(100, minlat = 25, maxlat = 45, minlon = -100, maxlon = -60)
#'  nears=as.data.frame( get.nearest(t10, t100, return.latlons=TRUE) )
#'  plot(t10)
#'  plot(t100, pch='.')
#'  linesegments(nears$fromlon, nears$fromlat, nears$tolon, nears$tolat)
#' @export
linesegments <- function(xfrom, yfrom, xto, yto, ...) {
  xvals =  as.vector(t(matrix( c(xfrom, xto, rep(NA, length(xto))), ncol=3) ))
  yvals =  as.vector(t(matrix( c(yfrom, yto, rep(NA, length(yto))), ncol=3) ))
  lines(xvals, yvals, ...)
  invisible(cbind(x=xvals, y=yvals))
}
