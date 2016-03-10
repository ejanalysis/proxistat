#' @title Read a series of dbf files and join compile them as a single data.frame
#' 
#' @description
#'   Can help with a series of downloaded Census data files, such as one file per State. 
#'   Reads each using \code{\link[foreign]{read.dbf}} and combines them all as a data.frame.
#'   They must all have the same columns, and each file just provides additional rows of data.
#' @param bin 
#' @param lat
#' @param lon
#' @param vartext default is 'x', text to use in describing the field mapped
#' @param areatext default is 'area', text to use in describing the area mapped
#' @param coloring default is some basic colors, vector specifying color for each bin 
#' @param asp default is c(1,1), aspect ratio of graphic
#' @param pch default is 1, defines type of marker for each point
#' @param ... other params to send to plot
#' @return Draws a map
#' @seealso \code{\link{countypointmap}}
#' @export
pointmap <- function(bin, lat, lon, vartext='x', areatext='area', coloring=colors[1:length(unique(bin))], asp=c(1,1), pch=1, ...) {
  
  ############ 
  # Very basic, simple color-coded points map of area based on user providing bin, lat lon, labels:
  ############
  # or maybe default could be 
  # colorlist <- c('gray', 'yellow','orange', 'red')
  par(cex = 1)
  plot( lon, lat, col = coloring[bin],
        pch = '.',
        main = paste('Map of ', vartext, ' for ', areatext, sep = ''),
        xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ...
  )
  
  # pointmap( cut(x, breaks=4), lat, lon, varname, areaname, coloring = c('gray', 'yellow','orange', 'red'))
}
