#' @title Read a series of dbf files and join compile them as a single data.frame
#' 
#' @description
#'   Can help with a series of downloaded Census data files, such as one file per State. 
#'   Reads each using \code{\link[foreign]{read.dbf}} and combines them all as a data.frame.
#'   They must all have the same columns, and each file just provides additional rows of data.
#' @param bin Indicates color of each point. A vector of integers used to index the coloring vector. That would ideally include just 1:n where n is the n number of unique values and the index to the coloring vector, so each unique value 1:5, for example, is assigned a map color that is coloring[bin]
#' @param lat vector of latitudes just interpreted as y values to plot
#' @param lon vector of longitudes just interpreted as x values to plot
#' @param vartext default is 'x', text to use in describing the field mapped
#' @param areatext default is 'area', text to use in describing the area mapped
#' @param coloring default is some basic colors from rainbow(). A vector specifying color for each bin (should be same length as number of unique bin values)
#' @param asp default is c(1,1), aspect ratio of graphic (not right for AK, e.g.)
#' @param pch default is '.', see \code{\link{par}} -- defines type of marker for each point
#' @param ... other params to send to plot
#' @return Draws a map
#' @seealso \code{\link{countypointmap}}
#' @examples  
#'   myfips <- bg.pts$FIPS[substr(bg.pts$FIPS,1,2)=='06'] # CA
#'   pointmap(bin = floor(runif(n = length(myfips),1,5.99)), lat = bg.pts$lat[match(myfips, bg.pts$FIPS)], lon = bg.pts$lon[match(myfips, bg.pts$FIPS)], vartext = 'Example', areatext = 'all of CA',coloring = c('gray', 'light blue', 'dark blue', 'orange', 'red') , pch=1, cex=0.3)
#' @export
pointmap <- function(bin, lat, lon, vartext='x', areatext='area', coloring=rainbow(length(unique(bin))), asp=c(1,1), pch='.', ...) {
  
  ############ 
  # Very basic, simple color-coded points map of area based on user providing bin, lat lon, labels:
  ########### #
  # or maybe default could be 
  #  <- c('gray', 'yellow','orange', 'red')
  if (length(coloring) != length(unique(bin))) {warning('Number of colors is not equal to number of unique values of bin parameter')}
  
  #par(cex = 1)
  #par(cex = 2)
  plot( lon, lat, col = coloring[bin],
        pch = pch, asp=asp, 
        main = paste('Map of ', vartext, ' for ', areatext, sep = ''),
        xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ...
  )
  
  # pointmap( cut(x, breaks=4), lat, lon, varname, areaname, coloring = c('gray', 'yellow','orange', 'red'))
}
