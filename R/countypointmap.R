#' @title Simple map of color-coded block group centroids in county
#' 
#' @description very simple mapping
#' @param query See \code{\link[ejanalysis]{get.county.info}}
#' @return Just draws a map
#' @seealso \code{\link{pointmap}}
#' @export
countypointmap <- function(query, vartext, varname, breaks=5, coloring, asp=c(1,1), pch=1, ......) {
  ###########
  # SIMPLE MAP OF BLOCK GROUP CENTROIDS IN COUNTY(IES)  - color-coded block group points
  ##########
  # given county query and name of field, and breaks cutoffs?, look in bg and bg.pts to map that county as bg points colored by bin
  # query Vector of search terms. Can be county's 5-digit FIPS code(s) (as numbers or strings with numbers), and also could be 'countyname, statename' (fullname, exactly matching formats in countiesall$fullname, but case insensitive).
  if (!exists('bg.pts') | !exists('bg')) {stop('requires bg and bg.pts data.frames in memory, with varname col and FIPS and FIPS.COUNTY in bg, FIPS and lat and lon in bg.pts which is in proxistat pkg')}
  x <- bg[ , 'varname']
  if (missing(x)) {stop('missing variable to map')}
  
  library(ejanalysis)  # or can use countiesall in proxistat pkg instead of get.county.info from ejanalysis pkg?
  
  if (missing(coloring)) { 
    if (length(breaks) == 1) {
      coloring <- colors[1:breaks]
    } else {
      coloring <- colors[1:(length(breaks) + 1)] 
    }
  }
  breaks <- cut(x, breaks)
  counties <- get.county.info(query)
  #allbgfips <- bg$FIPS   # substr(bg.pts$FIPS, 1,5)
  mybgfips <- bg$FIPS[ bg$FIPS.COUNTY %in% counties$FIPS.COUNTY ] # rewrite to use match or merge
  bgnum <- match(mybgfips, bg.pts$FIPS)
  mylat <- bg.pts$lat[ bgnum]
  mylat <- bg.pts$lat[ bgnum]
  pointmap(bin = breaks, lat = bg.pts$lat[bgnum], lon = bg.pts$lon[bgnum], 
           coloring = coloring, vartext = vartext, asp = asp, pch = pch, ...)
}

