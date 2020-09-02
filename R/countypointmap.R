#' @title Simple map of color-coded counties, wrapper for county_choropleth
#' 
#' @description very simple mapping of color coded map of US counties.
#'   This is just a wrapper for \code{\link[choroplethr]{county_choropleth}} 
#'   but it is much more flexible in how the counties are specified.
#'   While \code{\link[choroplethr]{county_choropleth}} requires statecounty fips numbers, 
#'   and zooms in on a state defined by 2-digit fips, 
#'   this wrapper lets you specify counties by full name like Cook County IL
#'   or even one or more states 
#'   
#'   Also note acs package is useful such as for
#'   county_choropleth_acs('B01001', endyear = 2018, state_zoom = 'new york')
#' @param x required vector of numeric values to plot
#' @param query flexible way to define what locations - default is all US counties - a set of countyfips  - See \code{\link[ejanalysis]{get.county.info}}
#' @param state_zoom optional but more flexible than in county_choropleth, which requires it be 
#'   the full state name all lowercase. This function instead accepts case-insensitive state name
#'   or two letter state abbreviation or state fips as number or text.
#' @param title required, used in \code{\link[choroplethr]{county_choropleth}} 
#' @param num_colors Default is 1 (unlike default in county_choroplethr) for graded colors to show outliers well, passed to \code{\link[choroplethr]{county_choropleth}} . Max allowed is 9.
#' @param ... Additional parameters like state_zoom (but in ), to pass to \code{\link[choroplethr]{county_choropleth}} 
#' @return Just draws a map
#' @seealso \code{\link[choroplethr]{county_choropleth}} \code{\link{pointmap}} 
#' @export
countypointmap <- function(x, query=NULL, state_zoom, title='', num_colors=1, ...) {

  if (!missing(state_zoom)) {
    state_zoom <- tolower(ejanalysis::get.state.info(state_zoom)$statename)
  }    
  if (is.null(query)) {
    region <- ejanalysis::get.county.info()[,'FIPS.COUNTY']
    if (!missing(x)) stop('it does not make sense to specify x but not specify query of which counties - omit both to see default map of uniform color')
    # ALL COUNTIES
  } else {
    region <- ejanalysis::get.county.info(query = query)[ , 'FIPS.COUNTY']
    # however, this expands to all counties in each whole state specified, 
    # so x has to be expanded accordingly!
    if (length(x) == length(region)) {
      # ok since one data value per region
    } else {
      # need merged results
      
      # to be done.... *******8
      
      
    }
  }
  if (missing(x)) x <- rep(1, times = length(region)) # just uniform color as default
  # this should recycle x? not sure why you would want to do that, however 
  mydf <- data.frame(region = as.numeric(region), value = as.numeric(x), stringsAsFactors = FALSE)
  
  #require(choroplethr)
  return(choroplethr::county_choropleth(mydf, num_colors = num_colors, state_zoom = state_zoom, title = title, ...))

# older approach:
  
  # mylat <- county.pts$lat[match(bg$FIPS.COUNTY, county.pts$FIPS.COUNTY)]
  # mylon <- county.pts$lat[match(bg$FIPS.COUNTY, county.pts$FIPS.COUNTY)]
  # 
  # if (missing(coloring)) { 
  #   if (length(breaks) == 1) {
  #     coloring <- colors()[1+(1:breaks)]
  #   } else {
  #     coloring <- colors()[1+(1:(length(breaks) + 1))] 
  #   }
  # }
  
  # could use this to get labels for legend i.e. cutpoints of bins, if labels = TRUE
  # bins <- cut(x, breaks, labels = FALSE) # assigns integer to each bin
  
  #library(ejanalysis)  # or can use countiesall in proxistat pkg instead of get.county.info from ejanalysis pkg?
  # mycounties <- ejanalysis::get.county.info(query)
  #allbgfips <- bg$FIPS   # substr(bg.pts$FIPS, 1,5)
  # bg.countyfips <- substr(bg.pts$FIPS, 1, 5)
  # used to just use lat lon of first matching block group in the county. 
  # mylat <- bg.pts$lat[match(mycounties, bg.countyfips)]
  # mylon <- bg.pts$lon[match(mycounties, bg.countyfips)]
  # mybgfips <- bg$FIPS[ bg$FIPS.COUNTY %in% counties$FIPS.COUNTY ] # rewrite to use match or merge
  # bgnum <- match(mybgfips, bg.pts$FIPS)
  # mylat <- bg.pts$lat[ bgnum]
  # mylat <- bg.pts$lat[ bgnum]
  
  # pointmap(bin = bins, lat = mylat, lon = mylon,
  #          coloring = coloring, vartext = vartext, asp = asp, pch = pch, ...)
}

