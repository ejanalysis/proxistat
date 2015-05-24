#' @title Convert Census Block Proximity Statistics to Block Group Statistics
#' 
#' @description Aggregate proximity statistics already calculated for each Census block, up to one summary for each Census block group. 
#'   The resulting proximity score, distance to nearest single point, or count of nearby points is just the 
#'   population-weighted mean of values in the blocks within a given block group.
#' @details The population-weighted mean might not be the only statistic of interest. \cr
#'   To get the maximum count of sites near any single block in the block group, try         aggregate(output[ , 'count.near'],   by=list(blocks$FIPS.BG), FUN=max). \cr
#'   To get the shortest distance from any block in the block group to the nearest site, try aggregate(output[ , 'nearestone.d'], by=list(blocks$FIPS.BG), FUN=min). \cr
#'   To find out how many unique sites are within X km of the internal point of any block in the block group, for example, 
#'   is harder, because it requires retaining details on which sites were near a given block, i.e., much more data would be the input to an aggregating function. 
#' @param output Required matrix of results from \code{\link{proxistat}} or \code{\link{proxistat.chunked}}. Must be same number of rows and order as blocksfips.
#'   Output parameter must be output of proxistat or proxistat.chunked and contain the colname scores, and can also have colnames nearestone.d and/or count.near.
#' @param blocksfips Required character vector of 15-digit Census block FIPS codes (not numeric, must have leading zeroes as needed).
#' @param blocksfipsbg Required character vector of 12-digit Census block group FIPS codes (not numeric, must have leading zeroes as needed). Same length and order as blocksfips.
#' @param blockspop Required numeric vector of population counts in Census blocks. Same length and order as blocksfips.
#' @return Returns a data.frame with FIPS.BG and same fields proxistat can provide (depending on what is in the parameter called output): scores, nearestone.d, count.near, but with one row for each of the block groups defined by FIPS.BG. 
#'   Units (miles or km) are unchanged from those used to create input parameters.
#' @seealso \code{\link{proxistat}} and \code{\link{proxistat.chunked}} to create proximity statistics, and see \code{\link{get.distances}} and \code{\link{get.distances.all}} for distances between points, and
#'   \code{\link{get.nearest}} which finds the distance to the single nearest point
#'   within a specified search radius instead of all topoints.
#' @concept proximity
#' @examples
#' \donotrun{
#' require(UScensus2010blocks); require(Hmisc); require(data.table); require(analyze.stuff); require(ejanalysis)
#' blocks=get.blocks()
#' bgp <- proxistat.rollup(output=output, blocksfips=blocks$fips, blocksfipsbg=blocks$FIPS.BG, blockspop=blocks$pop)
#' }
#' @export
proxistat.rollup <- function(output, blocksfips, blocksfipsbg, blockspop) {
  
  if (any(missing(output), missing(blocksfips), missing(blocksfipsbg), missing(blockspop))) {stop('must specify output, blocksfips, blocksfipsbg, blockspop')}
  #if (!all( c('scores', 'nearestone.d', 'count.near') %in% colnames(output) ) ) {stop('output parameter must be output of proxistat and contain colnames scores, nearestone.d, count.near')}
  if (!('scores' %in% colnames(output) )) {stop('output parameter must be output of proxistat or proxistat.chunked and contain a column named scores')}
  
  output$FIPS <- blocksfips
  output$FIPS.BG <- blocksfipsbg
  output$pop <- blockspop
  rm(blocksfips, blocksfipsbg, blockspop)
  
  output=data.table::data.table(output, key='FIPS.BG')
  bgp = output[ , .(scores=Hmisc::wtd.mean(scores, pop)), by=FIPS.BG] 
  if ('nearestone.d' %in% colnames(output)) {
    bgp$nearestone.d <- output[ , .(nearestone.d=wtd.mean(nearestone.d, pop)), by=FIPS.BG][ , nearestone.d]
  }
  if ('count.near' %in% colnames(output)) {
    bgp$count.near   <- output[ , .(  count.near=wtd.mean(nearestone.d, pop)), by=FIPS.BG][ , count.near]
  }
  bgp = data.frame( bgp, stringsAsFactors = FALSE)
  
  return(bgp)
}
