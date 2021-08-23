#' Get lat lon for random US census blocks (where avg person lives or other weighting)
#'
#'  Note this is not optimized for speed- it can take 30 seconds to pick just 1,000 points if pop wtd. 
#'  May recode this to allow wts to be pop (using blocks.pop), or area (using blocks.area), or none (no weight).
#' @param size number of unique test points needed (no duplicates will be returned)
#' @param popwtd default is TRUE, and pop weighted block points represent where the avg US resident lives.
#'   If FALSE, unweighted, so each block has equal chance of being selected. 
#' @return data.frame with columns lon and lat, in decimal degrees
#' @export
#'
#' @examples 
#' #library(jsonlite)
#' #library(httr)
#' #library(tidyverse) # need magrittr
#' #library(data.table)
#' #library(sf)
#' # pts <- proxistat::testpoints_block2010(10)
#' # benchmark.start <- Sys.time()
#' # outlist <- proxistat::bufferapi(pts$lon, lat=pts$lat, radius = 5)
#' # benchmark.end <- Sys.time()
#' # proxistat::speedsummary(benchmark.start, benchmark.end, NROW(pts))
#'  # VISUALIZE variability in speed
#'  # hist(as.numeric(unlist(lapply(outlist, FUN =function(x) x$timeSeconds)), na.rm = T),100, 
#'  #   main = 'Histogram of seconds per buffer', xlab='Seconds elapsed for a buffer query of API')
#'  
testpoints_block2010 <- function(size, popwtd = TRUE) {
  if (popwtd) {
    randomrows <- sample(1:length(UScensus2010blocks::blocks.pop), size = size, replace = FALSE, prob = UScensus2010blocks::blocks.pop)
  } else {
    randomrows <- sample(1:length(UScensus2010blocks::blocks.fips), size = size, replace = FALSE)
  }
  pts <- data.frame(lon = UScensus2010blocks::blocks.lon[randomrows], lat = UScensus2010blocks::blocks.lat[randomrows])
  return(pts)
  #testpoints_x(x=UScensus2010blocks::blocks.lon[ , c('lon', 'lat')], size = size, prob = ejscreen::bg20$pop)
}
