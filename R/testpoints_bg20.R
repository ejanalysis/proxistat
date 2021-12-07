#' Get lat lon for random US census block groups (where avg person lives or other weighting)
#'
#' @details 
#'  Note this is not optimized for speed. can be slow if pop wtd. 
#'  May recode this to allow wts to be pop, area, or none (no weight).
#'  Area weighted would mean a random location means each square meter is equally likely to be picked, 
#'  like throwing a dart at a map except it is rounded to the nearest internal point. 
#'  Since block groups generally almost all tend to have somewhat similar pop counts,
#'  popwtd and bg fips wtd are not all that different. But square meters varies a lot, 
#'  so area weighted is very different than pop or fips weighted.
#' @param size number of unique test points needed (no duplicates will be returned)
#' @param popwtd default is TRUE, and pop weighted points represent block group where the avg US resident lives.
#'   If FALSE, unweighted, so each block group has equal chance of being selected. 
#'   
#' @return data.frame with columns lon and lat, in decimal degrees
#' @export
#'
#' @examples 
#' #library(jsonlite)
#' #library(httr)
#' #library(tidyverse) # need magrittr
#' #library(data.table)
#' #library(sf)
#' # pts <- proxistat::testpoints_bg20(10)
#' # benchmark.start <- Sys.time()
#' # outlist <- batch.summarizer::ejscreenapi(pts$lon, lat=pts$lat, radius = 5)
#' # benchmark.end <- Sys.time()
#' # proxistat::speedsummary(benchmark.start, benchmark.end, NROW(pts))
#'  
testpoints_bg20 <- function(size, popwtd = TRUE) {

  if (popwtd) {
    randomrows <- sample(1:length(ejscreen::bg20$pop), size = size, replace = FALSE, prob = ejscreen::bg20$pop)
    #testpoints_x(x=ejscreen::bg20[ , c('lon', 'lat')], size = size, prob = ejscreen::bg20$pop)
  } else {
    randomrows <- sample(1:length(ejscreen::bg20$pop), size = size, replace = FALSE)
    #testpoints_x(x=ejscreen::bg20[ , c('lon', 'lat')], size = size, prob = NULL)
  }
  pts <- data.frame(lon = ejscreen::bg20$lon[randomrows], lat= ejscreen::bg20$lat[randomrows])
  return(pts)
}

testpoints_bg21 <- function(size, popwtd = TRUE) {
  if (popwtd) {
    randomrows <- sample(1:length(ejscreen::bg21$pop), size = size, replace = FALSE, prob = ejscreen::bg21$pop)
    #testpoints_x(x=ejscreen::bg21[ , c('lon', 'lat')], size = size, prob = ejscreen::bg21$pop)
  } else {
    randomrows <- sample(1:length(bg21$pop), size = size, replace = FALSE)
    #testpoints_x(x=ejscreen::bg21[ , c('lon', 'lat')], size = size, prob = NULL)
  }
  pts <- data.frame(lon = ejscreen::bg21$lon[randomrows], lat= ejscreen::bg21$lat[randomrows])
  return(pts)
}

testpoints_x <- function(x, size, prob = NULL) {
  randomrows <- sample(1:NROW(x), size = size, replace = FALSE, prob = prob)
  pts <- x[randomrows, c('lon', 'lat')]
  return(pts)
}
