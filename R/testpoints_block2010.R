#' Get lat lon for random US census blocks (where avg person lives or other weighting)
#'
#'  Note the 2010 version of this is not optimized for speed- it can take 30 seconds to pick just 1,000 points if pop wtd. 
#'  May recode this to allow wts to be pop (using blocks.pop), or area (using blocks.area), or none (no weight).
#' @details This is probably obsolete at this point. 
#' @param size number of unique test points needed (no duplicates will be returned)
#' @param popwtd default is TRUE, and pop weighted block points represent where the avg US resident lives.
#'   If FALSE, unweighted, so each block has equal chance of being selected. 
#' @return data.frame with columns lon and lat, in decimal degrees
#' @export
#'
#'  
testpoints_block2010 <- function(size, popwtd = TRUE) {
  if (popwtd) {
    randomrows <- sample(1:length(UScensus2010blocks::blocks.pop), size = size, replace = FALSE, prob = UScensus2010blocks::blocks.pop)
  } else {
    randomrows <- sample(1:length(UScensus2010blocks::blocks.fips), size = size, replace = FALSE)
  }
  pts <- data.frame(lon = UScensus2010blocks::blocks.lon[randomrows], lat = UScensus2010blocks::blocks.lat[randomrows])
  return(pts)
  #testpoints_x(x=UScensus2010blocks::blocks.lon[ , c('lon', 'lat')], size = size, prob = ejscreen::bg22$pop)
}



