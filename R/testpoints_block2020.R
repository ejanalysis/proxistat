#' Get lat lon pop and area for random US census blocks (where avg person lives or other weighting)
#' @param size number of unique test points needed (no duplicates will be returned)
#' @param weighting is "people" for average resident; or "block" for average census block,
#'   or "area" for average square meter 
#'   If FALSE, unweighted, so each block has equal chance of being selected. 
#' @return data.frame with columns lon and lat, in decimal degrees, and area and pop count
#' @export
#'  
testpoints_block2020 <- function(size, weighting = 'area') {
  if (weighting == 'people') {
    randomrows <- sample(1:NROW(proxistat::blockpoints_area_pop), size = size, replace = FALSE, 
                         prob = proxistat::blockpoints_area_pop$pop)
  } 
  if (weighting == 'area') {
    randomrows <- sample(1:NROW(proxistat::blockpoints_area_pop), size = size, replace = FALSE, 
                         prob = proxistat::blockpoints_area_pop$area)
  }
  if (weighting == 'block') {
    randomrows <- sample(1:NROW(proxistat::blockpoints_area_pop), size = size, replace = FALSE)
  }
  
  pts <- data.table::setDF(proxistat::blockpoints_area_pop[randomrows, .(blockid, lon, lat, area, pop)])
  return(pts)
  
}

