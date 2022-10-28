#' approx lat lon of each US County by FIPS - 2020
#' A dataset containing latitude and longitude approx of each US county,
#'  created as average lat and average lon of all block group internal points.
#' @import choroplethrMaps
#' @details 
#'   \preformatted{
#'  Creating county.pts from bg.pts to get lat lon of middle roughly of county from block group lat lon dataset: 
#'  
#' countylat <- aggregate(bg.pts$lat, by = list(substr(bg.pts$FIPS, 1, 5)), FUN = mean) 
#' countylon <- aggregate(bg.pts$lon, by = list(substr(bg.pts$FIPS, 1, 5)), FUN = mean) 
#' countyfips <- countylat$Group.1 
#' countylat  <- countylat$x 
#' countylon  <- countylon$x 
#' county.pts <- data.frame(FIPS.COUNTY = countyfips, lat = countylat, lon = countylon, stringsAsFactors = FALSE) 
#' 
#'  plot(bg.pts$lon, bg.pts$lat, pch = '.', col='gray', xlim = c(-170,-60), ylim = c(18, 75)) 
#'  points(county.pts$lon, county.pts$lat, pch = '.', col='red')
#'  text(lookup.states$lon, lookup.states$lat, labels = lookup.states$ST, cex = 0.6)
#'  
#'  Also see https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2020_Gazetteer/2020_Gaz_counties_national.zip
#'   }
#' @format A data frame with one row per county and FIPS.COUNTY, lat, lon as colnames
#'
"county.pts"

