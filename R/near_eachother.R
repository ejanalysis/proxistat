near_eachother <- function(lat, lon, distance=1) {
  # another way to do a quick check for overlapping circular buffers
  # i.e., are any of these points near each other?
  
  
# distance in miles converted to decimal degrees
meters_per_mile <- 1609.34
radius_meters <- distance * meters_per_mile
radius_in_lat_units <- proxistat::meters.per.degree.lat(radius_meters)

# see if any 2 have similar latitudes
latorder <- order(lat)
latgaps <- diff(lat[latorder])
longaps <- diff(lon[latorder])
nearby_nextone_latorder <- (latgaps <= radius_in_lat_units) & (longaps <= radius_in_lat_units)
# which(nearby_nextone_latorder)

results <- data.frame(lat,lon,orignum=1:NROW(lat))[order(latorder), ]
results$near1 <- c(nearby_nextone_latorder,0) * results$orignum
results$near2[1:(NROW(results)-1)] <- results$orignum[2:NROW(results)]




stop('not done')



# near1 = c( * nearby_nextone_latorder, 0),
near2 = 0  

results[order(results[,'orignum']), ]

}
