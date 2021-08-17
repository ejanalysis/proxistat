#' Use EJSCREEN API to get buffer results (conditions near each site)
#'
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param radius single number, for all buffer circles (distance from site) (meters?)
#'
#' @return list of data.frames, one per buffer (one per site defined by a lat lon pair)
#' @export
#'
#' @examples 
#' # pts <- proxistat::testpoints_bg20(10)
#' # benchmark.start <- Sys.time()
#' # outlist <- proxistat::bufferapi(pts$lon, lat=pts$lat, radius = 5)
#' # benchmark.end <- Sys.time()
#' # proxistat::speedsummary(benchmark.start, benchmark.end, NROW(pts))
#' 
#'  # VISUALIZE variability in speed
#'  # hist(as.numeric(unlist(lapply(outlist, FUN =function(x) x$timeSeconds)), na.rm = T),100, 
#'  #   main = 'Histogram of seconds per buffer', xlab='Seconds elapsed for a buffer query of API')
#'     
#'  # api.datalist <- data.table::rbindlist(outlist, fill = T, idcol = 'id')
#'  
bufferapi <- function(lon, lat, radius=5) {
  
  benchmark.start <- Sys.time()
  cat("\n", "Buffering for ", length(lon), ' points for radius of ', radius, '\n')
  pts <- data.frame(lon=lon, lat=lat)
  outlist <- vector(mode = 'list', length = dim(pts)[1])
  
  for (i in 1:dim(pts)[1]){
    print(paste0('Iteration #: ', i))
    
    ej.data <- GET(
      paste0('https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&',
             'geometry={"spatialReference":{"wkid":4326},"x":',
             pts$lon[[i]], ',"y":', pts$lat[[i]],
             '}&distance=', radius,'&unit=9035&areatype=&areaid=&f=pjson')
      #'}&distance=5&unit=9035&areatype=&areaid=&f=pjson')
    )$content %>%
      rawToChar() %>%
      fromJSON() %>% 
      as.data.table()
    
    # Not clear why this: 
    # Only return data from calls without errors
    # Note: api doesn't return values for coords in highly nonpopulated areas.
    if (dim(ej.data)[2] > 100){
      outlist[[i]] <- unique(ej.data[, geometry := NULL])
    }
    #rm(ej.data)
  } # end of loop over buffers
  
  benchmark.end <- Sys.time()
  # report time elapsed and avg speed
  total.benchmark <- difftime(benchmark.end, benchmark.start)
  total.seconds <- difftime(benchmark.end, benchmark.start, units = 'secs')
  perhour <- round((dim(pts)[1] / as.numeric(total.seconds))*3600,0)
  print(paste0(
    format(round((dim(pts)[1] / as.numeric(total.seconds))*3600,0), big.mark=',', scientific=FALSE), 
    ' buffers per hour (',
    format(dim(pts)[1],big.mark = ',', scientific = FALSE),
    ' lat/long pairs per ',
    format(round(as.numeric(total.seconds),0), big.mark = ',', scientific = FALSE),
    ' seconds)'
  )  )
  print(round(total.benchmark,1))
  cat('Note that results were unavailable for', sum(sapply(outlist, FUN = is.null)),'out of the', length(outlist), 'buffers.', '\n')
  #print(table(sapply(outlist, FUN = is.null)))
  
  return(outlist)
}

