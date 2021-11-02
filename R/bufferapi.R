#' Use the EJSCREEN API to get standard report for each of one or more circular buffers
#'
#' @param lon Longitude numeric vector
#' @param lat Latitude numeric vector
#' @param radius radius of circular buffer - I think in miles but should confirm
#'
#' @export
#'
#' @examples  
#'   \dontrun{
#'   sample.count <- 10
#'   pts <- proxistat::testpoints_block2010(sample.count)
#'   outlist <- proxistat::bufferapi(pts$lon, lat=pts$lat, radius = 3)
#'   summary(outlist)
#'   class(outlist[[1]])
#'   names(outlist[[1]])
#'   x <- do.call(rbind, outlist)
#'   x[,c(1:18,162:173)]
#'   t(x[1:2,]) 
#'   }
bufferapi <- function(lon, lat, radius=5) {
  
  benchmark.start <- Sys.time()
  cat("\n", "Buffering for ", length(lon), ' points for radius of ', radius, '\n')
  pts <- data.frame(lon=lon, lat=lat)
  outlist <- vector(mode = 'list', length = dim(pts)[1])
  
  for (i in 1:dim(pts)[1]){
    print(paste0('Iteration #: ', i))
    
    # Not entirely sure this GET() is the one from the httr package, so I guessed
    ej.data <- httr::GET(
      paste0('https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&',
             'geometry={"spatialReference":{"wkid":4326},"x":',
             pts$lon[[i]], ',"y":', pts$lat[[i]],
             '}&distance=', radius,'&unit=9035&areatype=&areaid=&f=pjson')
      #    '}&distance=5&unit=9035&areatype=&areaid=&f=pjson')
    )$content
    
    #
    ### Not sure which package this fromJSON() was intended to be from...  (and not sure they are identical, ie if it matters)
    # That same function name is used by RJSONIO and  jsonlite  and  rjson
    # This use of fromJSON was I think from an example provided by Victoria S maybe
    #
    ej.data <- data.table::as.data.table(jsonlite::fromJSON(rawToChar(ej.data)))
    
    # Not clear why this:
    # Only return data from calls without errors
    # Note: api does not return values for coords in highly nonpopulated areas.
    if (dim(ej.data)[2] > 100){
      # outlist[[i]] <- unique(ej.data[, geometry := NULL]) # not sure why this line stopped working. was it relying on data.table or some other package? for the := maybe?
      outlist[[i]] <- unique(ej.data[, -166])
    }
    # rm(ej.data)
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
