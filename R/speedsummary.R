#' Print summary of speed in buffers per hour, given start and end times of batch
#'
#' @param start time when finished from Sys.time()
#' @param end time when finished from Sys.time()
#' @param n how many buffers were done? 
#'
#' @return Prints to console and returns rate of buffers per hour
#' @export
#'
#' @examples \dontrun{
#'  pts <- proxistat::testpoints_block2010(10)
#'  benchmark.start <- Sys.time()
#'  outlist <- proxistat::bufferapi(pts$lon, lat=pts$lat, radius = 5)
#'  benchmark.end <- Sys.time()
#'  proxistat::speedsummary(benchmark.start, benchmark.end, NROW(pts))
#'  # VISUALIZE variability in speed
#'  hist(as.numeric(unlist(lapply(outlist, FUN =function(x) x$timeSeconds)), na.rm = T),100, 
#'     main = 'Histogram of seconds per buffer', xlab='Seconds elapsed for a buffer query of API')
#'  cat('Note that results were unavailable for', sum(sapply(outlist, FUN = is.null)),'out of the', length(outlist), 'buffers.', '\n')
#' }
#' 
speedsummary <- function(start, end, n) {
  
  benchmark.start <- start; benchmark.end <- end 
  # REPORT THE SPEED 
  # based on benchmark.start <- Sys.time(); print('buffering done here'); benchmark.end <- Sys.time()
  total.benchmark <- difftime(benchmark.end, benchmark.start) # automatic units like minutes or seconds
  total.seconds <- difftime(benchmark.end, benchmark.start, units = 'secs') # in seconds
  perhour <- round((n / as.numeric(total.seconds))*3600,0)
  print(paste0(
    format(round((n / as.numeric(total.seconds))*3600,0), big.mark=',', scientific=FALSE), 
    ' buffers per hour (',
    format(n,big.mark = ',', scientific = FALSE),
    ' lat/long pairs per ',
    format(round(as.numeric(total.seconds),0), big.mark = ',', scientific = FALSE),
    ' seconds)'
  ))
  print(round(total.benchmark,1))
  ## cat('Note that results were unavailable for', sum(sapply(BUFFER_RESULTS_AS_LIST, FUN = is.null)),'out of the', n, 'buffers.', '\n')
  return(perhour)
  }
