#' @title Call proxistat3 once per chunk & save output as file (breaks large input data into chunks)
#' @description Call proxistat3 function in chunks, when list of frompoints is so long it taxes RAM (e.g. 11m blocks),
#'   saving each chunk as a separate .RData file in current working directory
#' @details   filesizes if crosstab format (FASTEST & avoid needing rownums which take >twice as long & 1.25x sized file):
#'   80MB file/chunk if   1k blocks x 11k topoints/chunk:  y=get.distances.chunked(testpoints(11e6), testpoints(11000), 1e3, units='km',return.crosstab=TRUE)
#'   800MB file/chunk if 10k blocks x 11k topoints/chunk:  y=get.distances.chunked(testpoints(11e6), testpoints(11000), 1e4, units='km',return.crosstab=TRUE)
#' @param frompoints Require matrix or data.frame of lat/lon vauels that can be passed to get.distances3 function (colnames 'lat' and 'lon')
#' @param topoints Require matrix or data.frame of lat/lon vauels that can be passed to get.distances3 function (colnames 'lat' and 'lon')
#' @param fromchunksize Required, number specifying how many points to analyze at a time (per chunk).
#' @param tochunksize (not currently required - current default is to use all topoints at once) number specifying how many points to analyze at a time (per chunk).
#' @param savechunks Optional logical defaults to TRUE. Specifies whether to save .RData file of each chunk
#' @param assemble Optional logical defaults to TRUE. Specifies whether to assemble all chunks into one variable called outputs, 
#'     which is saved as outputs.RData and returned by this function.
#' @param ... Other parameters to pass to \code{\link{get.distances3}}, such as units
#' @param folder Optional path specifying where to save .RData files, default is getwd()
#' @param FUN Optional function, \code{\link{proxistat3}} by default, and other values not implemented yet.
#' @return If assemble=TRUE, returns assembled set of all chunks as vector or matrix. 
#'   If assemble=FALSE, returns vector of character elements that are filenames for saved .RData output files in current working directory or specified folder.
#'   Each saved output is a vector of proximity scores if FUN=get.proxistat3, or matrix with extra columns depending on return. parameters above.
#' @export
proxistat.chunked <- function(frompoints, topoints, fromchunksize, tochunksize, FUN=proxistat3, area, folder=getwd(), savechunks=TRUE, assemble=TRUE, ...) {
  
  nfrom = length(frompoints[ , 1])
  nto   = length(  topoints[ , 1])
  
  #warning('chunking frompoints only, not topoints, in this version of this function')
  #if ( missing(tochunksize)) {stop('must specify  tochunksize')}
  if (  missing(tochunksize)) {tochunksize <- nto}
  if (missing(fromchunksize)) {stop('must specify fromchunksize')}
  if (fromchunksize > nfrom ) {
    warning('fromchunksize was more than frompoints size so doing all at once')
    fromchunksize <- nfrom
  }
  if (tochunksize > nto ) {
    warning('tochunksize was more than topoints size so doing all at once')
    tochunksize <- nto
  }
  
  fromchunks = ceiling(nfrom / fromchunksize) 
  tochunks =   ceiling(nto     / tochunksize) 
  
  fromchunklast =  nfrom %% fromchunksize; if (fromchunklast==0) {fromchunklast=fromchunksize}
  tochunklast = nto %% tochunksize; if (tochunklast==0) {tochunklast=tochunksize}
  
  # loop over topoint chunks would go here, but would need to keep track of rownums 
  # & still would have to assemble results of all topoints for a given frompoint to be able to find nearest one point, for example, to a given frompoint
  
  filenames <- paste('output', 1:fromchunks, '.RData', sep='')
  
  started <- Sys.time()
  
  for (fchunk in 1:fromchunks) {
    
    elapsedSec <- as.numeric(difftime(Sys.time() , started, units='secs'))
    elapsed <- round(difftime(Sys.time() , started), 1)
    frompointsDone <- fromchunksize * (fchunk - 1)
    frompointsLeft <- nfrom - frompointsDone
    frompointsPerSec <- frompointsDone / elapsedSec
    frompointsPerMinute <- 60 * frompointsPerSec
    remainingSec <- frompointsLeft / frompointsPerSec
    remaining <- round( difftime(Sys.time() + remainingSec, Sys.time() ), 1)
    cat(
      'Starting chunk', fchunk, '/', fromchunks, ',',
      frompointsDone, 'frompoints done in', elapsed, units(elapsed), '(', 
      round(frompointsPerMinute, 0), '/minute),',
      remaining, units(remaining), 'est. left to completion\n')
    
    if (fchunk == fromchunks) {
      # last chunk
      fromrow.start = 1 + (fchunk - 1) * fromchunklast
      fromrow.end = fromrow.start -1 + fromchunklast
    } else {
      fromrow.start = 1 + (fchunk - 1) * fromchunksize
      fromrow.end = fromrow.start -1 + fromchunksize
    }
    
    # later might also try to chunk the topoints:
    #output <- get.distances3(frompoints=frompoints[fromrow.start:fromrow.end, ], topoints=topoints[torow.start:torow.end, ], ...) 
    
    # ***  area needs to be chunked just like frompoints or topoints, when proxistat is called:
    output <- proxistat3(frompoints=frompoints[fromrow.start:fromrow.end, ], topoints=topoints, 
     area=area[fromrow.start:fromrow.end], ...)

    if (savechunks) {
      save(output, file=file.path(folder, filenames[fchunk]))
    }
    
    if (assemble) {
      if (fchunk==1) {
        outputs <- output
      } else {
        outputs <- rbind( outputs, output )  
      }
    }
    rm(output)
  }
    
  print(round( difftime(Sys.time(), started), 1))
  
  if (assemble) {
    save(outputs, file=file.path(folder, 'outputs.RData'))
    return(outputs)
  } else {
    return(filenames)
  }
}
