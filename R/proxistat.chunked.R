#' @title Call proxistat once per chunk & save output as file (breaks large input data into chunks)
#' @description Call proxistat function in chunks, when list of frompoints is so long it taxes RAM (e.g. 11m blocks),
#'   saving each chunk as a separate .RData file in current working directory
#' @details   
#'   *** Still slow for all blocks in USA & 10k topoints (several hours)
#'   Filesizes: \cr\cr
#'   80MB file/chunk if   1k blocks x 11k topoints/chunk:  y=get.distances.chunked(testpoints(11e6), testpoints(11000), 1e3, units='km') \cr\cr
#'   800MB file/chunk if 10k blocks x 11k topoints/chunk:  y=get.distances.chunked(testpoints(11e6), testpoints(11000), 1e4, units='km')
#' @param frompoints Require matrix or data.frame of lat/lon vauels that can be passed to get.distances function (colnames 'lat' and 'lon')
#' @param topoints Require matrix or data.frame of lat/lon vauels that can be passed to get.distances function (colnames 'lat' and 'lon')
#' @param fromchunksize Required, number specifying how many points to analyze at a time (per chunk).
#' @param tochunksize (not currently required - current default is to use all topoints at once) number specifying how many points to analyze at a time (per chunk).
#' @param savechunks Optional logical defaults to FALSE. Specifies whether to save .RData file of each chunk
#' @param saveproxistats Optional logical defaults to FALSE. Specifies whether to save .RData file of assembled results as proxistats matrix. Ignored if assemble=FALSE.
#' @param startchunk Optional integer defaults to 1. Specifies which chunk to start with, in case some already have been done. 
#'   Currently, still must pass entire dataset to this function even if some of the earlier chunks have already been analyzed.
#' @param assemble Optional logical defaults to TRUE. Specifies whether to assemble all chunks into one variable called proxistats, 
#'     which is saved as \code{file} in \code{folder} and returned by this function.
#' @param folder Optional path specifying where to save .RData file(s) -- chunk-specific files and/or assembled results file -- default is getwd()
#' @param file Optional name of file created if assemble=TRUE and saveproxistats=TRUE, defaults to proxistats.RData using save(proxistats, 'proxistats.RData')
#' @param FUN Optional function, \code{\link{proxistat}} by default, and other values not implemented yet.
#' @param ... Other parameters to pass to \code{\link{proxistat}} such as \code{area} and \code{units}
#' @return If assemble=TRUE, returns assembled set of all chunks as matrix of 1 or more columns.
#'   If assemble=FALSE but savechunks=TRUE, returns vector of character elements that are filenames for saved .RData output files in current working directory or specified folder.
#'   Each saved output is a vector of proximity scores if FUN=proxistat, or matrix with extra columns depending on return. parameters above.
#'   Otherwise, returns NULL.
#' @export
proxistat.chunked <- function(frompoints, topoints, fromchunksize, tochunksize, startchunk=1, FUN=proxistat, folder=getwd(), savechunks=FALSE, assemble=TRUE, saveproxistats=FALSE, file='proxistats.RData', ...) {
  
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
  
  
  filenames <- paste('output', 1:fromchunks, '.RData', sep='') # but not all will get used if startchunk>1
  
  started <- Sys.time()
  
  for (fchunk in startchunk:fromchunks) {
    
    # loop over topoint chunks would go here??, but would need to keep track of rownums and 
    #   still would have to assemble results of all topoints for a given frompoint to be able to find nearest one point, for example, to a given frompoint

    elapsedSec <- as.numeric(difftime(Sys.time() , started, units='secs'))
    elapsed <- round(difftime(Sys.time() , started), 1)
    frompointsDone <- fromchunksize * (fchunk - startchunk) # done in this set from startchunk onwards, not cumulative ever from chunk 1 onwards
    frompointsDonePreviously <- fromchunksize * (startchunk - 1) # done before this set, i.e., prior to startchunk
    frompointsLeft <- nfrom - (frompointsDone + frompointsDonePreviously)
    frompointsPerSec <- frompointsDone / elapsedSec
    frompointsPerMinute <- 60 * frompointsPerSec
    remainingSec <- frompointsLeft / frompointsPerSec
    remaining <- round( difftime(Sys.time() + remainingSec, Sys.time() ), 1)
    cat(
      format(Sys.time()), 
      'Started chunk', fchunk, '/', fromchunks, ',',
      frompointsDone, 'frompoints x',nto,'=',format(frompointsDone*nto, big.mark = ',', scientific=FALSE),'done in', elapsed, units(elapsed), '(', 
      round(frompointsPerMinute, 0), '/minute, or', round(nto*frompointsPerMinute/11.1e6, 0),'topoints/minute from 11m blocks),',
      remaining, units(remaining), 'left\n')
    
    if (fchunk == fromchunks) {
      # last chunk
      fromrow.start = 1 + (fchunk - 1) * fromchunksize
      fromrow.end = fromrow.start -1 + fromchunklast
    } else {
      fromrow.start = 1 + (fchunk - 1) * fromchunksize
      fromrow.end = fromrow.start -1 + fromchunksize
    }
    
    # later might also try to chunk the topoints:
    #output <- get.distances(frompoints=frompoints[fromrow.start:fromrow.end, ], topoints=topoints[torow.start:torow.end, ], ...) 
    
    # ***  area needs to be chunked just like frompoints or topoints, when proxistat is called:
    if (!missing(area)) {
      output <- proxistat(frompoints=frompoints[fromrow.start:fromrow.end, ], topoints=topoints, 
                          area=area[fromrow.start:fromrow.end], ...)
    } else {
      output <- proxistat(frompoints=frompoints[fromrow.start:fromrow.end, ], topoints=topoints, ...)
    }
    
    if (savechunks) {
      if (!file.exists(folder)) { dir.create(folder); warning('specified folder not found so it was created') }
      save(output, file=file.path(folder, filenames[fchunk]))
    }
    
    if (assemble) {
      if (fchunk==startchunk) {
        proxistats <- output
      } else {
        proxistats <- rbind( proxistats, output )
      }
    }
    rm(output)
  }
  
  print(round( difftime(Sys.time(), started), 1))

  if (assemble) {
    if (saveproxistats) {
      save(proxistats, file=file.path(folder, file))
    }
    return(proxistats)
  } else {
    if (savechunks) {
      return(filenames)
    } else {
      return(NULL)
    }
  }
}
