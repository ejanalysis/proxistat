#' @title Get block group internal points directly from Census shapefiles
#' @description Download, unzip, read, and assemble lat lon of internal points for US Census block groups.
#' @details Note this is obsolete if used to create data() containing the results:
#'   bg.pts = get.bg.latlons(mytempdir = getwd(), overwrite=FALSE, silent = TRUE)
#'   save(bg.pts, file='bg.pts') # and then this can be saved in the data folder of a package, then build pkg, then access via data('bg.pts')
#' @param myyear Year such as 2014 (default)
#' @param mytempdir Optional. Default is TIGERTEMP created inside the current working directory. Character string of path to where temporary directory is or will be, for downloaded zip and dbf files.
#' @param mystatenums Optional, default is all including PR/VI/DC, etc. Vector of strings of 2-character FIPS codes for states to get data for.
#' @param overwrite Optional, FALSE by default (unlike in unzip), which means
#'  if zip file exists locally do not download and if contents exist do not unzip.
#'  BUT note this does not yet check to see if contents exist and zip does not, in which case could avoid downloading.
#' @param silent Optional logical FALSE by default. If TRUE, print more results.
#' @examples 
#' #get.bg.latlons(mystatenums=c('09','10'))
#' @export
get.bg.latlons <- function( myyear=2014, mytempdir, mystatenums, overwrite=FALSE, silent=FALSE) {
  
  if (missing(mytempdir)) {mytempdir <- file.path(getwd(), 'TIGERTEMP') }
  if (!file.exists(mytempdir)) {dir.create(mytempdir)}
  yeartxt <- as.character(myyear)
  
  if (missing(mystatenums)) {
      statenums <- get.state.info()$FIPS
      statenums <- statenums[!is.na(statenums)]
  } else {
    statenums <- mystatenums
  }

  ftpurl <- paste('ftp://ftp2.census.gov/geo/tiger/TIGER', yeartxt,'/BG/',sep='')
  zipnames <- paste('tl_', yeartxt, '_', statenums,'_bg.zip',sep='')
  dbfnames <- paste('tl_', yeartxt, '_', statenums,'_bg.dbf',sep='')
  
  x=download.files(ftpurl, zipnames, mytempdir, overwrite=overwrite, silent=silent)
  #if (!silent) {cat(x, '\n\n')}
  
  x=unzip.files(file.path(mytempdir, zipnames), as.list(dbfnames), exdir = mytempdir, overwrite=overwrite)
  #if (!silent) {print(x); cat('\n\n')}

  require(foreign)
  
  out=compile.dbfs(file.path(mytempdir, dbfnames))
  
  out$INTPTLAT <- as.numeric(out$INTPTLAT)
  out$INTPTLON <- as.numeric(out$INTPTLON)
  #out$FIPS <- with(out, paste( STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, sep=''))
  out$STATEFP <- NULL
  out$COUNTYFP <- NULL
  out$TRACTCE <- NULL
  out$BLKGRPCE <- NULL
  out$NAMELSAD <- NULL
  out$MTFCC <- NULL 
  out$FUNCSTAT<- NULL
  names(out) <- c('FIPS', 'aland', 'awater', 'lat', 'lon')
  return(out)
  
}
