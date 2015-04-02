#' @title Read a series of dbf files and join compile them as a single data.frame
#' 
#' @description
#'   Can help with a series of downloaded Census data files, such as one file per State. 
#'   Reads each using \code{\link[foreign]{read.dbf}} and combines them all as a data.frame.
#'   They must all have the same columns, and each file just provides additional rows of data.
#' @param filepaths Required charater vector with full paths including file names of dbf files to be read.
#' @return Returns a data.frame with as many columns as each dbf file, and as many rows as there are in all the dbf files read in.
#' @seealso \code{\link[foreign]{read.dbf}}
#' @export
compile.dbfs <- function(filepaths) {
  thisdbf=foreign::read.dbf(filepaths[1]) # assume we can read the first and all have same number of cols
  colcount=length(thisdbf[1, ])
  #print(colcount)
  out <- matrix(nrow=250000, ncol=colcount) # cannot easily preallocate one row per block group unless know total for those states
  out <- data.frame(out)
  names(out) <- names(thisdbf)
  nextrow=1
  
  for (i in 1:length(filepaths)) {
    cat('Reading ', filepaths[i], '\n')
    thisdbf = try( foreign::read.dbf(filepaths[i], as.is=TRUE) )
    if (class(thisdbf)!='try-error') {
      lastrow = length(thisdbf[,1]) - 1 + nextrow
      out[nextrow:lastrow, ] <-  thisdbf 
      nextrow=lastrow+1
    } else {
      cat('Problem reading file ', filepaths[i],'\n')
    }
  }
  out <- out[1:lastrow, ]
  return(out)
  
  # # example of results of reading dbf files from Census shapefiles of block groups
  # head(z)
  # STATEFP COUNTYFP TRACTCE BLKGRPCE        GEOID      NAMELSAD MTFCC FUNCSTAT
  # 1      01      095  030202        4 010950302024 Block Group 4 G5030        S
  # 2      01      095  030600        2 010950306002 Block Group 2 G5030        S
  # 3      01      095  030702        3 010950307023 Block Group 3 G5030        S
  # 4      01      095  030801        2 010950308012 Block Group 2 G5030        S
  # 5      01      095  030801        3 010950308013 Block Group 3 G5030        S
  # 6      01      095  030903        1 010950309031 Block Group 1 G5030        S
  
  # ALAND   AWATER    INTPTLAT     INTPTLON
  # 1 14969994 15040133 +34.4266825 -086.2437349
  # 2  6751877 16610261 +34.3176285 -086.3439885
  # 3  2192151 12572351 +34.3588277 -086.3334338
  # 4 27390863        0 +34.3069111 -086.1681492
  # 5  9286065    49547 +34.2889344 -086.2042557
  # 6  6599302    47967 +34.2847238 -086.2257010
}
