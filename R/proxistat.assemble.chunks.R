#' read files storing proxistat result chunks and assemble into one result
#'
#' @param files Required character vector of names of .RData files to read and combine
#' @param folder Optional character element specifying directory where files are stored, defaults to getwd()
#'
#' @return Matrix that contains combined results found in all the files
#' @export
#'
#' @examples
#' \dontrun{
#'  fnames=proxistat.chunked(testpoints(10), testpoints(5), fromchunksize = 4, assemble=FALSE, 
#'    folder=file.path(getwd(), 'temp'))
#'  output=proxistat.assemble.chunks(files=fnames, folder=file.path(getwd(), 'temp'))
#'   }
proxistat.assemble.chunks <- function(files, folder=getwd()) {
  load(file.path(folder, files[1]))
  if (exists('output')) {  try(outputs <- output)} else {stop('Each .RData file must have a saved matrix called output')}
  for (thisfilename in files[-1]) {
    cat('Getting', thisfilename, '\n')
    load(file.path(folder, thisfilename))
    #if (dim(output)==) # if output weren't saved as matrix this would be needed
      outputs <- rbind( outputs, output )
  }
  return(outputs)
}

