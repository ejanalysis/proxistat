proxistat.assemble.chunks <- function(files, folder=getwd()) {
  load(file.path(folder, files[1]))
  outputs <- output
  for (thisfilename in files[-1]) {
    cat('Getting', thisfilename)
    load(file.path(folder, thisfilename))
    outputs <- rbind( outputs, output )
  }
  return(outputs)
}
