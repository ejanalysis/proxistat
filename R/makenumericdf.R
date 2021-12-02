makenumericdf <- function(x) {
  
  cleanit <- function(z) {
    as.data.frame(lapply(z, function(y) (
      gsub('th', '', 
           gsub('<', '', 
                gsub(' miles', '', 
                     gsub('N/A','', 
                          gsub('%','', 
                               gsub(',', '', y)) )))))),
      stringsAsFactors = FALSE)
  }
  
  clean <- cleanit(x)

for (i in 1:NCOL(x)) {
  if (all(is.na(as.numeric(clean[,i]))) & !all(is.na(clean[,i]))) {
    # was not all NA but got forced to NA via as.numeric means was a real character col
  } else {
    clean[, i] <- as.numeric(clean[, i])
  }
}
return(clean)    
}
