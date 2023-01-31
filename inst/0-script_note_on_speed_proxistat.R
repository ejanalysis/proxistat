
# speed of proxistat::get.distances.all  
# which uses sf package to get distance pairs
if (1 == 0) {
  tp = EJAMejscreenapi::testpoints_1000
  tp = tp[!is.na(tp$LONG),]
  names(tp) <- c('lon', 'lat')
  # tp10k <- testpoints_block2010(10000) # super slow !!
  # tp10k <- tp10k[!is.na(tp10k$lon),]  
   
  Sys.time(); x=get.distances.all(tp, tp, return.crosstab=TRUE); Sys.time()
  
  # localtree should be available after EJAM package is loaded
  # Sys.time(); y=EJAM::getblocksnearby(tp,  quadtree = localtree ); Sys.time()
  
  
    
}
