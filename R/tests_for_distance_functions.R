if (1==0) {
  
  set.seed(999)
  t1=testpoints(1, 25, 47)
  t10=testpoints(10, 25, 47)
  t100=testpoints(100, 25, 47)
  t1k=testpoints(1e3 )
  t10k=testpoints(1e4)
  t100k=testpoints(1e5)
  t1m=testpoints(1e6)
  
  #return.rownums=TRUE, return.latlons=FALSE, radius=Inf, ignore0=FALSE
  get.nearest(t1, t1)
  get.nearest(t10, t10)
  get.nearest(t10, t10, ignore0=TRUE)
  get.nearest(t10, t10, ignore0=TRUE, radius=100)
  
  get.nearest(t1, t10[2, ,drop=FALSE])
  get.nearest(t1, t10[2, ,drop=FALSE], return.rownums=FALSE, return.latlons=FALSE, radius=Inf, ignore0=FALSE)
  get.nearest(t1, t10[2, ,drop=FALSE], return.rownums=FALSE, return.latlons=TRUE, radius=Inf, ignore0=FALSE)
  get.nearest(t1, t10[2, ,drop=FALSE], return.rownums=TRUE, return.latlons=TRUE, radius=Inf, ignore0=FALSE)
  
  get.nearest(t10, t100k, units='miles')
  get.nearest(t10, t100k, radius=100, units='miles')
  x=as.data.frame(get.nearest(t10, t1m, radius=100, units='miles', return.latlons = TRUE))
  with(x, plot(fromlon, fromlat))
  with(x, points(tolon, tolat))

  tf=testpoints(100,25,47)
  tt=testpoints(1000,25,47)
  tf=data.frame(tf)
  tt=data.frame(tt)
  
  
  
  x=get.nearest(tf,tt,return.latlons = TRUE)
  
  x=as.data.frame(x)
  plot(tt$lon, tt$lat, pch='.', main='Line drawn to the point nearest a given red circle')
  points(tf$lon, tf$lat,col='red')
  #for (i in 1:length(x$fromlat)) {lines(c(x$fromlon[i], x$tolon[i]), c(x$fromlat[i],x$tolat[i]) )}
  with(x, linesegments(fromlon, fromlat, tolon, tolat))
  
}
