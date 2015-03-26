
# Just a test, mapping the results of get.nearest2()

if (1==0) {
  tf=testpoints(100,25,47)
  tt=testpoints(1000,25,47)
  tf=data.frame(tf)
  tt=data.frame(tt)
  x=get.nearest2(tf,tt,return.latlons = TRUE)
  x=as.data.frame(x)
  for (i in 1:length(x$fromlat)) {lines(c(x$fromlon[i], x$tolon[i]), c(x$fromlat[i],x$tolat[i]) )}
  plot(tt$lon, tt$lat, pch='.', main='Line drawn to the point nearest a given red circle')
  points(tf$lon, tf$lat,col='red')
  for (i in 1:length(x$fromlat)) {lines(c(x$fromlon[i], x$tolon[i]), c(x$fromlat[i],x$tolat[i]) )}
}
