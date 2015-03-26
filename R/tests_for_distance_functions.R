set.seed(999)
t1=testpoints(1)
t10=testpoints(10)
t100=testpoints(100)
t1k=testpoints(1e3)
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


get.nearest(t10, t1k)
get.nearest(t10, t1k, radius=500, units='km')
get.nearest(t10, t1k, radius=10, units='km')
