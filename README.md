**proxistat** package

The [proxistat package](http://ejanalysis.github.io/proxistat/) for R has functions helping to calculate distances between geographic points, such as the distances between all points, distances to all points within some maximum distance, distance to nearest single point, etc. It also can create a proximity score for each spatial unit like a Census block group, to quantify the distance-weighted count of nearby points. This proximity score can be used in environmental justice (EJ) analysis, for example. This package uses the sp package for the basic distance calculation.  

Key functions include get.nearest() to find the one topoint nearest each frompoint, get.distances() to find distances quickly within an optional search radius, and get.distances.all() to find distances from all frompoints to alltopoints. The function proxistat() creates a proximity score that quantifies, for each spatial unit like a Census block group, how many topoints are nearby and how close they are.   

The analyze.stuff, proxistat, ejanalysis, and countyhealthrankings packages, once made public can be installed from github using the devtools package: devtools::install_github(c("ejanalysis/analyze.stuff", "ejanalysis/proxistat", "ejanalysis/ejanalysis", "ejanalysis/countyhealthrankings")) 
