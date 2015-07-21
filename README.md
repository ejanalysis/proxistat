**proxistat** package

The [proxistat package](http://ejanalysis.github.io/proxistat/) for R has functions helping to calculate distances between geographic points, such as the distances between all points, distances to all points within some maximum distance, distance to nearest single point, etc. It also can create a proximity score for each spatial unit like a Census block group, to quantify the distance-weighted count of nearby points. This proximity score can be used in environmental justice (EJ) analysis, for example. This package uses the sp package for the basic distance calculation.  

Key functions include get.nearest() to find the one topoint nearest each frompoint, get.distances() to find distances quickly within an optional search radius, and get.distances.all() to find distances from all frompoints to alltopoints. The function proxistat() creates a proximity score that quantifies, for each spatial unit like a Census block group, how many topoints are nearby and how close they are.   

This and related packages, once each is made available as a public repository on GitHub, until available on cran, can be installed using the devtools package: 

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github("ejanalysis/analyze.stuff")  
devtools::install_github("ejanalysis/countyhealthrankings")  
devtools::install_github("ejanalysis/UScensus2010blocks")  
devtools::install_github("ejanalysis/ACSdownload")  
devtools::install_github(c("ejanalysis/proxistat", "ejanalysis/ejanalysis"))
devtools::install_github("ejanalysis/ejscreen")
```


