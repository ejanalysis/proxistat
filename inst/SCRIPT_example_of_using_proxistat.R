# example of using proxistat() 


library(proxistat)
library(ejscreen)

test.from <- structure(list(fromlat = c(38.9567309094, 38.9507043428),
                            fromlon = c(-77.0896572305, -77.0896199948)), .Names = c("lat", "lon"),
                       row.names = c("6054762", "6054764"), class = "data.frame")
test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 38.9514152435),
                          tolon = c(-77.0892818598, -77.0896199948, -77.0972395245)), .Names = c("lat", "lon"),
                     class = "data.frame", row.names = c("6054762", "6054763", "6054764"))

set.seed(999)
t1=testpoints(1)
t10=testpoints(10)  # t1=t10[3,] 
t100=testpoints(100) # t10[2,] <- t100[5,] 
t1k=testpoints(1e3)
t10k=testpoints(1e4)
t100k=testpoints(1e5)
t1m=testpoints(1e6)

proxistat(t1, t10k, radius=50, units='km')

proxistat(t10, t10k)

subunitscores = proxistat(frompoints=test.from, topoints=test.to,
                          area=rep(0.2, length(test.from[,1])), radius=50, units='km')
print(subunitscores)
subunitpop = rep(1000, length(test.from$lat))
subunits = data.frame(FIPS=substr(rownames(test.from), 1, 5), 
                      pop=subunitpop, stringsAsFactors=FALSE )
unitscores = aggregate(subunits,
                       by=list(subunits$FIPS), FUN=function(x) {Hmisc::wtd.mean(x$score, weights=x$pop, na.rm=TRUE)}
)
print(unitscores)
## Not run: 
output = proxistat.chunked(blocks[ , c('lon','lat')], topoints=rmp, fromchunksize=10000, area=blocks$area / 1e6,
                           return.count=TRUE, return.nearest=TRUE )
output=as.data.frame(output)
if (class(blocks$fips)!='character') {blocks$fips <- lead.zeroes(blocks$fips, 15)}
blocks$FIPS.BG <- get.fips.bg(blocks$fips)
bg.proxi <- data.frame()
bg.proxi$scores  <-  aggregate( cbind(d=output$scores, pop=blocks$pop), by=list(blocks$FIPS.BG), function(x) Hmisc::wtd.mean(1/x[,'d'], x[,'pop']))
if ('nearestone.d' %in% colnames(output)) { bg.proxi$nearestone.d <- aggregate( output$d, by=list(blocks$FIPS.BG), min) }
if ('count.near' %in% colnames(output))   { bg.proxi$count.near   <- aggregate( cbind(d=output$count.near, pop=blocks$pop), by=list(blocks$FIPS.BG), function(x) Hmisc::wtd.mean(1/x[,'d'], x[,'pop'])) }
