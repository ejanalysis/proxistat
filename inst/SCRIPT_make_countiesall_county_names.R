## NOTES on naming of counties and county equivalent areas in USA
#  FROM 2022-07:

download.file( 'https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/2021_Gaz_counties_national.zip' , '~/Downloads/countyinfo.zip')
unzip('~/Downloads/countyinfo.zip', '2021_Gaz_counties_national.txt', exdir='~/Downloads/')
library(readr)
x <- readr::read_tsv(
  '~/Downloads/2021_Gaz_counties_national.txt',
  # We do not need all of these columns, and want to rename the 3 we want:
  # col_names = c('USPS',  'GEOID', 'ANSICODE', 'NAME', 'ALAND', 'AWATER', 'ALAND_SQMI', 'AWATER_SQMI', 'INTPTLAT', 'INTPTLONG'),
  # col_types = 'ccccnnnnnn', skip = 1
  col_names = c('ST',  'FIPS.COUNTY',  'countyname'),
  col_types = 'cc-c------', skip = 1
)
x <- as.data.frame(x)
x$statename <- proxistat::lookup.states[ match(x[ , 'ST'], proxistat::lookup.states[ , 'ST']), 'statename']
# x$FIPS.ST 
x$fullname <- apply( x[ , c('countyname', 'statename')] , 1, function(myrow) paste(myrow[1], myrow[2], sep=', '))
metadata <- list(
  ejscreen_version = '2.1', 
  ejscreen_releasedate = 'August 2022', 
  ACS_version = '2016-2020', 
  ACS_releasedate = '3/17/2022')
attributes(x) <- c(attributes(x), metadata)
rownames(x) <- seq_len(nrow(x))

countiesall <- x ; rm(x)
# for the proxistat package 
# setwd("~/Documents/R PACKAGES/proxistat")
usethis::use_data(countiesall, overwrite=TRUE) 
# save(countiesall_alternate, file = '~/Downloads/countiesall_alternate.rdata')

###  also see
### # https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2020_Gazetteer/2020_Gaz_cbsa_national.zip
# https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2020_Gazetteer/2020_Gaz_counties_national.zip
# https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/2021_Gaz_counties_national.zip

stop('the other source gives exactly the same info')

######################################### #
# ANOTHER WAY TO GET THE SAME EXACT county info: ####


columntitles <- readLines('https://www2.census.gov/programs-surveys/acs/summary_file/2020/prototype/Geos20205YR.csv', n = 1)
# columntitles <- "FILEID,STUSAB,SUMLEVEL,COMPONENT,LOGRECNO,US,REGION,DIVISION,STATECE,STATE,COUNTY,COUSUB,PLACE,TRACT,BLKGRP,CONCIT,AIANHH,AIANHHFP,AIHHTLI,AITSCE,AITS,ANRC,CBSA,CSA,METDIV,MACC,MEMI,NECTA,CNECTA,NECTADIV,UA,BLANK,CDCURR,SLDU,SLDL,BLANK,BLANK,ZCTA5,SUBMCD,SDELM,SDSEC,SDUNI,UR,PCI,BLANK,BLANK,PUMA5,DADSID,GEOID,NAME,BTTR,BTBG,BLANK"
columntitles <- unlist(strsplit(columntitles,','))
blanktitlecount <- sum(columntitles == 'BLANK')
columntitles[columntitles == 'BLANK'] <- paste0('BLANK', 1:blanktitlecount)
library(readr)

geo <- as.data.frame(readr::read_csv(
  'https://www2.census.gov/programs-surveys/acs/summary_file/2020/prototype/Geos20205YR.csv', 
  col_names = columntitles,
  col_select = c('SUMLEVEL', 'GEOID', 'STUSAB', 'NAME'), 
  locale = readr::locale(encoding = "ISO-8859-1") )
  # census file county names seemed to be in latin-1 not UTF-8
)
# SUMLEVEL == '050' # for COUNTY &  SUMLEVEL == '150' for BLOCKGROUP 
# NAME == "Autauga County, Alabama" 
# GEOID == '05000US01001'

# geobg <- geo[geo$SUMLEVEL == '150', ]# in case you wanted a list of valid blockgroup FIPS for this year. 
geo <- geo[geo$SUMLEVEL == '050', ]  

geo$SUMLEVEL <- NULL
geo$FIPS.COUNTY <- gsub(x = geo$GEOID, pattern = '.*US(.*)', '\\1')
geo$GEOID <- NULL
Encoding(geo$NAME) <- 'UTF-8'
geo$fullname <- geo$NAME
geo$NAME <- NULL
geo$ST <- geo$STUSAB; geo$STUSAB <- NULL

geo$countyname <- gsub('(.*), (.*)', '\\1', geo$fullname)
geo$statename <- gsub('(.*), (.*)', '\\2', geo$fullname)
# geo$fullname <- NULL 
geo <- geo[ , c('ST', 'FIPS.COUNTY', 'countyname', 'statename', 'fullname')]
Encoding(geo$countyname) <- 'UTF-8'
Encoding(geo$fullname) <- 'UTF-8'
# > head(geo)
#     ST FIPS.COUNTY     countyname statename                fullname
# 778 AL       01001 Autauga County   Alabama Autauga County, Alabama
# 779 AL       01003 Baldwin County   Alabama Baldwin County, Alabama
metadata <- list(
  ejscreen_version = '2.1', 
  ejscreen_releasedate = 'August 2022', 
  ACS_version = '2016-2020', 
  ACS_releasedate = '3/17/2022')
attributes(geo) <- c(attributes(geo ), metadata)
rownames(geo) <- seq_len(nrow(geo))

countiesall <- geo
rm(geo, columntitles, blanktitlecount, blanktitles); rm("countname",   "countyfips" , "countyname" )
# > dim(countiesall)
# [1] 3221    5
# table(sapply(gregexec(',', as.vector(countiesall$fullname)), function(x) length(x)))


# for the proxistat package 
# setwd("~/Documents/R PACKAGES/proxistat")
usethis::use_data(countiesall, overwrite=TRUE) 
#   or
# save(countiesall, file = './data/countiesall.rda')
# 
# data(countiesall, package = 'proxistat')




stop('done')




# NOTES FROM PRE-2022:

## All countylike places have one (or more!) of these terms in their full name:
# for (term in c('County', 'City', 'city', 'Parish', 'Municipio', 'Municipality', 'Borough', 'Island','Census Area', 'District of', 'District','Guam')) {cat('\n'); cat(sum(grepl(term, countiesall$fullname)), ' ', term)}

# 3007   County
# 7   City
# 41   city
# 64   Parish
# 78   Municipio
# 6   Municipality
# 16   Borough
# 18   Island
# 11   Census Area
# 1   District of
# 4   District
# 1   Guam

## > 3007+7+41+64+78+6+16+18+11+1+4+1
## [1] 3254  # this doublecounts the ones with 2+ terms

# length(countiesall$fullname[grepl('(County|City|city|Parish|Municipio|Municipality|Borough|Island|Census Area|District of|District,|Guam)', countiesall$fullname)])
## [1] 3235
# length(countiesall$fullname[!grepl('(County|City|city|Parish|Municipio|Municipality|Borough|Island|Census Area|District of|District,|Guam)', countiesall$fullname)])
## 0
