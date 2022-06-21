## NOTES on naming of counties and county equivalent areas in USA

# updating
# data(countiesall, package = 'proxistat')
library(readr)

# NOTES FROM 2022-06:

columntitles <- readLines('https://www2.census.gov/programs-surveys/acs/summary_file/2020/prototype/Geos20205YR.csv', n = 1)
# columntitles <- "FILEID,STUSAB,SUMLEVEL,COMPONENT,LOGRECNO,US,REGION,DIVISION,STATECE,STATE,COUNTY,COUSUB,PLACE,TRACT,BLKGRP,CONCIT,AIANHH,AIANHHFP,AIHHTLI,AITSCE,AITS,ANRC,CBSA,CSA,METDIV,MACC,MEMI,NECTA,CNECTA,NECTADIV,UA,BLANK,CDCURR,SLDU,SLDL,BLANK,BLANK,ZCTA5,SUBMCD,SDELM,SDSEC,SDUNI,UR,PCI,BLANK,BLANK,PUMA5,DADSID,GEOID,NAME,BTTR,BTBG,BLANK"
columntitles <- unlist(strsplit(columntitles,','))
blanktitlecount <- sum(columntitles == 'BLANK')
columntitles[columntitles == 'BLANK'] <- paste0('BLANK', 1:blanktitlecount)
geo <- readr::read_csv(
  'https://www2.census.gov/programs-surveys/acs/summary_file/2020/prototype/Geos20205YR.csv', 
  col_names = columntitles,
  col_select = c('SUMLEVEL', 'GEOID', 'STUSAB', 'NAME'), 
  locale = readr::locale(encoding = "ISO-8859-1") 
  # census file county names seemed to be in latin-1 not UTF-8
)

# SUMLEVEL == '050' # for COUNTY &  SUMLEVEL == '150' for BLOCKGROUP 
# NAME == "Autauga County, Alabama" 
# GEOID == '05000US01001'

geo <- geo[geo$SUMLEVEL == '050', ] 
geo$SUMLEVEL <- NULL
geo$FIPS.COUNTY <- gsub(x = geo$GEOID, pattern = '.*US(.*)', '\\1') 
geo$GEOID <- NULL
Encoding(geo$NAME) <- 'UTF-8'
geo$fullname <- geo$NAME
geo$NAME <- NULL
geo$ST <- geo$STUSAB
geo$STUSAB <- NULL
geo$countyname <- gsub('(.*), (.*)', '\\1', geo$fullname)
geo$statename <- gsub('(.*), (.*)', '\\2', geo$fullname)
# geo$fullname <- NULL 
geo <- geo[ , c('ST', 'FIPS.COUNTY', 'countyname', 'statename', 'fullname')]
Encoding(geo$NAME) <- 'UTF-8'
countiesall <- geo
rm(geo, columntitles, blanktitlecount, blanktitles); rm("countname",   "countyfips" , "countyname" )
attr(countiesall, 'year') <- 'ACS 2020 (2016-2020) geographies as of June 2022'
# > dim(geo)
# [1] 3221    5
# table(sapply(gregexec(',', as.vector(countiesall$fullname)), function(x) length(x)))
countiesall <- as.data.frame(countiesall)
rownames(countiesall) <- NULL
attr(countiesall, 'row.names') <- NULL
attr(countiesall, 'spec') <- NULL

# for the proxistat package 
# setwd("~/Documents/R PACKAGES/proxistat")
# usethis::use_data(countiesall, overwrite=TRUE) 
#   or
# save(countiesall, file = './data/countiesall.rda')
# 
# data(countiesall, package = 'proxistat')






stop('done')




# NOTES FROM PRE-2022:

## All countylike places have one or more of these terms in their full name:

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
