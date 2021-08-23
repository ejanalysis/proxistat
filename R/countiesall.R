#' @name countiesall
#' @docType data
#' @aliases counties
#' @title Counties information from U.S. Census Bureau from 2021 
#' @description This data set provides names and FIPS codes for U.S. Counties and County equivalents.
#'  \preformatted{
#' # How the 2021 version of proxistat::countiesall was created:
#' 
#' download.file( 'https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/2021_Gaz_counties_national.zip' , 'countyinfo.zip')
#' unzip('countyinfo.zip', '2021_Gaz_counties_national.txt')
#' 
#' x <- readr::read_tsv(
#'   '~/Downloads/2021_Gaz_counties_national.txt',
#'   # We do not need all of these columns, and want to rename the 3 we want:
#'   # col_names = c('USPS',  'GEOID', 'ANSICODE', 'NAME', 'ALAND', 'AWATER', 'ALAND_SQMI', 'AWATER_SQMI', 'INTPTLAT', 'INTPTLONG'),
#'   # col_types = 'ccccnnnnnn', skip = 1
#'   col_names = c('ST',  'FIPS.COUNTY',  'countyname'),
#'   col_types = 'cc-c------', skip = 1
#' )
#' x <- as.data.frame(x)
#' x$statename <- proxistat::lookup.states[ match(x[ , 'ST'], proxistat::lookup.states[ , 'ST']), 'statename']
#' x$fullname <- apply( x[ , c('countyname', 'statename')] , 1, function(myrow) paste(myrow[1], myrow[2], sep=', '))
#' countiesall <- x
#' save(countiesall, file = '~/Downloads/countiesall.rdata')
#' }
#' 
#' @seealso \code{\link[ejanalysis]{get.county.info}} from the \pkg{ejanalysis} package (\url{http://ejanalysis.github.io/ejanalysis/}), 
#'   and the \pkg{UScensus2010county} package and the \pkg{acs} package
#'   and the Census 2010 packages \url{http://lakshmi.calit2.uci.edu/census2000/} and \url{http://www.jstatsoft.org/v37/i06}
#' @source Derived from Census Bureau, obtained 8/2021.
#' @keywords datasets
#' @format A data.frame of 3221 U.S. Counties or County equivalents, with 5 variables. \cr\cr
#' > str(countiesall) \cr
#'  'data.frame':	3221 obs. of  5 variables:
#' \itemize{
#' \item  $ ST         : chr  "AL" "AL" "AL" "AL" ...
#' \item  $ FIPS.COUNTY: chr  "01001" "01003" "01005" "01007" ...
#' \item  $ countyname : chr  "Autauga County" "Baldwin County" "Barbour County" "Bibb County" ...
#' \item  $ statename  : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
#' \item  $ fullname   : chr  "Autauga County, Alabama" "Baldwin County, Alabama" "Barbour County, Alabama" "Bibb County, Alabama" ...
#' }
#' \code{> head(countiesall) \cr \cr
#'   ST FIPS.COUNTY     countyname statename                fullname\cr
#' 1 AL       01001 Autauga County   Alabama Autauga County, Alabama\cr
#' 2 AL       01003 Baldwin County   Alabama Baldwin County, Alabama\cr
#' 3 AL       01005 Barbour County   Alabama Barbour County, Alabama\cr
#'   }
NULL
