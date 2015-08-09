#' @name countiesall
#' @docType data
#' @aliases counties
#' @title Counties information from U.S. Census Bureau from 2015
#' @description This data set provides names and FIPS codes for U.S. Counties and County equivalents.
#' @seealso \code{\link[ejanalysis]{get.county.info}} from the \pkg{ejanalysis} package (\url{http://ejanalysis.github.io/ejanalysis/}), 
#'   and the \pkg{UScensus2010county} package and the \pkg{acs} package
#'   and the Census 2010 packages \url{http://lakshmi.calit2.uci.edu/census2000/} and \url{http://www.jstatsoft.org/v37/i06}
#' @usage data('bg.pts', package='proxistat')
#' @source Derived from Census Bureau, obtained early 2015.
#' @keywords datasets
#' @format A data.frame of 3234 U.S. Counties or County equivalents, with 5 variables. \cr\cr
#' > str(countiesall) \cr
#' 'data.frame':	3234 obs. of  5 variables: \cr
#' \itemize{
#' \item  $ ST         : chr  "AL" "AL" "AL" "AL" ...
#' \item  $ countyname : chr  "Baldwin County" "Barbour County" "Bibb County" "Blount County" ...
#' \item  $ FIPS.COUNTY: chr  "01003" "01005" "01007" "01009" ...
#' \item  $ statename  : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
#' \item  $ fullname   : chr  "Baldwin County, Alabama" "Barbour County, Alabama" "Bibb County, Alabama" "Blount County, Alabama" ...
#' # > head(countiesall) \cr
#' \cr
#'   ST     countyname FIPS.COUNTY statename                fullname \cr
#' 1 AL Baldwin County       01003   Alabama Baldwin County, Alabama \cr
#' 2 AL Barbour County       01005   Alabama Barbour County, Alabama \cr
#' 3 AL    Bibb County       01007   Alabama    Bibb County, Alabama \cr
#' 4 AL  Blount County       01009   Alabama  Blount County, Alabama \cr
#' 5 AL Bullock County       01011   Alabama Bullock County, Alabama \cr
#' 6 AL  Butler County       01013   Alabama  Butler County, Alabama \cr
#' 
#' }
NULL
