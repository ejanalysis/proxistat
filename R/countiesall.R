#' @name countiesall
#' @docType data
#' @aliases counties
#' @title Counties information from U.S. Census Bureau from 2021 
#' @description This data set provides names and FIPS codes for U.S. Counties and County equivalents.
#'   For how the 2021 version of proxistat::countiesall was created, 
#'   see   proxistat/inst/SCRIPT_make_countiesall_county_names.R
#' 
#' @seealso \code{\link[ejanalysis]{get.county.info}} from the \pkg{ejanalysis} package (\url{http://ejanalysis.github.io/ejanalysis/}), 
#'   and maybe the \pkg{UScensus2010county} package and the \pkg{acs} package
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
