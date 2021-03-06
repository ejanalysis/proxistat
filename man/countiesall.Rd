% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/countiesall.R
\docType{data}
\name{countiesall}
\alias{countiesall}
\alias{counties}
\title{Counties information from U.S. Census Bureau from 2015}
\format{
A data.frame of 3234 U.S. Counties or County equivalents, with 5 variables. \cr\cr
> str(countiesall) \cr
 'data.frame':	3235 obs. of  5 variables:
\itemize{
\item  $ ST         : chr  "AL" "AL" "AL" "AL" ...
\item  $ countyname : chr  "Autauga County" "Baldwin County" "Barbour County" "Bibb County" ...
\item  $ FIPS.COUNTY: chr  "01001" "01003" "01005" "01007" ...
\item  $ statename  : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
\item  $ fullname   : chr  "Autauga County, Alabama" "Baldwin County, Alabama" "Barbour County, Alabama" "Bibb County, Alabama" ...
}
\code{> head(countiesall) \cr \cr
   ST     countyname FIPS.COUNTY statename                fullname \cr
 1 AL Autauga County       01001   Alabama Autauga County, Alabama \cr
 2 AL Baldwin County       01003   Alabama Baldwin County, Alabama \cr
 3 AL Barbour County       01005   Alabama Barbour County, Alabama \cr
 4 AL    Bibb County       01007   Alabama    Bibb County, Alabama \cr
 5 AL  Blount County       01009   Alabama  Blount County, Alabama \cr
 6 AL Bullock County       01011   Alabama Bullock County, Alabama \cr
  }
}
\source{
Derived from Census Bureau, obtained early 2015.
}
\description{
This data set provides names and FIPS codes for U.S. Counties and County equivalents.
}
\seealso{
\code{\link[ejanalysis]{get.county.info}} from the \pkg{ejanalysis} package (\url{http://ejanalysis.github.io/ejanalysis/}), 
  and the \pkg{UScensus2010county} package and the \pkg{acs} package
  and the Census 2010 packages \url{http://lakshmi.calit2.uci.edu/census2000/} and \url{http://www.jstatsoft.org/v37/i06}
}
\keyword{datasets}
