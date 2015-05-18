#' @name bg.pts
#' @docType data
#' @aliases bg.pts
#' @title Block group internal points and areas (square meters) from Census Bureau for 2010 geographies
#' @description This data set provides a data.frame with 220740 block groups, with 5 variables.
#' @seealso \pkg{UScensus2010blocks} and  \pkg{acs}  
#'   and the Census 2010 packages \url{http://lakshmi.calit2.uci.edu/census2000/}  and \url{http://www.jstatsoft.org/v37/i06}
#' @usage data('bg.pts')
#' @source Derived from Census Bureau, obtained early 2015.
#' @keywords datasets
#' @format A data.frame of 220740 block groups, with 5 variables. \cr\cr
#' \itemize{
#'  \item  [1,] "FIPS  (e.g., '010950302024')"  \cr\cr
#'  \item  [2,] "aland  numeric, land area in square meters" \cr\cr
#'  \item  [3,] "awater  numeric, water area in square meters" \cr\cr
#'  \item  [4,] "lat  numeric, latitude in decimal degrees" \cr\cr
#'  \item  [5,] "lon  numeric, longitude in decimal degrees" \cr\cr
#' }
NULL
