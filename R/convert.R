#' @title Convert units of distance or area
#'
#' @description
#' \code{convert} converts distance/length or area from specified units to other specified units.
#' 
#' @details
#' This function takes a number, or vector of numbers, representing distance/length or area
#' in one type of specified units, such as miles, and returns the corresponding number(s)
#' converted to some other units, such as kilometers. Units can be specified in various ways.
#' All inputs must be in the same units. All outputs must be in a single set of units as well.
#'
#' @param x A number or vector of numbers to be converted.
#' @param from A string specifying original units of input parameter. Default is 'km' which is kilometers.
#'   Note all must be in the same units.
#'   Units can be specified as any of the following: 
#'   c(
#'   'millimeter', 'millimeters', 'centimeter', 'centimeters', 'meter', 'meters', 'kilometer', 'kilometers',
#'   "mm", "cm", "m", "km",
#'   "sqmm", "sqcm", "sqm", "sqkm", 
#'   "mm2", "cm2", "m2", "km2", 
#'   'inch', 'inches', 'foot', 'feet', 'yard', 'yards', 'mile', 'miles',
#'   "in", "ft", "yd", "mi", 
#'   "sqin", "sqft", "sqyd", "sqmi",
#'   "in2", 'ft2', 'yd2', 'mi2'
#'   )
#'   Note that m2 is for square meters not square miles.
#' @param towhat A strings specifying new units to convert to. Default is 'mi' which is miles.
#' @return Returns a number or vector of numbers then length of the input x, 
#'   with each element corresponding to an input element converted to new units.
#' @seealso \code{\link{get.distances}} for get.distances() which allows you to specify a search radius and 
#'   get distances only within that radius, and related functions.
#' @concept proximity
#' @examples
#' convert(1, 'mi', 'km')
#' convert(c(1e6, 1), 'sqm', 'sqkm')
#' @export
convert <- function(x, from='km', towhat='mi') {

  warning('these draft conversion factors have some errors as of 1/2015 - need to be corrected!!')

  mult <- structure(list(from = c("mm", "cm", "m", "km", "sqmm", "sqcm", 
  "sqm", "sqkm", "in", "ft", "yd", "mi", "sqin", "sqft", "sqyd", 
  "sqmi"), mm = c(1L, 10L, 1000L, 1000000L, NA, NA, NA, NA, 25L, 
  300L, 900L, 1609300L, NA, NA, NA, NA), cm = c(0.1, 1, 100, 1e+05, 
  NA, NA, NA, NA, 2.5, 30, 90, 160930, NA, NA, NA, NA), m = c(0.001, 
  0.01, 1, 1000, NA, NA, NA, NA, 0.025, 0.3, 0.9, 1609.3, NA, NA, 
  NA, NA), km = c(1e-06, 1e-05, 0.001, 1, NA, NA, NA, NA, 2.5e-05, 
  3e-04, 9e-04, 1.6093, NA, NA, NA, NA), 
  sqmm = c(NA, NA, NA, NA, 
  1, 100, 1e+06, 1e+12, NA, NA, NA, NA, 625, 90000, 810000, 2.50906e+12), 
  sqcm = c(NA, NA, NA, NA, 0.01, 1, 10000, 1e+10, NA, NA, NA, 
  NA, 6.25, 900, 8100, 25090560000), sqm = c(NA, NA, NA, NA, 1e-06, 
  1e-04, 1, 1e+06, NA, NA, NA, NA, 0.000625, 0.09, 0.81, 2509056
  ), sqkm = c(NA, NA, NA, NA, 1e-12, 1e-10, 1e-06, 1, NA, NA, NA, 
  NA, 6.25e-10, 9e-08, 8.1e-07, 2.509056), `in` = c(0.04, 0.4, 
  40, 40000, NA, NA, NA, NA, 1, 12, 36, 63360, NA, NA, NA, NA), 
  ft = c(0.003333333, 0.033333333, 3.333333333, 3333.333333, 
  NA, NA, NA, NA, 0.083333333, 1, 3, 5280, NA, NA, NA, NA), 
  yd = c(0.001111111, 0.011111111, 1.111111111, 1111.111111, 
  NA, NA, NA, NA, 0.027777778, 0.333333333, 1, 1760, NA, NA, 
  NA, NA), mi = c(6.213882e-07, 6.213882e-06, 0.0006213882, 
  0.6213882, NA, NA, NA, NA, 1.57828e-05, 0.000189394, 0.000568182, 
  1, NA, NA, NA, NA), sqin = c(NA, NA, NA, NA, 0.0016, 0.16, 
  1600, 1.6e+09, NA, NA, NA, NA, 1, NA, NA, NA), sqft = c(NA, 
  NA, NA, NA, 1.11111e-05, 0.001111111, 11.11111111, 11111111.11, 
  NA, NA, NA, NA, NA, 1, NA, NA), sqyd = c(NA, NA, NA, NA, 
  1.23457e-06, 0.000123457, 1.234567901, 1234567.901, NA, NA, 
  NA, NA, NA, NA, 1, NA), sqmi = c(NA, NA, NA, NA, 3.98556e-13, 
  3.98556e-11, 3.98556e-07, 0.39855627, NA, NA, NA, NA, NA, 
  NA, NA, 1)), .Names = c("from", "mm", "cm", "m", "km", "sqmm", 
  "sqcm", "sqm", "sqkm", "in", "ft", "yd", "mi", "sqin", "sqft", 
  "sqyd", "sqmi"), class = "data.frame", row.names = c(NA, -16L))

  # synonyms
  
  if (!(from %in% mult$from)) {
    if (from %in% c('km2')) {from <- 'sqkm'}
    if (from %in% c('m2'))  {from <- 'sqm'}
    if (from %in% c('cm2'))  {from <- 'sqcm'}
    if (from %in% c('mm2'))  {from <- 'sqmm'}
    
    if (from %in% c('kilometer', 'kilometers')) {from <- 'km'}
    if (from %in% c('meter', 'meters')) {from <- 'm'}
    if (from %in% c('centimeter', 'centimeters')) {from <- 'cm'}
    if (from %in% c('millimeter', 'millimeters')) {from <- 'mm'}
    
    if (from %in% c('mi2')) {from <- 'sqmi'}
    if (from %in% c('yd2')) {from <- 'sqyd'}
    if (from %in% c('ft2')) {from <- 'sqft'}
    if (from %in% c('in2')) {from <- 'sqin'}
    
    if (from %in% c('mile', 'miles')) {from <- 'mi'}
    if (from %in% c('yard', 'yards')) {from <- 'yd'}
    if (from %in% c('foot', 'feet')) {from <- 'ft'}
    if (from %in% c('inch', 'inches')) {from <- 'in'}
  }

  if (!(towhat %in% names(mult))) {
    if (towhat %in% c('km2')) {towhat <- 'sqkm'}
    if (towhat %in% c('m2'))  {towhat <- 'sqm'}
    if (towhat %in% c('cm2'))  {towhat <- 'sqcm'}
    if (towhat %in% c('mm2'))  {towhat <- 'sqmm'}
    
    if (towhat %in% c('kilometer', 'kilometers')) {towhat <- 'km'}
    if (towhat %in% c('meter', 'meters')) {towhat <- 'm'}
    if (towhat %in% c('centimeter', 'centimeters')) {towhat <- 'cm'}
    if (towhat %in% c('millimeter', 'millimeters')) {towhat <- 'mm'}
    
    if (towhat %in% c('mi2')) {towhat <- 'sqmi'}
    if (towhat %in% c('yd2')) {towhat <- 'sqyd'}
    if (towhat %in% c('ft2')) {towhat <- 'sqft'}
    if (towhat %in% c('in2')) {towhat <- 'sqin'}
    
    if (towhat %in% c('mile', 'miles')) {towhat <- 'mi'}
    if (towhat %in% c('yard', 'yards')) {towhat <- 'yd'}
    if (towhat %in% c('foot', 'feet')) {towhat <- 'ft'}
    if (towhat %in% c('inch', 'inches')) {towhat <- 'in'}
  }
  
  if (!(from %in% mult$from)) {stop(paste(from, 'not found'))}
  if (!(towhat %in% names(mult))) {stop(paste(towhat, 'not found'))}
  
  return( x * mult[ match(from, mult$from), towhat] )
}
