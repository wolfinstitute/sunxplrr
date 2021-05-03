#' @title Calculates delta T in seconds according to formulae by Fred Espenak
#'
#' @description Calculates delta T = TD - UT in seconds according to formulae 
#'   given by Fred Espenak. Before 2005, the polynomial expressions for delta T
#'   are from the Five Millennium Canon of Solar Eclipses: -1999 to +3000. 
#'   Source: http://www.eclipsewise.com/help/deltatpoly2014.html
#' 
#' @param date_time vector containing date and time in FITS format.
#'
#' @return value of delta T in seconds.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-28 / Frt
# - `Created`    : 2019-12-28 / Frt
# - `Last test`  : 2019-12-28 / Frt
#
fun_deltaT <- function(date_time){
  
    datetime <- stringr::str_replace(date_time, "T", " ")
    
    year   <- lubridate::year(datetime)
    month  <- lubridate::month(datetime)

    y = year + (month - 0.5)/12

    if (y < 1600 & y >= 3000){
      stop("year out of the implemented range in fun_deltaT")
    }

    if (y >= 1600 & y < 1700){
      t <- y - 1600
      deltaT <- 120 - 0.9808 * t - 0.01532 * t^2 + t^3 / 7129
    }
    
    if (y >= 1700 & y < 1800){
      t <- y - 1700
      deltaT <- 8.83 + 0.1603 * t - 0.0059285 * t^2 + 0.00013336 * t^3 - 
        t^4 / 1174000
    }
    
    if (y >= 1800 & y < 1860){
      t <- y - 1800
      deltaT <- 13.72 - 0.332447 * t + 0.0068612 * t^2 + 0.0041116 * t^3 - 
        0.00037436 * t^4 + 0.0000121272 * t^5 - 0.0000001699 * t^6 + 
        0.000000000875 * t^7
    }
    
    if (y >= 1860 & y < 1900){
      t <- y - 1860
      deltaT <- 7.62 + 0.5737 * t - 0.251754 * t^2 + 0.01680668 * t^3 - 
        0.0004473624 * t^4 + t^5 / 233174
    }
    
    if (y >= 1900 & y < 1920){
      t <- y - 1900
      deltaT <- -2.79 + 1.494119 * t - 0.0598939 * t^2 + 0.0061966 * t^3 - 
        0.000197 * t^4
    }
    
    if (y >= 1920 & y < 1941){
      t <- y - 1920
      deltaT <- 21.20 + 0.84493*t - 0.076100 * t^2 + 0.0020936 * t^3
    }

    if (y >= 1941 & y < 1961){
      t <- y - 1950
      deltaT <- 29.07 + 0.407*t - t^2/233 + t^3 / 2547
    }

    if (y >= 1961 & y < 1986){
      t <- y - 1975
      deltaT <- 45.45 + 1.067*t - t^2/260 - t^3 / 718
    }

    if (y >= 1986 & y < 2005){
      t <- y - 2000
      deltaT <- 63.86 + 0.3345 * t - 0.060374 * t^2 + 0.0017275 * t^3 + 
        0.000651814 * t^4 + 0.00002373599 * t^5
    }
    
    if (y >= 2005 & y < 2015){
      t <- y - 2000
      deltaT <- 64.69 + 0.2930 * t
    }

    if (y >= 2015 & y < 3000){
      t <- y - 2015
      deltaT <- 67.62 + 0.3645 * t + 0.0039755 * t^2
    }

    return(deltaT)

}