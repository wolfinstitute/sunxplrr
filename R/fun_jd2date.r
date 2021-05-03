#' @title Calculates date and time vector from Julian Dates vector
#'
#' @description Calculates date and time vector from vector containing Julian
#'   Dates objects. Does not work for negative years.
#' 
#' @return vector containing date with time in FITS format.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-20 / Frt
# - `Created`    : 2019-12-20 / Frt
# - `Last test`  : 2019-12-20 / Frt
#
fun_jd2date <- function(JD){
  
  Z <- floor(JD + 0.5)
  F <- (JD + 0.5) - Z
      
  A     <- F # initialisation
  alpha <- F # initialisation
      
  for (i in seq(1,length(Z))){
    if(Z[i] < 2299161){
      A[i] <- Z[i]
    } else {
      alpha[i] <- floor((Z[i] - 1867216.25) / 36524.25)
      A[i] <- Z[i] + 1 + alpha[i] - floor(alpha[i] / 4)
    }
  }
      
  B <- A + 1524
  C <- floor((B - 122.1) / 365.25)
  D <- floor(365.25 * C)
  E <- floor((B - D) / 30.6001)
      
  day <- B - D - floor(30.6001 * E)
      
  hour <- floor(F * 24)
  minute <- floor((F * 24 - hour) * 60)
  second <- floor(((F * 24 - hour) * 60 - minute) * 60)
      
  month <- Z # initialisation
      
  for (i in seq(1,length(Z))){
    if(E[i] < 14){
      month[i] <- E[i] - 1
    } else {
      month[i] <- E[i] - 13
    }
  }
      
  year <- Z # initialisation
      
  for (i in seq(1,length(Z))){
    if(month[i] > 2){
      year[i] <- C[i] - 4716
    } else {
      year[i] <- C[i] - 4715
    }  
  }
      
  datetime <- lubridate::make_datetime(year = year, month = month, day = day, 
                                       hour = hour, min = minute, 
                                       sec = second, tz = "UTC") %>% 
    stringr::str_replace(" ", "T")
      
  return(datetime)
      
}