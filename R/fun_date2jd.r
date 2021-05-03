#' @title Calculates Julian Date from a vector containing date with time objects
#'
#' @description Calculates Julian Date from a vector containing date with time
#'   objects. Does not work for negative years. Requires 4 digits for year.
#' 
#' @param date_time vector containing date and time in FITS format.
#'
#' @return vector with Julian Dates.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-20 / Frt
# - `Created`    : 2019-12-20 / Frt
# - `Last test`  : 2019-12-20 / Frt
#
fun_date2jd <- function(date_time){
  
    datetime <- stringr::str_replace(date_time, "T", " ")
    
    year   <- lubridate::year(datetime)
    month  <- lubridate::month(datetime)
    day    <- lubridate::mday(datetime)
    hour   <- lubridate::hour(datetime)
    minute <- lubridate::minute(datetime)
    second <- lubridate::second(datetime)
    
    D <- day + hour / 24 + minute / (24 * 60) + second / (24 * 60 * 60)
    
    for (i in seq(1,length(datetime))){
      if(month[i] <= 2){
        year[i] <- year[i] - 1
        month[i] <- month[i] + 12
      }
    }
    
    A <- floor(year / 100)
    
    B <- A # initialisation
    
    for (i in seq(1,length(datetime))){
      if (datetime[i] < lubridate::ymd(15821015)){ 
        B[i] <- 0
      } else {
        B[i] <- 2 - A[i] + floor(A[i] / 4)
      }
    }
    
    JD <- floor(365.25 * (year + 4716)) +
      floor(30.6001 * (month + 1)) +
      D +
      B -
      1524.5
    
    return(JD)

}