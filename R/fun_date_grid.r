#' @title Provides a tibble with dates from start to end
#'
#' @description Provides a tibble with dates and columns containing year, month
#'   and day. The granularity can be chosen from year month or day. Works for
#'   all years within the Gregorian calendar. 
#' 
#' @param start_date First date of grid.
#' 
#' @param end_date Last date of grid.
#' 
#' @param granularity Implemented are "year", "month" and "day".
#' 
#' @return tibble containing date, and depending on the granularity also year,
#'   month and day.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2023-02-07 / Frt
# - `Created`    : 2019-12-22 / Frt
# - `Last test`  : 2019-12-23 / Frt
#
fun_date_grid <- function(start_date, end_date, granularity = "day"){
  
  if (granularity == "day"){
    
    date_grid <- tibble::tibble(date = lubridate::make_date(1610,12,18), 
                                year = as.integer(lubridate::year(date)), 
                                month = as.integer(lubridate::month(date)), 
                                day = as.integer(lubridate::day(date)))
    
    for (jdx in (1:(lubridate::year(end_date) - 
                    lubridate::year(start_date) + 1))){
      
      date <- lubridate::make_date((lubridate::year(start_date) + jdx - 1), 
                                    lubridate::month(start_date), 
                                    lubridate::day(start_date))
  
      if (lubridate::leap_year(lubridate::year(date))){
    
        for (idx in 2:366){
      
          date[idx] <- date[idx-1] + lubridate::days(1)
      
        }
    
      } else {
    
        for (idx in 2:365){
      
          date[idx] <- date[idx-1] + lubridate::days(1)
      
        }
    
      }
  
    date_grid <- bind_rows(date_grid, 
                           tibble::tibble(date = date, 
                                  year = as.integer(lubridate::year(date)), 
                                  month = as.integer(lubridate::month(date)), 
                                  day = as.integer(lubridate::day(date))))
  
    }
    
    date_grid <- date_grid[2:nrow(date_grid),]
    
  }
  
  if (granularity == "month"){
    
    date_grid <- tibble::tibble(date = lubridate::make_date(1610,12,01), 
                        year = as.integer(lubridate::year(date)), 
                        month = as.integer(lubridate::month(date)), 
                        day = as.integer(lubridate::day(date)))
    
    for (jdx in (1:(lubridate::year(end_date) - 
                    lubridate::year(start_date) + 1))){
      
      date <- lubridate::make_date((lubridate::year(start_date) + jdx - 1), 
                                   lubridate::month(start_date), 1)
      
      for (idx in 2:12){
        
        date[idx] <- date[idx-1] + lubridate::months(1)
        
      }
          
      date_grid <- bind_rows(date_grid, 
                             tibble::tibble(date = date, 
                                    year = as.integer(lubridate::year(date)), 
                                    month = as.integer(lubridate::month(date)), 
                                    day = as.integer(lubridate::day(date))))
      
    }
    
    date_grid <- date_grid[2:nrow(date_grid),] %>% 
      select("date", "year", "month")
    
  }
  
  if (granularity == "year"){
    
    date_grid <- tibble::tibble(date = lubridate::make_date(1610,01,01), 
                        year = as.integer(lubridate::year(date)), 
                        month = as.integer(lubridate::month(date)), 
                        day = as.integer(lubridate::day(date)))
    
    for (jdx in (1:(lubridate::year(end_date) - 
                    lubridate::year(start_date) + 1))){
      
      date <- lubridate::make_date((lubridate::year(start_date) + 
                                      jdx - 1), 1, 1)
      
      for (idx in 1:1){
        
        date[idx] <- date[idx-1] + lubridate::years(1)
        
      }
      
      date_grid <- bind_rows(date_grid, 
                    tibble::tibble(date = date, 
                                   year = as.integer(lubridate::year(date)), 
                                  month = as.integer(lubridate::month(date)), 
                                    day = as.integer(lubridate::day(date))))
      
    }
    
    date_grid <- date_grid[2:nrow(date_grid),] %>% 
      select("date", "year")
    
  }
  
  return(date_grid)
      
}