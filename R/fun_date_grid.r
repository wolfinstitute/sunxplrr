#' @title Provides a tibble with dates from start to end
#'
#' @description Privides a tibble with dates and columns containing year, month
#'   and day. The granularity can be chosen from year month or day. Works for
#'   all years within the gregorian calendar. 
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
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-23 / Frt
# - `Created`    : 2019-12-22 / Frt
# - `Last test`  : 2019-12-23 / Frt
#
fun_date_grid <- function(start_date, end_date, granularity = "day"){
  
  if (granularity == "day"){
    
    date_grid <- tibble(date = make_date(1610,12,18), 
                        year = as.integer(year(date)), 
                        month = as.integer(month(date)), 
                        day = as.integer(day(date)))
    
    for (jdx in (1:(year(end_date) - year(start_date) + 1))){
      
      date <- make_date((year(start_date) + jdx - 1), 
                        month(start_date), 
                        day(start_date))
  
      if (leap_year(year(date))){
    
        for (idx in 2:366){
      
          date[idx] <- date[idx-1] + days(1)
      
        }
    
      } else {
    
        for (idx in 2:365){
      
          date[idx] <- date[idx-1] + days(1)
      
        }
    
      }
  
    date_grid <- bind_rows(date_grid, 
                           tibble(date = date, 
                                  year = as.integer(year(date)), 
                                  month = as.integer(month(date)), 
                                  day = as.integer(day(date))))
  
    }
    
    date_grid <- date_grid[2:nrow(date_grid),]
    
  }
  
  if (granularity == "month"){
    
    date_grid <- tibble(date = make_date(1610,12,01), 
                        year = as.integer(year(date)), 
                        month = as.integer(month(date)), 
                        day = as.integer(day(date)))
    
    for (jdx in (1:(year(end_date) - year(start_date) + 1))){
      
      date <- make_date((year(start_date) + jdx - 1), month(start_date), 1)
      
      for (idx in 2:12){
        
        date[idx] <- date[idx-1] + months(1)
        
      }
          
      date_grid <- bind_rows(date_grid, 
                             tibble(date = date, 
                                    year = as.integer(year(date)), 
                                    month = as.integer(month(date)), 
                                    day = as.integer(day(date))))
      
    }
    
    date_grid <- date_grid[2:nrow(date_grid),] %>% 
      select(date, year, month)
    
  }
  
  if (granularity == "year"){
    
    date_grid <- tibble(date = make_date(1610,01,01), 
                        year = as.integer(year(date)), 
                        month = as.integer(month(date)), 
                        day = as.integer(day(date)))
    
    for (jdx in (1:(year(end_date) - year(start_date) + 1))){
      
      date <- make_date((year(start_date) + jdx - 1), 1, 1)
      
      for (idx in 1:1){
        
        date[idx] <- date[idx-1] + years(1)
        
      }
      
      date_grid <- bind_rows(date_grid, 
                             tibble(date = date, 
                                    year = as.integer(year(date)), 
                                    month = as.integer(month(date)), 
                                    day = as.integer(day(date))))
      
    }
    
    date_grid <- date_grid[2:nrow(date_grid),] %>% 
      select(date, year)
    
  }
  
  return(date_grid)
      
}