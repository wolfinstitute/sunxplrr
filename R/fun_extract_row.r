#' @title Extracts a single row of image frame
#'
#' @description Extracts a single row of image frame.
#'
#' @param x tibble containing 3 columns wih pixel coordinates i and j and pixel
#'   intensity values x.
#'
#' @param row int index of row to be extracted.
#'
#' @return tibble containing 2 columns wih column numbers i and pixel
#'   intensity values x.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-11-17 / Frt
# - `Created`    : 2019-11-17 / Frt
# - `Last test`  : 2019-11-17 / Frt
#
fun_extract_row <- function(x, row = NULL){
  
  if(row > min(unique(x$j))-1){
    if(row < max(unique(x$j))+1){
      y <- x %>% 
        filter(j==row)
    } else {
      y <- NA
    }
  } else {
    y <- NA
  }

  return(y)
  
}