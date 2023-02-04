#' @title Extracts a single column of image frame
#'
#' @description Extracts a single column of image frame.
#'
#' @param x tibble containing 3 columns with pixel coordinates i and j and pixel
#'   intensity values x.
#'
#' @param col int index of column to be extracted.
#'
#' @return tibble containing 2 columns with row numbers j and pixel
#'   intensity values x.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-11-17 / Frt
# - `Last test`  : 2019-11-17 / Frt
#
fun_extract_col <- function(x, col = NULL){
  
  if(col > min(unique(x$i))-1){
    if(col < max(unique(x$i))+1){
      y <- x %>% 
        filter(i==col)
    } else {
      y <- NA
    }
  } else {
    y <- NA
  }
  
  return(y)
  
}