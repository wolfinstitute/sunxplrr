#' @title Transforms a tibble or a data.frame to a matrix
#'
#' @description Transforms a tibble or a data.frame to a matrix.
#'
#' @param x tibble with 3 columns for i, j pixel coordinates and intensity 
#'   value x.
#'
#' @return matrix with i columns and j rows.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2020-02-04 / Frt
# - `Created`    : 2019-11-15 / Frt
# - `Last test`  : 2019-11-15 / Frt
#
fun_tibbl2mat <- function(x){
  
  y <- x %>% 
    tidyr::spread(key = "j", value = "x") %>% 
    select(-i)
  
  y <- as.matrix(y)
  colnames(y) <- NULL
  
  return(y)
  
}