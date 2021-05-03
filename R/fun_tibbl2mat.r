#' @title Umwandlung einer tibble oder eines data.frames in eine Matrix
#'
#' @description Wandelt ein tibble oder ein data.frame in eine Matrix um.
#'
#' @param x tibble with 3 columns for i, j pixel coordinates and intensity 
#'   value x.
#'
#' @return matrix with i columns and j rows.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-11-15 / Frt
# - `Created`    : 2019-11-15 / Frt
# - `Last test`  : 2019-11-15 / Frt
#
fun_tibbl2mat <- function(x){
  
  y <- x %>% 
    spread(key = "j", value = "x") %>% 
    select(-i)
  
  y <- as.matrix(y)
  colnames(y) <- NULL
  
  return(y)
  
}