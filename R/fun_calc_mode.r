#' @title Calculates the mode of the provided data vector
#'
#' @description Calculates the mode (most frequent value) of the provided data
#'   vector.
#'
#' @param x data vector.
#' 
#' @return mode value.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-01-02 / Frt
# - `Created`    : 2020-01-02 / Frt
# - `Last test`  : 2020-01-02 / Frt
#
fun_calc_mode <- function(x) {
  
  y <- unique(x)
  
  z <- y[which.max(tabulate(match(x, y)))]

  return(z)
  
}