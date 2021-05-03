#' @title Calculates disc center coordinates and radius of the indicator disc
#'
#' @description Calculates disc center coordinates and the radius for a given
#'   disc indicator.
#'
#' @param x tibble containing at least 3 columns wih pixel coordinates i and j
#'   and disc indicator values disk.
#'
#' @param disc name of variable in x with disc indicator values.
#' 
#' @return tibble with intensity center coordinates x_i for image columns 
#'   and y_j for image rows and radius of the disc indicator circle.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-11-24 / Frt
# - `Created`    : 2019-11-17 / Frt
# - `Last test`  : 2019-11-24 / Frt
#
fun_disc_center <- function(x, disc = "fill"){
  
  y <- cbind(x %>% select(i, j), x[,disc])
  names(y) <- c("i","j","th")
  
  z <- y %>% 
    mutate(i_th = i * th, j_th = j * th) %>% 
    summarize(sum_i_th = sum(i_th), sum_j_th = sum(j_th), sum_th = sum(th))
  
  disc_center <- z %>% 
    mutate(x_i = z$sum_i_th / z$sum_th) %>%   # Column sum
    mutate(y_j = z$sum_j_th / z$sum_th) %>%   # Row sum
    mutate(r = sqrt(z$sum_th / pi)) %>%       # Column sum
    select(x_i, y_j, r)
  
  return(disc_center)
  
}