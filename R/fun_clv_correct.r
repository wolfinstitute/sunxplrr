#' @title Flattens an image
#'
#' @description Flattens a solar image according to a provided clv function.
#'
#' @param x tibble containing columns with pixel coordinates i and j and columns
#'   with the pixel values of the image and the clv function.
#'
#' @param name.image name of the column containing the image pixel values.
#'   
#' @param name.clv name of the column containing the clv function values.
#'   
#' @return tibble with additional column to x containing the flattened image.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-05 / Frt
# - `Created`    : 2019-11-25 / Frt
# - `Last test`  : 2020-01-01 / Frt
#
fun_clv_correct <- function(x, name.image = "calib", name.clv = "sclv"){
  
  y <- x %>% 
    filter(fill > 0) %>% 
    select(i, j, image = eval(name.image), clv = eval(name.clv)) %>% 
    mutate(flat = image / clv) %>%
    select(i, j, flat)
    
  z <-  x %>% 
    left_join(y, by=c("i","j")) %>% 
    mutate(flat = if_else(is.na(flat),0,flat))
    
  return(z)
  
}