#' @title Marks the border pixels of a given mask
#'
#' @description Marks the border pixels of a given mask.
#'
#' @param x tibble containing columns with pixel coordinates i and j and mask.
#'
#' @param mask name of the column in x containing the mask.
#'
#' @return z tibble with additional column to x containing the border pixels.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-08 / Frt
# - `Created`    : 2019-11-23 / Frt
# - `Last test`  : 2019-12-08 / Frt
#
fun_mask_border <- function(x, mask = "th"){
  
  msk <- x[,mask]
  
  y <- x %>% 
    filter(msk == 1) %>% 
    group_by(j) %>% 
    summarize(min_i = min(i), max_i = max(i))
  
  z <- x %>% 
    left_join(y, by = c("j")) %>% 
    mutate(borde0 = if_else(abs(i - min_i) < 1 | abs(i - max_i) < 1, 1, 0)) %>% 
    mutate(border = if_else(is.na(borde0) == TRUE, 0, borde0)) %>% 
    select(-min_i,-max_i,-borde0)
  
  return(z)
  
}