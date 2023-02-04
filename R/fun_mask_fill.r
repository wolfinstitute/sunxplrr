#' @title Fills out a given mask
#'
#' @description Fills out a given mask within their biggest contours.
#'
#' @param x tibble containing columns with pixel coordinates i and j and mask.
#'
#' @param mask name of the column in x containing the mask.
#'
#' @return z tibble with additional column to x containing the filled mask.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-11-23 / Frt
# - `Last test`  : 2019-12-08 / Frt
#
fun_mask_fill <- function(x, mask = "th"){
  
  msk <- x[,mask]
  
  y <- x %>% 
    filter(msk == 1) %>% 
    group_by(j) %>% 
    summarize(min_i = min(i), max_i = max(i))
  
  z <- x %>% 
    left_join(y, by = c("j")) %>% 
    mutate(fil0 = if_else(i >= min_i & i <= max_i, 1, 0)) %>% 
    mutate(fill = if_else(is.na(fil0) == TRUE, 0, fil0)) %>% 
    select(-min_i,-max_i,-fil0)
  
  return(z)
  
}