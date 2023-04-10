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
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-04-09 / Frt
# - `Created`    : 2019-11-23 / Frt
# - `Last test`  : 2019-12-08 / Frt
#
fun_mask_border <- function(x, mask = "th"){
  
  msk <- x[,mask]
  
  yi <- x %>% 
    filter(msk == 1) %>% 
    group_by(i) %>% 
    summarize(min_j = min(j), max_j = max(j))
  
  yj <- x %>% 
    filter(msk == 1) %>% 
    group_by(j) %>% 
    summarize(min_i = min(i), max_i = max(i))
  
  z <- x %>% 
    left_join(yj, by = c("j")) %>% 
    mutate(borde0j = if_else(abs(i - min_i) < 1 | abs(i - max_i) < 1, 1, 0)) %>% 
    mutate(borderj = if_else(is.na(borde0j) == TRUE, 0, borde0j)) %>% 
    left_join(yi, by = c("i")) %>% 
    mutate(borde0i = if_else(abs(j - min_j) < 1 | abs(j - max_j) < 1, 1, 0)) %>% 
    mutate(borderi = if_else(is.na(borde0i) == TRUE, 0, borde0i)) %>% 
    mutate(border = if_else(borderi == 1 | borderj == 1, 1, 0)) %>% 
    select(-min_i,-max_i,-borde0j,-borderj,-min_j,-max_j,-borde0i,-borderi)
  
  return(z)
  
}