#' @title Assigns ring number to disc pixels
#'
#' @description Computes a table with the ring borders given the number of rings 
#'   and assigns the number of the ring for each disc pixel.
#'
#' @param x tibble containing columns with pixel coordinates i and j and pixel
#'   values x from the original image.
#'   
#' @param num.rings number of rings counted from limb to disc center.
#'
#' @return z tibble with additional column to x containing the ring number.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-05 / Frt
# - `Created`    : 2019-12-14 / Frt
# - `Last test`  : 2019-12-15 / Frt
#
fun_disc_rings <- function(x, num.rings = 50){

  # table with ring definitions depending on theta 

  tab <- tibble::tibble(i = as.double(seq(1:num.rings))) %>% 
    mutate(mu = cumsum((sqrt(i) - sqrt(i-1)) / sqrt(num.rings))) %>% 
    mutate(theta = acos(mu) * 180 / pi) %>% 
    mutate(lag_theta = if_else(is.na(lag(theta)),90,lag(theta)))
  
  # for disc pixels select coordinates and theta
  
  y <- x %>% 
    filter(fill > 0) %>% 
    select(i,j,theta) 

  # assign ring number to disc pixels 
  
  rings <- NULL
  for (k in 1:nrow(y)){
    if (y$theta[k] == 0)
      rings[k] <- 50
    else
      rings[k] <- tab$i[(y$theta[k] > tab$theta & y$theta[k] <= tab$lag_theta)]
  }
  
  y <- cbind(y, rings) %>% 
    select(-theta)
  
  # join to original image
  
  z <-  x %>% 
    left_join(y, by=c("i","j")) %>% 
    mutate(rings = if_else(is.na(rings),0,rings))
    
  return(z)
  
}