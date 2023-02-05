#' @title Returns ring parameters given a theta value
#'
#' @description Computes a table with the ring borders given the number of rings 
#'   and returns the ring characteristics given a single theta value.
#'
#' @param theta angle measured in degrees from the disc center to the limb.
#'
#' @param num.rings number of rings counted from limb to disc center.
#'
#' @return table and a vector with ring characteristic for a given theta value.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-05 / Frt
# - `Created`    : 2019-12-15 / Frt
# - `Last test`  : 2019-12-15 / Frt
#
fun_ring_get <- function(theta = 75, num.rings = 50){ 
    
    tab <- tibble::tibble(ring = seq(1:num.rings)) %>% 
      mutate(delta.mu = (sqrt(ring) - sqrt(ring-1)) / sqrt(num.rings)) %>% 
      mutate(mu_in = cumsum(delta.mu)) %>% 
      mutate(mu_ou = if_else(is.na(lag(mu_in)),0,lag(mu_in))) %>% 
      mutate(theta_in = acos(mu_in) * 180 / pi) %>% 
      mutate(theta_ou = if_else(is.na(lag(theta_in)),90,lag(theta_in))) %>% 
      mutate(radius_in = sin(theta_in * pi / 180)) %>% 
      mutate(radius_ou = if_else(is.na(lag(radius_in)),1,lag(radius_in))) 
      
    i <- tab[(theta > tab$theta_in & theta <= tab$theta_ou),]
    
    return(list(ring_table = tab, ring = i))
    
}