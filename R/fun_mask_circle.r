#' @title Determines a circle given the center coordinates and the radius
#'
#' @description Determines limb pixels of a circle with given center
#'   coordinates and radius and stores them in a mask. Optionally,  
#'   additional border pixels may be added to the circle radius.
#'
#' @param x tibble containing columns with pixel coordinates i and j.
#'
#' @param disc_center tibble with center coordinates x_i for image columns and 
#'   y_j for image rows and radius of the circle.
#'   
#' @param border.pix number of pixels to be added to the radius of the circle.
#'   
#' @return z tibble with additional column to x containing the limb pixels of 
#'   the circle.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-28 / Frt
# - `Created`    : 2019-11-23 / Frt
# - `Last test`  : 2019-12-28 / Frt
#
fun_mask_circle <- function(x, disc.center, border.pix = 0){
  
  x <- x %>% 
    mutate(x = 0)
  
  x_c <- disc.center$x_i
  y_c <- disc.center$y_j
  r   <- disc.center$r + border.pix 

  numbr.val <- 8 * r
  angle.inc <- 2 * pi / numbr.val
  angles    <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  
  for (idx in 1:length(r)) {
    xv <- cos(angles) * r[idx] + x_c
    yv <- sin(angles) * r[idx] + y_c
  }
  
  coords <- as.data.frame(list(x = xv, y = yv))
  
  coordinates <- coords %>% 
    mutate(xint = as.integer(round(x))) %>% 
    mutate(yint = as.integer(round(y))) %>% 
    mutate(limb = 1) %>% 
    select(-x, -y)
  
  limb.coords <- unique(coordinates)
  names(limb.coords) <- c("i","j","limb")
  
  x <- x %>% 
    left_join(limb.coords, by=c("i","j")) %>% 
    mutate(circle = if_else(is.na(limb),0,limb)) %>% 
    select(i,j,circle)
  
  return(x)
  
}