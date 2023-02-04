#' @title Centers the provided image
#'
#' @description Centers the provided image by shifting the x-axis and the 
#'   y-axis of the image appropriately. Do not center SDO images, since they
#'   are centered already. The algorithm is slightly adapted from
#'   Berry, R. and Burnell, J.: The Handbook of Astronomical Image Processing, 
#'   2nd edition 2005, p. 323.
#'
#' @param image tibble image with i and j pixel coordinates and pixel values x. 
#'   Additional columns are ignored and will be not returned. 
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param sdo.image boolean switch for dummy use in the case of non SDO calcium
#'   images.
#'
#' @return tibble with centered image.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-12-30 / Frt
# - `Last test`  : 2019-12-31 / Frt
#
fun_center_image <- function (image, hdrlst, header, sdo.image = "FALSE"){
  
  # dummy use for non sdo calcium images allowed
  
  if (sdo.image){
    
    z <- image
    
    return(z)
    
  } else {
    
    xc = as.numeric(hdrlst$NAXIS1) / 2
    yc = as.numeric(hdrlst$NAXIS2) / 2
      
    x_c <- hdrlst$CENTER_X
    y_c <- hdrlst$CENTER_Y
      
    xt <- xc - x_c
    yt <- yc - y_c
      
    xmax <- max(image$i) + 1
    ymax <- max(image$j) + 1
      
    centered_image <- image %>%
      mutate(xi = as.integer(i - xt)) %>%
      mutate(yj = as.integer(j - yt)) %>%
      filter(xi > 0) %>% 
      filter(xi < xmax) %>% 
      filter(yj > 0) %>% 
      filter(yj < ymax) %>% 
      select(-i, -j, xi, yj, x) %>% 
      select(i=xi, j=yj, x)
      
    z <-  image %>% 
      select(i,j) %>% 
      left_join(centered_image, by=c("i","j")) %>% 
      mutate(x = if_else(is.na(x),0,x))
    
    return(z)
      
  }
  
}