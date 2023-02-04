#' @title Adds grid to an image
#'
#' @description Adds grid to an image, both provided by the same tibble.
#'
#' @param x tibble containing columns with pixel coordinates i and j, pixel
#'   values x from the image and a binarised mask with the matching grid.
#'   
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @return tibble with additional column containing the image with ring borders.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-12-31 / Frt
# - `Last test`  : 2019-12-31 / Frt
#
fun_grid_plt <- function(x, hdrlst){ 
  
  # extract image
  
  image <- x %>% 
    select(i, j, image)
  
  # extract grid
  
  grid <- x %>% 
    mutate(plt = grid * (2^as.numeric(hdrlst$BITPIX)-1)) %>% 
    select(i, j, grid = plt)
  
  # join grid with image
  
  image.grid <- fun_disc_math(im1 = image, values_1 = "image", 
                             im2= grid, values_2 = "grid", 
                             method = "summ", values.name = "image.grid")
  
  image.grid <- image.grid %>% 
    mutate(image.grid = if_else(image.grid > (2^as.numeric(hdrlst$BITPIX)-1),
                          (2^as.numeric(hdrlst$BITPIX)-1), image.grid)) %>% 
    select(i, j, image.grid)
  
  z <-  x %>% 
    left_join(image.grid, by=c("i","j"))

 
  return(z)
    
}