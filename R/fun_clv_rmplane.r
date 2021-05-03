#' @title Subtracts plane from image
#'
#' @description Subtracts a previously fitted plane from the provided image
#'   values. 
#' 
#' @param x tibble containing columns with pixel coordinates i and j, pixel
#'   values x from the original image, fill from the disc mask, image from the 
#'   extracted image disc and theta.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param coeff containing the previously fitted coefficients of an 
#'   eight-parameter fit function.
#'   
#' @return tibble containing the corrected image values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-12-01 / Frt
# - `Created`    : 2019-12-16 / Frt
# - `Last test`  : 2020-12-01 / Frt
#
fun_clv_rmplane <- function(x, hdrlst, coeff){
  
  # extract plane coefficients from an eight-parameter fit function vector 
  
  g <- as.numeric(coeff[2])
  h <- as.numeric(coeff[3])

  # subtract plane from image values
  
  y <- x %>%
    filter(fill > 0) %>%
    mutate(flatfield = image - (g*(i - hdrlst$CENTER_X) - 
                                h*(j - hdrlst$CENTER_Y))) %>% 
    select(i,j,flatfield)
  
  # join with original image
    
  z <-  x %>% 
    left_join(y, by=c("i","j")) %>% 
    select(-image) %>% 
    mutate(image = if_else(is.na(flatfield),0,flatfield)) %>% 
    mutate(image = if_else(image < 0,0,image)) %>% 
    select(-flatfield)
  
  # return
  
  return(z)
  
}