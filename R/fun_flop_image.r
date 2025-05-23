#' @title Flops the provided image
#'
#' @description Flops the provided image by reversing the x-axis of the image 
#'   (interchanging left and right side). Do not flop if flag is set. The 
#'   algorithm is from Berry, R. and Burnell, J.: The Handbook of Astronomical
#'   Image Processing, 2nd edition 2005, p. 332.
#'
#' @param image tibble image with i and j pixel coordinates and pixel values x.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param flop.image boolean if TRUE the image will be flopped.
#'
#' @return tibble with flopped image, updated hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-03-02 / Frt
# - `Created`    : 2019-12-30 / Frt
# - `Last test`  : 2025-03-02 / Frt
#
fun_flop_image <- function (image, hdrlst, header, flop.image = "FALSE"){
  
  # flops image
  
  if (flop.image){
    
    xmax <- max(image$i) + 1
      
    flopped_image <- image %>% 
      mutate(xi = as.integer(xmax - i)) %>% 
      select(-i, xi, j, x) %>% 
      select(i=xi, j, x)
      
    hdrlst$GEOTRANS <- "flopped"
    cimages <- addHistory("  Image flopped with sunxplrr::fun_flop_image",
                            header)
    header <- cimages
      
    z <- list(flopped_image = flopped_image, hdrlst = hdrlst, header = header)
      
    return(z)
      
  }
  
  # dummy use if no flop is needed 
  
  z <- list(flopped_image = image, hdrlst = hdrlst, header = header)
  
  return(z)
  
}