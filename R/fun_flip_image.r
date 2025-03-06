#' @title Flips the provided image
#'
#' @description Flips the provided image by reversing the y-axis of the image 
#'   (interchanging up and down). Do not flip if flag is set. Algorithm from
#'   Berry, R. and Burnell, J.: The Handbook of Astronomical Image Processing, 
#'   2nd edition 2005, p. 331f.
#'
#' @param image tibble image with i and j pixel coordinates and pixel values x.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param flip.image boolean if TRUE the image will be flipped.
#'
#' @return tibble with flipped image, updated hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-03-02 / Frt
# - `Created`    : 2019-12-30 / Frt
# - `Last test`  : 2025-03-02 / Frt
#
fun_flip_image <- function (image, hdrlst, header, flip.image = "FALSE"){

  # flips image
  
  if (flip.image){
 
    ymax <- max(image$j) + 1
      
    flipped_image <- image %>% 
      mutate(yj = as.integer(ymax - j)) %>% 
      select(-j, yj, i, x) %>% 
      select(i, j=yj, x)
      
    hdrlst$GEOTRANS <- "flipped"
    cimages <- addHistory("  Image flipped with sunxplrr::fun_flip_image",
                              header)
    header <- cimages
      
    z <- list(flipped_image = flipped_image, hdrlst = hdrlst, header = header)
      
    return(z)
    
  }
  
  # dummy use if no flip is needed 
      
  z <- list(flipped_image = image, hdrlst = hdrlst, header = header)
      
  return(z)
  
}