#' @title Flips the provided image
#'
#' @description Flips the provided image by reversing the y-axis of the image 
#'   (interchanging up and down). Do not flip for sdo images. Algorithm from
#'   Berry, R. and Burnell, J.: The Handbook of Astronomical Image Processing, 
#'   2nd edition 2005, p. 331f.
#'
#' @param image tibble image with i and j pixel coordinates and pixel values x.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param sdo.image boolean switch for dummy use in the case of non sdo calcium
#'   images.
#'
#' @return tibble with flipped image, updated hdrlst and header.
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
fun_flip_image <- function (image, hdrlst, header, sdo.image = "FALSE"){

  # dummy use for non sdo calcium images allowed
  
  if (sdo.image){
    
    z <- list(flipped_image = image, hdrlst = hdrlst, header = header)
    
    return(z)
    
  } else {
  
    if (!is.null(hdrlst$FLIPSTAT)){
      
      ymax <- max(image$j) + 1
      
      flipped_image <- image %>% 
        mutate(yj = as.integer(ymax - j)) %>% 
        select(-j, yj, i, x) %>% 
        select(i, j=yj, x)
      
      hdrlst$GEOTRANS <- "flipped"
      cimages <- addHistory("  Image flipped with sunviewr::fun_flip_image",
                              header)
      header <- cimages
      
      z <- list(flipped_image = flipped_image, hdrlst = hdrlst, header = header)
      
      return(z)
      
    } else {
      
      # dummy use if no flip is needed 
      
      z <- list(flipped_image = image, hdrlst = hdrlst, header = header)
      
      return(z)
      
    }
  
  }
  
}