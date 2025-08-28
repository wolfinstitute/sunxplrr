#' @title Corrects gamma of an image
#'
#' @description Corrects gamma and returns an image with the provided 
#'  bit depth.
#'
#' @param image tibble image with i and j pixel coordinates and pixel values x.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param gamma num gamma correction factor. For flat SDO images a gamma of 1.5
#'  is recommended.
#'
#' @param correct.gamma boolean if TRUE the image will be gamma corrected.
#'
#' @return list with image tibble, parsed header and full header vectors.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#' 
#' @export

# - `Last change`: 2025-05-20 / Frt
# - `Created`    : 2025-05-20 / Frt
# - `Last test`  : 2025-05-20 / Frt
#
fun_correct_gamma <- function (image, hdrlst, header, gamma = 1.5, 
                               correct.gamma = "FALSE"){
  
  if (correct.gamma){
  
    bitpix <- hdrlst$BITPIX
  
    # Scales image intensity values
    fitsim <- image |> 
      mutate(x = as.integer(x / (2^bitpix - 1)))
  
    # Corrects gamma
    fitsim <- fitsim |>
      mutate(x = x^(1/gamma))
  
    # Scales image intensity values
    fitsim <- fitsim |> 
      mutate(x = as.integer(x * (2^bitpix - 1)))
 
    z <- list(image = fitsim, hdrlst = hdrlst, header = header)
    
    return(z)
    
  }
  
  # dummy use if no gamma correction is needed 
  
  z <- list(image = image, hdrlst = hdrlst, header = header)
  
  return(z)

}
