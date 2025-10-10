#' @title Corrects gamma of an image
#'
#' @description Corrects gamma of the provided image.
#'
#' @param image tibble image with i and j pixel coordinates and pixel values x.
#'
#' @param gamma num gamma correction factor. For flat SDO images a gamma of 1.5
#'  is recommended.
#'
#' @param correct.gamma boolean if TRUE the image will be gamma corrected.
#'
#' @return tibble image tibble.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#' 
#' @export

# - `Last change`: 2025-10-10 / Frt
# - `Created`    : 2025-05-20 / Frt
# - `Last test`  : 2025-10-10 / Frt
#
fun_correct_gamma <- function (image, gamma = 1.5, correct.gamma = "FALSE"){
  
  if (correct.gamma){
  
    # corrects gamma
    fitsim <- image |>
      mutate(x = x^(1/gamma))
  
    z <- fitsim
    
    return(z)
    
  }
  
  # dummy use if no gamma correction is needed 
  
  z <- image
  
  return(z)

}
