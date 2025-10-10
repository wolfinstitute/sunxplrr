#' @title Rotates image
#'
#' @description Rotates jpg image about -90 degrees.
#'
#' @param image tibble image with i and j pixel coordinates and pixel values x.
#'
#' @param rotate.jpg boolean if TRUE the image will be rotated -90 degrees.
#'
#' @return tibble image tibble.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#' 
#' @export

# - `Last change`: 2025-10-10 / Frt
# - `Created`    : 2025-10-10 / Frt
# - `Last test`  : 2025-10-10 / Frt
#
fun_rotate_jpg <- function (image, rotate.jpg = "FALSE"){
  
  if (rotate.jpg){
  
    fitsim <- image |> 
      select(i=j, j=i, x)
    
    ymax <- max(fitsim$j) + 1
    
    fitsim <- fitsim |>  
      mutate(yj = as.integer(ymax - j)) |>   
      select(-j, yj, i, x) |> 
      select(i, j=yj, x)
    
    z <- fitsim
    
    return(z)
    
  }
  
  # dummy use if no rotation is needed 
  
  z <- image
  
  return(z)

}
