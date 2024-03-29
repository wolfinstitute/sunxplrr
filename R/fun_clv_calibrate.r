#' @title Calibrates the image
#'
#' @description Calibrate the image by adding the rescaled fitted degree-5 clv 
#'   function to the residuals from the multi-parameter fit of the image. 
#' 
#' @param x tibble containing columns with pixel coordinates i and j, pixel
#'   values x from the original image, fill from the disc mask, sclv and 
#'   residuals from the sclv image.
#'
#' @param sdo.image boolean switch for dummy use in the case of non sdo calcium
#'   images.
#'
#' @return tibble containing the calibrated image values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-07 / Frt
# - `Created`    : 2019-12-16 / Frt
# - `Last test`  : 2020-01-07 / Frt
#
fun_clv_calibrate <- function(x, sdo.image = "FALSE"){
  
  # add fit residuals to the sclv function
  
  if (sdo.image){
    y <- x %>%
    filter(.data$fill > 0) %>%
    mutate(calib = .data$clv + .data$resid) %>% 
    select("i","j","calib")
  } else {
    y <- x %>%
      filter(.data$fill > 0) %>%
      mutate(calib = .data$sclv + .data$resid) %>% 
      select("i","j","calib")
  }
  
  # join with original image
    
  z <-  x %>% 
    left_join(y, by=c("i","j")) %>% 
    mutate(calib = if_else(is.na(.data$calib),0,.data$calib)) %>% 
    mutate(calib = if_else(.data$calib < 0,0,.data$calib))
  
  # return
  
  return(z)
  
}