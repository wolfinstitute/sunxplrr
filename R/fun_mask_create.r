#' @title Marks all pixels above or equal a given threshold
#'
#' @description Calculates binarised pixel mask according to a given thereshold.
#'
#' @param x tibble containing 3 columns wih pixel coordinates i and j and pixel
#'   intensity values x.
#'
#' @param hdrlst tibble containing image FITS header keywords and values.
#'
#' @param threshold cutoff threshold in absolute or relative units.
#'
#' @param method If method = 'absolute' then the cutoff thereshold value is in
#'   ADU units. Otherwise it is the threshold / 100 fraction of the maximal ADU
#'   value given by 2^bitpix.
#'
#' @return list containing tibble with 4 columns for i, j pixel coordinates, 
#'   intensity value x and binary mask th and tibble with mask.threshold value.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-08/ Frt
# - `Created`    : 2019-11-17 / Frt
# - `Last test`  : 2019-12-08 / Frt
#
fun_mask_create <- function(x, hdrlst, threshold = 10, method = "relative"){
  
  if (method == "absolute"){
    y <- x %>% 
      mutate(th = if_else(x >= threshold, 1, 0))
  } else {
    max_intensity <- 2^as.numeric(hdrlst$BITPIX)
    threshold <- threshold / 100 * max_intensity
    y <- x %>% 
      mutate(th = if_else(x >= threshold, 1, 0))
  }
  
  z <- list(mask = y, mask.threshold = threshold)
  
  return(z)
  
}
