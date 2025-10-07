#' @title Marks all pixels above or equal a given threshold
#'
#' @description Calculates binarised pixel mask according to a given threshold 
#'   and a provided non mask pixel value.
#'
#' @param x tibble containing 3 columns with pixel coordinates i and j and pixel
#'   intensity values x.
#'
#' @param hdrlst tibble containing image FITS header keywords and values.
#'
#' @param threshold num cutoff threshold in absolute or relative units.
#'
#' @param min.mask.value num minimal pixel value in the mask. Thus, specialized 
#'   masks may be formulated for the thresholding of solar features on the disk.
#'   Binary masks used for convolutional purposes need min.mask.value = 0.
#'
#' @param method string if method = 'absolute' then the cutoff threshold value 
#'   is in ADU units. Otherwise it is the threshold / 100 fraction of the 
#'   maximal ADU value calculated as 2^bitpix.
#'
#' @return list containing tibble with 4 columns for i, j pixel coordinates, 
#'   intensity value x and binary mask th, a tibble with mask.threshold and a 
#'   tibble with mask.value.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-07 / Frt
# - `Created`    : 2019-11-17 / Frt
# - `Last test`  : 2025-10-07 / Frt
#
fun_mask_create <- function(x, hdrlst, 
                            threshold = 10,
                            min.mask.value = 0,
                            method = "relative"){
  
  if (method == "absolute"){
    
    y <- x %>% 
    mutate(th = if_else(x >= threshold, 1, min.mask.value))
    
  } else {
    
    max_intensity <- 2^as.numeric(hdrlst$BITPIX)
    threshold <- threshold / 100 * max_intensity
    
    y <- x %>% 
    mutate(th = if_else(x >= threshold, 1, min.mask.value))
    
  }
  
  z <- list(mask = y, 
            mask.threshold = threshold, 
            min.mask.value = min.mask.value)
  
  return(z)
  
}
