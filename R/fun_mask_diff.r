#' @title Marks all pixels between two given thresholds
#'
#' @description Calculates binarised pixel mask for all pixels above or equal
#'   a given lower.threshold but lower than a given upper.threshold.
#'
#' @param x tibble containing 3 columns wih pixel coordinates i and j and pixel
#'   intensity values x.
#'
#' @param hdrlst tibble containing image FITS header keywords and values.
#'
#' @param lower.threshold cutoff threshold in absolute or relative units.
#'
#' @param upper.threshold cutoff threshold in absolute or relative units.
#'
#' @param method If method = 'absolute' then the cutoff thereshold value is in
#'   ADU units. Otherwise it is the threshold / 100 fraction of the maximal ADU
#'   value given by 2^bitpix.
#'
#' @return list containing tibble with 4 columns for i, j pixel coordinates, 
#'   intensity value x, binarised mask th and tibbles with mask.lower.threshold
#'   and mask.upper.threshold values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-01-02 / Frt
# - `Created`    : 2020-01-02 / Frt
# - `Last test`  : 2020-01-02 / Frt
#
fun_mask_diff <- function(x, hdrlst, lower.threshold = 5, 
                          upper.threshold = 10, method = "relative"){
  
  if (method == "absolute"){
    y <- x %>% 
      mutate(th = if_else(x >= lower.threshold & x < upper.threshold, 1, 0))
  } else {
    max_intensity <- 2^as.numeric(hdrlst$BITPIX)
    lower.threshold <- lower.threshold / 100 * max_intensity
    upper.threshold <- upper.threshold / 100 * max_intensity
    y <- x %>% 
      mutate(th = if_else(x >= lower.threshold & x < upper.threshold, 1, 0))
  }
  
  z <- list(mask = y, mask.lower.threshold = lower.threshold, 
            mask.upper.threshold = upper.threshold)
  
  return(z)
  
}
