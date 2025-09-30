#' @title Extracts central part of image without text in the corners
#'
#' @description If the original image contains additional text in the image 
#'   corners, the approximate center coordinates and the approximate radius of 
#'   the image disc are estimated and the image disk is extracted with an 
#'   oversized mask. The size of the original image and the number of bits per 
#'   pixel are taken from the FITS header provided by hdrlst. 
#'
#' @param x tibble containing 3 columns with pixel coordinates i and j and pixel
#'   intensity values x.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param cut.threshold cutoff threshold for images with text in the corners 
#'   in absolute or relative units.
#'
#' @param cut.method If cut.method = 'absolute' then the cutoff threshold value
#'   for images with text in the corners is in ADU units. Otherwise it is the
#'   cut.threshold / 100 fraction of the maximal ADU value given by 2^bitpix.
#'   
#' @param cut.border.pix number of pixels to be added to the radius of the 
#'   circle to extract the image without the text in the corners.
#'   
#' @return list with disc.image, hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2025-09-30 / Frt
# - `Created`    : 2019-12-03 / Frt
# - `Last test`  : 2025-09-30 / Frt
#
fun_cut_image <- function(x, hdrlst, header, 
                          cut.threshold = 20, cut.method = "relative",
                          cut.border.pix = 100){

  if (hdrlst$CUTIMAGE == "TRUE"){
    
    # extract middle row of original image
  
    cut.im.row <- fun_extract_row(x, row = round(as.numeric(hdrlst$NAXIS2) / 2))
  
    # find mask borders
  
    oversized.mask <- fun_mask_create(cut.im.row, hdrlst = hdrlst,
                                      threshold = cut.threshold, 
                                      method = cut.method)
  
    cut.im.mask <- oversized.mask$mask
    
    # find approximate disc center coordinates and radius
  
    cut.disc.center <- NULL
  
    cut.im.mask.stat <- cut.im.mask %>% 
      filter(th == 1) %>% 
      group_by(j) %>% 
      summarize(min_i = min(i), max_i = max(i))
  
    cut.disc.center$x_i <- 
      round((cut.im.mask.stat$min_i + cut.im.mask.stat$max_i) / 2)
    cut.disc.center$y_j <- cut.im.mask.stat$j
    cut.disc.center$r   <- 
      round((cut.im.mask.stat$max_i - cut.im.mask.stat$min_i) / 2)
  
    # draw oversized circle with known center coordinates and radius
  
    cut.disc.border <- fun_mask_circle(x, disc.center = cut.disc.center, 
                                       border.pix = cut.border.pix)
  
    # fill oversized circle
  
    cut.disc.mask <- fun_mask_fill(cut.disc.border, mask = "circle")
  
    # extract oversized disc from original image
  
    cut.x <- fun_disc_math(x, values_1 = "x", cut.disc.mask, 
                              values_2 = "fill", method = "mult",
                              values.name = "image")
   
    x <- cut.x %>% 
      mutate(i = as.integer(i)) %>% 
      mutate(j = as.integer(j)) %>% 
      select(i, j, x = image)
  
  }
  
  # return
  
  return(x)
  
}