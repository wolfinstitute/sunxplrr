#' @title Adds a ring to the image given a theta value
#'
#' @description Computes the ring characteristics given a single theta value and
#'   adds the inner and outer ring borders to the image.
#'
#' @param x tibble containing columns with pixel coordinates i and j, pixel
#'   values x from the original image, fill from the disc mask, image from the 
#'   extracted image disc and theta.
#'   
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param theta angle measured in degrees from the disc center to the limb.
#'
#' @param num.rings number of rings counted from limb to disc center.
#'
#' @return tibble with additional column containing the image with ring borders.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-05 / Frt
# - `Created`    : 2019-12-15 / Frt
# - `Last test`  : 2019-12-15 / Frt
#
fun_ring_plt <- function(x, hdrlst, theta = 75, num.rings = 50){ 
  
  # get disc center coordinates and radius
  
  disc.center <- tibble::tibble(x_i = hdrlst$CENTER_X,
                                y_j = hdrlst$CENTER_Y,
                                  r = hdrlst$RADIUS)
  
  # get ring definitions
  
  ring <- fun_ring_get(theta = theta, num.rings = num.rings)$ring
    
  # draw inner circle 
    
  border.pix.in <- disc.center$r * (ring$radius_in - 1)
    
  disc.ring.in <- fun_mask_circle(x = x, disc.center = disc.center, 
                                  border.pix = border.pix.in)    
  
  disc.ring.in <- disc.ring.in %>% 
    mutate(ring.in = circle * (2^as.numeric(hdrlst$BITPIX)-1)) %>% 
    select(-circle)
  
  disc.ring.in <- x %>% 
    left_join(disc.ring.in, by=c("i","j")) %>% 
    mutate(ring.in = if_else(is.na(ring.in),0,ring.in))
  
  # draw outer circle
  
  border.pix.ou <- disc.center$r * (ring$radius_ou - 1)
  
  disc.ring.ou <- fun_mask_circle(x = x, disc.center = disc.center, 
                                border.pix = border.pix.ou)    
  
  disc.ring.ou <- disc.ring.ou %>% 
    mutate(ring.ou = circle * (2^as.numeric(hdrlst$BITPIX)-1)) %>% 
    select(-circle)
  
  disc.ring.ou <- x %>% 
    left_join(disc.ring.ou, by=c("i","j")) %>% 
    mutate(ring.ou = if_else(is.na(ring.ou),0,ring.ou))
  
  # join rings with image
  
  disc.ring <- fun_disc_math(im1 = disc.ring.in, values_1 = "ring.in", 
                             im2 = disc.ring.ou, values_2 = "ring.ou", 
                             method = "summ", values.name = "ring.inou")
  
  image.ring <- fun_disc_math(im1 = x, values_1 = "image", 
                              im2 = disc.ring, values_2 = "ring.inou", 
                              method = "summ", values.name = "ring.add")
  
  image.ring <- image.ring %>% 
    mutate(ring = if_else(ring.add > (2^as.numeric(hdrlst$BITPIX)-1),
                          (2^as.numeric(hdrlst$BITPIX)-1),ring.add)) %>% 
    select(-ring.inou,-ring.add)
 
  return(image.ring)
    
}