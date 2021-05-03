#' @title Determines heliografic coordinates
#'
#' @description Determines heliografic coodinates, including the pixel 
#'   coordinates from disc center, the angle theta, measured in degrees from the 
#'   disc center to each disc pixel. For limb pixels theta = 90°. Provided are 
#'   also the radius r, the angle phi, the heliografic latitude B, the 
#'   heliografic longitude l and L as the Carrington rotation number for each
#'   pixel.
#'
#' @param x tibble containing columns with pixel coordinates i and j and 
#'   additional columns which were all passed to the output tibble z.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @return z tibble with additional columns to x containing the heliografic 
#'   coordinate information.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-01-06 / Frt
# - `Created`    : 2019-12-21 / Frt
# - `Last test`  : 2020-01-06 / Frt
#
fun_disc_coordinates <- function(x, hdrlst){
  
  x_c <- hdrlst$CENTER_X
  y_c <- hdrlst$CENTER_Y
  r_d <- hdrlst$RADIUS
  
  P0 <- hdrlst$SOLAR_P0
  B0 <- hdrlst$SOLAR_B0
  L0 <- hdrlst$SOLAR_L0
  CR <- hdrlst$CAR_ROT
  
  y <- suppressWarnings(x %>% 
    filter(fill > 0) %>% 
    mutate(xi = i - x_c) %>% 
    mutate(yj = j - y_c) %>%
    mutate(r0 = if_else(sqrt(xi^2 + yj^2) / r_d >= 1, 
                        r_d, sqrt(xi^2 + yj^2))) %>% 
    mutate(theta = if_else(is.nan(asin(r0 / r_d) * 180 / pi), 
                                  asin(1) * 180 / pi,
                                  asin(r0 / r_d) * 180 / pi)) %>%
    mutate(r = if_else(sqrt(xi^2 + yj^2) / r_d >= 1, 
                       r_d, sqrt(xi^2 + yj^2))) %>% 
    mutate(phi = atan2(yj, xi) * 180 / pi) %>% 
    mutate(B = asin(cos(theta * pi / 180) * sin(B0 * pi / 180) + 
      sin(theta * pi / 180) * cos(B0 * pi / 180) * sin((phi - P0) * pi / 180)) * 
      180 / pi) %>%
    mutate(l = asin((cos((phi - P0) * pi / 180) * sin(theta * pi / 180)) / 
                    (cos(B * pi / 180))) * 180 / pi) %>% 
    mutate(L1 = L0 + l) %>%
    mutate(L = if_else(L1 < 0, L1 + 360, if_else(L1 > 360, L1 - 360, L1))) %>% 
    mutate(cr = if_else(L1 < 0, CR + 1,if_else(L1 > 360, CR - 1, CR))) %>% 
    select(-x, -border, -circle, -fill, -image, -r0, -L1))
    
  z <-  x %>% 
    left_join(y, by=c("i","j")) %>% 
    mutate(xi = if_else(is.na(xi),0,xi)) %>% 
    mutate(yj = if_else(is.na(yj),0,yj)) %>% 
    mutate(theta = if_else(is.na(theta),0,theta)) %>%
    mutate(r = if_else(is.na(r),0,r)) %>% 
    mutate(phi = if_else(is.na(phi),0,phi)) %>% 
    mutate(B = if_else(is.na(B),0,B)) %>% 
    mutate(l = if_else(is.na(l),0,l)) %>% 
    mutate(L = if_else(is.na(L),0,L)) %>%
    mutate(cr = if_else(is.na(cr),0,cr)) 
  
  return(z)
  
}