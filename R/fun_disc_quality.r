#' @title Calculates the image quality as standard error of the border pixel
#'
#' @description Calculates the image quality as standard deviation of the 
#'   individual disc radius determinations of each border pixel expressed both 
#'   in pixels and in arcseconds.
#'
#' @param x tibble containing at least 3 columns with pixel coordinates i and j
#'   and border indicator values.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param disc.center tibble with coordinates x_i for image columns and 
#'   y_j for image rows of the disc center and radius r of the disc.
#'   
#' @param border name of variable in x with border indicator values.
#' 
#' @return tibble data containing for each border pixel the calculated radius 
#'   and the residuals against the provided disc radius. Furthermore a
#'   tibble sigma.border including the mean radius, the standard deviation 
#'   of the mean, the standard deviation of a single measurement in pixels and 
#'   in arcseconds.
#' 
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-04-10 / Frt
# - `Created`    : 2023-04-09 / Frt
# - `Last test`  : 2023-04-10 / Frt
#
fun_disc_quality <- function(x, hdrlst, disc.center, border = "border"){

  data <- x %>% 
    select(i, j, border) %>% 
    filter(border == 1) %>% 
    mutate(
      r = sqrt((i - 0.5 - disc.center$x_i)^2 + (j - 0.5 - disc.center$y_j)^2),
      res = r - disc.center$r)
    
  
  sigma.border <- data %>% 
    summarise(r.mean = mean(r),
              sd.r.mean = sqrt(var(r)/(n())), 
              sd.pix = sd(r),
              sd.arcsec = sd(r) * hdrlst$SOLAR_R / (2 * disc.center$r))
  
  return(list(data = data, sigma.border = sigma.border))
  
}