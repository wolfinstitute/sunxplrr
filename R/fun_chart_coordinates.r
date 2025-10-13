#' @title Determines heliographic chart coordinates
#'
#' @description Determines of each entry of a calculated chart.frame the 
#'   i, j pixel coordinates in the provided image frames.
#'
#' @param x tibble containing columns with pixel coordinates i and j and 
#'   additional columns which were all passed to the output tibble z.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @return z tibble sorted by the calculated coordinate values according to the 
#'   chart.frame with the corresponding i, j pixel coordinates and the joined 
#'   columns of x.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-12 / Frt
# - `Created`    : 2019-12-21 / Frt
# - `Last test`  : 2025-10-12 / Frt
#
fun_chart_coordinates <- function(x, hdrlst, chart.frame){
  
  # Solar disc info from hdrlst 
  
  x_c <- hdrlst$CENTER_X
  y_c <- hdrlst$CENTER_Y
  r_d <- hdrlst$RADIUS
  
  P0  <- hdrlst$SOLAR_P0
  B0  <- hdrlst$SOLAR_B0
  
  # calculates for each pixel in the chart frame the i, j coordinates in x
  
  chart.frame.names <- names(chart.frame)
  
  chart.frame <- chart.frame
  names(chart.frame) <- c("longitude", "latitude")
  
  y <- chart.frame %>% 
    mutate(xi = r_d * cos(latitude * pi / 180) * sin(longitude * pi / 180) * 
           cos(P0 * pi / 180) - r_d * (sin(latitude * pi / 180) * 
           cos(B0 * pi / 180) - cos(latitude * pi / 180) * sin(B0 * pi / 180) * 
           cos(longitude * pi / 180)) * sin(P0 * pi / 180)) %>% 
    mutate(yj = r_d * cos(latitude * pi / 180) * sin(longitude * pi / 180) * 
           sin(P0 * pi / 180) + r_d * (sin(latitude * pi / 180) * 
           cos(B0 * pi / 180) - cos(latitude * pi / 180) * sin(B0 * pi / 180) * 
           cos(longitude * pi / 180)) * cos(P0 * pi / 180)) %>% 
    mutate(i = as.integer(xi + x_c)) %>% 
    mutate(j = as.integer(yj + y_c)) %>% 
    select(longitude, latitude, i, j)
  
  names(y) <- c(chart.frame.names, "i", "j")
  
  z <-  y %>% 
    left_join(x, by=c("i","j"))

  return(z)
  
}