#' @title Calculates grid of heliografic longitudes and latitudes
#'
#' @description Calculates positions of grid pixels for heliografic longitudes
#'   and latitudes in intervals of 10 degrees.
#'
#' @param x tibble containing columns with pixel coordinates i and j and 
#'   additional columns which were all passed to the output tibble z.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param grid_each_deg num number of degrees between grid lines. Default value
#'   is one grid line every 10 heliografic degrees.
#'   
#' @param res_each_deg_on_grid num indicates the resolution of points of one 
#'   degree along the grid lines. Default is 0.01, e.g. 100 points per degree.
#'
#' @return z tibble with additional column to x containing the flag for grid
#'   pixels.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-31 / Frt
# - `Created`    : 2019-12-29 / Frt
# - `Last test`  : 2019-12-31 / Frt
#
fun_disc_grid <- function(x, hdrlst, grid_each_deg = 10, 
                          res_each_deg_on_grid = 0.01){
  
  x_c <- hdrlst$CENTER_X
  y_c <- hdrlst$CENTER_Y
  r_d <- hdrlst$RADIUS
  
  P0  <- hdrlst$SOLAR_P0
  B0  <- hdrlst$SOLAR_B0
  
  lo <- tibble(l=rep(seq(-90,90, by = res_each_deg_on_grid), 
                     each = ((180/grid_each_deg) + 1)), 
               B=rep(seq(-90,90, by = grid_each_deg), 
                     ((180/res_each_deg_on_grid) + 1)))
  
  la <- tibble(l=rep(seq(-90,90, by = grid_each_deg), 
                     each = ((180/res_each_deg_on_grid) + 1)), 
               B=rep(seq(-90,90, by = res_each_deg_on_grid), 
                     ((180/grid_each_deg) + 1)))
  
  y <- rbind(lo, la) %>% 
    mutate(grid = 1) %>% 
    mutate(xi = r_d * cos(B * pi / 180) * sin(l * pi / 180) * 
                cos(P0 * pi / 180) - r_d * (sin(B * pi / 180) * 
                cos(B0 * pi / 180) - cos(B * pi / 180) * sin(B0 * pi / 180) * 
                cos(l * pi / 180)) * sin(P0 * pi / 180)) %>% 
    mutate(yj = r_d * cos(B * pi / 180) * sin(l * pi / 180) * 
                sin(P0 * pi / 180) + r_d * (sin(B * pi / 180) * 
                cos(B0 * pi / 180) - cos(B * pi / 180) * sin(B0 * pi / 180) * 
                cos(l * pi / 180)) * cos(P0 * pi / 180)) %>% 
    mutate(r = sqrt(xi^2 + yj^2)) %>% 
    mutate(i = as.integer(xi + x_c)) %>% 
    mutate(j = as.integer(yj + y_c)) %>% 
    select(i, j, grid)
  
  circle <- x %>% 
    filter(circle > 0) %>% 
    select(i, j, grid = circle)
  
  y <- rbind(y, circle)
  
  y <- y[!duplicated(y),]
  
  z <-  x %>% 
    left_join(y, by=c("i","j")) %>% 
    mutate(grid = if_else(is.na(grid),0,grid))
    
  return(z)
  
}