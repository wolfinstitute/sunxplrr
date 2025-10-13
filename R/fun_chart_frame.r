#' @title Calculates coordinate grid for x, y charts
#'
#' @description Calculates grid frame for x, y charts with known resolution.
#'
#' @param xname string name of the chart's x coordinates.
#'
#' @param yname string name of the chart's y coordinates.
#'
#' @param xmin num minimal value of the chart's x coordinates.
#'
#' @param xmax num maximal value of the chart's x coordinates.
#'
#' @param ymin num minimal value of the chart's y coordinates.
#'
#' @param ymax num maximal value of the chart's y coordinates.
#'
#' @param resolution num indicates the resolution of one degree along the 
#'  grid lines. Default is 0.05, e.g. 20 points per degree.
#'
#' @return z tibble with chart frame
#'   pixels.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-12 / Frt
# - `Created`    : 2019-12-29 / Frt
# - `Last test`  : 2025-10-12 / Frt
#
fun_chart_frame <- function(xname = "longitude", yname = "latitude",
                            xmin = -90, xmax = 90, 
                            ymin = -90, ymax = 90, 
                            resolution = 0.05){
  
  z <- tibble::tibble(
    x=rep(seq(xmin,xmax, by = resolution), 
          each = length(seq(xmin,xmax, by = resolution))), 
    y=rep(seq(ymin,ymax, by = resolution), 
          length(seq(ymin,ymax, by = resolution))))
  
  names(z) <- c(xname, yname)

  return(z)
  
}