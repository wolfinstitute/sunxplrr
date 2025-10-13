#' @title Calculates activity indices and provides data for images and charts
#'
#' @description Calculates activity indices and provides input data for FITS 
#'   images and charts. 
#'
#' @param x tibble containing the previously calculated images.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
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
#' @return list with images, hdrlst, header, indices, charts and syopsis.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-12 / Frt
# - `Created`    : 2020-01-05 / Frt
# - `Last test`  : 2025-10-12 / Frt
#
mod_spot_indices <- function(x, hdrlst, header,
                             xmin = -90, xmax = 90, 
                             ymin = -90, ymax = 90, 
                             resolution = 0.05){

  # total indices
  
  total.indices <- x %>% 
    summarize(full_disc = sum(fill), 
              sparea_disc = 1E6*sum(sparea)/full_disc,
              umarea_disc = 1E6*sum(umarea)/full_disc,
              penarea_disc = 1E6*sum(penarea)/full_disc,
              full_hem = sum(fill_hem), 
              sparea_hem = 1E6*sum(sparea_hem)/full_hem,
              umarea_hem = 1E6*sum(umarea_hem)/full_hem,
              penarea_hem = 1E6*sum(penarea_hem)/full_hem)
              
  hemisphere.indices <- x %>%
    mutate(hemisphere = if_else(B > 0,"N","S")) %>% 
    group_by(hemisphere) %>% 
    summarize(sparea_disc = 1E6*sum(sparea)/total.indices$full_disc,
              umarea_disc = 1E6*sum(umarea)/total.indices$full_disc,
              penarea_disc = 1E6*sum(penarea)/total.indices$full_disc,
              sparea_hem = 1E6*sum(sparea_hem)/total.indices$full_hem,
              umarea_hem = 1E6*sum(umarea_hem)/total.indices$full_hem,
              penarea_hem = 1E6*sum(penarea_hem)/total.indices$full_hem)

  latitude.indices <- x %>%
    mutate(latitude = as.integer(B*100)/100) %>% 
    group_by(latitude) %>% 
    summarize(sparea_disc = 1E6*sum(sparea)/total.indices$full_disc,
              umarea_disc = 1E6*sum(umarea)/total.indices$full_disc,
              penarea_disc = 1E6*sum(penarea)/total.indices$full_disc,
              sparea_hem = 1E6*sum(sparea_hem)/total.indices$full_hem,
              umarea_hem = 1E6*sum(umarea_hem)/total.indices$full_hem,
              penarea_hem = 1E6*sum(penarea_hem)/total.indices$full_hem)
  
  # charts: daily heliographic maps
  
  chart.indices <- x %>%
    select(i, j, flat, penandum)

  chart.frame <- fun_chart_frame(xname = "longitude", yname = "latitude",
                                 xmin = xmin, xmax = xmax, 
                                 ymin = ymin, ymax = ymax, 
                                 resolution = resolution)
  
  charts <-fun_chart_coordinates(x = chart.indices, hdrlst = hdrlst, 
                                 chart.frame = chart.frame)
  # update hdrlst and header
  
    hdrlst$SAREADSK  <- total.indices$sparea_disc
    hdrlst$UAREADSK  <- total.indices$umarea_disc
    hdrlst$PAREADSK  <- total.indices$penarea_disc
    hdrlst$SAREAHEM  <- total.indices$sparea_hem
    hdrlst$UAREAHEM  <- total.indices$umarea_hem
    hdrlst$PAREAHEM  <- total.indices$penarea_hem
    
    cimages <- addKwv("SAREADSK", total.indices$sparea_disc, 
                  "Total spot area (millionth of disc)", header)
    cimages <- addKwv("UAREADSK", total.indices$umarea_disc, 
                  "Total umbra area (millionth of disc)", cimages)
    cimages <- addKwv("PAREADSK", total.indices$penarea_disc, 
                  "Total penumbra area (millionth of disc)", cimages)
    cimages <- addKwv("SAREAHEM", total.indices$sparea_hem, 
                  "Total spot area (millionth of hemisphere)", cimages)
    cimages <- addKwv("UAREAHEM", total.indices$umarea_hem, 
                  "Total umbra area (millionth of hemisphere)", cimages)
    cimages <- addKwv("PAREAHEM", total.indices$penarea_hem, 
                  "Total penumbra area (millionth of hemisphere)", cimages)
    cimages <- 
      addHistory("  Index calculation with sunviewr::mod_spot_indices",
                          cimages)

    header <- cimages
  
  # return
  
  z <- list(images = x, hdrlst = hdrlst, header = header,
            total.indices = total.indices, 
            hemisphere.indices = hemisphere.indices,
            latitude.indices = latitude.indices,
            charts = charts)
  
  return(z)
  
}