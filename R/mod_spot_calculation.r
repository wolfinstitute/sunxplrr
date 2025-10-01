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
#' @return list with images, hdrlst, header, indices, charts and syopsis.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2025-10-01 / Frt
# - `Created`    : 2020-01-05 / Frt
# - `Last test`  : 2025-10-01 / Frt
#
mod_spot_calculation <- function(x, hdrlst, header){

  total.indices <- x %>% 
    summarize(full_disc = sum(fill), 
              spcntrst_disc = 1E6*sum(spcntrst)/full_disc,
              spot_disc = 1E6*sum(spot)/full_disc,
              pencntrst_disc = 1E6*sum(pencntrst)/full_disc,
              penumbra_disc = 1E6*sum(penumbra)/full_disc,
              umcntrst_disc = 1E6*sum(umcntrst)/full_disc,
              umbra_disc = 1E6*sum(umbra)/full_disc,
              full_hem = sum(fill_hem), 
              spcntrst_hem = 1E6*sum(spcntrst_hem)/full_hem,
              spot_hem = 1E6*sum(spot_hem)/full_hem,
              pencntrst_hem = 1E6*sum(pencntrst_hem)/full_hem,
              penumbra_hem = 1E6*sum(penumbra_hem)/full_hem,
              umcntrst_hem = 1E6*sum(umcntrst_hem)/full_hem,
              umbra_hem = 1E6*sum(umbra_hem)/full_hem)

  hemisphere.indices <- x %>%
    mutate(hemisphere = if_else(B > 0,"N","S")) %>% 
    group_by(hemisphere) %>% 
    summarize(spcntrst_disc = 1E6*sum(spcntrst)/total.indices$full_disc,
              spot_disc = 1E6*sum(spot)/total.indices$full_disc,
              pencntrst_disc = 1E6*sum(pencntrst)/total.indices$full_disc,
              penumbra_disc = 1E6*sum(penumbra)/total.indices$full_disc,
              umcntrst_disc = 1E6*sum(umcntrst)/total.indices$full_disc,
              umbra_disc = 1E6*sum(umbra)/total.indices$full_disc,
              spcntrst_hem = 1E6*sum(spcntrst_hem)/total.indices$full_hem,
              spot_hem = 1E6*sum(spot_hem)/total.indices$full_hem,
              pencntrst_hem = 1E6*sum(pencntrst_hem)/total.indices$full_hem,
              penumbra_hem = 1E6*sum(penumbra_hem)/total.indices$full_hem,
              umcntrst_hem = 1E6*sum(umcntrst_hem)/total.indices$full_hem,
              umbra_hem = 1E6*sum(umbra_hem)/total.indices$full_hem)

  latitude.indices <- x %>%
    mutate(latitude = as.integer(B)) %>% 
    group_by(latitude) %>% 
    summarize(spcntrst_disc = 1E6*sum(spcntrst)/total.indices$full_disc,
              spot_disc = 1E6*sum(spot)/total.indices$full_disc,
              pencntrst_disc = 1E6*sum(pencntrst)/total.indices$full_disc,
              penumbra_disc = 1E6*sum(penumbra)/total.indices$full_disc,
              umcntrst_disc = 1E6*sum(umcntrst)/total.indices$full_disc,
              umbra_disc = 1E6*sum(umbra)/total.indices$full_disc,
              spcntrst_hem = 1E6*sum(spcntrst_hem)/total.indices$full_hem,
              spot_hem = 1E6*sum(spot_hem)/total.indices$full_hem,
              pencntrst_hem = 1E6*sum(pencntrst_hem)/total.indices$full_hem,
              penumbra_hem = 1E6*sum(penumbra_hem)/total.indices$full_hem,
              umcntrst_hem = 1E6*sum(umcntrst_hem)/total.indices$full_hem,
              umbra_hem = 1E6*sum(umbra_hem)/total.indices$full_hem)
  
  
  chart.indices <- x %>%
    mutate(latitude = as.integer(B)) %>% 
    mutate(longitude = as.integer(l)) %>% 
    group_by(latitude, longitude) %>% 
    summarize(spcntrst_disc = 1E6*sum(spcntrst)/total.indices$full_disc,
              spot_disc = 1E6*sum(spot)/total.indices$full_disc,
              pencntrst_disc = 1E6*sum(pencntrst)/total.indices$full_disc,
              penumbra_disc = 1E6*sum(penumbra)/total.indices$full_disc,
              umcntrst_disc = 1E6*sum(umcntrst)/total.indices$full_disc,
              umbra_disc = 1E6*sum(umbra)/total.indices$full_disc,
              spcntrst_hem = 1E6*sum(spcntrst_hem)/total.indices$full_hem,
              spot_hem = 1E6*sum(spot_hem)/total.indices$full_hem,
              pencntrst_hem = 1E6*sum(pencntrst_hem)/total.indices$full_hem,
              penumbra_hem = 1E6*sum(penumbra_hem)/total.indices$full_hem,
              umcntrst_hem = 1E6*sum(umcntrst_hem)/total.indices$full_hem,
              umbra_hem = 1E6*sum(umbra_hem)/total.indices$full_hem)
  
  chart2.indices <- x %>%
    mutate(latitude = as.integer(B)) %>% 
    mutate(longitude = as.integer(l)) %>% 
    group_by(latitude, longitude) %>% 
    summarize(spcntrst_disc = 1E6*mean(spcntrst)/total.indices$full_disc,
              spot_disc = 1E6*mean(spot)/total.indices$full_disc,
              pencntrst_disc = 1E6*mean(pencntrst)/total.indices$full_disc,
              penumbra_disc = 1E6*mean(penumbra)/total.indices$full_disc,
              umcntrst_disc = 1E6*mean(umcntrst)/total.indices$full_disc,
              umbra_disc = 1E6*mean(umbra)/total.indices$full_disc,
              spcntrst_hem = 1E6*mean(spcntrst_hem)/total.indices$full_hem,
              spot_hem = 1E6*mean(spot_hem)/total.indices$full_hem,
              pencntrst_hem = 1E6*mean(pencntrst_hem)/total.indices$full_hem,
              penumbra_hem = 1E6*mean(penumbra_hem)/total.indices$full_hem,
              umcntrst_hem = 1E6*mean(umcntrst_hem)/total.indices$full_hem,
              umbra_hem = 1E6*mean(umbra_hem)/total.indices$full_hem)
  
  chart.frame <- tibble::tibble(longitude=rep(seq(-90,90, by = 1), each = 181), 
                        latitude=rep(seq(-90,90, by = 1), 181))
  
  charts <- chart.frame %>% 
    left_join(chart.indices, by=c("longitude","latitude")) %>% 
    mutate(spcntrst_disc = if_else(is.na(spcntrst_disc),0,spcntrst_disc)) %>%
    mutate(spot_disc = if_else(is.na(spot_disc),0,spot_disc)) %>%
    mutate(pencntrst_disc = if_else(is.na(pencntrst_disc),0,pencntrst_disc)) %>%
    mutate(penumbra_disc = if_else(is.na(penumbra_disc),0,penumbra_disc)) %>%
    mutate(umcntrst_disc = if_else(is.na(umcntrst_disc),0,umcntrst_disc)) %>%
    mutate(umbra_disc = if_else(is.na(umbra_disc),0,umbra_disc)) %>%
    mutate(spcntrst_hem = if_else(is.na(spcntrst_hem),0,spcntrst_hem)) %>%
    mutate(spot_hem = if_else(is.na(spot_hem),0,spot_hem)) %>%
    mutate(pencntrst_hem = if_else(is.na(pencntrst_hem),0,pencntrst_hem)) %>%
    mutate(penumbra_hem = if_else(is.na(penumbra_hem),0,penumbra_hem)) %>%
    mutate(umcntrst_hem = if_else(is.na(umcntrst_hem),0,umcntrst_hem)) %>%
    mutate(umbra_hem = if_else(is.na(umbra_hem),0,umbra_hem))

  charts2 <- chart.frame %>% 
    left_join(chart2.indices, by=c("longitude","latitude")) %>% 
    mutate(spcntrst_disc = if_else(is.na(spcntrst_disc),0,spcntrst_disc)) %>%
    mutate(spot_disc = if_else(is.na(spot_disc),0,spot_disc)) %>%
    mutate(pencntrst_disc = if_else(is.na(pencntrst_disc),0,pencntrst_disc)) %>%
    mutate(penumbra_disc = if_else(is.na(penumbra_disc),0,penumbra_disc)) %>%
    mutate(umcntrst_disc = if_else(is.na(umcntrst_disc),0,umcntrst_disc)) %>%
    mutate(umbra_disc = if_else(is.na(umbra_disc),0,umbra_disc)) %>%
    mutate(spcntrst_hem = if_else(is.na(spcntrst_hem),0,spcntrst_hem)) %>%
    mutate(spot_hem = if_else(is.na(spot_hem),0,spot_hem)) %>%
    mutate(pencntrst_hem = if_else(is.na(pencntrst_hem),0,pencntrst_hem)) %>%
    mutate(penumbra_hem = if_else(is.na(penumbra_hem),0,penumbra_hem)) %>%
    mutate(umcntrst_hem = if_else(is.na(umcntrst_hem),0,umcntrst_hem)) %>%
    mutate(umbra_hem = if_else(is.na(umbra_hem),0,umbra_hem))
  
  synopsis.indices <- x %>%
    filter(fill > 0) %>%
    mutate(rotation = as.integer(cr)) %>% 
    mutate(latitude = as.integer(B)) %>% 
    mutate(Longitude = as.integer(L)) %>% 
    group_by(rotation, latitude, Longitude) %>% 
    summarize(spcntrst_disc = 1E6*sum(spcntrst)/total.indices$full_disc,
              spot_disc = 1E6*sum(spot)/total.indices$full_disc,
              pencntrst_disc = 1E6*sum(pencntrst)/total.indices$full_disc,
              penumbra_disc = 1E6*sum(penumbra)/total.indices$full_disc,
              umcntrst_disc = 1E6*sum(umcntrst)/total.indices$full_disc,
              umbra_disc = 1E6*sum(umbra)/total.indices$full_disc,
              hemisphere_hem = sum(fill_hem), 
              spcntrst_hem = 1E6*sum(spcntrst_hem)/total.indices$full_hem,
              spot_hem = 1E6*sum(spot_hem)/total.indices$full_hem,
              pencntrst_hem = 1E6*sum(pencntrst_hem)/total.indices$full_hem,
              penumbra_hem = 1E6*sum(penumbra_hem)/total.indices$full_hem,
              umcntrst_hem = 1E6*sum(umcntrst_hem)/total.indices$full_hem,
              umbra_hem = 1E6*sum(umbra_hem)/total.indices$full_hem)
  
  synopsis.frame <- tibble::tibble(
    rotation=sort(rep(unique(synopsis.indices$rotation), 65341)),
    Longitude=rep(rep(seq(0,360, by = 1), each = 181), 
                  length(unique(synopsis.indices$rotation))), 
    latitude=rep(rep(seq(-90,90, by = 1), 361), 
                 length(unique(synopsis.indices$rotation))))
  
  # synopsis.frame <- tibble(Longitude=rep(seq(0,360, by = 1), each = 181), 
  #                       latitude=rep(seq(-90,90, by = 1), 361))
  
  synopsis <- synopsis.frame %>% 
    left_join(synopsis.indices, by=c("rotation", "Longitude", "latitude")) %>% 
    mutate(spcntrst_disc = if_else(is.na(spcntrst_disc),0,spcntrst_disc)) %>%
    mutate(spot_disc = if_else(is.na(spot_disc),0,spot_disc)) %>%
    mutate(pencntrst_disc = if_else(is.na(pencntrst_disc),0,pencntrst_disc)) %>%
    mutate(penumbra_disc = if_else(is.na(penumbra_disc),0,penumbra_disc)) %>%
    mutate(umcntrst_disc = if_else(is.na(umcntrst_disc),0,umcntrst_disc)) %>%
    mutate(umbra_disc = if_else(is.na(umbra_disc),0,umbra_disc)) %>%
    mutate(spcntrst_hem = if_else(is.na(spcntrst_hem),0,spcntrst_hem)) %>%
    mutate(spot_hem = if_else(is.na(spot_hem),0,spot_hem)) %>%
    mutate(pencntrst_hem = if_else(is.na(pencntrst_hem),0,pencntrst_hem)) %>%
    mutate(penumbra_hem = if_else(is.na(penumbra_hem),0,penumbra_hem)) %>%
    mutate(umcntrst_hem = if_else(is.na(umcntrst_hem),0,umcntrst_hem)) %>%
    mutate(umbra_hem = if_else(is.na(umbra_hem),0,umbra_hem))
  
  # update hdrlst and header
  
    hdrlst$TAREADSK  <- total.indices$spot_disc
    hdrlst$TCNTRSTD  <- total.indices$spcntrst_disc
    hdrlst$TAREAHEM  <- total.indices$spot_hem
    hdrlst$TCNTRSTH  <- total.indices$spcntrst_hem
    
    cimages <- addKwv("TAREADSK", total.indices$spot_disc, 
                  "Total features area (millionth of disc)", header)
    cimages <- addKwv("TCNTRSTD", total.indices$spcntrst_disc, 
                  "Total features contrast (millionth of disc)", cimages)
    cimages <- addKwv("TAREAHEM", total.indices$spot_hem, 
                  "Total features area (millionth of hemisphere)", cimages)
    cimages <- addKwv("TCNTRSTH", total.indices$spcntrst_hem, 
                  "Total features contrast (millionth of hemisphere)", cimages)
    cimages <- 
      addHistory("  Index calculation with sunviewr::mod_spot_calculation",
                          cimages)

    header <- cimages
  
  # return
  
  z <- list(images = x, hdrlst = hdrlst, header = header,
            total.indices = total.indices, 
            hemisphere.indices = hemisphere.indices,
            latitude.indices = latitude.indices,
            chart.indices = chart.indices, 
            charts = charts,
            charts2 = charts2,
            synopsis.indices = synopsis.indices,
            synopsis = synopsis)
  
  return(z)
  
}