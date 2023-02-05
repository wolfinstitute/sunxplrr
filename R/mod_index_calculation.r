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

# - `Last change`: 2023-02-05 / Frt
# - `Created`    : 2020-01-05 / Frt
# - `Last test`  : 2020-01-19 / Frt
#
mod_index_calculation <- function(x, hdrlst, header){

  total.indices <- x %>% 
    summarize(full_disc = sum(fill), 
              ttarea_disc = 1E6*sum(ttarea)/full_disc,
              plage_disc = 1E6*sum(plage)/full_disc,
              ehnetwork_disc = 1E6*sum(ehnetwork)/full_disc,
              atnetwork_disc = 1E6*sum(atnetwork)/full_disc,
              ttcntrst_disc = 1E6*sum(ttcntrst)/full_disc,
              pgcntrst_disc = 1E6*sum(pgcntrst)/full_disc,
              ehncntrst_disc = 1E6*sum(ehncntrst)/full_disc,
              atncntrst_disc = 1E6*sum(atncntrst)/full_disc,
              full_hem = sum(fill_hem), 
              ttarea_hem = 1E6*sum(ttarea_hem)/full_hem,
              plage_hem = 1E6*sum(plage_hem)/full_hem,
              ehnetwork_hem = 1E6*sum(ehnetwork_hem)/full_hem,
              atnetwork_hem = 1E6*sum(atnetwork_hem)/full_hem,
              ttcntrst_hem = 1E6*sum(ttcntrst_hem)/full_hem,
              pgcntrst_hem = 1E6*sum(pgcntrst_hem)/full_hem,
              ehncntrst_hem = 1E6*sum(ehncntrst_hem)/full_hem,
              atncntrst_hem = 1E6*sum(atncntrst_hem)/full_hem)
  
  hemisphere.indices <- x %>%
    mutate(hemisphere = if_else(B > 0,"N","S")) %>% 
    group_by(hemisphere) %>% 
    summarize(hemisphere_disc = sum(fill), 
              ttarea_disc = 1E6*sum(ttarea)/total.indices$full_disc,
              plage_disc = 1E6*sum(plage)/total.indices$full_disc,
              ehnetwork_disc = 1E6*sum(ehnetwork)/total.indices$full_disc,
              atnetwork_disc = 1E6*sum(atnetwork)/total.indices$full_disc,
              ttcntrst_disc = 1E6*sum(ttcntrst)/total.indices$full_disc,
              pgcntrst_disc = 1E6*sum(pgcntrst)/total.indices$full_disc,
              ehncntrst_disc = 1E6*sum(ehncntrst)/total.indices$full_disc,
              atncntrst_disc = 1E6*sum(atncntrst)/total.indices$full_disc,
              hemisphere_hem = sum(fill_hem), 
              ttarea_hem = 1E6*sum(ttarea_hem)/total.indices$full_hem,
              plage_hem = 1E6*sum(plage_hem)/total.indices$full_hem,
              ehnetwork_hem = 1E6*sum(ehnetwork_hem)/total.indices$full_hem,
              atnetwork_hem = 1E6*sum(atnetwork_hem)/total.indices$full_hem,
              ttcntrst_hem = 1E6*sum(ttcntrst_hem)/total.indices$full_hem,
              pgcntrst_hem = 1E6*sum(pgcntrst_hem)/total.indices$full_hem,
              ehncntrst_hem = 1E6*sum(ehncntrst_hem)/total.indices$full_hem,
              atncntrst_hem = 1E6*sum(atncntrst_hem)/total.indices$full_hem)
  
  latitude.indices <- x %>%
    mutate(latitude = as.integer(B)) %>% 
    group_by(latitude) %>% 
    summarize(latitude_disc = sum(fill), 
              ttarea_disc = 1E6*sum(ttarea)/total.indices$full_disc,
              plage_disc = 1E6*sum(plage)/total.indices$full_disc,
              ehnetwork_disc = 1E6*sum(ehnetwork)/total.indices$full_disc,
              atnetwork_disc = 1E6*sum(atnetwork)/total.indices$full_disc,
              ttcntrst_disc = 1E6*sum(ttcntrst)/total.indices$full_disc,
              pgcntrst_disc = 1E6*sum(pgcntrst)/total.indices$full_disc,
              ehncntrst_disc = 1E6*sum(ehncntrst)/total.indices$full_disc,
              atncntrst_disc = 1E6*sum(atncntrst)/total.indices$full_disc,
              latitude_hem = sum(fill_hem), 
              ttarea_hem = 1E6*sum(ttarea_hem)/total.indices$full_hem,
              plage_hem = 1E6*sum(plage_hem)/total.indices$full_hem,
              ehnetwork_hem = 1E6*sum(ehnetwork_hem)/total.indices$full_hem,
              atnetwork_hem = 1E6*sum(atnetwork_hem)/total.indices$full_hem,
              ttcntrst_hem = 1E6*sum(ttcntrst_hem)/total.indices$full_hem,
              pgcntrst_hem = 1E6*sum(pgcntrst_hem)/total.indices$full_hem,
              ehncntrst_hem = 1E6*sum(ehncntrst_hem)/total.indices$full_hem,
              atncntrst_hem = 1E6*sum(atncntrst_hem)/total.indices$full_hem)
  
  
  chart.indices <- x %>%
    mutate(latitude = as.integer(B)) %>% 
    mutate(longitude = as.integer(l)) %>% 
    group_by(latitude, longitude) %>% 
    summarize(ttarea_disc = 1E6*sum(ttarea)/total.indices$full_disc,
              plage_disc = 1E6*sum(plage)/total.indices$full_disc,
              ehnetwork_disc = 1E6*sum(ehnetwork)/total.indices$full_disc,
              atnetwork_disc = 1E6*sum(atnetwork)/total.indices$full_disc,
              ttcntrst_disc = 1E6*sum(ttcntrst)/total.indices$full_disc,
              pgcntrst_disc = 1E6*sum(pgcntrst)/total.indices$full_disc,
              ehncntrst_disc = 1E6*sum(ehncntrst)/total.indices$full_disc,
              atncntrst_disc = 1E6*sum(atncntrst)/total.indices$full_disc,
              ttarea_hem = 1E6*sum(ttarea_hem)/total.indices$full_hem,
              plage_hem = 1E6*sum(plage_hem)/total.indices$full_hem,
              ehnetwork_hem = 1E6*sum(ehnetwork_hem)/total.indices$full_hem,
              atnetwork_hem = 1E6*sum(atnetwork_hem)/total.indices$full_hem,
              ttcntrst_hem = 1E6*sum(ttcntrst_hem)/total.indices$full_hem,
              pgcntrst_hem = 1E6*sum(pgcntrst_hem)/total.indices$full_hem,
              ehncntrst_hem = 1E6*sum(ehncntrst_hem)/total.indices$full_hem,
              atncntrst_hem = 1E6*sum(atncntrst_hem)/total.indices$full_hem)
  
  chart.frame <- tibble(longitude=rep(seq(-90,90, by = 1), each = 181), 
                        latitude=rep(seq(-90,90, by = 1), 181))
  
  charts <- chart.frame %>% 
    left_join(chart.indices, by=c("longitude","latitude")) %>% 
    mutate(ttarea_disc = if_else(is.na(ttarea_disc),0,ttarea_disc)) %>% 
    mutate(plage_disc = if_else(is.na(plage_disc),0,plage_disc)) %>%
    mutate(ehnetwork_disc = if_else(is.na(ehnetwork_disc),0,ehnetwork_disc)) %>%
    mutate(atnetwork_disc = if_else(is.na(atnetwork_disc),0,atnetwork_disc)) %>%
    mutate(ttcntrst_disc = if_else(is.na(ttcntrst_disc),0,ttcntrst_disc)) %>%
    mutate(pgcntrst_disc = if_else(is.na(pgcntrst_disc),0,pgcntrst_disc)) %>%
    mutate(ehncntrst_disc = if_else(is.na(ehncntrst_disc),0,ehncntrst_disc)) %>%
    mutate(atncntrst_disc = if_else(is.na(atncntrst_disc),0,atncntrst_disc)) %>%
    mutate(ttarea_hem = if_else(is.na(ttarea_hem),0,ttarea_hem)) %>%
    mutate(plage_hem = if_else(is.na(plage_hem),0,plage_hem)) %>%
    mutate(ehnetwork_hem = if_else(is.na(ehnetwork_hem),0,ehnetwork_hem)) %>%
    mutate(atnetwork_hem = if_else(is.na(atnetwork_hem),0,atnetwork_hem)) %>%
    mutate(ttcntrst_hem = if_else(is.na(ttcntrst_hem),0,ttcntrst_hem)) %>%
    mutate(pgcntrst_hem = if_else(is.na(pgcntrst_hem),0,pgcntrst_hem)) %>%
    mutate(ehncntrst_hem = if_else(is.na(ehncntrst_hem),0,ehncntrst_hem)) %>%
    mutate(atncntrst_hem = if_else(is.na(atncntrst_hem),0,atncntrst_hem))
  
  synopsis.indices <- x %>%
    filter(fill > 0) %>%
    mutate(rotation = as.integer(cr)) %>% 
    mutate(latitude = as.integer(B)) %>% 
    mutate(Longitude = as.integer(L)) %>% 
    group_by(rotation, latitude, Longitude) %>% 
    summarize(ttarea_disc = 1E6*sum(ttarea)/total.indices$full_disc,
              plage_disc = 1E6*sum(plage)/total.indices$full_disc,
              ehnetwork_disc = 1E6*sum(ehnetwork)/total.indices$full_disc,
              atnetwork_disc = 1E6*sum(atnetwork)/total.indices$full_disc,
              ttcntrst_disc = 1E6*sum(ttcntrst)/total.indices$full_disc,
              pgcntrst_disc = 1E6*sum(pgcntrst)/total.indices$full_disc,
              ehncntrst_disc = 1E6*sum(ehncntrst)/total.indices$full_disc,
              atncntrst_disc = 1E6*sum(atncntrst)/total.indices$full_disc,
              ttarea_hem = 1E6*sum(ttarea_hem)/total.indices$full_hem,
              plage_hem = 1E6*sum(plage_hem)/total.indices$full_hem,
              ehnetwork_hem = 1E6*sum(ehnetwork_hem)/total.indices$full_hem,
              atnetwork_hem = 1E6*sum(atnetwork_hem)/total.indices$full_hem,
              ttcntrst_hem = 1E6*sum(ttcntrst_hem)/total.indices$full_hem,
              pgcntrst_hem = 1E6*sum(pgcntrst_hem)/total.indices$full_hem,
              ehncntrst_hem = 1E6*sum(ehncntrst_hem)/total.indices$full_hem,
              atncntrst_hem = 1E6*sum(atncntrst_hem)/total.indices$full_hem)
  
  synopsis.frame <- tibble(
    rotation=sort(rep(unique(synopsis.indices$rotation), 65341)),
    Longitude=rep(rep(seq(0,360, by = 1), each = 181), 
                  length(unique(synopsis.indices$rotation))), 
    latitude=rep(rep(seq(-90,90, by = 1), 361), 
                 length(unique(synopsis.indices$rotation))))
  
  # synopsis.frame <- tibble(Longitude=rep(seq(0,360, by = 1), each = 181), 
  #                       latitude=rep(seq(-90,90, by = 1), 361))
  
  synopsis <- synopsis.frame %>% 
    left_join(synopsis.indices, by=c("rotation", "Longitude", "latitude")) %>% 
    mutate(ttarea_disc = if_else(is.na(ttarea_disc),0,ttarea_disc)) %>% 
    mutate(plage_disc = if_else(is.na(plage_disc),0,plage_disc)) %>%
    mutate(ehnetwork_disc = if_else(is.na(ehnetwork_disc),0,ehnetwork_disc)) %>%
    mutate(atnetwork_disc = if_else(is.na(atnetwork_disc),0,atnetwork_disc)) %>%
    mutate(ttcntrst_disc = if_else(is.na(ttcntrst_disc),0,ttcntrst_disc)) %>%
    mutate(pgcntrst_disc = if_else(is.na(pgcntrst_disc),0,pgcntrst_disc)) %>%
    mutate(ehncntrst_disc = if_else(is.na(ehncntrst_disc),0,ehncntrst_disc)) %>%
    mutate(atncntrst_disc = if_else(is.na(atncntrst_disc),0,atncntrst_disc)) %>%
    mutate(ttarea_hem = if_else(is.na(ttarea_hem),0,ttarea_hem)) %>%
    mutate(plage_hem = if_else(is.na(plage_hem),0,plage_hem)) %>%
    mutate(ehnetwork_hem = if_else(is.na(ehnetwork_hem),0,ehnetwork_hem)) %>%
    mutate(atnetwork_hem = if_else(is.na(atnetwork_hem),0,atnetwork_hem)) %>%
    mutate(ttcntrst_hem = if_else(is.na(ttcntrst_hem),0,ttcntrst_hem)) %>%
    mutate(pgcntrst_hem = if_else(is.na(pgcntrst_hem),0,pgcntrst_hem)) %>%
    mutate(ehncntrst_hem = if_else(is.na(ehncntrst_hem),0,ehncntrst_hem)) %>%
    mutate(atncntrst_hem = if_else(is.na(atncntrst_hem),0,atncntrst_hem))

  # update hdrlst and header
  
    hdrlst$TAREADSK  <- total.indices$ttarea_disc
    hdrlst$TCNTRSTD  <- total.indices$ttcntrst_disc
    hdrlst$TAREAHEM  <- total.indices$ttarea_hem
    hdrlst$TCNTRSTH  <- total.indices$ttcntrst_hem
    
    cimages <- addKwv("TAREADSK", total.indices$ttarea_disc, 
                  "Total features area (millionth of disc)", header)
    cimages <- addKwv("TCNTRSTD", total.indices$ttcntrst_disc, 
                  "Total features contrast (millionth of disc)", cimages)
    cimages <- addKwv("TAREAHEM", total.indices$ttarea_hem, 
                  "Total features area (millionth of hemisphere)", cimages)
    cimages <- addKwv("TCNTRSTH", total.indices$ttcntrst_hem, 
                  "Total features contrast (millionth of hemisphere)", cimages)
    cimages <- 
      addHistory("  Index calculation with sunviewr::mod_index_calculation",
                          cimages)

    header <- cimages
  
  # return
  
  z <- list(images = x, hdrlst = hdrlst, header = header,
            total.indices = total.indices, 
            hemisphere.indices = hemisphere.indices,
            latitude.indices = latitude.indices,
            chart.indices = chart.indices, 
            charts = charts,
            synopsis.indices = synopsis.indices,
            synopsis = synopsis)
  
  return(z)
  
}