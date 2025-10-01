#' @title Extracts the spot, penumbra and umbra features
#'
#' @description Extracts the spot, penumbra and umbra features by applying 
#'   cutoff thresholds. Calculates also the hemispheric corrected values for 
#'   the extracted feature areas and contrasts. 
#'
#' @param x tibble containing 3 columns wih pixel coordinates i and j and flat 
#'   image pixel values.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param spot.contrast cutoff threshold for spots.
#'
#' @param umbra.contrast cutoff threshold for umbrae.
#'
#' @return tibble with mask of the extracted features and their contrast values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2025-10-01 / Frt
# - `Created`    : 2020-01-02 / Frt
# - `Last test`  : 2025-10-01 / Frt
#
mod_spot_extraction <- function(x, hdrlst, header, 
                               spot.contrast = 0.85, 
                               umbra.contrast = 0.70){

  # select flat image
  
  y.flat <- x %>% 
    filter(fill > 0) %>% 
    select(i, j, x = flat)
  
  # extract spots
  
  y.spot <- fun_mask_create(y.flat, hdrlst = hdrlst, 
                            threshold = spot.contrast,
                            method = "absolute")
  
  y.spot <- y.spot$mask %>% 
    filter(th > 0) %>% 
    select(i, j, spcntrst = x, spot = th)
  
  # extract penumbra
  
  lower.threshold <- umbra.contrast     
  upper.threshold <- spot.contrast  
  
  y.penumbra <- fun_mask_diff(y.flat, hdrlst = hdrlst, 
                               lower.threshold = lower.threshold, 
                               upper.threshold = upper.threshold, 
                               method = "absolute")
  
  y.penumbra <- y.penumbra$mask %>% 
    filter(th > 0) %>% 
    select(i, j, pencntrst = x, penumbra = th)

  # extract umbra
  
  y.umbra <- fun_mask_create(y.flat, hdrlst = hdrlst, 
                            threshold = umbra.contrast,
                            method = "absolute")
  
  y.umbra <- y.umbra$mask %>% 
    filter(th > 0) %>% 
    select(i, j, umcntrst = x, umbra = th)
  
  # collect extracted features
  
  y <- x %>% 
    left_join(y.spot, by=c("i","j")) %>% 
    mutate(spcntrst = if_else(is.na(spcntrst),0,spcntrst)) %>% 
    mutate(spot = if_else(is.na(spot),0,spot)) %>% 
    left_join(y.penumbra, by=c("i","j")) %>% 
    mutate(pencntrst = if_else(is.na(pencntrst),0,pencntrst)) %>% 
    mutate(penumbra = if_else(is.na(penumbra),0,penumbra)) %>% 
    left_join(y.umbra, by=c("i","j")) %>% 
    mutate(umcntrst = if_else(is.na(umcntrst),0,umcntrst)) %>% 
    mutate(umbra = if_else(is.na(umbra),0,umbra))
  
  # calculate hemispheric corrected feature values
  
  z <- y %>%
    filter(fill > 0) %>%
    mutate(theta = 
           if_else(theta > 89.4, cos(89.4 * pi /180), cos(theta * pi /180))) %>%
    mutate(fill_hem      = fill / theta) %>% 
    mutate(spcntrst_hem = spcntrst / theta) %>% 
    mutate(spot_hem = spot / theta) %>% 
    mutate(pencntrst_hem = pencntrst / theta) %>% 
    mutate(penumbra_hem = penumbra / theta) %>% 
    mutate(umcntrst_hem  = umcntrst / theta) %>% 
    mutate(umbra_hem     = umbra / theta) %>% 
    select(i,j,fill_hem,spcntrst_hem,spot_hem,pencntrst_hem,
           penumbra_hem,umcntrst_hem,umbra_hem)
  
  disc.features <-  y %>% 
    left_join(z, by=c("i","j")) %>% 
    mutate(fill_hem      = if_else(is.na(fill_hem),0,fill_hem)) %>% 
    mutate(spcntrst_hem = if_else(is.na(spcntrst_hem),0,spcntrst_hem)) %>% 
    mutate(spot_hem = if_else(is.na(spot_hem),0,spot_hem)) %>% 
    mutate(pencntrst_hem = if_else(is.na(pencntrst_hem),0,pencntrst_hem)) %>% 
    mutate(penumbra_hem = if_else(is.na(penumbra_hem),0,penumbra_hem)) %>% 
    mutate(umcntrst_hem  = if_else(is.na(umcntrst_hem),0,umcntrst_hem)) %>% 
    mutate(umbra_hem     = if_else(is.na(umbra_hem),0,umbra_hem))

  # update hdrlst and header
  
    hdrlst$SPCNTRST  <- spot.contrast
    hdrlst$UMCNTRST  <- umbra.contrast

    cimages <- addKwv("SPCNTRST", spot.contrast, "Threshold spot contrast",
                      header)
    cimages <- 
      addKwv("UMCNTRST", umbra.contrast, "Threshold umbra contrast",
                      cimages)
    cimages <- 
      addHistory("  Feature extraction with sunviewr::mod_spot_extraction",
                          cimages)

    header <- cimages
  
  # return
  
  z <- list(disc.features = disc.features, hdrlst = hdrlst, header = header)
  
  return(z)
  
}