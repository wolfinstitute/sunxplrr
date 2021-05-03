#' @title Extracts the plage, enhanced network and active network features
#'
#' @description Extracts the plage, enhanced network and active network features 
#'   by applying cutoff thresholds. Calculates also the hemispheric corrected 
#'   values for the extracted feature areas and contrasts. 
#'
#' @param x tibble containing 3 columns wih pixel coordinates i and j and flat 
#'   image pixel values.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param plage.contrast cutoff threshold for plages.
#'
#' @param en.contrast cutoff threshold for enhanced network.
#'
#' @param qt.contrast cutoff threshold for quiet network.
#'
#' @return tibble with mask of the extracted features and their contrast values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-01-06 / Frt
# - `Created`    : 2020-01-02 / Frt
# - `Last test`  : 2020-01-06 / Frt
#
mod_feature_extraction <- function(x, hdrlst, header, 
                               plage.contrast = 1.35, 
                               en.contrast = 1.25, 
                               qn.contrast = 1.10){

  # select flat image
  
  y.flat <- x %>% 
    filter(fill > 0) %>% 
    select(i, j, x = flat)
  
  # extract plage
  
  y.plage <- fun_mask_create(y.flat, hdrlst = hdrlst, 
                                threshold = plage.contrast,
                                method = "absolute")
  
  y.plage <- y.plage$mask %>% 
    filter(th > 0) %>% 
    select(i, j, pgcntrst = x, plage = th)
  
  # extract enhanced network
  
  lower.threshold <- en.contrast     
  upper.threshold <- plage.contrast  
  
  y.ehnetwork <- fun_mask_diff(y.flat, hdrlst = hdrlst, 
                               lower.threshold = lower.threshold, 
                               upper.threshold = upper.threshold, 
                               method = "absolute")
  
  y.ehnetwork <- y.ehnetwork$mask %>% 
    filter(th > 0) %>% 
    select(i, j, ehncntrst = x, ehnetwork = th)
  
  # extract active network
  
  lower.threshold <- qn.contrast  
  upper.threshold <- en.contrast  
  
  y.atnetwork <- fun_mask_diff(y.flat, hdrlst = hdrlst, 
                               lower.threshold = lower.threshold, 
                               upper.threshold = upper.threshold, 
                               method = "absolute")
  
  y.atnetwork <- y.atnetwork$mask %>% 
    filter(th > 0) %>% 
    select(i, j, atncntrst = x, atnetwork = th)
  
  # collect extracted features
  
  y <- x %>% 
    left_join(y.atnetwork, by=c("i","j")) %>% 
    mutate(atncntrst = if_else(is.na(atncntrst),0,atncntrst)) %>% 
    mutate(atnetwork = if_else(is.na(atnetwork),0,atnetwork)) %>% 
    left_join(y.ehnetwork, by=c("i","j")) %>% 
    mutate(ehncntrst = if_else(is.na(ehncntrst),0,ehncntrst)) %>% 
    mutate(ehnetwork = if_else(is.na(ehnetwork),0,ehnetwork)) %>% 
    left_join(y.plage, by=c("i","j")) %>% 
    mutate(pgcntrst = if_else(is.na(pgcntrst),0,pgcntrst)) %>% 
    mutate(plage = if_else(is.na(plage),0,plage)) %>% 
    mutate(ttcntrst = ehncntrst + pgcntrst) %>% 
    mutate(ttarea =  ehnetwork + plage)
  
  # calculate hemispheric corrected feature values
  
  z <- y %>%
    filter(fill > 0) %>%
    mutate(theta = 
           if_else(theta > 89.4, cos(89.4 * pi /180), cos(theta * pi /180))) %>%
    mutate(fill_hem      = fill / theta) %>% 
    mutate(atncntrst_hem = atncntrst / theta) %>% 
    mutate(atnetwork_hem = atnetwork / theta) %>% 
    mutate(ehncntrst_hem = ehncntrst / theta) %>% 
    mutate(ehnetwork_hem = ehnetwork / theta) %>% 
    mutate(pgcntrst_hem  = pgcntrst / theta) %>% 
    mutate(plage_hem     = plage / theta) %>% 
    mutate(ttcntrst_hem  = ttcntrst / theta) %>% 
    mutate(ttarea_hem    = ttarea / theta) %>% 
    select(i,j,fill_hem,atncntrst_hem,atnetwork_hem,ehncntrst_hem,
           ehnetwork_hem,atncntrst_hem,atnetwork_hem,pgcntrst_hem,
           plage_hem,ttcntrst_hem,ttarea_hem)
  
  disc.features <-  y %>% 
    left_join(z, by=c("i","j")) %>% 
    mutate(fill_hem      = if_else(is.na(fill_hem),0,fill_hem)) %>% 
    mutate(atncntrst_hem = if_else(is.na(atncntrst_hem),0,atncntrst_hem)) %>% 
    mutate(atnetwork_hem = if_else(is.na(atnetwork_hem),0,atnetwork_hem)) %>% 
    mutate(ehncntrst_hem = if_else(is.na(ehncntrst_hem),0,ehncntrst_hem)) %>% 
    mutate(ehnetwork_hem = if_else(is.na(ehnetwork_hem),0,ehnetwork_hem)) %>% 
    mutate(pgcntrst_hem  = if_else(is.na(pgcntrst_hem),0,pgcntrst_hem)) %>% 
    mutate(plage_hem     = if_else(is.na(plage_hem),0,plage_hem)) %>% 
    mutate(ttcntrst_hem  = if_else(is.na(ttcntrst_hem),0,ttcntrst_hem)) %>% 
    mutate(ttarea_hem    = if_else(is.na(ttarea_hem),0,ttarea_hem))
  
  # update hdrlst and header
  
    hdrlst$PGCNTRST  <- plage.contrast
    hdrlst$EHCNTRST  <- en.contrast
    hdrlst$QNCNTRST  <- qn.contrast
    
    cimages <- addKwv("PGCNTRST", plage.contrast, "Threshold plage contrast",
                      header)
    cimages <- 
      addKwv("ENCNTRST", en.contrast, "Threshold enhanced network contrast",
                      cimages)
    cimages <- 
      addKwv("QNCNTRST", qn.contrast, "Threshold quiet network contrast",
                      cimages)
    cimages <- 
      addHistory("  Feature extraction with sunviewr::mod_feature_extraction",
                          cimages)

    header <- cimages
  
  # return
  
  z <- list(disc.features = disc.features, hdrlst = hdrlst, header = header)
  
  return(z)
  
}