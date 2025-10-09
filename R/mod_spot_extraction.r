#' @title Extracts the spot, penumbra and umbra features, their masks and areas
#'
#' @description Extracts the spot, penumbra and umbra features by applying 
#'   cutoff thresholds. Provides quantised feature masks. Calculates the total
#'   and the hemispheric corrected values for the extracted feature areas. 
#'
#' @param x tibble containing 3 columns wih pixel coordinates i and j and flat 
#'   image pixel values.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param spot.threshold cutoff threshold for spots.
#'
#' @param umbra.threshold cutoff threshold for umbrae.
#'
#' @return tibble with mask of the extracted features and their area marks.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2025-10-08 / Frt
# - `Created`    : 2020-01-02 / Frt
# - `Last test`  : 2025-10-08 / Frt
#
mod_spot_extraction <- function(x, hdrlst, header, 
                               spot.threshold = 0.85, 
                               umbra.threshold = 0.70){
x <- disc.flat
  # select flat image
  
  y.flat <- x %>% 
    filter(fill > 0) %>% 
    select(i, j, x = flat)
  
  # extracts spots mask
  
  y.spot <- fun_mask_create(y.flat, hdrlst = hdrlst, 
                            threshold = spot.threshold,
                            min.mask.value = umbra.threshold,
                            method = "absolute")
  
  y.spot <- y.spot$mask %>% 
    select(i, j, spot = th)
  
  # extracts umbra mask
  
  y.umbra <- fun_mask_create(y.flat, hdrlst = hdrlst, 
                             threshold = umbra.threshold,
                             min.mask.value = 0,
                             method = "absolute")
  
  y.umbra <- y.umbra$mask %>% 
    select(i, j, umbra = th)
  
  # extracts penumbra + umbra mask
  
  y.penandum <- fun_disc_math(im1 = y.spot, values_1 = "spot", 
                              im2 = y.umbra, values_2 = "umbra",
                              method = "mult", values.name = "penandum")
  
  y.penandum <- y.penandum %>% 
    select(i, j, penandum)

  # marks spots

  y.sparea <- fun_mask_diff(y.flat, hdrlst = hdrlst, 
                            lower.threshold = 0, 
                            upper.threshold = spot.threshold, 
                            method = method)
  
  y.sparea <- y.sparea$mask %>% 
    select(i, j, sparea = th)
  
  # marks umbrae
  
  y.umarea <- fun_mask_diff(y.flat, hdrlst = hdrlst, 
                            lower.threshold = 0, 
                            upper.threshold = umbra.threshold, 
                            method = method)
   
  y.umarea <- y.umarea$mask %>% 
    select(i, j, umarea = th)
  
  # marks spots
  
  y.penarea <- fun_mask_diff(y.flat, hdrlst = hdrlst, 
                             lower.threshold = umbra.threshold, 
                             upper.threshold = spot.threshold, 
                             method = method)
   
  y.penarea <- y.penarea$mask %>% 
    select(i, j, penarea = th)
  
  # collects extracted features
  
  y <- x %>% 
    left_join(y.spot, by=c("i","j")) |>  
    mutate(spot = if_else(is.na(spot),0,spot)) |>  
    left_join(y.umbra, by=c("i","j")) |>  
    mutate(umbra = if_else(is.na(umbra),0,umbra)) |>  
    left_join(y.penandum, by=c("i","j")) |>  
    mutate(penandum = if_else(is.na(penandum),0,penandum)) |> 
    left_join(y.sparea, by=c("i","j")) |>  
    mutate(sparea = if_else(is.na(sparea),0,sparea)) |>  
    left_join(y.umarea, by=c("i","j")) |>  
    mutate(umarea = if_else(is.na(umarea),0,umarea)) |>  
    left_join(y.penarea, by=c("i","j")) |>  
    mutate(penarea= if_else(is.na(penarea),0,penarea))
  
  # calculate hemispheric corrected feature values
  
  z <- y %>%
    filter(fill > 0) %>%
    mutate(theta = 
           if_else(theta > 89.4, cos(89.4 * pi /180), cos(theta * pi /180))) %>%
    mutate(fill_hem      = fill / theta) %>% 
    mutate(sparea_hem    = sparea / theta) %>% 
    mutate(umarea_hem    = umarea / theta) %>% 
    mutate(penarea_hem   = penarea / theta) %>% 
    select(i,j,fill_hem,sparea_hem,umarea_hem,penarea_hem)
  
  disc.features <-  y %>% 
    left_join(z, by=c("i","j")) %>% 
    mutate(fill_hem      = if_else(is.na(fill_hem),0,fill_hem)) %>% 
    mutate(sparea_hem    = if_else(is.na(sparea_hem),0,sparea_hem)) %>% 
    mutate(umarea_hem    = if_else(is.na(umarea_hem),0,umarea_hem)) %>% 
    mutate(penarea_hem   = if_else(is.na(penarea_hem),0,penarea_hem)) 

  # update hdrlst and header
  
    hdrlst$SPTHSHLD  <- spot.threshold
    hdrlst$UMTHSHLD  <- umbra.threshold

    cimages <- addKwv("SPTHSHLD", spot.threshold, "Threshold spot contrast",
                      header)
    cimages <- 
      addKwv("UMTHSHLD", umbra.threshold, "Threshold umbra contrast",
                      cimages)
    cimages <- 
      addHistory("  Feature extraction with sunviewr::mod_spot_extraction",
                          cimages)

    header <- cimages
  
  # return
  
  z <- list(disc.features = disc.features, hdrlst = hdrlst, header = header)
  
  return(z)
  
}