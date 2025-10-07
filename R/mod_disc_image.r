#' @title Calculates disc center coordinates and radius and extracts the image
#'
#' @description Calculates disc center coordinates and radius and extracts the 
#'   image. If the original image contains additional text in the image corners,
#'   the approximate center coordinates and the approximate radius of the image
#'   disc are estimated and the image disk is extracted with an oversized mask.
#'   The size of the image and the number of bits per pixel are taken from the 
#'   FITS header provided by hdrlst. 
#'
#' @param x tibble containing 3 columns with pixel coordinates i and j and pixel
#'   intensity values x.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param cut.image if TRUE, image contains text in the corners.
#'
#' @param threshold cutoff threshold in absolute or relative units.
#'
#' @param method If method = 'absolute' then the cutoff threshold value is in
#'   ADU units. Otherwise it is the threshold / 100 fraction of the maximal ADU
#'   value given by 2^bitpix.
#'
#' @param cut.threshold cutoff threshold for images with text in the corners 
#'   in absolute or relative units.
#'
#' @param cut.method If cut.method = 'absolute' then the cutoff threshold value
#'   for images with text in the corners is in ADU units. Otherwise it is the
#'   cut.threshold / 100 fraction of the maximal ADU value given by 2^bitpix.
#'   
#' @param cut.border.pix number of pixels to be added to the radius of the 
#'   circle to extract the image without the text in the corners.
#'   
#' @param add.border.pix number of pixels to be added to the radius of the 
#'   circle to extract the final image.
#'   
#' @param image.values.name name of column with image values in final image.
#'   
#' @param grid_each_deg num number of degrees between grid lines. Default value
#'   is one grid line every 10 heliografic degrees.
#'   
#' @param res_each_deg_on_grid num indicates the resolution of points of one 
#'   degree along the grid lines. Default is 0.01, e.g. 100 points per degree.
#'
#' @return list with disc.image, hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2025-10-03 / Frt
# - `Created`    : 2019-12-03 / Frt
# - `Last test`  : 2025-10-03 / Frt
#
mod_disc_image <- function(x, hdrlst, header, cut.image,
                           threshold = 10, method = "relative",
                           cut.threshold = 20, cut.method = "relative",
                           cut.border.pix = 100, add.border.pix = 0,
                           image.values.name = "image", grid_each_deg = 10,
                           res_each_deg_on_grid = 0.01){

  # cut image
  
  x <- fun_cut_image(x, hdrlst, header, cut.image,
                     cut.threshold = cut.threshold, 
                     cut.method = cut.method,
                     cut.border.pix = cut.border.pix)
  
  # create final mask 
  
  final.mask <- fun_mask_create(x, hdrlst = hdrlst, threshold = threshold, 
                                method = method)
  
  image.mask.threshold <- final.mask$mask.threshold
  image.mask           <- final.mask$mask
  
  # mark image border
  
  image.border <- fun_mask_border(image.mask, mask = "th")
  
  # fill final mask
  
  image.disc <- fun_mask_fill(image.border, mask = "border")
  
  # calculate final mask center coordinates and radius
  
  disc.center <- fun_disc_center(image.disc, disc = "fill")
  
  # draw final circle with known center coordinates and radius
  
  disc.circle <- fun_mask_circle(x, disc.center = disc.center, 
                                 border.pix = add.border.pix)
  
  # fill final circle 
  
  disc.mask <- fun_mask_fill(disc.circle, mask = "circle")

  # gather available image information
  
  disc.info <- image.border %>% 
    left_join(disc.circle, by=c("i","j")) %>% 
    select(-th)
  
  # extract disc image
  
  disc.image <- fun_disc_math(disc.info, values_1 = "x", disc.mask, 
                              values_2 = "fill", method = "mult", 
                              values.name = image.values.name)
  
  # asses image quality
  
  disc.quality <- fun_disc_quality(disc.image, hdrlst = hdrlst, 
                                   disc.center = disc.center, 
                                   border = "border")
  
  # update hdrlst and header
  
  hdrlst$CENTER_X <- disc.center$x_i
  hdrlst$CENTER_Y <- disc.center$y_j
  hdrlst$RADIUS   <- disc.center$r
  hdrlst$DSKTHRSH <- image.mask.threshold
  hdrlst$BORDRPIX <- add.border.pix
  hdrlst$PIXSCALE <- hdrlst$SOLAR_D / (2 * disc.center$r)
  hdrlst$IMQTYPIX <- disc.quality$sigma.border$sd.pix
  hdrlst$IMQTYARC <- disc.quality$sigma.border$sd.arcsec
    
  cimages <- addKwv("CENTER_X", disc.center$x_i, "Sun center in X dim (pix)",
                    header)
  cimages <- addKwv("CENTER_Y", disc.center$y_j, "Sun center in Y dim (pix)",
                    cimages)
  cimages <- addKwv("X_RADIUS", disc.center$r, 
                    "Solar disc radius in X dim (pix)",
                    cimages)
  cimages <- addKwv("Y_RADIUS", disc.center$r, 
                    "Solar disc radius in Y dim (pix)",
                    cimages)
  cimages <- addKwv("DSKTHRSH", image.mask.threshold,
                    "Threshold defining solar limb mask (ADU)",
                    cimages)
  cimages <- addKwv("BORDRPIX", add.border.pix,
                    "Number of pixels added to solar radius (pix)",
                    cimages)
  cimages <- addKwv("PIXSCALE", hdrlst$SOLAR_D / (2 * disc.center$r),
                    "Image scale (arcsec/pix)",
                    cimages)
  cimages <- addKwv("IMQTYPIX", hdrlst$IMQTYPIX,
                    "Image quality (pix)",
                    cimages)
  cimages <- addKwv("IMQTYARC", hdrlst$IMQTYARC,
                    "Image quality (arcsec/pix)",
                    cimages)
  cimages <- addHistory("  Disc extraction with sunxplrr::mod_disc_image",
                    cimages)

  header <- cimages

  # add heliographic coordinate information to disc.image
  
  disc.coordinates <- fun_disc_coordinates(disc.image, hdrlst = hdrlst)
  
  # add grid coordinates to disc.image
  
  disc.grid <- fun_disc_grid(disc.coordinates, hdrlst = hdrlst, 
                             grid_each_deg = grid_each_deg, 
                             res_each_deg_on_grid = res_each_deg_on_grid)
  
  # add image with disc.grid on disc.image
  
  disc.grid.image <- fun_grid_plt(disc.grid, hdrlst = hdrlst)
  
  # return
  
  z <- list(disc.image = disc.grid.image, hdrlst = hdrlst, header = header)
  
  return(z)
  
}