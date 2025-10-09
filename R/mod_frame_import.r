#' @title Imports a single image file
#'
#' @description Imports single image file given the file path and name. Returns 
#'   tibble with image, tibble with hdrlst and character vector header. Changes 
#'   image header, flips and flops the image, if requested.
#'
#' @param inp_data_path string with input data path.
#'
#' @param inp_file_name string with input file name.
#'
#' @param parse.filename boolean switch for dummy use for frames with 
#'   existing fits keywords.
#' 
#' @param parse.method string method for parsing the provided file.name.
#'
#' @param zero.pos.angle boolean if TRUE P0 is set to zero indicating, that the 
#'   heliographic aequator lies parallel to the image x-axis.
#'
#' @param delta.p num angle in degrees between image x-axes and true RA-axis
#'   as measured counterclockwise from image axes. Not considered in the case
#'   where zero.pos.angle is TRUE.
#'   
#' @param flip.image boolean if TRUE the image will be flipped.
#'   
#' @param flop.image boolean if TRUE the image will be flopped.
#'   
#' @return list with fitsim, hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-09 / Frt
# - `Created`    : 2025-05-20 / Frt
# - `Last test`  : 2025-10-09 / Frt
#
mod_frame_import <- function(inp_data_path,
                             inp_file_name,
                             parse.filename = "TRUE",
                             parse.method = "SDO/HMI",
                             zero.pos.angle = "TRUE",
                             delta.p = 0,
                             flip.image = "FALSE",
                             flop.image = "FALSE"){
  
  # parses file extension of input file name
  
  file.ext <- fun_parse_file_ext(inp_file_name)
  
  if (file.ext %in% c("jpg", "jpeg", "JPG", "JPEG")){
    
    # reads JPG image 
    
    im <- fun_read_jpg_image(paste0(inp_data_path,inp_file_name))
    
  } else {
    
    stop(c("Image file extension is not supported"))
    
  }
  
  # image header
  
  header <- im$header
  
  # parses header list
  
  hdrlst <- fun_hdr2list(im$hdr) 
  
  # constructs file-specific keywords, if necessary
  
  parsed.keywords <- fun_parse_keywords(file.name = inp_file_name,
                                        header = header, 
                                        hdrlst = hdrlst,
                                        parse.filename = parse.filename,
                                        parse.method = parse.method)
  
  header <- parsed.keywords$header
  hdrlst <- parsed.keywords$hdrlst
  
  # calculates ephemeris for physical coordinates of the Sun
  
  sun.ephem <- fun_sun_ephem(header = header,
                             hdrlst = hdrlst, 
                             zero.pos.angle = zero.pos.angle,
                             delta.p = delta.p) 
  
  header <- sun.ephem$header
  hdrlst <- sun.ephem$hdrlst
  
  # converts matrix containing image frame in a tibble 
  
  fitsim <- fun_mat2tibbl(im$imDat)
  
  # flips the image, if required
  
  im_flip <- fun_flip_image(image = fitsim, 
                            hdrlst = hdrlst,
                            header = header,
                            flip.image = flip.image)
  
  fitsim <- im_flip$flipped_image
  hdrlst <- im_flip$hdrlst
  header <- im_flip$header
  
  # flops the image, if required
  
  im_flop <- fun_flop_image(image = fitsim, 
                            hdrlst = hdrlst,
                            header = header,
                            flop.image = flop.image)
  
  fitsim <- im_flop$flopped_image
  hdrlst <- im_flop$hdrlst
  header <- im_flop$header
  
  # updates hdrlst and header
  
  hdrlst$FILENAME <- inp_file_name
  
  cimages <- addKwv("FILENAME", inp_file_name, "Original input file name",
                    header)
  cimages <- addHistory("  image file import with sunxplrr::mod_frame_import",
                        cimages)
  
  header <- cimages
  
  # return
  
  z <- list(fitsim = fitsim, hdrlst = hdrlst, header = header)
  
  return(z)
  
}