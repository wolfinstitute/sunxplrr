#' @title Reads JPG image from disc
#'
#' @description Reads monochrome or three color JPG image from disc. Constructs
#'  minimal FITS header and returns image with the provided bit depth.
#'
#' @param filename input path and file name.
#'
#' @param bitpix int image will be scaled to the provided bit depth.
#'
#' @return list with image matrix, parsed header and full header vectors.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#' 
#' @export

# - `Last change`: 2025-05-21 / Frt
# - `Created`    : 2025-05-20 / Frt
# - `Last test`  : 2025-05-21 / Frt
#
fun_tile_jpg_image <- function(filename = "sun_logo.jpg", out_data_path){
  
  file.name <- basename(filename)
  key.words <- strsplit(strsplit(file.name, "[.]")[[1]][1], "_")[[1]]
  date <- key.words[1] 
  time <- key.words[2]
  instr <- "SDO"
  resolution <- "2048"
  
  out.path.2048 <- paste(out_data_path,"2048", sep = "/")
  out.path.512 <- paste(out_data_path,"512", sep = "/")
  
  out.filename <- letters[1:8]
  out_path_512 <- letters[1:7]
  
  im   <- image_read(filename)
  
  im2k <- image_scale(im, "2048")
  
  im_flip <- image_flip(im2k)
  
  im_0 <- image_flop(im_flip)
  out.filename[1] <- paste(date,time,instr,resolution,"00.jpg", sep = "_") 
  out_path_2048 <- image_write(im_0, path = paste(out.path.2048, out.filename[1], sep = "/"), format = "jpg")
  
  im_1 <- image_crop(im_0, "512x512+512+512")
  out.filename[2] <- paste(date,time,instr,resolution,"01.jpg", sep = "_")
  out_path_512[1] <- image_write(im_1, path = paste(out.path.512, out.filename[2], sep = "/"), format = "jpg")
  
  im_2 <- image_crop(im_0, "512x512+768+512")
  out.filename[3] <- paste(date,time,instr,resolution,"02.jpg", sep = "_") 
  out_path_512[2] <- image_write(im_2, path = paste(out.path.512, out.filename[3], sep = "/"), format = "jpg")
  
  im_3 <- image_crop(im_0, "512x512+1024+512")
  out.filename[4] <- paste(date,time,instr,resolution,"03.jpg", sep = "_") 
  out_path_512[3] <- image_write(im_3, path = paste(out.path.512, out.filename[4], sep = "/"), format = "jpg")
  
  im_4 <- image_crop(im_0, "512x512+512+1024")
  out.filename[5] <- paste(date,time,instr,resolution,"04.jpg", sep = "_") 
  out_path_512[4] <- image_write(im_4, path = paste(out.path.512, out.filename[5], sep = "/"), format = "jpg")
  
  im_5 <- image_crop(im_0, "512x512+768+1024")
  out.filename[6] <- paste(date,time,instr,resolution,"05.jpg", sep = "_") 
  out_path_512[5] <- image_write(im_5, path = paste(out.path.512, out.filename[6], sep = "/"), format = "jpg")
  
  im_6 <- image_crop(im_0, "512x512+1024+1024")
  out.filename[7] <- paste(date,time,instr,resolution,"06.jpg", sep = "_") 
  out_path_512[6] <- image_write(im_6, path = paste(out.path.512, out.filename[7], sep = "/"), format = "jpg")
  
  im_7 <- image_crop(im_0, "512x512+768+768")
  out.filename[8] <- paste(date,time,instr,resolution,"07.jpg", sep = "_") 
  out_path_512[7] <- image_write(im_7, path = paste(out.path.512, out.filename[8], sep = "/"), format = "jpg")
  
  # Return
  z <- list(out_filename = out.filename, out_path_512 = out_path_512, out_path_2048 = out_path_2048)
  
  return(z)
  
}
