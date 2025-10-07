#' @title Runs the sunxplrr modules for the SDO whitelight jpg image analysis
#'
#' @description Runs the sunxplrr modules for the exploration of whitelight
#'   jpg images as provided by the jsoc server of SDO.
#'
#' @param inp_file_name input file name with extension.
#'
#' @param sdo.image if TRUE, param.lst for SDO image analysis is chosen.
#'
#' @param inp_data_path full path to input directory.
#'
#' @param out_data_path full path to output directory.
#' 
#' @param exchange.header boolean if TRUE the header of the original image will
#'   be changed.
#'   
#' @param inp_hfile_name string with input file name of the new header file.
#'
#' @param rds.output if TRUE all results are saved as R data file.
#'
#' @param full.output if TRUE the full image data table is saved as csv file.
#'
#' @param light.save if TRUE only a small selection of csv files are saved.
#'
#' @param fits.save if TRUE some fits images are saved.
#'
#' @param jpg.save if TRUE some daily charts are saved as jpg files.
#'
#' @return list with resulting image and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-07 / Frt
# - `Created`    : 2019-12-28 / Frt
# - `Last test`  : 2025-10-07 / Frt
#
wrap_mod_jpg_sdo <- function(inp_file_name, sdo.image, 
                             inp_data_path, out_data_path,
                             rds.output = "FALSE", 
                             full.output = "FALSE",
                             light.save = "FALSE",
                             fits.save = "FALSE",
                             jpg.save = "FALSE"){
  
# Modul Zusammenstellen der Parameter Liste ------------------------------------
  
elapsed0 <- system.time(

  param.lst <- mod_load_param_jpg_sdo(inp_file_name, sdo.image, 
                                      inp_data_path, out_data_path,
                                      rds.output = rds.output,
                                      full.output = full.output,
                                      light.save = light.save,
                                      fits.save = fits.save,
                                      jpg.save = jpg.save)
      
)[1]
  
message("  sunxplrr::mod_load_param_jpg_sdo for file ", param.lst$inp_file_name,
        " finished. Elapsed time ", elapsed0, " seconds")

# Modul Importieren Bilddatei --------------------------------------------------

elapsed1 <- system.time(

  mod.frame.import <- mod_frame_import(inp_data_path = param.lst$inp_data_path,
                                       inp_file_name = param.lst$inp_file_name,
                                       sdo.image = param.lst$sdo.image,
                                       parse.method = param.lst$parse.method,
                                       flip.image = param.lst$flip.image,
                                       flop.image = param.lst$flop.image)
    
)[1]

fitsim     <- mod.frame.import$fitsim
hdrlst     <- mod.frame.import$hdrlst
header     <- mod.frame.import$header

message("  sunxplrr::mod_frame_import ", param.lst$inp_file_name,
        " finished. Elapsed time ", elapsed1, " seconds")

# Modul Identifizieren der Sonnenscheibe ---------------------------------------

elapsed2 <- system.time(
  
  mod.disc.image <- mod_disc_image(fitsim = fitsim,
                          hdrlst = hdrlst,
                          header = header,
                          cut.image = param.lst$cut.image,
                          threshold = param.lst$threshold,
                          method = param.lst$method,
                          cut.threshold = param.lst$cut.threshold,
                          cut.method = param.lst$cut.method,
                          cut.border.pix = param.lst$cut.border.pix,
                          add.border.pix = param.lst$add.border.pix,
                          image.values.name = param.lst$image.values.name,
                          grid_each_deg = param.lst$grid_each_deg,
                          res_each_deg_on_grid = param.lst$res_each_deg_on_grid)

)[1]

disc.image  <- mod.disc.image$disc.image
hdrlst      <- mod.disc.image$hdrlst
header      <- mod.disc.image$header

message("  sunxplrr::mod_disc_image ... finished. Elapsed time ", 
        elapsed2, " seconds")

# Modul Flatkorrektur ----------------------------------------------------------

elapsed3 <- system.time(

  mod.disc.flat <- mod_disc_flat(disc.image,
                                 hdrlst = hdrlst,
                                 header = header,
                                 mean.method = param.lst$mean.method,
                                 sdo.image = sdo.image)

)[1]

disc.flat     <- mod.disc.flat$disc.flat
hdrlst        <- mod.disc.flat$hdrlst
header        <- mod.disc.flat$header

message("  sunxplrr::mod_disc_flat ... finished. Elapsed time ", 
        elapsed3, " seconds")

# Modul Plage und Network Extraktion -------------------------------------------

elapsed4 <- system.time(

  mod_feature_extraction <- mod_feature_extraction(disc.flat,
                                    hdrlst = hdrlst,
                                    header = header,
                                    plage.contrast = param.lst$plage.contrast, 
                                    en.contrast = param.lst$en.contrast, 
                                    qn.contrast = param.lst$qn.contrast)

)[1]

disc.features <- mod_feature_extraction$disc.features
hdrlst        <- mod_feature_extraction$hdrlst
header        <- mod_feature_extraction$header

message("  sunxplrr::mod_feature_extraction ... finished. Elapsed time ",
        elapsed4, " seconds")

# Modul Index Berechnung -------------------------------------------------------

elapsed5 <- system.time(

  mod_index_calculation <- mod_index_calculation(disc.features,
                                    hdrlst = hdrlst,
                                    header = header)

)[1]

images             <- mod_index_calculation$images
hdrlst             <- mod_index_calculation$hdrlst
header             <- mod_index_calculation$header
total.indices      <- mod_index_calculation$total.indices 
hemisphere.indices <- mod_index_calculation$hemisphere.indices
latitude.indices   <- mod_index_calculation$latitude.indices
chart.indices      <- mod_index_calculation$chart.indices 
charts             <- mod_index_calculation$charts
synopsis.indices   <- mod_index_calculation$synopsis.indices 
synopsis           <- mod_index_calculation$synopsis

message("  sunxplrr::mod_index_calculation ... finished. Elapsed time ",
        elapsed5, " seconds")

# Modul Output -----------------------------------------------------------------

elapsed6 <- system.time(
  
  mod_output <- mod_output(images = images,
                           hdrlst = hdrlst,
                           header = header,
                           total.indices = total.indices, 
                           hemisphere.indices = hemisphere.indices,
                           latitude.indices = latitude.indices,
                           chart.indices = chart.indices, 
                           charts = charts,
                           synopsis.indices = synopsis.indices,
                           synopsis = synopsis,
                           inp_file_name = param.lst$inp_file_name,
                           rds.output = param.lst$rds.output,
                           full.output = param.lst$full.output,
                           light.save = param.lst$light.save,
                           fits.save = param.lst$fits.save,
                           jpg.save = param.lst$jpg.save,
                           out_data_path = param.lst$out_data_path)

)[1]

message("  sunxplrr::mod_output ... finished. Elapsed time ", elapsed6,
        " seconds")

# Return -----------------------------------------------------------------------

message("  sunxplrr::wrap_mod_calcium of file ", param.lst$inp_file_name,
        " finished.")

z <- "done"

# if (param.lst$light.save){
#
#  z <- "done"
#  
# } else {
# 
#   z <- mod_output
# 
# }

return(z)

}