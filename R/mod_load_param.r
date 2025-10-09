#' @title Sets the parameter list for solar image analysis
#'
#' @description Sets the parameter list for SDO jpg image analysis.
#'
#' @param inp_file_name input file name with extension.
#'
#' @param sdo.image if TRUE, param.lst for SDO image analysis is chosen.
#'
#' @param inp_data_path full path to input directory.
#'
#' @param out_data_path full path to output directory.
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
#' @return list containing param.lst.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-09 / Frt
# - `Created`    : 2020-01-13 / Frt
# - `Last test`  : 2025-10-09 / Frt
#
mod_load_param <- function(inp_file_name, 
                           sdo.image = "TRUE",
                           inp_data_path, 
                           out_data_path,
                           rds.output = "FALSE", 
                           full.output = "FALSE",
                           light.save = "FALSE",
                           fits.save = "FALSE",
                           jpg.save = "FALSE"){

  if (sdo.image){
  
    param.lst <- list(
      #
      # mod_frame_import
      #
      inp_data_path = inp_data_path, 
      inp_file_name = inp_file_name,
      sdo.image = "TRUE",
      parse.filename = "TRUE",
      parse.method = "SDO/HMI",
      zero.pos.angle = "TRUE",
      delta.p = 0,
      flip.image = "FALSE",
      flop.image = "FALSE",
      #
      # mod_disc_image
      #
      cut.image = "TRUE",
      threshold = 10, 
      method = "relative",
      cut.threshold = 20, 
      cut.method = "relative",
      cut.border.pix = 100,
      add.border.pix = -4,
      image.values.name = "image",
      grid_each_deg = 10,
      res_each_deg_on_grid = 0.01,
      #
      # mod_disc_flat
      #
      mean.method = "mode",
      #
      # mod_spot_extraction
      #
      spot.threshold = 0.85,
      umbra.threshold = 0.55,
      #
      # mod_output
      #
      light.save = light.save,
      rds.output = rds.output,
      full.output = full.output,
      fits.save = fits.save,
      jpg.save = jpg.save
    )

  } else {
    
    param.lst <- list(
      #
      # mod_fits_import
      #
      inp_data_path = inp_data_path,
      sdo.image = "FALSE",
      inp_file_name = inp_file_name,
      delta.p = 0,
      #
      # mod_disc_image
      #
      cut.image = "FALSE",
      threshold = 10, 
      method = "relative",
      cut.threshold = 20, 
      cut.method = "relative",
      cut.border.pix = 100,
      add.border.pix = -3,
      image.values.name = "image",
      grid_each_deg = 10,
      res_each_deg_on_grid = 0.01,
      #
      # mod_clv_correction
      #
      model = "poly_with_plane",
      run = 3,
      clip.resid.out = "FALSE",
      sclv.method = "NL",
      #
      # mod_feature_extraction
      #
      plage.contrast = 1.35, 
      en.contrast = 1.25, 
      qn.contrast = 1.10,
      #
      # mod_output
      #
      light.save = light.save,
      rds.output = rds.output,
      full.output = full.output,
      fits.save = fits.save,
      jpg.save = jpg.save,
      out_data_path = out_data_path
    )

  }
  
  # return
  
  return(param.lst)
  
}