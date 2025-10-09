#' @title Saves results to disk
#'
#' @description Saves results to disk. 
#'
#' @param images tibble containing the previously calculated images.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param total.indices tibble containing total.indices.
#'
#' @param hemisphere.indices tibble containing hemisphere.indices.
#'
#' @param latitude.indices tibble containing latitude.indices.
#'
#' @param chart.indices tibble containing chart.indices.
#'
#' @param charts tibble containing charts data.
#'
#' @param synopsis.indices tibble containing synopsis.indices.
#'
#' @param synopsis tibble containing synopsis data.
#'
#' @param inp_file_name string with input file name.
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
#' @return list containing images, hdrlst, header, indices and charts.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-09 / Frt
# - `Created`    : 2020-01-05 / Frt
# - `Last test`  : 2025-10-09 / Frt
#
mod_output <- function(x, hdrlst, header,
                       total.indices, 
                       hemisphere.indices,
                       latitude.indices, 
                       chart.indices, 
                       charts, 
                       synopsis.indices,
                       synopsis,
                       rds.output = "FALSE",
                       full.output = "FALSE",
                       light.save = "FALSE",
                       fits.save = "FALSE",
                       jpg.save = "FALSE"){
  
  # strip file name
  
  name <- strsplit(hdrlst$inp_file_name, "[.]")[[1]][1]
  
  # out data path
  
  out_data_path = hdrlst$out_data_path
  
  # construct full data
  
  z <- list(images = x, hdrlst = hdrlst, header = header,
            total.indices = total.indices, 
            hemisphere.indices = hemisphere.indices,
            latitude.indices = latitude.indices,
            chart.indices = chart.indices, 
            charts = charts,
            synopsis.indices = synopsis.indices,
            synopsis = synopsis)

  # save full data
    
  if (rds.output){  
  
    # filename <- paste0(hdrlst$out_data_path,name,"_results.rds") 
    # readr::write_rds(z, filename)
    
  }
  
  if (full.output){  
    
    # filename.images <- paste0(out_data_path,name,"_images.csv")
    # readr::write_csv2(x, filename.images)
    
  }
  
  # save csv output
  
  filename.total.indices <- 
    paste0(out_data_path,name,"_total_indices.csv")
  readr::write_csv2(total.indices, filename.total.indices)
  
  filename.hemisphere.indices <- 
    paste0(out_data_path,name,"_hemisphere_indices.csv")
  readr::write_csv2(hemisphere.indices, filename.hemisphere.indices)
  
  filename.latitude.indices <- 
    paste0(out_data_path,name,"_latitude_indices.csv")
  readr::write_csv2(latitude.indices, filename.latitude.indices)
  
  if (light.save == "FALSE"){
  
    filename.charts <- 
      paste0(out_data_path,name,"_charts.csv")
    readr::write_csv2(charts, filename.charts)
    
    filename.synopsis <- 
      paste0(out_data_path,name,"_synopsis.csv")
    readr::write_csv2(synopsis, filename.synopsis)
  
  }
  
  if (fits.save){
    
    # calib
    disc.calib.save <- images %>% 
      select(i, j, x=calib)
  
    imDat <- fun_tibbl2mat(disc.calib.save)
  
    out_file_name = paste0(name,"_calib.fit") 
    fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)
  
    #flat
    disc.flat.save <- images %>% 
      mutate(flat = 100*flat) %>%
      select(i, j, x=flat)
  
    imDat <- fun_tibbl2mat(disc.flat.save)
  
    out_file_name = paste0(name,"_flat.fit") 
    fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)
  
    #ttarea
    disc.ttarea.save <- images %>% 
      mutate(ttarea = 100*ttarea) %>%
      select(i, j, x=ttarea)
  
    imDat <- fun_tibbl2mat(disc.ttarea.save)
  
    out_file_name = paste0(name,"_ttarea.fit") 
    fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)
  
    #ttcntrst
    disc.ttcntrst.save <- images %>% 
      mutate(ttcntrst = 100*ttcntrst) %>%
      select(i, j, x=ttcntrst)
  
    imDat <- fun_tibbl2mat(disc.ttcntrst.save)
  
    out_file_name = paste0(name,"_ttcntrst.fit") 
    fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)
 
  }
  
  if(jpg.save){
    
    # ggplot(charts, aes(longitude, latitude)) +
    #   geom_raster(aes(fill = ttcntrst_disc)) +
    #   coord_fixed(ratio = 1)
    
  }
  
  # return
  
  return(z)
  
}
