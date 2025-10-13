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
#' @param inp_file_name string with input file name.
#'
#' @param out_data_path full path to output directory.
#'
#' @param total.indices tibble containing total.indices.
#'
#' @param hemisphere.indices tibble containing hemisphere.indices.
#'
#' @param latitude.indices tibble containing latitude.indices.
#'
#' @param charts tibble containing charts data.
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

# - `Last change`: 2025-10-13 / Frt
# - `Created`    : 2020-01-05 / Frt
# - `Last test`  : 2025-10-13 / Frt
#
mod_output <- function(images, hdrlst, header,
                       inp_file_name,
                       out_data_path,
                       total.indices, 
                       hemisphere.indices,
                       latitude.indices, 
                       charts, 
                       rds.output = "FALSE",
                       full.output = "FALSE",
                       light.save = "FALSE",
                       fits.save = "FALSE",
                       jpg.save = "FALSE"){
  
  # strip file name
  
  name <- strsplit(inp_file_name, "[.]")[[1]][1]
  
  # construct full data
 
  z <- list(images = images, 
            hdrlst = hdrlst, 
            header = header,
            total.indices = total.indices, 
            hemisphere.indices = hemisphere.indices,
            latitude.indices = latitude.indices,
            charts = charts)
  
  # selected output
  
  if (rds.output){  
  
    filename <- paste0(out_data_path, name,"_images.rds")
    readr::write_rds(z, filename)
    
  }
  
  if (full.output){  
    
    filename.images <- paste0(out_data_path, name,"_images.csv")
    readr::write_csv2(images, filename.images)

    filename.charts <- paste0(out_data_path, name,"_charts.csv")
    readr::write_csv2(charts, filename.charts)

  }
  
  if (light.save){
  
    # total indices
    
    filename.total.indices <- 
      paste0(out_data_path,name,"_total_indices.csv")
    readr::write_csv2(total.indices, filename.total.indices)
    
    # hemisphere indices
    
    filename.hemisphere.indices <- 
      paste0(out_data_path,name,"_hemisphere_indices.csv")
    readr::write_csv2(hemisphere.indices, filename.hemisphere.indices)
    
    # latitude indices
    
    filename.latitude.indices <- 
      paste0(out_data_path,name,"_latitude_indices.csv")
    readr::write_csv2(latitude.indices, filename.latitude.indices)
    
  }
  
  if (fits.save){
    
    # disc flat
    
    images.save <- images %>%
      mutate(flat = 55000*flat) %>%
      select(i, j, x = flat)
    
    imDat <- fun_tibbl2mat(images.save)
    
    out_file_name = "_disc_flat.fit"
    fun_write_image(imDat, paste0(out_data_path, name, out_file_name), 
                    hdrlst, header)
    
    # disc penandum
   images.save <- images %>%
      mutate(penandum = 55000*penandum) %>%
      select(i, j, x = penandum)
    
    imDat <- fun_tibbl2mat(images.save)
    
    out_file_name = "_disc_penandum.fit"
    fun_write_image(imDat, paste0(out_data_path, name, out_file_name), 
                    hdrlst, header)
    
    # chart flat
    
    charts.save <- charts %>%
      mutate(flat = 55000*flat) %>%
      select(i = longitude, j = latitude, x = flat)
    
    imDat <- fun_tibbl2mat(charts.save)
    
    out_file_name = "_chart_flat.fit"
    fun_write_image(imDat, paste0(out_data_path, name, out_file_name), 
                    hdrlst, header)
    
    # chart penandum
    
    charts.save <- charts %>%
      mutate(penandum = 55000*penandum) %>%
      select(i = longitude, j = latitude, x = penandum)
    
    imDat <- fun_tibbl2mat(charts.save)
    
    out_file_name = "_chart_penandum.fit"
    fun_write_image(imDat, paste0(out_data_path, name, out_file_name), 
                    hdrlst, header)
    
  }
  
  if(jpg.save){
    
    # disc flat
    
    plot.disc.flat <- ggplot(images, aes(i, j)) +
                        geom_raster(aes(fill = 55000*flat)) +
                        scale_fill_gradient(low="black", high="white") +
                        coord_fixed(ratio = 1)

    out_file_name = "_disc_flat.jpg"
    ggplot2::ggsave(paste0(out_data_path, name, out_file_name))
    
    # disc penandum
    
    plot.disc.penandum <- ggplot(images, aes(i, j)) +
                            geom_raster(aes(fill = 55000*penandum)) +
                            scale_fill_gradient(low="black", high="white") +
                            coord_fixed(ratio = 1)
    
    out_file_name = "_disc_penandum.jpg"
    ggplot2::ggsave(paste0(out_data_path, name, out_file_name))
    
    # chart flat
    
    plot.chart.flat <- ggplot(charts, aes(longitude, latitude)) +
                         geom_raster(aes(fill = 55000*flat)) +
                         scale_fill_gradient(low="black", high="white") +
                         coord_fixed(ratio = 1)

    out_file_name = "_chart_flat.jpg"
    ggplot2::ggsave(paste0(out_data_path, name, out_file_name))
    
    # chart penandum
    
    plot.chart.penandum <- ggplot(charts, aes(longitude, latitude)) +
                             geom_raster(aes(fill = 55000*penandum)) +
                             scale_fill_gradient(low="black", high="white") +
                             coord_fixed(ratio = 1)
    
    out_file_name = "_chart_penandum.jpg"
    ggplot2::ggsave(paste0(out_data_path, name, out_file_name))
    
  }
  
  # return
  
  return(z)
  
}
