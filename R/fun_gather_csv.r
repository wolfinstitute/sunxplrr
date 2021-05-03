#' @title Gathers output files of an index
#'
#' @description Gathers all output files of an index within an out_data_path. 
#' 
#' @param out_data_path full path to output directory.
#' 
#' @param index.name character string with full index name as contained in the 
#'   file name after the image id and before the file extension.
#'
#' @return tibble containing the calibrated image values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-01-17 / Frt
# - `Created`    : 2020-01-17 / Frt
# - `Last test`  : 2020-01-17 / Frt
#
fun_gather_csv <- function(out_data_path, index.name = "_total_indices"){
  
  # create list of all files in out_data_path

  list_files <- list.files(out_data_path, recursive = TRUE)
  
  # extract file names of index.name

  list_index_files <- list_files[grep(index.name, list_files)]
  
  # add image_id, year, month and day
  
  list_results <- as.data.frame(list_index_files, stringsAsFactors = FALSE) %>%  
    mutate(image_id = str_replace(string = list_index_files, 
                                  pattern = paste0(index.name,".csv"), 
                                  replacement = ""))
  
  pos = str_locate(list_results$image_id, "/")[,1] 
  
  list_results <- bind_cols(list_results, pos = pos) %>% 
    mutate(pos = if_else(is.na(pos), as.integer(0), pos)) %>% 
    mutate(image_id = str_sub(string = image_id, start = pos+1)) %>%
    mutate(year = 
             as.numeric(str_sub(string = image_id, start = 1, end = 4))) %>% 
    mutate(month = 
             as.numeric(str_sub(string = image_id, start = 5, end = 6))) %>% 
    mutate(day = 
             as.numeric(str_sub(string = image_id, start = 7, end = 8)))
  
  # read all files in list_results and add year, month and day
  
  out <- NULL
  
  for (i in 1:nrow(list_results)) {
    
    path <- paste0(out_data_path,list_results$list_index_files[i])
    
    results <- read.csv2(path) %>%   
      mutate(year = list_results$year[i]) %>% 
      mutate(month = list_results$month[i]) %>% 
      mutate(day = list_results$day[i])
    
    out <- bind_rows(out, results)
    
  }

  # return

  z <- bind_cols(out[,(ncol(out)-2):ncol(out)],out[,1:(ncol(out)-3)])

  return(z)
  
}