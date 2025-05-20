#' @title Searches for the best matching sdo image of a provided observing time
#'
#' @description Searches for the best matching sdo image of a provided observing
#' time. The resulting path is optimised for the download of image files from 
#' jsoc. Some of the code was provided by chatGPT.
#' 
#' @param url Character vector containing root path to observing day.
#'
#' @param time Character time vector containing HHMM.
#' 
#' @return character vector containing resulting path to matching SDO image.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2025-05-20 / Frt
# - `Created`    : 2025-05-19 / Frt
# - `Last test`  : 2025-05-20 / Frt
#
fun_match_sdo_image <- function(url, time){
  
  # HTML-Seite parsen
  page <- read_html(url)
  
  # Alle Links extrahieren
  file_links <- page %>%
    html_nodes("a") %>%
    html_attr("href")
  
  # Nur Dateien mit gültigem Format (z. B. .fits, .jpg, .png etc.)
  file_links <- file_links[grepl("\\.(fits|jpg|jpeg|png|txt|dat|gz)$", 
                                 file_links, ignore.case = TRUE)]
  
  # Nur Dateien die den String "Ic_flat_4k" enthalten weiterverwenden
  file_links <- file_links[grepl("(?=.*Ic_flat_4k)", file_links, perl = TRUE)]
  
  # Loop für die nächstliegende Datei zur Beobachtungszeit

  file_links <- tibble(file_links = file_links) |> 
    mutate(obstime = 0)
  
  # Absolute Differenz Standard-Beobachtungszeit versus SDO-Beobachtungszeit
  for (idx2 in 1:nrow(file_links)){
    
    file_links[idx2,2] <- abs(time - 
        as.integer(
          fun_parse_obs_time(file.name = as.character(file_links[idx2,1]))))
    
  }
  
  # Filter nach der kleinsten absoluten Differenz
  file_links <- file_links |> 
    filter(obstime == min(obstime))

  # URL erstellen
  path_to_matching_sdo_image <- paste0(url, file_links[1,1])
  
  # Return
  
  return(path_to_matching_sdo_image)
  
}