#' @title Utilities for modifying FITS header entries
#'
#' @description Utilities for modifying FITS header entries.
#'
#' @return string vector with modified header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-25 / Frt
# - `Created`    : 2019-12-25 / Frt
# - `Last test`  : 2019-12-25 / Frt
#
newKwv <- function (keyw, val, note = ""){
  if (!is.character(keyw)) 
    stop("*** Non-numeric keyword must be a string ***")
  keyw <- toupper(strtrim(keyw, 8))
  noteSep <- ifelse(note == "", " ", "/")
  if (is.numeric(val)) {
    txt <- sprintf("%-20.14g   %s %-46s", val, noteSep, 
                   note)
  }
  else {
    if (nchar(val) > 68) {
      val <- strtrim(val, 68)
      cat("   *** Truncated value string for keyword =", 
          keyw, "to 68 characters ***\n")
      warning("Truncated value string in newKw")
    }
    txt <- sprintf("'%-1s' %s %-59s", val, noteSep, note)
  }
  sprintf("%-8s= %-70s", keyw, strtrim(txt, 70))
}

addKwv <- function (keyw, val, note = "", headerName){
  if (!is.character(keyw)) 
    stop("*** Keyword must be a string ***")
  if (!is.character(note)) 
    stop("*** Note must be a string ***")
  keyw <- toupper(strtrim(keyw, 8))
  note <- strtrim(note, 47)
  headerName <- c(headerName, newKwv(keyw, val, note))
  headerName
}

delKwv <- function (keyw, headerName){
  keyw <- sprintf("%-8s=", substr(keyw, 1, 8))
  idx <- which(toupper(keyw) == substr(toupper(headerName), 
                                       1, 9))
  if (length(idx) == 0) 
    stop("*** No such keyword to delete ***")
  headerName <- headerName[-idx]
  headerName
}

modVal <- function (keyw, val, note = "", headerName){
  keyw <- sprintf("%-8s=", substr(keyw, 1, 8))
  idx <- which(toupper(keyw) == substr(toupper(headerName), 
                                       1, 9))
  if (length(idx) == 0) 
    stop("*** No such keyword to modify ***")
  if (length(idx) > 1) 
    stop("*** Multiple keywords to modify ***")
  tmp <- headerName[idx]
  tmp <- unlist(strsplit(tmp, "/"))
  if (note == "") {
    note <- ifelse(length(tmp) == 2, tmp[2], "")
  }
  else {
    if (!is.character(note)) 
      stop("*** Note must be a string ***")
    note <- strtrim(note, 47)
  }
  keyw <- unlist(strsplit(gsub(" ", "", tmp[1]), "="))[1]
  headerName[idx] <- newKwv(keyw, val, note)
  headerName
}

addComment <- function (comment, headerName){
  if (!is.character(comment)) 
    stop("*** Comment must be a string ***")
  comment <- strtrim(comment, 72)
  headerName <- c(headerName, sprintf("COMMENT %-72s", comment))
  headerName
}

addHistory <- function (history, headerName){
  if (!is.character(history)) 
    stop("*** History must be a string ***")
  history <- strtrim(history, 72)
  headerName <- c(headerName, sprintf("HISTORY %-72s", history))
  headerName
}

closeHdr <- function (headerName){
  headerName <- c(headerName, sprintf("END%-77s", " "))
  tmp <- paste(headerName, collapse = "")
  len <- nchar(tmp)
  nch <- 36 * 80
  nblocks <- ceiling(len/nch)
  nfill <- nblocks * nch - len
  tmp <- paste(c(tmp, sprintf("%*s", nfill, " ")), collapse = "")
  out <- character(nblocks)
  sta <- 1
  sto <- nch
  for (i in 1:nblocks) {
    out[i] <- substr(tmp, sta, sto)
    sta <- sta + nch
    sto <- sto + nch
  }
  out
}
