# A tidylist is a list collecting one or several tidy data frames

#' Creator function for tidylists
#'
#' @export
tidylist <- function(...) {
  z <- list(...)
  nnl <- as.list(match.call())[-1]
  nn <- vapply(nnl, as.character, "")
  names(z) <- nn
  tidylist_ensure(z)
}


#' Ensure a tidylist contains only 'tidy' dfs
#'
#' @export
tidylist_ensure <- function(x) {
  stopifnot(inherits(x, "list"))
  is.df <- vapply(x, function(e) inherits(e, "data.frame"), TRUE)
  if (any(!is.df)) {
    stop(
      "some objects are not of class 'data.frame': ",
      paste(names(is.df)[!is.df], collapse = ", ")
    )
  }
  if (is.null(names(x)) || any("" %in% names(x))) {
    stop("all elements of list must be named")
  }
  lapply(x, as_tibble)
}


#' @export
tidylist_write <- function(x, path = ".", fst = FALSE, verbose = TRUE) {
  x <- tidylist_ensure(x)
  path <- normalizePath(path, mustWork = TRUE)
  file.ending <- if (fst) "fst" else "csv"

  for (name.i in names(x)) {
    file.i <- paste0(file.path(path, name.i), ".", file.ending)
    if (verbose) cat("writing: ", file.i, "\n")
    if (fst) {
      library(fst)
      write_fst(x[[name.i]], path = file.i)
    } else {
      data.table::fwrite(x[[name.i]], file = file.i, sep = ";")
    }
  }
}

#' Import a Tidylist from a Collection of tidy CSV files
#'
#' This funtion is also used in caching mode.
#'
#' @param path character, containg the path(s) to a directory or to one or several CSV files.
#'
#' @export
tidylist_read <- function(path = ".", fst = FALSE, verbose = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)

  file.ending <- if (fst) "fst" else "csv"
  ptrn <- paste0("\\.", file.ending, "$")

  if (!grepl(ptrn, path[1])) {
    files <- list.files(path, full.names = TRUE)
    is.file <- grepl(ptrn, files)
    if (length(files[!is.file]) > 0) {
      message("ignoring non data files: \n", paste(files[!is.file], collapse = "\n"))
      files <- files[is.file]
    }
  } else {
    files <- path
  }

  z <- list()
  for (file.i in files) {
    if (verbose) cat("reading: ", file.i, "\n")
    # message("reading: ", file.i)
    if (fst) {
      library(fst)
      z[[file.i]] <- read_fst(path = file.i)
    } else {
      z[[file.i]] <- data.table::fread(file = file.i, sep = ";", na.strings = c("NA", ""), encoding = "UTF-8")
    }
  }
  names(z) <- gsub(ptrn, "", basename(files))
  tidylist_ensure(z)
}
