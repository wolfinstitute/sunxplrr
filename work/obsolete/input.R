
# Grundpfad

path_init <- "D:/Test/In"

path_in <- file.path( path_init
   , "KALZIUM"
#  , "SDO"
)

path_out <-  file.path("D:/Test", "Out")

path <- normalizePath(path_in, mustWork = TRUE)

# unzip container if necessary
file.info(path)$isdir
dir.exists(unzip_path)
dir.create(unzip_path)

path_param <- normalizePath(file.path(path, "param"), mustWork = TRUE)
if (length(list.files(path_param)) == 0) stop("'path_param' is empty.")

list.files(path_in)
length(list.files(path_in))


asked_version <- readLines(file.path(path, "delfin"))[1]


# Identifier number construction

tl_PARAM_GLOBAL <- param_tidylist_read(file.path(path, "param"))


# ^ beginning of string
# . any character
# + at least one time (by default, + is greedy, meaning it catches as much as it can)
# ? non-greedy
container_suffix <- gsub("^.+?_", "", basename(path))

# vz hinzufuegen falls noch nicht vorhanden
if (!grepl(paste0("^", vz, "_"), container_suffix)) {
  container_suffix <- paste0(vz, "_", container_suffix)
}

identifier_number <- paste0(
  "fhh",
  format(Sys.time(), format = "%Y%m%d%H%M%S"),
  Sys.getenv("USERNAME"),
  "_",
  container_suffix
)

tl_PARAM_GLOBAL$PARAM_GLOBAL$identifier_number <- identifier_number

path_out_base <- file.path(
  path_out,
  identifier_number
)

dir.create(path_out_base)

path_out_out <- file.path(path_out_base, "out")
path_post_process <- file.path(path_out_base, "post_process")

# copy input to output
path_inp_container <- file.path(path_out_base, "inp_container")

cont <- load_container(path)

if (parsimonify) {
  cont <- parsimonify_param(cont)
} 

# names(prm$param$ahv$massnahmen)
save_container(cont, to = path_inp_container)
# overwrite, to ensure the parsimonious parameters are read
path_param <- normalizePath(file.path(path_inp_container, "param"), mustWork = TRUE)


dir.create(path_out_out)
dir.create(path_post_process)


## Daten einlesen ------------------------------------------------------------

# Common Data
path_inp_vz <- normalizePath(file.path(path_inp, vz), mustWork = TRUE)
if (length(list.files(path_inp_vz)) == 0) stop("'path_inp_vz' is empty.")

path_param_vz <- normalizePath(file.path(path_param, vz), mustWork = TRUE)
if (length(list.files(path_param_vz)) == 0) stop("'path_param_vz' is empty.")

## Raw Data, Parameter, GO
if (vz == "rententab") {
  # If rententab, force to read the GO  inp folder in the ahv folder
  path_inp_vz <- normalizePath(file.path(path_inp, "ahv"), mustWork = TRUE)
  
  tl_inp <- c(
    tl_PARAM_GLOBAL,
    tidylist_read(file.path(path_inp_vz, "go")),
    param_tidylist_read(file.path(path_param_vz, "go"))
  )
} else if (vz == "beitragstab") {
  # If beitragstab, force to read the GO  inp folder in the ahv folder
  path_inp_vz <- normalizePath(file.path(path_inp, "ahv"), mustWork = TRUE)
  
  tl_inp <- c(
    tl_PARAM_GLOBAL,
    tidylist_read(file.path(path_inp_vz, "go")),
    param_tidylist_read(file.path(path_param_vz, "go"))
  )
} else {
  tl_inp <- c(
    tl_PARAM_GLOBAL,
    tidylist_read(file.path(path_inp_vz, "go")),
    param_tidylist_read(file.path(path_param_vz, "go"))
  )
}

## Data of specific branches of the insurances
if (vz == "rententab") {
  
  # Raw Data, Parameter, Specific Rententab
  tl_inp_rententab <- c(
    tidylist_read(file.path(path_inp, "rententab")),
    param_tidylist_read(file.path(path_param, "rententab"))
  )
}  else if (vz == "beitragstab") {
  
  # Raw Data, Parameter, Specific Rententab
  tl_inp_beitragstab <- c(
    tidylist_read(file.path(path_inp, "beitragstab")),
    param_tidylist_read(file.path(path_param, "beitragstab"))
  )
} else if (tl_inp$PARAM_GLOBAL$flag_param_massn) {
  
  # Raw Data, Parameter, Specific Massnahmen
  tl_inp_massnahmen <- c(
    tidylist_read(file.path(path_inp_vz, "massnahmen")),
    param_tidylist_read(file.path(path_param_vz, "massnahmen"))
  )
}


#--- All Params -------------------------------------------------------------
tl_param0 <- if (tl_inp$PARAM_GLOBAL$flag_param_massn) {
  c(
    tl_inp[grep("^PARAM_", names(tl_inp), value = TRUE)],
    tl_inp_massnahmen[grep("^PARAM_", names(tl_inp_massnahmen), value = TRUE)]
  )
} else {
  tl_inp[grep("^PARAM_", names(tl_inp), value = TRUE)]
}

# only keep dfs with NCOL > 0 (non-empty dfs)
tl_param1 <- Filter(function(e) NCOL(e) > 0, tl_param0)

# same as:
# tl_param <- tl_param[unlist(lapply(tl_param, NCOL)) > 0]


param_list <- unlist(tl_param1, recursive = FALSE)

PARAM_ALL <- as_tibble(param_list)


write_param(as_tibble(param_list),
            file = file.path(path_out_out, "PARAM_ALL.csv")
)

# Core Params (with guaranteed colums), cf. core_param.yaml
core_param_file <- system.file(
  package = "delfin", "templates",
  "core_param.yaml"
)

# Read the yaml
core_param <- read_yaml(core_param_file)

# Construct the PARAM_CORE.csv
missing_param <- setdiff(core_param, names(param_list))
param_list[missing_param] <- ""
PARAM_CORE <- as_tibble(param_list[core_param]) %>%
  gather(key, value) %>%
  separate(key,
           into = c("df", "var"),
           sep = "\\.",
           remove = FALSE
  ) %>%
  select(-key)
tidylist_write(tidylist(PARAM_CORE),
               path = path_out_out
)

# browseURL(file.path(path_out_out, "PARAM_CORE.csv"))

