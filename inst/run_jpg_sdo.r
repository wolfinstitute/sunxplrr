# run_jpg_sdo.r
#
# Author: Thomas K. Friedli
#
# Last version: 2025-10-13 / Frt
# Created:      2019-12-28 / Frt
# ------------------------------------------------------------------------------

remotes::install_github("wolfinstitute/sunxplrr")

# Load package -----------------------------------------------------------------

library(sunxplrr)

# ------------------------------------------------------------------------------

devtools::load_all()

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(jpeg)

# Set parameters ---------------------------------------------------------------

# local path names

inp_data_path = "F:/work/sunxplrr/work/in/"
out_data_path = "F:/work/sunxplrr/work/out/"

# online path names

# inp_root_path = "https://jsoc1.stanford.edu/data/hmi/images/"
# inp_year_path = "2024/"
# inp_month_path = "08/"
# inp_day_path = "04/"
# 
# inp_data_path = paste0(inp_root_path,inp_year_path,inp_month_path,inp_day_path)
# out_data_path = "F:/work/sunxplrr/work/out/"

# file names

inp_file_name = "20240804_084500_Ic_flat_4k.jpg" # SDO jsoc yellow flat

# file names and parameters ----------------------------------------------------

sdo.image = "TRUE"
batch.use = "FALSE"
rds.output = "FALSE"
full.output = "FALSE"
light.save = "TRUE"
fits.save = "FALSE"
jpg.save = "TRUE"

## -----------------------------------------------------------------------------

results <- sunxplrr::wrap_mod_jpg_sdo(inp_file_name = inp_file_name,
                                      sdo.image = sdo.image,
                                      batch.use = batch.use,
                                      inp_data_path = inp_data_path, 
                                      out_data_path = out_data_path,
                                      rds.output = rds.output,
                                      full.output = full.output,
                                      light.save = light.save,
                                      fits.save = fits.save,
                                      jpg.save = jpg.save)



## -----------------------------------------------------------------------------
