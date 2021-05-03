# run_sunviewr.r
#
# Author: Thomas K. Friedli
#
# Last version: 2020-01-17 / Frt
# Created:      2019-12-28 / Frt
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

devtools::load_all(".")

## -----------------------------------------------------------------------------
## define parameter list

inp_data_path = "./data/"
out_data_path = "./work/out/"

sdo.image = "FALSE"
# inp_file_name = "20191118k233.fit"          # Plage from new cycle
# inp_file_name = "20191026k227.fit"          # No activity
# inp_file_name = "20191019k228.fit"          # No activity, Jeannine
# inp_file_name = "20191013k225.fit"          # No activity
# inp_file_name = "20190628k147.fit"          # Very low activity, Jeannine
# inp_file_name = "20180228k026.fit"          # With clouds
 inp_file_name = "20141024K814.fit"          # Very high activity
# inp_file_name = "20141024K814_8bit.fit"     # Very high activity
# inp_file_name = "20140610K331.fit"          # High activity
# inp_file_name = "20120422K350.fit"          # Template for w, h und k 

# sdo.image = "TRUE"
# inp_file_name = "20191116_080341_2048_1700.fits"

rds.output = "FALSE"
full.output = "FALSE"
light.save = "FALSE"
fits.save = "FALSE"
jpg.save = "FALSE"

## -----------------------------------------------------------------------------

results <- wrap_mod_calcium(inp_file_name = inp_file_name,
                            sdo.image = sdo.image,
                            inp_data_path = inp_data_path, 
                            out_data_path = out_data_path,
                            rds.output = rds.output,
                            full.output = full.output,
                            light.save = light.save,
                            fits.save = fits.save,
                            jpg.save = jpg.save)

images  <- results$images
header  <- results$header
hdrlst  <- results$hdrlst

## -----------------------------------------------------------------------------
