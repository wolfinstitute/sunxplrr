# run_sunviewr.r
#
# Author: Thomas K. Friedli
#
# Last version: 2020-01-19 / Frt
# Created:      2019-12-28 / Frt
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

devtools::load_all(".")

# Set initial parameters -------------------------------------------------------

inp_data_path = "./data/test_in/"
out_data_path = "./data/test_out/"

sdo.image = "TRUE"

rds.output = "FALSE"
full.output = "FALSE"
light.save = "FALSE"
fits.save = "TRUE"
jpg.save = "FALSE"

file.lst <- list.files(inp_data_path)

res.log <- rep("failed", length(file.lst))

log.results <- cbind(file.name=file.lst, analysis=res.log)

# run --------------------------------------------------------------------------

for (idx in 1:nrow(log.results)){
  
  log.results[idx,2] <- wrap_mod_calcium(inp_file_name = log.results[idx,1],
                                         sdo.image = sdo.image,
                                         inp_data_path = inp_data_path, 
                                         out_data_path = out_data_path,
                                         rds.output = rds.output,
                                         full.output = full.output,
                                         light.save = light.save,
                                         fits.save = fits.save,
                                         jpg.save = jpg.save)
  
}

log.results

# collect results --------------------------------------------------------------

out_data_path = "./work/out2/"
results_data_path = "./work/res_out/"

# total indices ----------------------------------------------------------------

index.name = "_total_indices"

results <- fun_gather_csv(out_data_path = out_data_path, 
                          index.name = index.name)

filename <- paste0(results_data_path,"results_total_indices.csv")

readr::write_csv2(results, filename)

monthly.means <- results %>% 
  group_by(year, month) %>% 
  summarize(obs = n(),
            ttarea_disc = mean(ttarea_disc),
            plage_disc = mean(plage_disc),
            ehnetwork_disc = mean(ehnetwork_disc),
            atnetwork_disc = mean(atnetwork_disc),
            ttcntrst_disc = mean(ttcntrst_disc),
            pgcntrst_disc = mean(pgcntrst_disc),
            ehncntrst_disc = mean(ehncntrst_disc),
            atncntrst_disc = mean(atncntrst_disc),
            ttarea_hem = mean(ttarea_hem),
            plage_hem = mean(plage_hem),
            ehnetwork_hem = mean(ehnetwork_hem),
            atnetwork_hem = mean(atnetwork_hem),
            ttcntrst_hem = mean(ttcntrst_hem),
            pgcntrst_hem = mean(pgcntrst_hem),
            ehncntrst_hem = mean(ehncntrst_hem),
            atncntrst_hem = mean(atncntrst_hem))

filename <- paste0(results_data_path,"monthly_means_total_indices.csv")

readr::write_csv2(monthly.means, filename)

# hemisphere indices -----------------------------------------------------------

index.name = "_hemisphere_indices"

results <- fun_gather_csv(out_data_path = out_data_path, 
                          index.name = index.name)

filename <- paste0(results_data_path,"results_hemisphere_indices.csv")

readr::write_csv2(results, filename)


monthly.means <- results %>% 
  group_by(year, month, hemisphere) %>% 
  summarize(obs = n(),
            ttarea_disc = mean(ttarea_disc),
            plage_disc = mean(plage_disc),
            ehnetwork_disc = mean(ehnetwork_disc),
            atnetwork_disc = mean(atnetwork_disc),
            ttcntrst_disc = mean(ttcntrst_disc),
            pgcntrst_disc = mean(pgcntrst_disc),
            ehncntrst_disc = mean(ehncntrst_disc),
            atncntrst_disc = mean(atncntrst_disc),
            ttarea_hem = mean(ttarea_hem),
            plage_hem = mean(plage_hem),
            ehnetwork_hem = mean(ehnetwork_hem),
            atnetwork_hem = mean(atnetwork_hem),
            ttcntrst_hem = mean(ttcntrst_hem),
            pgcntrst_hem = mean(pgcntrst_hem),
            ehncntrst_hem = mean(ehncntrst_hem),
            atncntrst_hem = mean(atncntrst_hem))

filename <- paste0(results_data_path,"monthly_means_hemisphere_indices.csv")

readr::write_csv2(monthly.means, filename)

# latitude indices -----------------------------------------------------------

index.name = "_latitude_indices"

results <- fun_gather_csv(out_data_path = out_data_path, 
                          index.name = index.name)

filename <- paste0(results_data_path,"results_latitude_indices.csv")

readr::write_csv2(results, filename)

monthly.means <- results %>% 
  group_by(year, month, latitude) %>% 
  summarize(obs = n(),
            ttarea_disc = mean(ttarea_disc),
            plage_disc = mean(plage_disc),
            ehnetwork_disc = mean(ehnetwork_disc),
            atnetwork_disc = mean(atnetwork_disc),
            ttcntrst_disc = mean(ttcntrst_disc),
            pgcntrst_disc = mean(pgcntrst_disc),
            ehncntrst_disc = mean(ehncntrst_disc),
            atncntrst_disc = mean(atncntrst_disc),
            ttarea_hem = mean(ttarea_hem),
            plage_hem = mean(plage_hem),
            ehnetwork_hem = mean(ehnetwork_hem),
            atnetwork_hem = mean(atnetwork_hem),
            ttcntrst_hem = mean(ttcntrst_hem),
            pgcntrst_hem = mean(pgcntrst_hem),
            ehncntrst_hem = mean(ehncntrst_hem),
            atncntrst_hem = mean(atncntrst_hem))

filename <- paste0(results_data_path,"monthly_means_latitude_indices.csv")

readr::write_csv2(monthly.means, filename)

# synopsis ---------------------------------------------------------------------

index.name = "_synopsis"

results <- fun_gather_csv(out_data_path = out_data_path, 
                          index.name = index.name)

# filename <- paste0(results_data_path,"results_synopsis.csv")
# 
# readr::write_csv2(results, filename)

rotational.means <- results %>% 
  group_by(rotation, Longitude, latitude) %>% 
  summarize(obs = n(),
            ttarea_disc = max(ttarea_disc),
            plage_disc = max(plage_disc),
            ehnetwork_disc = max(ehnetwork_disc),
            atnetwork_disc = max(atnetwork_disc),
            ttcntrst_disc = max(ttcntrst_disc),
            pgcntrst_disc = max(pgcntrst_disc),
            ehncntrst_disc = max(ehncntrst_disc),
            atncntrst_disc = max(atncntrst_disc),
            ttarea_hem = max(ttarea_hem),
            plage_hem = max(plage_hem),
            ehnetwork_hem = max(ehnetwork_hem),
            atnetwork_hem = max(atnetwork_hem),
            ttcntrst_hem = max(ttcntrst_hem),
            pgcntrst_hem = max(pgcntrst_hem),
            ehncntrst_hem = max(ehncntrst_hem),
            atncntrst_hem = max(atncntrst_hem))

filename <- paste0(results_data_path,"rotational_means_synopsis.csv")

readr::write_csv2(rotational.means, filename)

synopsis <- rotational.means %>% 
  filter(rotation == 2224)

synoptic.chart <- ggplot(synopsis, aes(Longitude, latitude)) +
  geom_raster(aes(fill = ttcntrst_disc)) +
  coord_fixed(ratio = 1)

ggsave(paste0(results_data_path,"synoptic_chart.jpg"))

## -----------------------------------------------------------------------------

