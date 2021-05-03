# sdo_stu_analysis.r
#
# Author: Thomas K. Friedli
#
# Last version: 2020-01-26 / Frt
# Created:      2020-01-20 / Frt
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

devtools::load_all(".")

# Set path and file names ------------------------------------------------------

stu_data_path = "./results_STU/"
sdo_data_path = "./results_SDO/"
out_data_path = "./work/"

stu_daily_total_path = "stu_results_total_indices.csv"
sdo_daily_total_path = "sdo_results_total_indices.csv"
stu_monthly_total_path = "stu_monthly_means_total_indices.csv"
sdo_monthly_total_path = "sdo_monthly_means_total_indices.csv"

stu_daily_total_hem_path = "stu_results_hemisphere_indices.csv"
sdo_daily_total_hem_path = "sdo_results_hemisphere_indices.csv"
stu_monthly_total_hem_path = "stu_monthly_means_hemisphere_indices.csv"
sdo_monthly_total_hem_path = "sdo_monthly_means_hemisphere_indices.csv"


## -----------------------------------------------------------------------------
# Import Tages- und Monatswerte ------------------------------------------------

inp_data_path <- paste0(stu_data_path,stu_daily_total_path)
stu_daily_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(stu_data_path,stu_monthly_total_path)
stu_monthly_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(sdo_data_path,sdo_daily_total_path)
sdo_daily_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(sdo_data_path,sdo_monthly_total_path)
sdo_monthly_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(stu_data_path,stu_daily_total_hem_path)
stu_daily_hem_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(stu_data_path,stu_monthly_total_hem_path)
stu_monthly_hem_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(sdo_data_path,sdo_daily_total_hem_path)
sdo_daily_hem_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(sdo_data_path,sdo_monthly_total_hem_path)
sdo_monthly_hem_total <- read.csv2(inp_data_path)

# Monatmittel darstellen -------------------------------------------------------

data_sdo <- sdo_monthly_total %>% 
  select(year,month,ttarea_disc, ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  mutate(date = make_date(year = year, month = month, day = 1L))
 
ggplot(data_sdo, aes(date, ttarea_disc)) +
  #geom_line(aes(date, ttarea_disc)) +
  geom_line(aes(date, ttarea_hem)) +
  geom_line(aes(date, plage_hem)) + 
  geom_line(aes(date, ehnetwork_hem)) +
  geom_line(aes(date, atnetwork_hem))

data_stu <- stu_monthly_total %>% 
  select(year,month,ttarea_disc) %>% 
  mutate(date = make_date(year = year, month = month, day = 1L))

ggplot(data_stu, aes(date, ttarea_disc)) +
  geom_line(aes(date, ttarea_disc))

data_sdo_hem <- sdo_monthly_hem_total %>% 
  select(year,month,hemisphere,ttarea_disc, ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  mutate(date = make_date(year = year, month = month, day = 1L)) %>% 
  group_by(hemisphere)

ggplot(data_sdo_hem, aes(date, ttarea_disc)) +
  #geom_line(aes(date, ttarea_disc, group = hemisphere, color = hemisphere)) +
  geom_line(aes(date, ttarea_hem, group = hemisphere, color = hemisphere)) +
  geom_line(aes(date, plage_hem, group = hemisphere, color = hemisphere)) + 
  geom_line(aes(date, ehnetwork_hem, group = hemisphere, color = hemisphere))
  # geom_line(aes(date, atnetwork_hem, group = hemisphere, color = hemisphere))

data_stu <- stu_monthly_total %>% 
  select(year,month,ttarea_disc) %>% 
  mutate(date = make_date(year = year, month = month, day = 1L))

ggplot(data_stu, aes(date, ttarea_disc)) +
  geom_line(aes(date, ttarea_disc))  
# Tageswerte darstellen --------------------------------------------------------

data_daily_sdo <- sdo_daily_total %>% 
  select(year,month,day,ttarea_disc, ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  mutate(diff = ttarea_disc - ttarea_hem) %>% 
  mutate(date = make_date(year = year, month = month, day = day))

ggplot(data_daily_sdo, aes(date, ttarea_disc)) +
  #geom_line(aes(date, ttarea_disc)) +
  geom_line(aes(date, ttarea_hem)) +
  geom_line(aes(date, plage_hem)) + 
  geom_line(aes(date, ehnetwork_hem)) +
  geom_line(aes(date, atnetwork_hem))

ggplot(data_daily_sdo, aes(date, ttarea_disc)) +
  geom_line(aes(date, diff))

data_daily_stu <- stu_daily_total %>% 
  select(year,month,day, ttarea_disc) %>% 
  mutate(date = make_date(year = year, month = month, day = day))

ggplot(data_daily_stu, aes(date, ttarea_disc)) +
  geom_line(aes(date, ttarea_disc))

# x gegen y darstelln ----------------------------------------------------------

daily_grid <- fun_date_grid(start_date = "2014-01-01", 
                            end_date = "2019-12-31", granularity = "day")

stu_data <- data_daily_stu %>% 
  select(-date)

sdo_data <- data_daily_sdo %>% 
  select(-date)

daily_data <- daily_grid %>% 
  left_join(stu_data, by=c("year", "month", "day")) %>% 
  rename(ttarea_disc_stu = ttarea_disc) %>% 
  left_join(sdo_data, by=c("year", "month", "day")) %>% 
  rename(ttarea_disc_sdo = ttarea_disc) %>% 
  drop_na(ttarea_disc_stu,ttarea_disc_sdo)

ggplot(daily_data, aes(ttarea_disc_stu, ttarea_disc_sdo)) +
  geom_point() +
  geom_smooth()

monthly_grid <- fun_date_grid(start_date = "2014-01-01", 
                            end_date = "2019-12-31", granularity = "month")

stu_data <- data_stu %>% 
  select(-date)

sdo_data <- data_sdo %>% 
  select(-date)

monthly_data <- monthly_grid %>% 
  left_join(stu_data, by=c("year", "month")) %>% 
  rename(ttarea_disc_stu = ttarea_disc) %>% 
  left_join(sdo_data, by=c("year", "month")) %>% 
  rename(ttarea_disc_sdo = ttarea_disc) %>% 
  drop_na(ttarea_disc_stu,ttarea_disc_sdo)

ggplot(monthly_data, aes(ttarea_disc_stu, ttarea_disc_sdo)) +
  geom_point() +
  geom_smooth()
