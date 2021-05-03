# sdo_stu_analysis.r
#
# Author: Thomas K. Friedli
#
# Last version: 2020-01-27 / Frt
# Created:      2020-01-20 / Frt
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

devtools::load_all(".")

# Set paths --------------------------------------------------------------------

stu_data_path = "./results_STU/"
sdo_data_path = "./results_SDO/"
silso_data_path = "./results_SILSO/"
out_data_path = "./work/plots/"

# Set file names ---------------------------------------------------------------

stu_daily_total_path = "stu_results_total_indices.csv"
sdo_daily_total_path = "sdo_results_total_indices.csv"

stu_daily_total_hem_path = "stu_results_hemisphere_indices.csv"
sdo_daily_total_hem_path = "sdo_results_hemisphere_indices.csv"

stu_monthly_total_path = "stu_monthly_means_total_indices.csv"
sdo_monthly_total_path = "sdo_monthly_means_total_indices.csv"

stu_monthly_total_hem_path = "stu_monthly_means_hemisphere_indices.csv"
sdo_monthly_total_hem_path = "sdo_monthly_means_hemisphere_indices.csv"

silso_monthly_path = "SN_m_tot_V2.0.csv"

## Import Data -----------------------------------------------------------------
# Import Monatswerte ------------------------------------------------

inp_data_path <- paste0(stu_data_path,stu_daily_total_path)
stu_daily_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(stu_data_path,stu_daily_total_hem_path)
stu_daily_hem_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(stu_data_path,stu_monthly_total_path)
stu_monthly_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(stu_data_path,stu_monthly_total_hem_path)
stu_monthly_hem_total <- read.csv2(inp_data_path)


inp_data_path <- paste0(sdo_data_path,sdo_daily_total_path)
sdo_daily_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(sdo_data_path,sdo_daily_total_hem_path)
sdo_daily_hem_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(sdo_data_path,sdo_monthly_total_path)
sdo_monthly_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(sdo_data_path,sdo_monthly_total_hem_path)
sdo_monthly_hem_total <- read.csv2(inp_data_path)

inp_data_path <- paste0(silso_data_path,silso_monthly_path)
silso_monthly <- read.csv2(inp_data_path, dec = ".")

# Monatmittel darstellen -------------------------------------------------------

# SDO Ganze Sonne

data_sdo <- sdo_monthly_total %>% 
  select(year, month, ttarea_disc, plage_disc, ehnetwork_disc, atnetwork_disc,
                      ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  mutate(date = make_date(year = year, month = month, day = 1L))
 
data_sdo_1 <- data_sdo %>% 
  select(date, ttarea_disc, ttarea_hem) %>% 
  gather(`ttarea_disc`, `ttarea_hem`, key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_1 <- ggplot(data_sdo_1, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Vergleich Index pro Scheibe zu Index pro Halbkugel")
  
ggsave(paste0(out_data_path,"sdo_plot_1.jpeg"), sdo_plot_1)

data_sdo_4 <- data_sdo %>% 
  select(date, ttarea_hem, plage_hem, ehnetwork_hem) %>% 
  gather(`ttarea_hem`, `plage_hem`, `ehnetwork_hem`, 
         key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_4 <- ggplot(data_sdo_4, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Monatliche Indices pro Halbkugel")

ggsave(paste0(out_data_path,"sdo_plot_4.jpeg"), sdo_plot_4)

data_sdo_6 <- data_sdo %>% 
  select(date, ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  gather(`ttarea_hem`, `plage_hem`, `ehnetwork_hem`, `atnetwork_hem`, 
         key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_6 <- ggplot(data_sdo_6, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Monatliche Indices pro Halbkugel")

ggsave(paste0(out_data_path,"sdo_plot_6.jpeg"), sdo_plot_6)

# SDO Nach Hemisphären getrennt

data_sdo_hem <- sdo_monthly_hem_total %>% 
  select(year, month, hemisphere, 
         ttarea_disc, plage_disc, ehnetwork_disc, atnetwork_disc,
         ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  mutate(date = make_date(year = year, month = month, day = 1L)) %>% 
  group_by(hemisphere)

data_sdo_8 <- data_sdo_hem %>% 
  select(date, hemisphere, ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  gather(`ttarea_hem`, `plage_hem`, `ehnetwork_hem`, `atnetwork_hem`, 
         key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_8 <- ggplot(data_sdo_8, aes(date, area)) +
  geom_line(aes(color = index, linetype = hemisphere)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Monatliche Indices pro Hemisphäre und Halbkugel")

ggsave(paste0(out_data_path,"sdo_plot_8.jpeg"), sdo_plot_8)

data_sdo_9 <- data_sdo_hem %>% 
  select(date, hemisphere, ttarea_hem, plage_hem, ehnetwork_hem) %>% 
  gather(`ttarea_hem`, `plage_hem`, `ehnetwork_hem`,  
         key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_9 <- ggplot(data_sdo_9, aes(date, area)) +
  geom_line(aes(color = index, linetype = hemisphere)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Monatliche Indices pro Hemisphäre und Halbkugel")

ggsave(paste0(out_data_path,"sdo_plot_9.jpeg"), sdo_plot_9)


# Tageswerte darstellen --------------------------------------------------------

data_daily_stu <- stu_daily_total %>% 
  select(year, month, day, 
         ttarea_disc, plage_disc, ehnetwork_disc, atnetwork_disc,
         ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  mutate(date = make_date(year = year, month = month, day = day))

data_daily_sdo <- sdo_daily_total %>% 
  select(year, month, day, 
         ttarea_disc, plage_disc, ehnetwork_disc, atnetwork_disc,
         ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  mutate(date = make_date(year = year, month = month, day = day))

data_sdo_2 <- data_daily_sdo %>% 
  select(date, ttarea_disc) %>% 
  gather(`ttarea_disc`, key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_2 <- ggplot(data_sdo_2, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0.07,0.5)) +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Täglicher Index pro Scheibe")

ggsave(paste0(out_data_path,"sdo_plot_2.jpeg"), sdo_plot_2)

data_sdo_3 <- data_daily_sdo %>% 
  select(date, ttarea_hem) %>% 
  gather(`ttarea_hem`, key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_3 <- ggplot(data_sdo_3, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0.07,0.5)) +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Täglicher Index pro Halbkugel")

ggsave(paste0(out_data_path,"sdo_plot_3.jpeg"), sdo_plot_3)

data_sdo_5 <- data_daily_sdo %>% 
  select(date, ttarea_hem, plage_hem, ehnetwork_hem) %>% 
  gather(`ttarea_hem`, `plage_hem`, `ehnetwork_hem`, 
         key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_5 <- ggplot(data_sdo_5, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Tägliche Indices pro Halbkugel")

ggsave(paste0(out_data_path,"sdo_plot_5.jpeg"), sdo_plot_5)

data_sdo_7 <- data_daily_sdo %>% 
  select(date, ttarea_hem, plage_hem, ehnetwork_hem, atnetwork_hem) %>% 
  gather(`ttarea_hem`, `plage_hem`, `ehnetwork_hem`, `atnetwork_hem`,
         key = "index", value = "area") %>% 
  mutate(area = area / 100000)

sdo_plot_7 <- ggplot(data_sdo_7, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("SDO: Tägliche Indices pro Halbkugel")

ggsave(paste0(out_data_path,"sdo_plot_7.jpeg"), sdo_plot_7)


# x gegen y darstelln ----------------------------------------------------------

daily_grid <- fun_date_grid(start_date = "2014-01-01", 
                            end_date = "2019-12-31", granularity = "day")

stu_data <- data_daily_stu %>% 
  select(-date)

sdo_data <- data_daily_sdo %>% 
  select(-date)

daily_data <- daily_grid %>% 
  left_join(stu_data, by=c("year", "month", "day")) %>% 
  rename(ttarea_hem_stu = ttarea_hem) %>%
  filter(ttarea_hem_stu < 50000) %>% 
  filter(date != "2014-03-07") %>% 
  filter(date != "2017-01-01") %>% 
  filter(date != "2017-06-15") %>% 
  filter(date != "2017-11-17") %>% 
  filter(date != "2017-11-22") %>% 
  filter(date != "2017-12-07") %>% 
  filter(date != "2018-12-26") %>% 
  filter(date != "2018-12-31") %>% 
  filter(date != "2019-10-19") %>% 
  filter(date != "2019-12-26") %>% 
  filter(date != "2019-12-03") %>% 
  left_join(sdo_data, by=c("year", "month", "day")) %>% 
  rename(ttarea_hem_sdo = ttarea_hem) %>% 
  drop_na(ttarea_hem_stu,ttarea_hem_sdo) %>% 
  select(date, year, month, day, ttarea_hem_stu, ttarea_hem_sdo)


data_daily <- daily_data %>% 
  select(date, ttarea_hem_stu, ttarea_hem_sdo) %>% 
  gather(`ttarea_hem_stu`, `ttarea_hem_sdo`, key = "index", value = "area") %>% 
  mutate(area = area / 100000)

daily_plot_1 <- ggplot(data_daily, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("Vergleich täglicher Index ttarea_hem STU und SDO")

ggsave(paste0(out_data_path,"daily_plot_1.jpeg"), daily_plot_1)


raw_monthly_data <- daily_grid %>% 
  left_join(stu_data, by=c("year", "month", "day")) %>% 
  rename(ttarea_hem_stu = ttarea_hem) %>%
  filter(ttarea_hem_stu < 50000) %>% 
  filter(date != "2014-03-07") %>% 
  filter(date != "2017-01-01") %>% 
  filter(date != "2017-06-15") %>% 
  filter(date != "2017-11-17") %>% 
  filter(date != "2017-11-22") %>% 
  filter(date != "2017-12-07") %>% 
  filter(date != "2018-12-26") %>% 
  filter(date != "2018-12-31") %>% 
  filter(date != "2019-10-19") %>% 
  filter(date != "2019-12-26") %>% 
  filter(date != "2019-12-03") %>% 
  left_join(sdo_data, by=c("year", "month", "day")) %>% 
  rename(ttarea_hem_sdo = ttarea_hem) %>% 
  drop_na(ttarea_hem_stu,ttarea_hem_sdo) %>% 
  select(date, year, month, day, ttarea_hem_stu, ttarea_hem_sdo) %>% 
  group_by(year, month) %>% 
  summarize(ttarea_hem_sdo = mean(ttarea_hem_sdo),
            ttarea_hem_stu = mean(ttarea_hem_stu))


monthly_grid <- fun_date_grid(start_date = "2014-01-01", 
                            end_date = "2019-12-31", granularity = "month")

monthly_data <- monthly_grid %>% 
  left_join(raw_monthly_data, by=c("year", "month"))

xy_monthly <- as.data.frame(monthly_data %>% 
  mutate(ttarea_hem_sdo = ttarea_hem_sdo / 100000,
         ttarea_hem_stu = ttarea_hem_stu / 100000))

monthly_plot_1 <- ggplot(xy_monthly, aes(ttarea_hem_stu, ttarea_hem_sdo)) +
  geom_point() +
  geom_smooth() +
  theme_bw() + xlab("ttarea_hem_stu [%]") + ylab("ttarea_hem_sdo [%]") + 
  ggtitle("Zusammenhang monatlicher Index ttarea_hem STU und SDO")

ggsave(paste0(out_data_path,"monthly_plot_1.jpeg"), monthly_plot_1)

data_monthly <- monthly_data %>% 
  select(date, ttarea_hem_stu, ttarea_hem_sdo) %>% 
  gather(`ttarea_hem_stu`, `ttarea_hem_sdo`, key = "index", value = "area") %>% 
  mutate(area = area / 100000)

monthly_plot_2 <- ggplot(data_monthly, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%]") + 
  ggtitle("Vergleich monatlicher Index ttarea_hem STU und SDO")

ggsave(paste0(out_data_path,"monthly_plot_2.jpeg"), monthly_plot_2)


monthly_data_silso <- monthly_grid %>% 
  left_join(sdo_monthly_total, by=c("year", "month")) %>% 
  select(date, year, month, ttarea_hem_sdo = ttarea_hem) %>%
  mutate(ttarea_hem_sdo = ttarea_hem_sdo / 100000) %>% 
  left_join(stu_monthly_total, by=c("year", "month")) %>% 
  select(date, year, month, ttarea_hem_sdo, ttarea_hem_stu = ttarea_hem) %>% 
  mutate(ttarea_hem_stu = ttarea_hem_stu / 100000) %>% 
  left_join(silso_monthly, by=c("year", "month")) %>% 
  select(date, ttarea_hem_sdo, ttarea_hem_stu, ssn)

monthly_plot_3 <- ggplot(monthly_data_silso, aes(ssn, ttarea_hem_sdo)) +
  geom_point() +
  geom_smooth() +
  theme_bw() + xlab("ssn [Wolfer]") + ylab("ttarea_hem_sdo [%]") + 
  ggtitle("Zusammenhang ttarea_hem_sdo und SSN")

ggsave(paste0(out_data_path,"monthly_plot_3.jpeg"), monthly_plot_3)

monthly_plot_4 <- ggplot(monthly_data_silso, aes(ssn, ttarea_hem_stu)) +
  geom_point() +
  geom_smooth() +
  theme_bw() + xlab("ssn [Wolfer]") + ylab("ttarea_hem_stu [%]") + 
  ggtitle("Zusammenhang ttarea_hem_stu und SSN")

ggsave(paste0(out_data_path,"monthly_plot_4.jpeg"), monthly_plot_4)

data_monthly <- monthly_data_silso %>% 
  select(date, ttarea_hem_sdo, ssn) %>% 
  mutate(ttarea_hem_sdo = ttarea_hem_sdo * 200) %>% 
  gather(`ttarea_hem_sdo`, `ssn`, key = "index", value = "area") 
  

monthly_plot_5 <- ggplot(data_monthly, aes(date, area)) +
  geom_line(aes(color = index)) +
  scale_x_date(limits = as.Date(c("2014-01-01", "2020-01-01")), 
               date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + xlab(NULL) + ylab("area [%] und [Wolfer]") + 
  ggtitle("Verlauf Monatsmittel ttarea_hem_sdo und SSN")

ggsave(paste0(out_data_path,"monthly_plot_5.jpeg"), monthly_plot_5)

