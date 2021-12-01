## ---------------------------
##
## Script name: 
##
## Author: Dr. Joan Dudney
##
## Date Created: 2021-12-01
##
## Copyright (c) Joan Dudney, 2021
## Email: jdudney@berkeley.edu
##
## ---------------------------
##
## Notes: classifies the plots into
## climate categories and calculates mean climate 
## values for each plot
## ---------------------------


library(tidyverse)
library(stringr)

# read in data
prism_ann <- read_csv("data/whitebark_plot_prism_annual.csv")
plot_class <- read_csv("data/plot_classification.csv")
plotnames <- read_csv("data/plot_names_normalized_v2.csv")

plot_class_rev <- plot_class %>% 
  mutate(plot=str_replace_all(plot, pattern = c("_NA" = ""))) %>% 
  rename(plot_id_needle=plot)

comb_plotdat <- plot_class_rev %>% 
  left_join(plotnames) %>% 
  rename(plot_id = plot_id_climate) %>% 
  select(plot_id, plot_type, plot_label_2) %>% 
  rename(plot=plot_label_2)

##mean weather data
prismdat <- prism_ann %>% 
  group_by(plot_id, variable) %>% 
  summarise(mean = mean(value)) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  left_join(comb_plotdat)

write_csv(prismdat, "data/plot_weather_type.csv")

