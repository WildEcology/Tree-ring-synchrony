##------------------------


# SCRIPT NAME: Wavelet

# AUTHOR: Kaitlyn McKnight (University of Wyoming)

# DATE CREATED: 2021-10-22


##------------------------

# load packages

library("tidyverse")
library("ggplot2")
library("tsvr")
library("codyn")
library("wsyn")
library("lubridate")
library("here")


# read in data

prismdat <- read.csv(here("data/rwi_prismdat.csv"))
anprismplot <- read.csv(here("data/whitebark_plot_prism_annual.csv"))
moprismplot <- read.csv(here("data/whitebark_plot_prism_monthly.csv"))


# subset 1900-2018
rwi_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)

# find plots and years with at least 5 trees 
test <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)

# subsetting only plots with at least 5 trees
test2 <- left_join(test, rwi_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, value)%>%
  group_by(plot, year)%>%
  pivot_wider(names_from = year, values_from = value)

# removing any trees with missing data along the timeseries
rwi_00s_filtered<- test2[rowSums(is.na(test2))==0,]


# subset 1970-2018
rwi_70s <- prismdat %>%
  filter(year >= 1970)%>%
  drop_na(value)

# find plots and years with at least 5 trees 
test3 <- rwi_70s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)

# subsetting only plots with at least 5 trees
test4 <- left_join(test3, rwi_70s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, value)%>%
  group_by(plot, year)%>%
  pivot_wider(names_from = year, values_from = value)

# removing any trees with missing data along the timeseries
rwi_70s_filtered<- test4[rowSums(is.na(test4))==0,]
