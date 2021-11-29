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


# determine precipitation quantiles for measures of drought (annual)

ppt_filter_an <- anprismplot %>%
  filter(variable == "ppt")

quant_det_an <- ppt_filter %>%
  summarize(quant = quantile(value))

print(quant_det_an)

# low = 12.36750
# low-med = 49.41833
# med = 73.73000
# high-med = 105.42667
# high = 250.75000

# create new column for precipitation percentiles
quant_ppt_an <-  ppt_filter_an %>%
  mutate(percentile = case_when(value < 12.36750 ~ "20",
                                value > 12.36750 & value <= 49.41833 ~ "40",
                                value > 49.41834 & value <= 73.7300 ~ "60",
                                value > 72.7300 & value <= 105.42667 ~ "80",
                                value > 105.42667 & value <= 250.7500 ~ "100")) %>%
  separate(plot_id, c("site", "plotnum", "assignment"), sep = "_") %>%
  unite("plot_id_needle", site:plotnum, sep = "_", remove = TRUE)%>%
  rename(ppt_raw = value)

# determine precipitation quantiles for measures of drought (monthly)

ppt_filter_mo <- moprismplot %>%
  filter(variable == "ppt")

quant_det_mo <- ppt_filter_mo %>%
  summarize(quant = quantile(value))

print(quant_det_mo)

# low = 2.436667
# low-med = 17.280000
# med = 25.863333
# high-med = 35.795000
# high = 96.220000

# create new column for precipitation percentiles

quant_ppt_mo <-  ppt_filter_mo %>%
  mutate(percentile = case_when(value < 2.436667 ~ "20",
                                value > 2.436667 & value <=  17.280000 ~ "40",
                                value >  17.280000 & value <= 25.863333 ~ "60",
                                value > 25.863333 & value <= 35.795000 ~ "80",
                                value > 35.795000 & value <= 96.220000 ~ "100")) %>%
  separate(plot_id, c("site", "plotnum", "assignment"), sep = "_") %>%
  unite("plot_id_needle", site:plotnum, sep = "_", remove = TRUE)%>%
  rename(ppt_raw = value)
