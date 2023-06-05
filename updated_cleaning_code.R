# load necessary packages
library("tidyverse")
library("ggplot2")
library("dplyr")
library("wsyn")
library("here")
library("tibble")

# load necessary data
rwi_dat <- read.csv(here("Data/rwi_prismdat.csv")) # RWI 
ppt_dat <- read.csv(here("Data/ppt_prism_filtered.csv")) # PPT
tmin_dat <- read.csv(here("Data/tmin_prism_filtered.csv")) # TMIN
vpdmax_dat <- read.csv(here("Data/vpdmax_prism_filtered.csv")) # VPDMAX

# wrangle the data
rwi_dat <- rwi_dat %>%
  select(year, plot_id_needle, tree_num, value) %>%
  rename(plot = "plot_id_needle", rwi = "value") 
ppt_dat <- ppt_dat %>%
  rename(plot = "plot_id_needle") 
tmin_dat <- tmin_dat %>%
  rename(plot = "plot_id_needle") 
vpdmax_dat <- vpdmax_dat %>%
  rename(plot = "plot_id_needle") 

# combine environmental data
env_data <- left_join(ppt_dat, tmin_dat) %>%
  left_join(vpdmax_dat)

#### GROWTH DATA ###############################################################
# subset 1900-2018, each row represents an individual tree's data
rwi_00s <- rwi_dat %>%
  filter(year >= 1900)%>%
  drop_na(rwi)%>%
  pivot_wider(names_from = year, values_from=rwi)

# remove any trees with missing data along the time series 
rwi_00s_tree_filtered<- rwi_00s[rowSums(is.na(rwi_00s))==0,]

# find plots with at least 5 trees with the whole time series
rwi_00s_plot_filtered <- rwi_00s_tree_filtered %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")%>%
  group_by(plot)%>%
  mutate(num_trees = round(n()/118))%>%
  filter(num_trees >= 5)

rwi_00s_plot_filtered_wide <- rwi_00s_plot_filtered %>%
  pivot_wider(names_from = year, values_from = rwi)

# calculate annual average growth per plot per year
avg_plot_growth <- rwi_00s_plot_filtered %>%
  filter(year >= 1900)%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

avg_plot_growth_wide <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")

# format matrix for wavelet analysis
avg_plot_growth_wide <- as.matrix(avg_plot_growth_wide)
colnames(avg_plot_growth_wide) <- NULL
avg_plot_growth_wide <- avg_plot_growth_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
avg_plot_growth_wide = as.data.frame(avg_plot_growth_wide, stringsAsFactors = FALSE)
avg_plot_growth_wide = map_df(avg_plot_growth_wide, as.numeric)
avg_plot_growth_mx <- as.matrix(avg_plot_growth_wide)

# clean data for wpmf
times <- 1900:2018
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, clev = 5)$cdat
avg_plot_growth_df <- as.data.frame(avg_plot_growth_mx)
colnames(avg_plot_growth_df) <- 1900:2018
avg_plot_growth_df <- avg_plot_growth_df %>%
  pivot_longer(1:119, names_to="year", values_to = "avg_growth_cleaned")
avg_plot_growth_df$plot <- avg_plot_growth$plot

# produce wpmf & wmf for growth data
res_growth_wpmf<-wpmf(avg_plot_growth_mx,times,sigmethod="none")
res_growth_wmf<-wmf(avg_plot_growth_mx,times)

#### ENVIRONMENTAL DATA ########################################################

## Winter - Precipitation ##
# aka. fall-spring water year, strongest predictor of growth in WBP in SN
# (Dudney et al. 2022)

# match up plots with rwi data
# remove annual measurements from dataset
ppt_dat <- na.omit(ppt_dat)
plots <- unique(rwi_00s_plot_filtered$plot)
ppt_dat <- ppt_dat %>%
  filter(plot %in% plots)

# produce water-year variable 
winter_ppt <- ppt_dat %>%
  mutate(wateryear = case_when(month == "10" ~ year+1,
                               month == "11" ~ year+1,
                               month == "12" ~ year+1,
                               TRUE ~ as.numeric(year)))

# filter for water-year months (Oct - May)
months <- c(1, 2, 3, 4, 5, 10, 11, 12)
winter_ppt <- winter_ppt %>%
  filter(month %in% months)

# calculate avg winter precip per plot per water-year
winter_ppt <- winter_ppt %>%
  group_by(plot, wateryear)%>%
  summarise(winter_ppt = sum(ppt))%>%
  filter(wateryear >= 1900)%>%
  filter(wateryear <= 2018)

winter_ppt_wide <- winter_ppt %>%
  pivot_wider(names_from = "wateryear", values_from = "winter_ppt")

# format matrix for wavelet analysis
winter_ppt_wide <- as.matrix(winter_ppt_wide)
colnames(winter_ppt_wide) <- NULL
winter_ppt_wide <- winter_ppt_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
winter_ppt_wide = as.data.frame(winter_ppt_wide, stringsAsFactors = FALSE)
winter_ppt_wide = map_df(winter_ppt_wide, as.numeric)
winter_ppt_mx <- as.matrix(winter_ppt_wide)

# clean data for wpmf
times <- 1900:2018
winter_ppt_mx <- cleandat(winter_ppt_mx, times, clev = 5)$cdat
winter_ppt_df <- as.data.frame(winter_ppt_mx)
colnames(winter_ppt_df) <- 1900:2018
winter_ppt_df <- winter_ppt_df %>%
  pivot_longer(1:119, names_to="year", values_to = "winter_ppt_cleaned")
winter_ppt_df$plot <- winter_ppt$plot

# produce wpmf for growth data
res_ppt_wpmf<-wpmf(winter_ppt_mx,times,sigmethod="none")
res_ppt_wmf<-wmf(winter_ppt_mx,times)

## Minimum summer temperatures ##
# remove annual measurements from the dataset
# match up plots with growth data
tmin_dat <- na.omit(tmin_dat)
tmin_dat <- tmin_dat %>%
  filter(plot %in% plots)

# filter for summer months (June - Aug)
months <- c(6, 7, 8)
summer_tmin <- tmin_dat %>%
  filter(month %in% months )

# calculate avg tmin in summer months
summer_tmin <- summer_tmin %>%
  group_by(plot, year)%>%
  summarise(summer_tmin = mean(tmin))%>%
  filter(year >= 1900)%>%
  filter(year <= 2018)

summer_tmin_wide <- summer_tmin %>%
  pivot_wider(names_from = "year", values_from = "summer_tmin")

# format matrix for wavelet analysis
summer_tmin_wide <- as.matrix(summer_tmin_wide)
colnames(summer_tmin_wide) <- NULL
summer_tmin_wide <- summer_tmin_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
summer_tmin_wide = as.data.frame(summer_tmin_wide, stringsAsFactors = FALSE)
summer_tmin_wide = map_df(summer_tmin_wide, as.numeric)
summer_tmin_mx <- as.matrix(summer_tmin_wide)

# clean data for wpmf
times <- 1900:2018
summer_tmin_mx <- cleandat(summer_tmin_mx, times, clev = 5)$cdat
summer_tmin_df <- as.data.frame(summer_tmin_mx)
colnames(summer_tmin_df) <- 1900:2018
summer_tmin_df <- summer_tmin_df %>%
  pivot_longer(1:119, names_to="year", values_to = "summer_tmin_cleaned")
summer_tmin_df$plot <- summer_tmin$plot

# produce wpmf for growth data
res_tmin_wpmf<-wpmf(summer_tmin_mx,times,sigmethod="none")
res_tmin_wmf<-wmf(summer_tmin_mx,times)

## VPD max ##
# remove annual measurements from the dataset
# match up plots with growth data
vpdmax_dat <- na.omit(vpdmax_dat)
vpdmax_dat <- vpdmax_dat %>%
  filter(plot %in% plots)
# create water-year variable 
vpdmax_dat <- vpdmax_dat %>%
  mutate(wateryear = case_when(month == "10" ~ year+1,
                               month == "11" ~ year+1,
                               month == "12" ~ year+1,
                               TRUE ~ as.numeric(year)))
# calculate avg annual 'max' vpd
avg_vpdmax <- vpdmax_dat %>%
  group_by(plot, wateryear)%>%
  summarise(avg_vpdmax = mean(vpdmax))%>%
  filter(wateryear >= 1900)%>%
  filter(wateryear <= 2018)

avg_vpdmax_wide <- avg_vpdmax %>%
  pivot_wider(names_from = "wateryear", values_from = "avg_vpdmax")

# format matrix for wavelet analysis
avg_vpdmax_wide <- as.matrix(avg_vpdmax_wide)
colnames(avg_vpdmax_wide) <- NULL
avg_vpdmax_wide <- avg_vpdmax_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
avg_vpdmax_wide = as.data.frame(avg_vpdmax_wide, stringsAsFactors = FALSE)
avg_vpdmax_wide = map_df(avg_vpdmax_wide, as.numeric)
avg_vpdmax_mx <- as.matrix(avg_vpdmax_wide)

# clean data for wpmf
times <- 1900:2018
avg_vpdmax_mx <- cleandat(avg_vpdmax_mx, times, clev = 5)$cdat
avg_vpdmax_df <- as.data.frame(avg_vpdmax_mx)
colnames(avg_vpdmax_df) <- 1900:2018
avg_vpdmax_df <- avg_vpdmax_df %>%
  pivot_longer(1:119, names_to="year", values_to = "avg_vpdmax_cleaned")
avg_vpdmax_df$plot <- avg_vpdmax$plot

# produce wpmf for growth data
res_vpdmax_wpmf<-wpmf(avg_vpdmax_mx,times,sigmethod="none")
res_vpdmax_wmf<-wmf(avg_vpdmax_mx,times)

