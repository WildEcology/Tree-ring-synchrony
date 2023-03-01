

# load necessary packages
library("tidyverse")
library("ggplot2")
library("dplyr")
library("wsyn")
library("here")
library("tibble")

# read in datasets
prismdat <- read.csv(here("Data/rwi_prismdat.csv")) # RWI & TMAX
ppt_month <- read.csv(here("Data/prism_plots_1900.csv")) # WATER-YEAR

#### PREPARE DATA ####

## RWI ##
# subset 1900-2018
rwi_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)
# find plots and years with at least 5 trees 
rwi_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)
# subsetting only plots with at least 5 trees
rwi_00s_unfiltered_long <- left_join(rwi_5, rwi_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, value)
rwi_00s_unfiltered_wide <- rwi_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = value)
# removing any trees with missing data along the timeseries
rwi_00s_filtered_wide<- rwi_00s_unfiltered_wide[rowSums(is.na(rwi_00s_unfiltered_wide))==0,]
rwi_00s_filtered_long <- rwi_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")

# avg growth per plot per year
avg_plot_growth <- rwi_00s_filtered_long %>%
  filter(year > 1900)%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

avg_plot_growth_wide <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")

# format matrix for analysis
avg_plot_growth_wide <- as.matrix(avg_plot_growth_wide)
colnames(avg_plot_growth_wide) <- NULL
avg_plot_growth_wide <- avg_plot_growth_wide[, c(2:119)] # timeseries 1901 -2018

# convert character matrix to numeric
avg_plot_growth_wide = as.data.frame(avg_plot_growth_wide, stringsAsFactors = FALSE)
avg_plot_growth_wide = map_df(avg_plot_growth_wide, as.numeric)
avg_plot_growth_mx <- as.matrix(avg_plot_growth_wide)

# clean data for wpmf
times <- 1901:2018
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, clev = 5)$cdat
avg_plot_growth_df <- as.data.frame(avg_plot_growth_mx)
colnames(avg_plot_growth_df) <- 1901:2018
avg_plot_growth_df <- avg_plot_growth_df %>%
  pivot_longer(1:118, names_to="year", values_to = "avg_growth_cleaned")
avg_plot_growth_df$plot <- avg_plot_growth$plot


## TMAX ##
tmax_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)
# find plots and years with at least 5 trees 
tmax_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)
# subsetting only plots with at least 5 trees
tmax_00s_unfiltered_long <- left_join(tmax_5, tmax_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, tmax)
tmax_00s_unfiltered_wide <- tmax_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = tmax)
# removing any trees with missing data along the timeseries
tmax_00s_filtered_wide<- tmax_00s_unfiltered_wide[rowSums(is.na(tmax_00s_unfiltered_wide))==0,]
tmax_00s_filtered_long <- tmax_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "tmax")

# avg tmax per plot per year
avg_plot_tmax <- tmax_00s_filtered_long %>%
  filter(year > 1900)%>%
  group_by(plot,year)%>%
  summarise(avg_tmax = mean(tmax))%>%
  pivot_wider(names_from = "year", values_from = "avg_tmax")

avg_plot_tmax_long <- avg_plot_tmax %>%
  pivot_longer(2:119, names_to = "year", values_to = "avg_tmax")

# format matrix for analysis 
tmax <- as.matrix(avg_plot_tmax)
colnames(tmax) <- NULL
tmax <- tmax[, c(2:119)] # timeseries 1901-2018


# convert character matrix to numeric
tmax = as.data.frame(tmax, stringsAsFactors = FALSE)
tmax = map_df(tmax, as.numeric)
tmax_mx <- as.matrix(tmax)

# clean data for wpmf
times <- 1901:2018
tmax_mx <- cleandat(tmax_mx, times, clev = 5)$cdat
tmax_df <- as.data.frame(tmax_mx)
colnames(tmax_df) <- 1901:2018
tmax_df <- tmax_df %>%
  pivot_longer(1:118, names_to="year", values_to = "avg_tmax_cleaned")
tmax_df$plot <- avg_plot_tmax_long$plot

## WATER - YEAR ##
ppt_month$plot <- as.character(ppt_month$plot)
ppt_month$type <- as.character(ppt_month$type)
ppt_month$year <- as.numeric(ppt_month$year)
ppt_month$month <- as.numeric(ppt_month$month)
ppt_month$value <- as.numeric(ppt_month$value)
# make wateryear column
ppt_month <- ppt_month %>%
  mutate(wateryear = case_when(month == "10" ~ year+1,
                               month == "11" ~ year+1,
                               month == "12" ~ year+1,
                               TRUE ~ as.numeric(year)))

# fix plot names in data to match rwi data
ppt_month_plot_update<- ppt_month %>%
  mutate(plot2 = case_when(plot == "RC_NA" ~ "RC",
                           plot == "K_NA" ~ "K",
                           plot == "JM_NA" ~ "JM", 
                           TRUE ~ as.character(plot)))
# prepare water year data
match_plots_rwi <- unique(rwi_00s_filtered_wide$plot)

water_year_filtered <- ppt_month_plot_update %>%
  filter(plot2 %in% match_plots_rwi)

water_year_avg <- water_year_filtered %>%
  filter(month == c(1, 2, 3, 4, 5, 10, 11, 12))%>%
  filter(type == "ppt")%>%
  group_by(plot2, wateryear)%>%
  summarize(total_ppt = sum(value))

water_year_wide <- water_year_avg %>%
  pivot_wider(names_from = wateryear, values_from = total_ppt, id_cols = plot2)%>%
  select(-c("1900", "2019", "2020"))%>%
  rename(plot = plot2)

water_year_long <- water_year_wide %>%
  pivot_longer(2:119, names_to = "year", values_to = "wy_ppt")

# format matrix for analysis
wateryear <- as.matrix(water_year_wide)
colnames(wateryear) <- NULL
wateryear <- wateryear[, c(2:119)]

# convert character matrix to numeric
wateryear = as.data.frame(wateryear, stringsAsFactors = FALSE)
wateryear = map_df(wateryear, as.numeric)
wateryear_mx <- as.matrix(wateryear)

# clean data for wpmf
times <- 1901:2018
wateryear_mx <- cleandat(wateryear_mx, times, clev = 5)$cdat
wateryear_df <- as.data.frame(wateryear_mx)
colnames(wateryear_df) <- 1901:2018
wateryear_df <- wateryear_df %>%
  pivot_longer(1:118, names_to="year", values_to = "total_wy_cleaned")
wateryear_df$plot <- water_year_long$plot

#### FUNCTIONS ####
n <- length(unique(rwi_00s_filtered_long$plot))
psync.by.chance <- function(n, nreps=10000, prob=c(0.025, 0.975)){
  
  #generates sets of random phasors
  rndphas <- matrix(complex(modulus=1, argument=2*pi*runif(n*nreps)), n, nreps)
  #takes the average--this is analogous to the wavelet phasor mean field
  rndphas.avg <- Mod(apply(rndphas, FUN=mean, MARGIN=2))
  #spit out quantiles corresponding to prob
  return(quantile(rndphas.avg, prob))
  
}

aictable<-function(X,m){
  rnames<-row.names(X)
  AICc<-X$AIC+2*X$df*(X$df+1)/(m-X$df-1)     # Small-sample correction
  logL<-X$df-X$AIC/2                         # Log-likelihood
  tab<-data.frame(X[,1],logL,AICc)           # Remove AIC column; add logL and AICc
  colnames(tab)[1]<-c("Params")              # Rename "df" column
  row.names(tab)<-rnames
  tab<-tab[order(tab$AICc),]                 # Sort by ascending AICc value
  deltaAICc<-tab$AICc-min(tab$AICc)          # Delta AICc
  weight<-exp(-deltaAICc/2)/sum(exp(-deltaAICc/2))  #Weights
  cumwt<-weight                              # Column for cumulative weight
  for(i in 2:dim(X)[1]){
    cumwt[i]<-cumwt[i-1]+cumwt[i]            # Accumulate weight from the top
  }
  tab<-data.frame(tab,deltaAICc,weight,cumwt)
  tab<-round(tab,4)
  tab
}

