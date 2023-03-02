

# load necessary packages
library("tidyverse")
library("ggplot2")
library("dplyr")
library("wsyn")
library("here")
library("tibble")

# read in datasets
prismdat <- read.csv(here("Data/rwi_prismdat.csv")) # RWI 
env_month <- read.csv(here("Data/prism_plots_1900.csv")) # WATER-YEAR & TMAX

#### PREPARE DATA ####

## GROWTH DATA ##
# subset 1900-2018, each row represent's and individual tree's data
rwi_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)%>%
  select(year, plot, value, tree_num) %>%
  pivot_wider(names_from = year, values_from=value)

# remove any trees with missing data along the time series 
rwi_00s_tree_filtered<- rwi_00s[rowSums(is.na(rwi_00s))==0,]

# find plots with at least 5 trees 
rwi_00s_plot_filtered <- rwi_00s_tree_filtered %>%
  pivot_longer(3:121, names_to = "year", values_to = "value")%>%
  group_by(plot)%>%
  mutate(num_trees = round(n()/118))%>%
  filter(num_trees >= 5)

rwi_00s_plot_filtered_wide <- rwi_00s_plot_filtered %>%
  pivot_wider(names_from = year, values_from = value)

# avg growth per plot per year
avg_plot_growth <- rwi_00s_plot_filtered %>%
  filter(year > 1900)%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(value))

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

res_growth_wpmf<-wpmf(avg_plot_growth_mx,times,sigmethod="none")


## ENVIRONMENTAL VARIABLES##
env_month$plot <- as.character(env_month$plot)
env_month$type <- as.character(env_month$type)
env_month$year <- as.numeric(env_month$year)
env_month$month <- as.numeric(env_month$month)
env_month$value <- as.numeric(env_month$value)

# fix plot names in data to match rwi data
env_month_plot_update<- env_month %>%
  mutate(plot2 = case_when(plot == "RC_NA" ~ "RC",
                           plot == "K_NA" ~ "K",
                           plot == "JM_NA" ~ "JM", 
                           TRUE ~ as.character(plot)))
match_plots_rwi <- unique(rwi_00s_plot_filtered_wide$plot)
env_filtered <- env_month_plot_update %>%
  filter(plot2 %in% match_plots_rwi)


## TMAX ##
avg_tmax <- env_filtered%>%
  filter(type == "tmax")%>%
  group_by(plot2, year)%>%
  summarize(avg_tmax = mean(value))

avg_tmax_wide <- avg_tmax %>%
  pivot_wider(names_from = year, values_from = avg_tmax, id_cols = plot2)%>%
  select(-c("1900", "2019", "2020"))%>%
  rename(plot = plot2)

avg_tmax_long <- avg_tmax_wide %>%
  pivot_longer(2:119,names_to = "year", values_to = "avg_tmax")

tmax <- as.matrix(avg_tmax_wide)
colnames(tmax) <- NULL
tmax <- tmax[, c(2:119)] # timeseries 1901-2018


# convert character matrix to numeric
tmax = as.data.frame(tmax, stringsAsFactors = FALSE)
tmax = map_df(tmax, as.numeric)
tmax_mx <- as.matrix(tmax)

# clean data for wpmf
tmax_mx <- cleandat(tmax_mx, times, clev = 5)$cdat
tmax_df <- as.data.frame(tmax_mx)
colnames(tmax_df) <- 1901:2018
tmax_df <- tmax_df %>%
  pivot_longer(1:118, names_to="year", values_to = "avg_tmax_cleaned")
tmax_df$plot <- avg_tmax_long$plot

res_tmax_wpmf<-wpmf(tmax_mx,times,sigmethod="none")

## WATER YEAR ##
# make wateryear column
ppt_month <- env_filtered %>%
  mutate(wateryear = case_when(month == "10" ~ year+1,
                               month == "11" ~ year+1,
                               month == "12" ~ year+1,
                               TRUE ~ as.numeric(year)))


water_year_total <- ppt_month %>%
  filter(month == c(1, 2, 3, 4, 5, 10, 11, 12))%>%
  filter(type == "ppt")%>%
  group_by(plot2, wateryear)%>%
  summarize(total_ppt = sum(value))

water_year_wide <- water_year_total %>%
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
wateryear_mx <- cleandat(wateryear_mx, times, clev = 5)$cdat
wateryear_df <- as.data.frame(wateryear_mx)
colnames(wateryear_df) <- 1901:2018
wateryear_df <- wateryear_df %>%
  pivot_longer(1:118, names_to="year", values_to = "total_wy_cleaned")
wateryear_df$plot <- water_year_long$plot

res_precip_wpmf<-wpmf(wateryear_mx,times,sigmethod="none")


#### FUNCTIONS ####
n <- length(match_plots_rwi)
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

