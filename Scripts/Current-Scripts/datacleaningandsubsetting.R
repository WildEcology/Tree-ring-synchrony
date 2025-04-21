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
  group_by(plot) %>%
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
  summarise(winter_ppt = mean(ppt))%>%
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



#### TWO TIME PERIODS ##########################################################
# split and clean all data into two time periods 

##1917 - 1967 time series ##
## RWI ##
# format matrix for wavelet analysis
early_growth <- avg_plot_growth_wide[, c(18:68)]
early_growth_raw <- avg_plot_growth_wide[, c(18:68)]
# pivot longer for plotting purposes
colnames(early_growth_raw) <- 1917:1967
early_growth_long <- early_growth_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_growth")
early_growth_plots <- avg_plot_growth %>%
  filter(year >= 1917) %>%
  filter(year <= 1967)
early_growth_long$plot <- early_growth_plots$plot
early_growth_long$year <- as.character(early_growth_long$year)
# convert character matrix to numeric
early_growth = as.data.frame(early_growth, stringsAsFactors = FALSE)
early_growth = map_df(early_growth, as.numeric)
early_growth_mx <- as.matrix(early_growth)

# clean data for wpmf
times <- 1917:1967
early_growth_mx <- cleandat(early_growth_mx, times, clev = 5)$cdat
early_growth_df <- as.data.frame(early_growth_mx)
colnames(early_growth_df) <- 1917:1967
early_growth_df <- early_growth_df %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_growth_cleaned") 
early_growth_df$plot <- early_growth_plots$plot

# produce wpmf & wmf for growth data
res_early_growth_wpmf<-wpmf(early_growth_mx,times,sigmethod="none")
res_early_growth_wmf<-wmf(early_growth_mx,times)

## PPT ##
# format matrix for wavelet analysis
early_ppt <- winter_ppt_wide[, c(18:68)] 
early_ppt_raw <- winter_ppt_wide[, c(18:68)] 
# pivot longer for plotting purposes
colnames(early_ppt_raw) <- 1917:1967
early_ppt_long <- early_ppt_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "winter_ppt")
early_ppt_long$plot <- early_growth_plots$plot
early_ppt_long$year <- as.character(early_ppt_long$year)
# convert character matrix to numeric
early_ppt = as.data.frame(early_ppt, stringsAsFactors = FALSE)
early_ppt = map_df(early_ppt, as.numeric)
early_ppt_mx <- as.matrix(early_ppt)

# clean data for wpmf
times <- 1917:1967
early_ppt_mx <- cleandat(early_ppt_mx, times, clev = 5)$cdat
early_ppt_df <- as.data.frame(early_ppt_mx)
colnames(early_ppt_df) <- 1917:1967
early_ppt_df <- early_ppt_df %>%
  pivot_longer(1:51, names_to="year", values_to = "winter_ppt_cleaned")
early_ppt_plots <- avg_plot_growth %>%
  filter(year >= 1917) %>%
  filter(year <= 1967)
early_ppt_df$plot <- early_ppt_plots$plot

# produce wpmf & wmf for ppt data
res_early_ppt_wpmf<-wpmf(early_ppt_mx,times,sigmethod="none")
res_early_ppt_wmf<-wmf(early_ppt_mx,times)

## TMIN ##
# format matrix for wavelet analysis
early_tmin <- summer_tmin_wide[, c(18:68)] 
early_tmin_raw <- summer_tmin_wide[, c(18:68)] 
# pivot longer for plotting purposes
colnames(early_tmin_raw) <- 1917:1967
early_tmin_long <- early_tmin_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "summer_tmin")
early_tmin_long$plot <- early_growth_plots$plot
early_tmin_long$year <- as.character(early_tmin_long$year)
# convert character matrix to numeric
early_tmin = as.data.frame(early_tmin, stringsAsFactors = FALSE)
early_tmin = map_df(early_tmin, as.numeric)
early_tmin_mx <- as.matrix(early_tmin)

# clean data for wpmf
times <- 1917:1967
early_tmin_mx <- cleandat(early_tmin_mx, times, clev = 5)$cdat
early_tmin_df <- as.data.frame(early_tmin_mx)
colnames(early_tmin_df) <- 1917:1967
early_tmin_df <- early_tmin_df %>%
  pivot_longer(1:51, names_to="year", values_to = "summer_tmin_cleaned")
early_tmin_plots <- avg_plot_growth %>%
  filter(year >= 1917) %>%
  filter(year <= 1967)
early_tmin_df$plot <- early_tmin_plots$plot

# produce wpmf & wmf for growth data
res_early_tmin_wpmf<-wpmf(early_tmin_mx,times,sigmethod="none")
res_early_tmin_wmf<-wmf(early_tmin_mx,times)

## vpdmax ##
# format matrix for wavelet analysis
early_vpdmax <- avg_vpdmax_wide[, c(18:68)] 
early_vpdmax_raw <- avg_vpdmax_wide[, c(18:68)] 
# pivot longer for plotting purposes
colnames(early_vpdmax_raw) <- 1917:1967
early_vpdmax_long <- early_vpdmax_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_vpdmax")
early_vpdmax_long$plot <- early_growth_plots$plot
early_vpdmax_long$year <- as.character(early_vpdmax_long$year)
# convert character matrix to numeric
early_vpdmax = as.data.frame(early_vpdmax, stringsAsFactors = FALSE)
early_vpdmax = map_df(early_vpdmax, as.numeric)
early_vpdmax_mx <- as.matrix(early_vpdmax)

# clean data for wpmf
times <- 1917:1967
early_vpdmax_mx <- cleandat(early_vpdmax_mx, times, clev = 5)$cdat
early_vpdmax_df <- as.data.frame(early_vpdmax_mx)
colnames(early_vpdmax_df) <- 1917:1967
early_vpdmax_df <- early_vpdmax_df %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_vpdmax_cleaned")
early_vpdmax_plots <- avg_plot_growth %>%
  filter(year >= 1917) %>%
  filter(year <= 1967)
early_vpdmax_df$plot <- early_vpdmax_plots$plot

# produce wpmf & wmf for growth data
res_early_vpdmax_wpmf<-wpmf(early_vpdmax_mx,times,sigmethod="none")
res_early_vpdmax_wmf<-wmf(early_vpdmax_mx,times)

## 1968 - 2018 time series ##
## RWI ##
# format matrix for wavelet analysis
late_growth <- avg_plot_growth_wide[, c(69:119)] 
late_growth_raw <- avg_plot_growth_wide[, c(69:119)]
# pivot longer for plotting purposes
colnames(late_growth_raw) <- 1968:2018
late_growth_long <- late_growth_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_growth")
late_growth_plots <- avg_plot_growth %>%
  filter(year >= 1917) %>%
  filter(year <= 1967)
late_growth_long$plot <- late_growth_plots$plot
late_growth_long$year <- as.character(late_growth_long$year)
# convert character matrix to numeric
late_growth = as.data.frame(late_growth, stringsAsFactors = FALSE)
late_growth = map_df(late_growth, as.numeric)
late_growth_mx <- as.matrix(late_growth)

# clean data for wpmf
times <- 1968:2018
late_growth_mx <- cleandat(late_growth_mx, times, clev = 5)$cdat
late_growth_df <- as.data.frame(late_growth_mx)
colnames(late_growth_df) <- 1968:2018
late_growth_df <- late_growth_df %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_growth_cleaned")
late_growth_plots <- avg_plot_growth %>%
  filter(year >= 1968) %>%
  filter(year <= 2018)
late_growth_df$plot <- late_growth_plots$plot

# produce wpmf & wmf for growth data
res_late_growth_wpmf<-wpmf(late_growth_mx,times,sigmethod="none")
res_late_growth_wmf<-wmf(late_growth_mx,times)

## PPT ##
# format matrix for wavelet analysis
late_ppt <- winter_ppt_wide[, c(69:119)] 
late_ppt_raw <- winter_ppt_wide[, c(69:119)]
# pivot longer for plotting purposes
colnames(late_ppt_raw) <- 1968:2018
late_ppt_long <- late_ppt_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "winter_ppt")
late_ppt_plots <- avg_plot_growth %>%
  filter(year >= 1968) %>%
  filter(year <= 2018)
late_ppt_long$plot <- late_growth_plots$plot
late_ppt_long$year <- as.character(late_ppt_long$year)
# convert character matrix to numeric
late_ppt = as.data.frame(late_ppt, stringsAsFactors = FALSE)
late_ppt = map_df(late_ppt, as.numeric)
late_ppt_mx <- as.matrix(late_ppt)

# clean data for wpmf
times <- 1968:2018
late_ppt_mx <- cleandat(late_ppt_mx, times, clev = 5)$cdat
late_ppt_df <- as.data.frame(late_ppt_mx)
colnames(late_ppt_df) <- 1968:2018
late_ppt_df <- late_ppt_df %>%
  pivot_longer(1:51, names_to="year", values_to = "winter_ppt_cleaned")
late_ppt_plots <- avg_plot_growth %>%
  filter(year >= 1968) %>%
  filter(year <= 2018)
late_ppt_df$plot <- late_ppt_plots$plot

# produce wpmf & wmf for growth data
res_late_ppt_wpmf<-wpmf(late_ppt_mx,times,sigmethod="none")
res_late_ppt_wmf<-wmf(late_ppt_mx,times)

## TMIN ##
# format matrix for wavelet analysis
late_tmin <- summer_tmin_wide[, c(69:119)] 
late_tmin_raw <- summer_tmin_wide[, c(69:119)]
# pivot longer for plotting purposes
colnames(late_tmin_raw) <- 1968:2018
late_tmin_long <- late_tmin_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "summer_tmin")
late_tmin_plots <- avg_plot_growth %>%
  filter(year >= 1917) %>%
  filter(year <= 1967)
late_tmin_long$plot <- late_tmin_plots$plot
late_tmin_long$year <- as.character(late_tmin_long$year)
# convert character matrix to numeric
late_tmin = as.data.frame(late_tmin, stringsAsFactors = FALSE)
late_tmin = map_df(late_tmin, as.numeric)
late_tmin_mx <- as.matrix(late_tmin)

# clean data for wpmf
times <- 1968:2018
late_tmin_mx <- cleandat(late_tmin_mx, times, clev = 5)$cdat
late_tmin_df <- as.data.frame(late_tmin_mx)
colnames(late_tmin_df) <- 1968:2018
late_tmin_df <- late_tmin_df %>%
  pivot_longer(1:51, names_to="year", values_to = "summer_tmin_cleaned")
late_tmin_plots <- avg_plot_growth %>%
  filter(year >= 1968) %>%
  filter(year <= 2018)
late_tmin_df$plot <- late_tmin_plots$plot

# produce wpmf & wmf for growth data
res_late_tmin_wpmf<-wpmf(late_tmin_mx,times,sigmethod="none")
res_late_tmin_wmf<-wmf(late_tmin_mx,times)

## vpdmax ##
# format matrix for wavelet analysis
late_vpdmax <- avg_vpdmax_wide[, c(69:119)] 
late_vpdmax_raw <- avg_vpdmax_wide[, c(69:119)]
# pivot longer for plotting purposes
colnames(late_vpdmax_raw) <- 1968:2018
late_vpdmax_long <- late_vpdmax_raw %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_vpdmax")
late_vpdmax_plots <- avg_plot_growth %>%
  filter(year >= 1917) %>%
  filter(year <= 1967)
late_vpdmax_long$plot <- late_vpdmax_plots$plot
late_vpdmax_long$year <- as.character(late_vpdmax_long$year)
# convert character matrix to numeric
late_vpdmax = as.data.frame(late_vpdmax, stringsAsFactors = FALSE)
late_vpdmax = map_df(late_vpdmax, as.numeric)
late_vpdmax_mx <- as.matrix(late_vpdmax)

# clean data for wpmf
times <- 1968:2018
late_vpdmax_mx <- cleandat(late_vpdmax_mx, times, clev = 5)$cdat
late_vpdmax_df <- as.data.frame(late_vpdmax_mx)
colnames(late_vpdmax_df) <- 1968:2018
late_vpdmax_df <- late_vpdmax_df %>%
  pivot_longer(1:51, names_to="year", values_to = "avg_vpdmax_cleaned")
late_vpdmax_plots <- avg_plot_growth %>%
  filter(year >= 1968) %>%
  filter(year <= 2018)
late_vpdmax_df$plot <- late_vpdmax_plots$plot

# produce wpmf & wmf for growth data
res_late_vpdmax_wpmf<-wpmf(late_vpdmax_mx,times,sigmethod="none")
res_late_vpdmax_wmf<-wmf(late_vpdmax_mx,times)


#### FUNCTIONS #################################################################
n <- length(plots)
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
