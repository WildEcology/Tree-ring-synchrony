

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

water_year_wide <- water_year_filtered %>%
  filter(month == c(1, 2, 3, 4, 5, 10, 11, 12))%>%
  group_by(plot2, wateryear)%>%
  summarize(mean_ppt = mean(value))%>%
  pivot_wider(names_from = wateryear, values_from = mean_ppt, id_cols = plot2)%>%
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


#### ACROSS PLOT WAVELET ####
times <- 1901:2018
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, 1)$cdat
res<-wpmf(avg_plot_growth_mx,times,sigmethod="quick")
plotmag(res)

#### COHERENCE ####

## RWI & TMAX ##
times <- 1:118
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, 1)$cdat
tmax_mx <- cleandat(tmax_mx, times,1)$cdat

res_tmax <- coh(dat1 = tmax_mx, dat2=avg_plot_growth_mx, times=times,norm="powall",
                sigmethod = "fast", nrand=1000)
plotmag(res_tmax)

short <- c(2,5)
medium <- c(5,10)
long <- c(10,20)
xlong <- c(20,30)

res_tmax<-bandtest(res_tmax,short)
res_tmax<-bandtest(res_tmax,medium)
res_tmax<-bandtest(res_tmax,long)
res_tmax<-bandtest(res_tmax,xlong)
tmax_coherence <- get_bandp(res_tmax)

tmax_coherence <- tmax_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

## RWI & WATER-YEAR ##
times <- 1:118
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, 1)$cdat
wateryear_mx <- cleandat(wateryear_mx, times,1)$cdat


res_wateryear <- coh(dat1 = wateryear_mx, dat2 = avg_plot_growth_mx, times=times, norm="powall",
                     sigmethod = "fast", nrand=1000)

plotmag(res_wateryear)

short <- c(2,5)
medium <- c(5,10)
long <- c(10,20)
xlong <- c(20,30)

res_wateryear<-bandtest(res_wateryear,short)
res_wateryear<-bandtest(res_wateryear,medium)
res_wateryear<-bandtest(res_wateryear,long)
res_wateryear<-bandtest(res_wateryear,xlong)
wateryear_coherence <- get_bandp(res_wateryear)

wateryear_coherence <- wateryear_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))


## TMAX & WATER-YEAR ##
times <- 1:118
tmax_mx <- cleandat(tmax_mx, times,1)$cdat
wateryear_mx <- cleandat(wateryear_mx, times,1)$cdat

res_wateryear_tmax <- coh(dat1 = wateryear_mx, dat2 = tmax_mx, times=times, norm="powall",
                     sigmethod = "fast", nrand=1000)

plotmag(res_wateryear_tmax)

short <- c(2,5) 
medium <- c(5,10)
long <- c(10,20)
xlong <- c(20,30)

res_wateryear_tmax <-bandtest(res_wateryear_tmax,short)
res_wateryear_tmax <-bandtest(res_wateryear_tmax,medium)
res_wateryear_tmax <-bandtest(res_wateryear_tmax,long)
res_wateryear_tmax <-bandtest(res_wateryear_tmax,xlong)
wateryear_tmax_coherence <- get_bandp(res_wateryear_tmax)

wateryear_tmax_coherence <- wateryear_tmax_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

#### WAVELET LINEAR MODEL ####
times <- 1:118
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, 1)$cdat
tmax_mx <- cleandat(tmax_mx, times,1)$cdat
wateryear_mx <- cleandat(wateryear_mx, times,1)$cdat
dat<-list(rwi=avg_plot_growth_mx,wateryear=wateryear_mx,tmax=tmax_mx)

# create model
wlm_all<-wlm(dat,times,resp=1,pred=2:3,norm="powall")
# run model for each environmental variable
wlm_all_drop_wy<-wlmtest(wlm_all,drop="wateryear",sigmethod="fft",nrand=1000)
wlm_all_drop_tmax<-wlmtest(wlm_all,drop="tmax",sigmethod="fft",nrand=1000)

#specify timescales to test significance
short <- c(2,5)
medium <- c(5,10)
long <- c(10,20)
xlong <- c(20,30)

# significance of both variables at each timescale
wlm_all_drop_wy<-bandtest(wlm_all_drop_wy,short)
wlm_all_drop_wy<-bandtest(wlm_all_drop_wy,medium)
wlm_all_drop_wy<-bandtest(wlm_all_drop_wy,long)
wlm_all_drop_wy<-bandtest(wlm_all_drop_wy,xlong)
wlm_all_drop_tmax<-bandtest(wlm_all_drop_tmax,short)
wlm_all_drop_tmax<-bandtest(wlm_all_drop_tmax,medium)
wlm_all_drop_tmax<-bandtest(wlm_all_drop_tmax,long)
wlm_all_drop_tmax<-bandtest(wlm_all_drop_tmax,xlong)
wlm_wy <- get_bandp(wlm_all_drop_wy) %>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))
wlm_tmax <- get_bandp(wlm_all_drop_tmax)%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

plotmag(wlm_all_drop_wy)
plotrank(wlm_all_drop_wy)
plotmag(wlm_all_drop_tmax)
plotrank(wlm_all_drop_tmax)


se <- syncexpl(wlm_all)
se_short <- se[se$timescales>=short[1] & se$timescales<=short[2],]
se_short_output <- data.frame(round(100*colMeans(se_short[,c(3:9)])/mean(se_short$sync),2))

se_medium <- se[se$timescales>=medium[1] & se$timescales<=medium[2],]
se_medium_output <- data.frame(round(100*colMeans(se_medium[,c(3:9)])/mean(se_medium$sync),2))

se_long <- se[se$timescales>=long[1] & se$timescales<=long[2],]
se_long_output <- data.frame(round(100*colMeans(se_long[,c(3:9)])/mean(se_long$sync),2))

se_xlong <- se[se$timescales>=xlong[1] & se$timescales<=xlong[2],]
se_xlong_output <- data.frame(round(100*colMeans(se_xlong[,c(3:9)])/mean(se_xlong$sync),2))

sync_explained <- cbind(se_short_output, se_medium_output, se_long_output, se_xlong_output)
names(sync_explained) <- c("short", "medium", "long", "xlong")


#### MOVING WINDOW COHERENCE ####

## RWI & WATER-YEAR ##
window_length <- 50
window_delta <- window_length - 1
avg_plot_growth$year <- as.numeric(avg_plot_growth$year)
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year) - window_delta
times <- 1:50
short <- c(2,5)
medium <- c(5,10)
long <- c(10,20)

wy_coh_window <- tibble()

for(i in start_year:end_year){
  window_start <- i
  window_end <- i + window_delta
  select_years <- window_start:window_end
  print(select_years)
  
  # PREPARE DATA FOR COH
  # WATER - YEAR
  wateryear_window <- water_year_long %>%
    filter(year %in% select_years)%>%
    pivot_wider(names_from = year, values_from = wy_ppt, id_cols = plot)
  wateryear_window <- as.matrix(wateryear_window)
  colnames(wateryear_window) <- NULL
  wateryear_window <- wateryear_window[,2:51]
  wateryear_window = as.data.frame(wateryear_window, stringsAsFactors = FALSE)
  wateryear_window = map_df(wateryear_window, as.numeric)
  wateryear_mx <- as.matrix(wateryear_window)
  

  # RWI
  rwi_window<- avg_plot_growth %>%
    filter(year %in% select_years)%>%
    pivot_wider(names_from = "year", values_from = "avg_growth", id_cols=plot)
  rwi_window<- as.matrix(rwi_window)
  colnames(rwi_window) <- NULL
  rwi_window <- rwi_window[, c(2:51)] 
  rwi_window = as.data.frame(rwi_window, stringsAsFactors = FALSE)
  rwi_window = map_df(rwi_window, as.numeric)
  rwi_mx <- as.matrix(rwi_window)
  
  
  # COH
  rwi_mx <- cleandat(rwi_mx, times,1)$cdat
  wateryear_mx <- cleandat(wateryear_mx, times,1)$cdat

  res_wateryear <- coh(dat1 = wateryear_mx, dat2 = rwi_mx, times=times, norm="powall",
                       sigmethod = "fast", nrand=1000)
  
  res_wateryear<-bandtest(res_wateryear,short)
  res_wateryear<-bandtest(res_wateryear,medium)
  res_wateryear<-bandtest(res_wateryear,long)
  wateryear_coherence <- get_bandp(res_wateryear)
  
  wateryear_coherence <- wateryear_coherence %>%
    mutate(phase_test = (mn_phs/pi))%>%
    mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                             phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                             phase_test >= 0.75 ~ "anti"))%>%
    mutate(sig = case_when(p_val >= 0.05 ~ "non",
                           p_val <= 0.05 ~ "sig"))
  wateryear_coherence$start_year <- window_start
  wateryear_coherence$end_year <- window_end
  
  
  wy_coh_window <- bind_rows(wy_coh_window, wateryear_coherence)
  
}


## RWI & TMAX ##
window_length <- 50
window_delta <- window_length - 1
avg_plot_growth$year <- as.numeric(avg_plot_growth$year)
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year) - window_delta
times <- 1:50
short <- c(2,5)
medium <- c(5,10)
long <- c(10,20)

tmax_coh_window <- tibble()

for(i in start_year:end_year){
  window_start <- i
  window_end <- i + window_delta
  select_years <- window_start:window_end
  print(select_years)
  
  # PREPARE DATA FOR COH

  # TMAX
  tmax_window <- avg_plot_tmax_long %>%
    filter(year %in% select_years)%>%
    pivot_wider(names_from = year, values_from = avg_tmax, id_cols = plot)
  tmax_window <- as.matrix(tmax_window)
  colnames(tmax_window) <- NULL
  tmax_window <- tmax_window[, c(2:51)] 
  tmax_window = as.data.frame(tmax_window, stringsAsFactors = FALSE)
  tmax_window = map_df(tmax_window, as.numeric)
  tmax_mx <- as.matrix(tmax_window)
  
  # RWI
  rwi_window<- avg_plot_growth %>%
    filter(year %in% select_years)%>%
    pivot_wider(names_from = "year", values_from = "avg_growth", id_cols=plot)
  rwi_window<- as.matrix(rwi_window)
  colnames(rwi_window) <- NULL
  rwi_window <- rwi_window[, c(2:51)] 
  rwi_window = as.data.frame(rwi_window, stringsAsFactors = FALSE)
  rwi_window = map_df(rwi_window, as.numeric)
  rwi_mx <- as.matrix(rwi_window)
  
  
  # COH
  rwi_mx <- cleandat(rwi_mx, times, 1)$cdat
  tmax_mx <- cleandat(tmax_mx, times,1)$cdat
  
  res_tmax <- coh(dat1 = tmax_mx, dat2 = rwi_mx, times=times, norm="powall",
                       sigmethod = "fast", nrand=1000)
  
  res_tmax<-bandtest(res_tmax,short)
  res_tmax<-bandtest(res_tmax,medium)
  res_tmax<-bandtest(res_tmax,long)
  tmax_coherence <- get_bandp(res_tmax)
  
  tmax_coherence <- tmax_coherence %>%
    mutate(phase_test = (mn_phs/pi))%>%
    mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                             phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                             phase_test >= 0.75 ~ "anti"))%>%
    mutate(sig = case_when(p_val >= 0.05 ~ "non",
                           p_val <= 0.05 ~ "sig"))
  tmax_coherence$start_year <- window_start
  tmax_coherence$end_year <- window_end
  
  
  tmax_coh_window <- bind_rows(tmax_coh_window, tmax_coherence)
  
}

## FIND SIG WINDOWS ##
wy_coh_sig <- wy_coh_window %>%
  filter(sig == "sig")

tmax_coh_sig <- tmax_coh_window %>%
  filter(sig == "sig")


#### MOVING WINDOW SYNC EXPLAINED ####


window_length <- 50
window_delta <- window_length - 1
avg_plot_growth$year <- as.numeric(avg_plot_growth$year)
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year) - window_delta
times <- 1:50
short <- c(2,5)
medium <- c(5,10)
long <- c(10,20)

se_window <- tibble()

for(i in start_year:end_year){
  window_start <- i
  window_end <- i + window_delta
  select_years <- window_start:window_end
  print(select_years)
  
  # PREPARE DATA FOR WLM
  # WATER - YEAR
  wateryear_window <- water_year_long %>%
    filter(year %in% select_years)%>%
    pivot_wider(names_from = year, values_from = wy_ppt, id_cols = plot)
  wateryear_window <- as.matrix(wateryear_window)
  colnames(wateryear_window) <- NULL
  wateryear_window <- wateryear_window[,2:51]
  wateryear_window = as.data.frame(wateryear_window, stringsAsFactors = FALSE)
  wateryear_window = map_df(wateryear_window, as.numeric)
  wateryear_mx <- as.matrix(wateryear_window)
  
  
  # TMAX
  tmax_window <- avg_plot_tmax_long %>%
    filter(year %in% select_years)%>%
    pivot_wider(names_from = year, values_from = avg_tmax, id_cols = plot)
  tmax_window <- as.matrix(tmax_window)
  colnames(tmax_window) <- NULL
  tmax_window <- tmax_window[, c(2:51)] 
  tmax_window = as.data.frame(tmax_window, stringsAsFactors = FALSE)
  tmax_window = map_df(tmax_window, as.numeric)
  tmax_mx <- as.matrix(tmax_window)
  
  
  # RWI
  rwi_window<- avg_plot_growth %>%
    filter(year %in% select_years)%>%
    pivot_wider(names_from = "year", values_from = "avg_growth", id_cols=plot)
  rwi_window<- as.matrix(rwi_window)
  colnames(rwi_window) <- NULL
  rwi_window <- rwi_window[, c(2:51)] 
  rwi_window = as.data.frame(rwi_window, stringsAsFactors = FALSE)
  rwi_window = map_df(rwi_window, as.numeric)
  rwi_mx <- as.matrix(rwi_window)
  
  
  # WLM
  dat<-list(rwi=rwi_mx,wy=wateryear_mx,tmax=tmax_mx)
  dat<-lapply(FUN=function(x){cleandat(x,times,1)$cdat},X=dat)
  wlm_all<-wlm(dat,times,resp=1,pred=2:3,norm="powall")
  
  # SYNCHRONY EXPLAINED
  se <- syncexpl(wlm_all)
  temp_s <- se[se$timescales>=short[1] & se$timescales<=short[2],]
  se_short_output  <- matrix(round(100*colMeans(temp_s[,c(3:9)])/mean(temp_s$sync),2),ncol=7, nrow=1)
  colnames(se_short_output) <- c("syncexpl", "crossterms", "resids", "wateryear", "tmax", "interactions", 
                                 "wateryear_tmax")
  se_short <- as.data.frame(se_short_output)%>%
    add_column(w_start = i, w_length = window_length , w_end = window_end, band = "short")
  
  
  temp_m <- se[se$timescales>=medium[1] & se$timescales<=medium[2],]
  se_medium_output  <- matrix(round(100*colMeans(temp_m[,c(3:9)])/mean(temp_m$sync),2),ncol=7, nrow=1)
  colnames(se_medium_output) <- c("syncexpl", "crossterms", "resids", "wateryear", "tmax", "interactions", 
                                  "wateryear_tmax")
  se_medium <- as.data.frame(se_medium_output)%>%
    add_column(w_start = i, w_length = window_length , w_end = window_end, band = "medium")
  
  temp_l <- se[se$timescales>=long[1] & se$timescales<=long[2],]
  se_long_output  <- matrix(round(100*colMeans(temp_l[,c(3:9)])/mean(temp_l$sync),2),ncol=7, nrow=1)
  colnames(se_long_output) <- c("syncexpl", "crossterms", "resids", "wateryear", "tmax", "interactions", 
                                "wateryear_tmax")
  se_long <- as.data.frame(se_long_output)%>%
    add_column(w_start = i, w_length = window_length , w_end = window_end, band = "long")
  
  se_window <- bind_rows(se_window,se_short, se_medium, se_long)
  
}


## VISUALIZE MOVING WINDOW RESULTS ##

se_window_long <- se_window %>%
  select(syncexpl,wateryear,tmax,interactions,w_start,band)%>%
  rename(all=syncexpl)%>%
  pivot_longer(all:interactions, names_to = "variable", values_to = "syncexpl")

se_window_long$band_f = factor(se_window_long$band , levels=c("short","medium","long"))
p_window <- ggplot(data=se_window_long, aes(x=as.numeric(w_start), y=syncexpl, group=variable, col=variable)) + 
  geom_jitter( size = 2,width = 0.2, height = 0, alpha = 0.3)+
  geom_smooth(method = 'lm')+
  labs(x="Starting year", y="Synchrony Explained") +
  facet_wrap(~ band_f)+
  #geom_hline(yintercept=1, color="darkgrey") +
  theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 13, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 13, angle = 90, hjust = .5, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())+
  scale_colour_brewer(palette = "Set1",
                      name="Variable",
                      breaks=c("all", "wateryear", "tmax", "interactions"),
                      labels=c("all", "wateryear", "tmax", "interactions"))

#### ACROSS PLOT PROPORTION SIGNIFICIANT ####

# avg tree per plot
trees_per_plot <-  rwi_00s_filtered_long %>%
  filter(year == 2018)%>%
  group_by(plot)%>%
  summarise(numtree = n())

avg_tree_per_plot <- round(mean(trees_per_plot$numtree))


n <- avg_tree_per_plot
psync.by.chance <- function(n, nreps=10000, prob=c(0.025, 0.975)){
  
  #generates sets of random phasors
  rndphas <- matrix(complex(modulus=1, argument=2*pi*runif(n*nreps)), n, nreps)
  #takes the average--this is analogous to the wavelet phasor mean field
  rndphas.avg <- Mod(apply(rndphas, FUN=mean, MARGIN=2))
  #spit out quantiles corresponding to prob
  return(quantile(rndphas.avg, prob))
  
}

temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

# avg growth per plot per year
avg_plot_growth <- rwi_00s_filtered_long %>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

avg_plot_growth_wide <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")

# format matrix for analysis
avg_plot_growth_wide <- as.matrix(avg_plot_growth_wide)
colnames(avg_plot_growth_wide) <- NULL
avg_plot_growth_wide <- avg_plot_growth_wide[, c(2:120)] # timeseries 1901 -2018

# convert character matrix to numeric
avg_plot_growth_wide = as.data.frame(avg_plot_growth_wide, stringsAsFactors = FALSE)
avg_plot_growth_wide = map_df(avg_plot_growth_wide, as.numeric)
avg_plot_growth_mx <- as.matrix(avg_plot_growth_wide)

# across plot wavelet 
times <- 1900:2018
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, 1)$cdat
res<-wpmf(avg_plot_growth_mx,times,sigmethod="quick")
plotmag(res)

#extract raw values
M1 <- as.data.frame(res$values)
colnames(M1) <- res$timescales
#fix the imaginary #s
M1 <- abs(M1)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M2 <- M1[,1:67]
year <- 1900:2018
M2$year <- year
# classify sync, async and ns
M2<- M2 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
M2 <- na.omit(M2)
M2events <- M2 %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M2events$ts <- as.numeric(M2events$ts)
M2events$ts <- as.numeric(M2events$ts)
M2events <- M2events %>%
  mutate(interval = case_when(ts >= 2 & ts <= 5 ~ "short",
                              ts > 5 & ts <= 10 ~ "medium",
                              ts > 10 & ts <= 20 ~ "long",
                              ts > 20 & ts <= 30 ~ "ancient"))


prop_sync_final <- data.frame(year = NA, synch = NA, asynch = NA, ns = NA, interval = NA)

for (xx in 1:length(unique(M2events$interval))) {
  
  current <- unique(M2events$interval)[xx]
  M2events_s <- M2events %>%
    filter(interval == current)
  
  #significantly synchronous in the short term
  SSs <- M2events_s %>%
    filter(event == "synchronous")%>%
    group_by(year)%>%
    summarise(short_sync = n())
  
  #significantly asynchronous in the short term
  SAs <- M2events_s %>%
    filter(event == "asynchronous")%>%
    group_by(year)%>%
    summarise(short_async = n())
  
  #not significant in the short term
  NSs <- M2events_s %>%
    filter(event == "NS")%>%
    group_by(year)%>%
    summarise(short_NS = n())
  
  #the number of observations per year to divide by to get the proportion
  prop_den_s <- M2events_s %>%
    group_by(year)%>%
    summarise(obs = n())
  
  # proportion synchronous
  prop_calc_Ss <- left_join(prop_den_s, SSs)
  prop_sync <- prop_calc_Ss %>%
    group_by(year)%>%
    summarise(synch = short_sync/obs)
  
  # proportion asynchronous
  prop_calc_As <- left_join(prop_den_s, SAs)
  prop_async <- prop_calc_As %>%
    group_by(year)%>%
    summarise(asynch = short_async/obs) 
  
  # proportion not significant
  prop_calc_Ns <- left_join(prop_den_s, NSs)
  prop_ns <- prop_calc_Ns %>%
    group_by(year)%>%
    summarise(ns = short_NS/obs)
  
  plot1_temp <- prop_sync %>%
    full_join(prop_async, by="year") %>%
    full_join(prop_ns, by="year")
  
  plot1_temp$interval <- current
  
  prop_sync_final <- rbind(prop_sync_final, plot1_temp)
}

prop_sync_final <- prop_sync_final[2:385,]
prop_sync_final[is.na(prop_sync_final)] <- 0


## ACROSS PLOT PROPORTION - STATS ##
library(splines)

# redefine timeseries for each band to include only years with at least half the timescales in that band

timescales_b <- data.frame(res$timescales)

timescales_per_band <- timescales_b %>%
  mutate(band = case_when(res.timescales >= 2 & res.timescales <= 5 ~ "short",
                          res.timescales > 5 & res.timescales <= 10 ~ "medium",
                          res.timescales > 10 & res.timescales <= 20 ~ "long",
                          res.timescales > 20 & res.timescales <= 30 ~ "ancient"))%>%
  count(band)%>%
  rename(interval = band)%>%
  na.omit()

num_timescales_per_year_per_band <- M2events %>% 
  group_by(year, interval)%>%
  summarise(num_ts = n())

num_timescales_per_year_per_band <- full_join(num_timescales_per_year_per_band,timescales_per_band)

new_timeseries <- num_timescales_per_year_per_band %>%
  mutate(keep_year = case_when(num_ts >= n/2 ~ "Y",
                               num_ts < n/2 ~ "N"))%>%
  filter(keep_year == "Y")%>%
  group_by(interval)%>%
  mutate(starting.year = min(year)) %>%
  mutate(ending.year = max(year))

final_timeseries <- new_timeseries %>%
  select(interval, starting.year, ending.year)%>%
  group_by(interval)%>%
  distinct(starting.year, .keep_all = TRUE)

# fit spline glms w/ binomal distributions & test different number of knots

prop_sync_final_newts <- inner_join(prop_sync_final, final_timeseries)

# short term
prop_sync_final_s <- prop_sync_final_newts %>%
  filter(interval == "short", year >= starting.year & year <= ending.year)

within_spline_9 <- glm(synch ~ ns(year,9), data = prop_sync_final_s, family=binomial(link = "logit"))
within_spline_8 <- glm(synch ~ ns(year,8), data = prop_sync_final_s, family=binomial(link = "logit"))
within_spline_7 <- glm(synch ~ ns(year,7), data = prop_sync_final_s, family=binomial(link = "logit"))
within_spline_6 <-  glm(synch ~ ns(year,6), data = prop_sync_final_s, family=binomial(link = "logit"))
within_spline_5 <-  glm(synch ~ ns(year,5), data = prop_sync_final_s, family=binomial(link = "logit"))
within_spline_4 <- glm(synch ~ ns(year,4), data = prop_sync_final_s, family=binomial(link = "logit"))
within_spline_3 <- glm(synch ~ ns(year,3), data = prop_sync_final_s, family=binomial(link = "logit"))
within_spline_2 <- glm(synch ~ ns(year,2), data = prop_sync_final_s, family=binomial(link = "logit"))
within_lm <- glm(synch~ year, data = prop_sync_final_s,family = binomial(link = "logit"))

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

rawaic <- AIC(within_lm,within_spline_2, within_spline_3,within_spline_4,within_spline_5,within_spline_6,within_spline_7,within_spline_8,within_spline_9)
nR <- dim(prop_sync_final_s)[1]
aic_output <- aictable(rawaic,nR)
aic_output[1,]
# spline with 8 knots is best fit
synchpredict_s <- ggpredict(within_spline_8, terms = "year [all]")
synchpredict_s$interval <- "short"

# medium - term
prop_sync_final_m <- prop_sync_final_newts %>%
  filter(interval == "medium", year >= starting.year & year <= ending.year)

within_spline_9 <- glm(synch ~ ns(year,9), data = prop_sync_final_m, family=binomial(link = "logit"))
within_spline_8 <- glm(synch ~ ns(year,8), data = prop_sync_final_m, family=binomial(link = "logit"))
within_spline_7 <- glm(synch ~ ns(year,7), data = prop_sync_final_m, family=binomial(link = "logit"))
within_spline_6 <-  glm(synch ~ ns(year,6), data = prop_sync_final_m, family=binomial(link = "logit"))
within_spline_5 <-  glm(synch ~ ns(year,5), data = prop_sync_final_m, family=binomial(link = "logit"))
within_spline_4 <- glm(synch ~ ns(year,4), data = prop_sync_final_m, family=binomial(link = "logit"))
within_spline_3 <- glm(synch ~ ns(year,3), data = prop_sync_final_m, family=binomial(link = "logit"))
within_spline_2 <- glm(synch ~ ns(year,2), data = prop_sync_final_m, family=binomial(link = "logit"))
within_lm <- glm(synch~ year, data = prop_sync_final_m,family = binomial(link = "logit"))


rawaic <- AIC(within_lm,within_spline_2, within_spline_3,within_spline_4,within_spline_5,within_spline_6,within_spline_7,within_spline_8,within_spline_9)
nR <- dim(prop_sync_final_m)[1]
aic_output <- aictable(rawaic,nR)
aic_output[1,]
# spline with 7 knots is best fit
synchpredict_m <- ggpredict(within_spline_7, terms = "year [all]")
synchpredict_m$interval <- "medium"

# long - term
prop_sync_final_l <- prop_sync_final_newts %>%
  filter(interval == "long", year >= starting.year & year <= ending.year)

within_spline_9 <- glm(synch ~ ns(year,9), data = prop_sync_final_l, family=binomial(link = "logit"))
within_spline_8 <- glm(synch ~ ns(year,8), data = prop_sync_final_l, family=binomial(link = "logit"))
within_spline_7 <- glm(synch ~ ns(year,7), data = prop_sync_final_l, family=binomial(link = "logit"))
within_spline_6 <-  glm(synch ~ ns(year,6), data = prop_sync_final_l, family=binomial(link = "logit"))
within_spline_5 <-  glm(synch ~ ns(year,5), data = prop_sync_final_l, family=binomial(link = "logit"))
within_spline_4 <- glm(synch ~ ns(year,4), data = prop_sync_final_l, family=binomial(link = "logit"))
within_spline_3 <- glm(synch ~ ns(year,3), data = prop_sync_final_l, family=binomial(link = "logit"))
within_spline_2 <- glm(synch ~ ns(year,2), data = prop_sync_final_l, family=binomial(link = "logit"))
within_lm <- glm(synch~ year, data = prop_sync_final_l,family = binomial(link = "logit"))


rawaic <- AIC(within_lm,within_spline_2, within_spline_3,within_spline_4,within_spline_5,within_spline_6,within_spline_7,within_spline_8,within_spline_9)
nR <- dim(prop_sync_final_l)[1]
aic_output <- aictable(rawaic,nR)
aic_output[1,]
# spline with 6 knots is best fit
synchpredict_l <- ggpredict(within_spline_6, terms = "year [all]")
synchpredict_l$interval <- "long"

# xlong - term
prop_sync_final_a <- prop_sync_final_newts %>%
  filter(interval == "ancient", year >= starting.year & year <= ending.year)

within_spline_9 <- glm(synch ~ ns(year,9), data = prop_sync_final_a, family=binomial(link = "logit"))
within_spline_8 <- glm(synch ~ ns(year,8), data = prop_sync_final_a, family=binomial(link = "logit"))
within_spline_7 <- glm(synch ~ ns(year,7), data = prop_sync_final_a, family=binomial(link = "logit"))
within_spline_6 <-  glm(synch ~ ns(year,6), data = prop_sync_final_a, family=binomial(link = "logit"))
within_spline_5 <-  glm(synch ~ ns(year,5), data = prop_sync_final_a, family=binomial(link = "logit"))
within_spline_4 <- glm(synch ~ ns(year,4), data = prop_sync_final_a, family=binomial(link = "logit"))
within_spline_3 <- glm(synch ~ ns(year,3), data = prop_sync_final_a, family=binomial(link = "logit"))
within_spline_2 <- glm(synch ~ ns(year,2), data = prop_sync_final_a, family=binomial(link = "logit"))
within_lm <- glm(synch~ year, data = prop_sync_final_a,family = binomial(link = "logit"))


rawaic <- AIC(within_lm,within_spline_2, within_spline_3,within_spline_4,within_spline_5,within_spline_6,within_spline_7,within_spline_8,within_spline_9)
nR <- dim(prop_sync_final_a)[1]
aic_output <- aictable(rawaic,nR)
aic_output[1,]
# spline with 4 knots is best fit
synchpredict_a <- ggpredict(within_spline_4, terms = "year [all]")
synchpredict_a$interval <- "ancient"


synchpredict <- bind_rows(synchpredict_s,synchpredict_m,synchpredict_l,synchpredict_a)

## VISUALIZE SPLINES ##
library("ggeffects")
library("purrr")

prop_sync_final_newts$interval_f <- factor(prop_sync_final_newts$interval)
synchpredict$interval_f <- factor(synchpredict$interval)

ggplot()+
  geom_line(data = synchpredict, aes(x=x, y=predicted, col = interval), size = 1)+
  geom_ribbon(data = synchpredict, aes(x=x, y=predicted,ymin=conf.low, ymax=conf.high, fill = interval), alpha = 0.1)+
  geom_point(data = prop_sync_final_newts, aes(x=year, y=synch, col=interval), alpha = 0.5, shape = 1)+
  xlab("Year")+
  ylab("Synchronous events (annual proportion)")+
  theme_bw()+
  scale_color_discrete(name = "Timescale Band",
                       labels = c("ancient", "long", "medium", "short"),
                       guide = guide_legend(reverse = TRUE))+
  scale_fill_discrete(name = "Timescale Band",
                      labels = c("ancient", "long", "medium", "short"),
                      guide = guide_legend(reverse = TRUE))

