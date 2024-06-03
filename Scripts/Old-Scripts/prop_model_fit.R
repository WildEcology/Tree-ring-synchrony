# load necessary packages
library("tidyverse")
library("ggplot2")
library("dplyr")
library("wsyn")
library("here")
library("tibble")

# read in datasets
prismdat <- read.csv(here("Data/rwi_prismdat.csv")) # RWI & TMAX

# aic table function
aictable<-function(X,m){

  rnames<-row.names(X)
  AICc<-X$AIC+2*X$df*(X$df+1)/(m-X$df-1)     #small-sample correction
  logL<-X$df-X$AIC/2                         #Log-likelihood
  tab<-data.frame(X[,1],logL,AICc)           #Remove AIC column; add logL and AICc
  colnames(tab)[1]<-c("Params")              #Rename "df" column
  row.names(tab)<-rnames
  tab<-tab[order(tab$AICc),]                 #Sort by ascending AICc value
  deltaAICc<-tab$AICc-min(tab$AICc)          #Delta AICc
  weight<-exp(-deltaAICc/2)/sum(exp(-deltaAICc/2))  #Weights
  cumwt<-weight                              #Column for cumulative weight
  for(i in 2:dim(X)[1]){
    cumwt[i]<-cumwt[i-1]+cumwt[i]              #Accumulate weight from the top
  }
  tab<-data.frame(tab,deltaAICc,weight,cumwt)
  tab<-round(tab,4)
  tab
}

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

#### ACROSS PLOT PROPORTION SIGNIFICIANT ####

n <- length(unique(rwi_00s_filtered_long$plot))
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
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, clev = 5)$cdat
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

prop_sync_final_newts <- inner_join(prop_sync_final, final_timeseries)

#### model selection per band ####

short_ts <- prop_sync_final_newts %>%
  filter(interval == "short")%>%
  mutate(times = year - 1900)

med_ts <- prop_sync_final_newts %>%
  filter(interval == "medium")%>%
  mutate(times = year - 1900)

long_ts <- prop_sync_final_newts %>%
  filter(interval == "long")%>%
  mutate(times = year - 1900)

xlong_ts <- prop_sync_final_newts %>%
  filter(interval == "ancient")%>%
  mutate(times = year - 1900)

# short
null_synch_s <- lm(synch ~ 1, data = short_ts)
linear_synch_s <- lm(synch ~ times, data = short_ts)
quad_synch_s <- lm(synch ~ times + I(times^2), data = short_ts)
sat_synch_s <- nls(synch ~ SSasymp(times, Asym, R0, lrc), data=short_ts)


rawaic <- AIC(null_synch_s, linear_synch_s,quad_synch_s)
nR <- dim(short_ts)[1]
short_fit <- aictable(rawaic, nR)

# medium
null_synch_m <- lm(synch ~ 1, data = med_ts)
linear_synch_m <- lm(synch ~ year, data = med_ts)
quad_synch_m <- lm(synch ~ year + I(year^2), data = med_ts)
sat_synch_m <- nls(synch ~ SSasymp(year, Asym, R0, lrc), data=med_ts)


rawaic <- AIC(null_synch_m, linear_synch_m,quad_synch_m)
nR <- dim(med_ts)[1]
med_fit <- aictable(rawaic, nR)


# long
null_synch_l <- lm(synch ~ 1, data = long_ts)
linear_synch_l <- lm(synch ~ year, data = long_ts)
quad_synch_l <- lm(synch ~ year + I(year^2), data = long_ts)
sat_synch_l <- nls(synch ~ SSasymp(year, Asym, R0, lrc), data=long_ts)


rawaic <- AIC(null_synch_l, linear_synch_l,quad_synch_l, sat_synch_l)
nR <- dim(long_ts)[1]
long_fit <- aictable(rawaic, nR)

# xlong
null_synch_xl <- lm(synch ~ 1, data = xlong_ts)
linear_synch_xl <- lm(synch ~ year, data = xlong_ts)
quad_synch_xl <- lm(synch ~ year + I(year^2), data = xlong_ts)
sat_synch_xl <- nls(synch ~ SSasymp(year, Asym, R0, lrc), data=xlong_ts, control=nls.control(minFactor=0.0009765))


rawaic <- AIC(null_synch_xl, linear_synch_xl,quad_synch_xl)
nR <- dim(xlong_ts)[1]
xlong_fit <- aictable(rawaic, nR)
