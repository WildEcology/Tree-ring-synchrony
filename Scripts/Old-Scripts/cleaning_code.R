

# load necessary packages
library("tidyverse")
library("ggplot2")
library("dplyr")
library("wsyn")
library("here")
library("tibble")
library("janitor")

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
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, clev = 1)$cdat
avg_plot_growth_df <- as.data.frame(avg_plot_growth_mx)
colnames(avg_plot_growth_df) <- 1901:2018
avg_plot_growth_df <- avg_plot_growth_df %>%
  pivot_longer(1:118, names_to="year", values_to = "avg_growth_cleaned")
avg_plot_growth_df$plot <- avg_plot_growth$plot

res_growth_wpmf<-wpmf(avg_plot_growth_mx,times,sigmethod="quick")
res_growth_wmf <- wmf(avg_plot_growth_mx,times)

regional_wavelet <- print(res_growth_wmf)

plotmag(res_growth_wmf)

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
tmax_mx <- cleandat(tmax_mx, times, clev = 1)$cdat
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
wateryear_mx <- cleandat(wateryear_mx, times, clev = 1)$cdat
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


coh_tv<-function(dat1,dat2,times,norm,sigmethod="none",nrand=1000,scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1)
{
  require(wsyn)
  #**error checking
  wsyn:::errcheck_times(times,"coh")
  wsyn:::errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"coh")
  
  wasvect1<-FALSE
  if (is.matrix(dat1) && dim(dat1)[1]>1)
  {
    wsyn:::errcheck_stdat(1:dim(dat1)[2],dat1,"coh")
  }else
  {
    if (!is.matrix(dat1)){wasvect1<-TRUE}
    wsyn:::errcheck_tsdat(1:length(dat1),dat1,"coh") 
    dat1<-matrix(dat1, nrow=1, ncol=length(dat1))
  }
  wasvect2<-FALSE
  if (is.matrix(dat2) && dim(dat2)[1]>1)
  {
    wsyn:::errcheck_stdat(1:dim(dat2)[2],dat2,"coh")
  }else
  {
    if (!is.matrix(dat2)){wasvect2<-TRUE}
    wsyn:::errcheck_tsdat(1:length(dat2),dat2,"coh")
    dat2<-matrix(dat2, nrow=1, ncol=length(dat2))
  }
  if (!isTRUE(all.equal(dim(dat1),dim(dat2))))
  {
    stop("Error in coh: dimensions of dat1 and dat2 must agree") 
  }
  
  if (!(norm %in% c("none","phase","powall","powind")))
  {
    stop("Error in coh: bad value for norm")
  }
  if (!(sigmethod %in% c("none","fftsurrog1","fftsurrog2","fftsurrog12",
                         "aaftsurrog1","aaftsurrog2","aaftsurrog12")))
  {
    stop("Error in coh: bad value for sigmethod")
  }  
  
  
  #**get wavelet transforms
  h<-wsyn:::warray(dat1,times,scale.min,scale.max.input,sigma,f0)
  W1<-h$wavarray
  timescales<-h$timescales
  h<-wsyn:::warray(dat2,times,scale.min,scale.max.input,sigma,f0)
  W2<-h$wavarray
  
  #**normalize
  W1<-wsyn:::normforcoh(W1,norm)
  W2<-wsyn:::normforcoh(W2,norm)
  
  #**compute coherence
  coher<-apply(X=W1*Conj(W2),FUN=mean,MARGIN=c(2,3),na.rm=T)
  
  #**for return
  wtopt<-list(scale.min=scale.min,scale.max.input=scale.max.input,
              sigma=sigma,f0=f0)
  
  #**now do the different cases for how significance is computed
  
  #*no significance requested by user - just return
  if (sigmethod=="none")
  {
    #prepare result  
    if (wasvect1){dat1<-as.vector(dat1)}
    if (wasvect2){dat2<-as.vector(dat2)}
    result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,norm=norm,wtopt=wtopt,
                 timescales=timescales,coher=coher,signif=NA,ranks=NA,bandp=NA)
    class(result)<-c("coh","coh_tv","list")
    return(result)    
  }
  
  #figure out what kind of surrogates to use
  if (sigmethod %in% c("fftsurrog1","fftsurrog2","fftsurrog12"))
  {
    surr<-"fft"
  }else
  {
    surr<-"aaft"
  }
  
  #surrogate the specified time series and take transforms and normalize
  f<-function(x,times,scale.min,scale.max.input,sigma,f0)
  {
    return(wsyn:::warray(x,times,scale.min,scale.max.input,sigma,f0)$wavarray)
  }
  sW1<-rep(list(W1),times=nrand)
  sW2<-rep(list(W2),times=nrand)
  if (sigmethod %in% c("fftsurrog1","fftsurrog12","aaftsurrog1","aaftsurrog12"))
  {
    sdat1<-wsyn::surrog(dat1,nrand,surrtype=surr,syncpres=TRUE)
    sW1<-lapply(FUN=f,X=sdat1,times=times,scale.min=scale.min,scale.max.input=scale.max.input,sigma=sigma,f0=f0) #take transforms
    sW1<-lapply(X=sW1,FUN=wsyn:::normforcoh,norm=norm) #normalize
  }
  if (sigmethod %in% c("fftsurrog2","fftsurrog12","aaftsurrog2","aaftsurrog12"))
  {
    sdat2<-wsyn::surrog(dat2,nrand,surrtype=surr,syncpres=TRUE)
    sW2<-lapply(FUN=f,X=sdat2,times=times,scale.min=scale.min,scale.max.input=scale.max.input,sigma=sigma,f0=f0) #take transforms
    sW2<-lapply(X=sW2,FUN=wsyn:::normforcoh,norm=norm) #normalize
  }
  
  #now compute coherences
  scoher<-array(complex(real=NA,imaginary=NA), dim=c(nrand,length(times),length(timescales)))
  for (counter in 1:nrand)
  {
    scoher[counter,,]<-apply(X=sW1[[counter]]*Conj(sW2[[counter]]),FUN=mean,MARGIN=c(2,3),na.rm=T)
  }
  
  gt<-matrix(NA,nrow(coher),ncol(coher))
  for (counter1 in 1:dim(coher)[1])
  {
    for (counter2 in 1:dim(coher)[2])
    {
      gt[counter1,counter2]<-sum(Mod(scoher[,counter1,counter2])<=Mod(coher[counter1,counter2]))/nrand
    }
  }
  
  #assemble the significance results
  signif<-list(sigmethod=surr,nsurrog=nrand,scoher=scoher,gt=gt)
  
  #prepare result  
  if (wasvect1){dat1<-as.vector(dat1)}
  if (wasvect2){dat2<-as.vector(dat2)}
  result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,
               norm=norm,wtopt=wtopt,timescales=timescales,coher=coher,
               signif=signif,ranks=NA,bandp=NA)
  class(result)<-c("coh_tv","list")
  return(result)    
}

