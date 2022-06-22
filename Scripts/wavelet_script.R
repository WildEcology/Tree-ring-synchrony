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
library("purrr")


# READ IN DATA

prismdat <- read.csv(here("data/rwi_prismdat.csv"))
anprismplot <- read.csv(here("data/whitebark_plot_prism_annual.csv"))
moprismplot <- read.csv(here("data/whitebark_plot_prism_monthly.csv"))

#### PREPARE TREE RING DATA ####

# subset 1900-2018
rwi_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot_id_needle)

# find plots and years with at least 5 trees 
rwi_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)

# subset only plots with at least 5 trees
rwi_00s_unfiltered_long <- left_join(rwi_5, rwi_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, value)

# create wide version of dataset
rwi_00s_unfiltered_wide <- rwi_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = value)

# removing any trees with missing data along the timeseries
rwi_00s_filtered_wide<- rwi_00s_unfiltered_wide[rowSums(is.na(rwi_00s_unfiltered_wide))==0,]

# create long formatted version of dataset
rwi_00s_filtered_long <- rwi_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")

#### PRECIPITATION QUANTILES ####
ppt_filter_an <- anprismplot %>%
  filter(variable == "ppt")

quant_det_an <- ppt_filter_an %>%
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


#### WITHIN-PLOT WAVELET ANALYSIS - PLOT 1 PRACTICE ####

#subset plot 1
rwi_00s_1 <- rwi_00s_filtered_wide %>%
  filter(plot == "site01")

# format matrix for analysis
rwi_00s_1 <- as.matrix(rwi_00s_1)
colnames(rwi_00s_1) <- NULL
rwi_00s_1 <- rwi_00s_1[, c(3:121)] 

# convert character matrix to numeric
rwi_00s_1 = as.data.frame(rwi_00s_1, stringsAsFactors = FALSE)
rwi_00s_1 = map_df(rwi_00s_1, as.numeric)
rwi_00s_1_mx <- as.matrix(rwi_00s_1)

# run wavelet coherence
times <- 1900:2018
rwi_00s_1_mx <- cleandat(rwi_00s_1_mx, times, 1)
res<-wpmf(rwi_00s_1_mx$cdat,times,sigmethod="quick")
plotmag(res)


#### WITHIN-PLOT WAVELET ANALYSIS - ALL PLOTS ####
{sites <- unique(rwi_00s_filtered_wide$plot)
times <- 1900:2018
pdf(file="./Figures/wpmf.pdf",width=11,height=11,onefile=T)
for (s in 1:length(sites)){
  temp <- rwi_00s_filtered_wide[which(rwi_00s_filtered_wide$plot == sites[s]),]
  temp <- temp[,-c(1,2)]
  temp <- as.matrix(temp)
  names(temp) <- NULL

  temp_clean <- cleandat(temp, times, 1)
  temp_res <- wpmf(temp_clean$cdat,times,sigmethod="quick")

  #png(filename=paste0("./wavelet_tree_",sites[s],".png"),width=1000,height=768)
  plotmag(temp_res)
  #dev.off()
}
dev.off()
}

## loop through plot to produce both WPMF's and WMF's for each plot to compare
## the role of magnitude
{sites <- unique(rwi_00s_filtered$plot)
  times <- 1900:2018
  pdf(file="./Figures/wpmf_wmf.pdf",width=22,height=11,onefile=T)
  for (s in 1:length(sites)){
    temp <- rwi_00s_filtered[which(rwi_00s_filtered$plot == sites[s]),]
    temp <- temp[,-c(1,2)]
    temp <- as.matrix(temp)
    names(temp) <- NULL
    
    temp_clean <- cleandat(temp, times, 1)
    temp_res <- wpmf(temp_clean$cdat,times,sigmethod="quick")
    temp_res2 <- wmf(temp_clean$cdat,times)
    
    par(mfcol=c(1,2))
    #png(filename=paste0("./Figures/wavelet_tree_",sites[s],".png"),width=2000,height=768)
    plotmag(temp_res)
    plotmag(temp_res2)
    #dev.off()
  }
  dev.off()
}


#### SYNCHRONY BY CHANCE ####

#n = number of trees
#nreps = number of randomizations to generate distribution
#q = quantiles to return


tree_per_plot <- rwi_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year")%>%
  group_by(plot, year)%>%
  summarize(numtree = n())
  
n <- unique(tree_per_plot$numtree)
psync.by.chance <- function(n, nreps=10000, prob=c(0.025, 0.975)){
  
  #generates sets of random phasors
  rndphas <- matrix(complex(modulus=1, argument=2*pi*runif(n*nreps)), n, nreps)
  #takes the average--this is analogous to the wavelet phasor mean field
  rndphas.avg <- Mod(apply(rndphas, FUN=mean, MARGIN=2))
  #spit out quantiles corresponding to prob
  return(quantile(rndphas.avg, prob))
  
}

temp_xx <- psync.by.chance(n)



plots <- unique(tree_per_plot$plot)
thresholds<- matrix(NA, ncol=3, nrow=length(plots))
colnames(thresholds)<- c("plot","lower","upper")

for(s in 1:length(plots)){
  current.plot<-plots[s]
  temp<-tree_per_plot %>% 
    filter(plot %in% current.plot) %>% 
    select(-plot)
  n<-temp$numtree[1]
  nreps <- 10000
  temp<-psync.by.chance(n=n, nreps=nreps, prob=c(0.01, 0.99))
  thresholds[s,1] <- current.plot
  thresholds[s,2:3] <- temp
}


#### WITHIN-PLOT PROPORTION SIGNIFICANT - PLOT 1 PRACTICE ####

rwi_00s_1 <- rwi_00s_filtered_wide %>%
  filter(plot == "site01")

# format matrix for analysis
rwi_00s_1 <- as.matrix(rwi_00s_1)
colnames(rwi_00s_1) <- NULL
rwi_00s_1 <- rwi_00s_1[, c(3:121)] 

# convert character matrix to numeric
rwi_00s_1 = as.data.frame(rwi_00s_1, stringsAsFactors = FALSE)
rwi_00s_1 = map_df(rwi_00s_1, as.numeric)
rwi_00s_1_mx <- as.matrix(rwi_00s_1)

# check if matrix is numeric
str(rwi_00s_1_mx)

# calculate wavelet
year <- 1900:2018
rwi_00s_1_mx <- cleandat(rwi_00s_1_mx, year, 1)
res<-wpmf(rwi_00s_1_mx$cdat,times,sigmethod="quick")
plotmag(res)

#extract raw values
M1 <- as.data.frame(res$values)
colnames(M1) <- res$timescales
M1 <- abs(M1)
#define thresholds for plot 1
sync <- thresholds[1,3]
sync <- as.numeric(sync)
async <- thresholds[1,2]
async <- as.numeric(async)

#separate ts <=4
M2 <- M1[,1:67]
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
M2events <- M2events %>%
  mutate(interval = case_when(ts >= 2 & ts <= 5 ~ "short",
                              ts > 5 & ts <= 10 ~ "medium",
                              ts > 10 & ts <= 20 ~ "long",
                              ts > 20 & ts <= 30 ~ "ancient"))
# filter for short term
M2events_s <- M2events %>%
  filter(interval == "short")

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
  filter(event == "synchronous")%>%
  group_by(year)%>%
  summarise(short_NS = n())

#the number of observations per year to divide by to get the proportion
prop_den_s <- M2events_s %>%
  group_by(year)%>%
  summarise(obs = n())

# proportion synchronous
prop_calc_Ss <- left_join(prop_den_s, SSs)
prop_sync_s <- prop_calc_Ss %>%
  group_by(year)%>%
  summarise(props = short_sync/obs)
prop_sync_s[is.na(prop_sync_s)] = 0
 
# proportion asynchronous
prop_calc_As <- left_join(prop_den_s, SAs)
prop_async_s <- prop_calc_As %>%
  group_by(year)%>%
  summarise(prop = short_async/obs) 

# proportion not significant
prop_calc_Ns <- left_join(prop_den_s, NSs)
prop_sync_s <- prop_calc_Ns %>%
  group_by(year)%>%
  summarise(prop = NS/obs)

# plot proportion short-term synchronous through time
short_sync_plot1 <- ggplot(prop_sync_s, aes(x=year, y=prop))+
  geom_point()+
  geom_smooth(method='loess', formula= y~x)


# repeat for other timescales
# filter for medium term
M2events_m <- M2events %>%
  filter(interval == "medium")
#significantly synchronous in the short term
SSm <- M2events_m %>%
  filter(event == "synchronous")%>%
  group_by(year)%>%
  summarise(med_sync = n())
#the number of observations per year to divide by to get the proportion
prop_den_m <- M2events_m %>%
  group_by(year)%>%
  summarise(obs = n())
# proportion synchronous
prop_calc_Sm <- left_join(prop_den_m, SSm)
prop_sync_m <- prop_calc_Sm %>%
  group_by(year)%>%
  summarise(propm = med_sync/obs)
prop_sync_m[is.na(prop_sync_m)] = 0
# plot proportion med-term synchronous through time
med_sync_plot1 <- ggplot(prop_sync_m, aes(x=year, y=prop))+
  geom_point()+
  geom_smooth(method='loess', formula= y~x)


# filter for long term
M2events_l <- M2events %>%
  filter(interval == "long")
#significantly synchronous in the long term
SSl <- M2events_l %>%
  filter(event == "synchronous")%>%
  group_by(year)%>%
  summarise(long_sync = n())
#the number of observations per year to divide by to get the proportion
prop_den_l <- M2events_l %>%
  group_by(year)%>%
  summarise(obs = n())
# proportion synchronous
prop_calc_Sl <- left_join(prop_den_l, SSl)
prop_sync_l <- prop_calc_Sl %>%
  group_by(year)%>%
  summarise(propl = long_sync/obs)
prop_sync_l[is.na(prop_sync_l)] = 0
# plot proportion med-term synchronous through time
long_sync_plot1 <- ggplot(prop_sync_l, aes(x=year, y=prop))+
  geom_point()+
  geom_smooth(method='loess', formula= y~x)

# filter for ancient term
M2events_a <- M2events %>%
  filter(interval == "ancient")
#significantly synchronous in the short term
SSa <- M2events_a %>%
  filter(event == "synchronous")%>%
  group_by(year)%>%
  summarise(ancient_sync = n())
#the number of observations per year to divide by to get the proportion
prop_den_a <- M2events_a %>%
  group_by(year)%>%
  summarise(obs = n())
# proportion synchronous
prop_calc_Sa <- left_join(prop_den_a, SSa)
prop_sync_a <- prop_calc_Sa %>%
  group_by(year)%>%
  summarise(propa = ancient_sync/obs)
prop_sync_a[is.na(prop_sync_a)] = 0
# plot proportion med-term synchronous through time
ancient_sync_plot1m <- ggplot(prop_sync_a, aes(x=year, y=prop))+
  geom_point()+
  geom_smooth(method='loess', formula= y~x)

# combine the proportions into one dataframe
s <- prop_sync_s$props
m <- prop_sync_m$propm
l <- prop_sync_l$propl
a <- prop_sync_a$propa

plot1_synchrony <- cbind(year, s, m, l, a)
plot1_synchrony <- as.data.frame(plot1_synchrony)

plot1_synchrony <- plot1_synchrony %>%
  pivot_longer(cols = s:a, names_to = "timescale_interval", values_to = "prop")

plot1_prop_sync <- ggplot(plot1_synchrony, aes(x=year, y=prop, col=timescale_interval))+
  geom_smooth(method='loess', formula= y~x, se = FALSE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Synchronous")+
  scale_colour_discrete(breaks=c("s","m","l","a"),
                        name="Timescale\nInterval",
                        labels=c("short = 2-5","medium = 5-10","long = 10-20","ancient = 20-30"))



#### WITHIN-PLOT PROPORTION SIGNIFICANT - ALL PLOTS ####

temp_xx <- psync.by.chance(n)
plots <- unique(tree_per_plot$plot)
thresholds<- matrix(NA, ncol=3, nrow=length(plots))
colnames(thresholds)<- c("plot","lower","upper")


proportions_final <- data.frame()

for(s in 1:length(plots)){
  current.plot<-plots[s]
  temp<-tree_per_plot %>% 
    filter(plot %in% current.plot) %>% 
    select(-plot)
  n<-temp$numtree[1]
  nreps <- 10000
  temp<-psync.by.chance(n=n, nreps=nreps, prob=c(0.005, 0.995))
  thresholds[s,1] <- current.plot
  thresholds[s,2:3] <- temp
  
  rwi_00s_plot <- rwi_00s_filtered_wide %>%
    filter(plot == current.plot)
  
  # format matrix for analysis
  rwi_00s_plot <- as.matrix(rwi_00s_plot)
  colnames(rwi_00s_plot) <- NULL
  rwi_00s_plot <- rwi_00s_plot[, c(3:121)] 
  
  # convert character matrix to numeric
  rwi_00s_plot = as.data.frame(rwi_00s_plot, stringsAsFactors = FALSE)
  rwi_00s_plot = map_df(rwi_00s_plot, as.numeric)
  rwi_00s_plot_mx <- as.matrix(rwi_00s_plot)
  
  # calculate wavelet
  year <- 1900:2018
  rwi_00s_plot_mx <- cleandat(rwi_00s_plot_mx, year, 1)
  res<-wpmf(rwi_00s_plot_mx$cdat,year,sigmethod="quick")
  
  #extract raw values
  M1 <- as.data.frame(res$values)
  colnames(M1) <- res$timescales
  #fix the imaginary #s
  M1 <- abs(M1)
  # define thresholds
  sync <- temp[2]
  sync <- as.numeric(sync)
  async <- temp[1]
  async <- as.numeric(async)
  
  M2 <- M1[,1:67]
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
  
  # make dataframe to save results
  plot_current_results <- data.frame()
  
  
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
    plot1_temp$location <- current.plot
    
    ### RBIND FOR ROWS
    plot_current_results <- rbind(plot_current_results, plot1_temp)
    
  }
  
  proportions_final <- rbind(proportions_final, plot_current_results)
  
}

proportions_final[is.na(proportions_final)] <- 0

proportions_final <- proportions_final %>%
  rename(plot = "location")

## VISUALIZE WITHIN-PLOT PROPORTION SIGNIFICANT ## 

prop_sync_within <- ggplot(proportions_final, aes(x=year, y=synch, color=interval))+
  geom_smooth(method='gam', span = 2.75,  se = TRUE)+
  #geom_smooth(method='lm', se = TRUE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Synchronous")+
  ggtitle("Within Plot")+
  scale_fill_discrete(breaks=c("short","medium","long","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-5","medium = 5-10","long = 10-20","ancient = 20-30"))


prop_async_within <- ggplot(proportions_final, aes(x=year, y=asynch, color=interval))+
  geom_smooth(method='gam', span = 2.75,  se = TRUE)+
  #geom_smooth(method='lm', se = TRUE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Synchronous")+
  ggtitle("Within Plot")+
  scale_fill_discrete(breaks=c("short","medium","long","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-5","medium = 5-10","long = 10-20","ancient = 20-30"))

prop_NS_within <- ggplot(proportions_final, aes(x=year, y=ns, color=interval))+
  geom_smooth(method='gam', span = 2.75,  se = TRUE)+
  #geom_smooth(method='lm', se = TRUE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Synchronous")+
  ggtitle("Within Plot")+
  scale_fill_discrete(breaks=c("short","medium","long","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-5","medium = 5-10","long = 10-20","ancient = 20-30"))


#### WITHIN-PLOT PROPORTION STATISTICS ####
library(splines)

## practice : short term timescale band ##
proportions_final_s <- proportions_final %>%
  filter(interval == "short")%>%
  filter(plot == "site01")

# fit spline glms w/ binomal distributions & test different number of knots
within_spline_9 <- glm(synch ~ ns(year,9), data = proportions_final_s, family=binomial(link = "logit"))
within_spline_8 <- glm(synch ~ ns(year,8), data = proportions_final_s, family=binomial(link = "logit"))
within_spline_7 <- glm(synch ~ ns(year,7), data = proportions_final_s, family=binomial(link = "logit"))
within_spline_6 <-  glm(synch ~ ns(year,6), data = proportions_final_s, family=binomial(link = "logit"))
within_spline_5 <-  glm(synch ~ ns(year,5), data = proportions_final_s, family=binomial(link = "logit"))
within_spline_4 <- glm(synch ~ ns(year,4), data = proportions_final_s, family=binomial(link = "logit"))
within_spline_3 <- glm(synch ~ ns(year,3), data = proportions_final_s, family=binomial(link = "logit"))
within_spline_2 <- glm(synch ~ ns(year,2), data = proportions_final_s, family=binomial(link = "logit"))
within_lm <- glm(synch~ year, data = proportions_final_s,family = binomial(link = "logit"))

within_spline <- list(within_spline_2,within_spline_3,within_spline_4)

## AIC FUNCTION ##
#  This function will create a full model selection table based on AICc.
#  Inputs:
#  X is the AIC table produced by R by using AIC(model1,model2, ...)
#  m is the sample size

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

rawaic <- AIC(within_spline_2, within_spline_3)
nR <- dim(proportions_final_s)[1]
aic_output <- aictable(rawaic,nR)

plots <- unique(proportions_final$plot)
knots <- seq(1,10,1)
spline_models <- data.frame()


for(s in 1:length(plots)){
  current.plot<-plots[s]
  prop_plot<-proportions_final %>%
    filter(plot %in% current.plot) %>%
    select(-plot)
  
  for (xx in 1:length(unique(prop_plot$interval))){
  current.interval <- unique(prop_plot$interval)[xx]
  prop_interval <- prop_plot %>%
    filter(interval == current.interval)

    for(i in 1:length(knots)){
      current.knot <- knots[i]
      model <- glm(synch ~ ns(year,current.knot), data = prop_interval, family=binomial(link = "logit"))
      aic <- AIC(model)
      df <- anova(model)[2,1]
      
      dframe <- data.frame(df = anova(model)[2,1], AIC = AIC(model))
      
      spline_models<- rbind(spline_models,df)
    }
  }
}



# for (s in 1:length(unique(proportions_final$plot))){
# current.plot <- unique(proportions_final$plot)[s]
# prop_plot<- proportions_final %>%
#   filter(plot == current.plot)
#   for(xx in unique(prop_plot$interval)){









for(i in 1:length(knots)){
  current.knot <- knots[i]
  model <- glm(synch ~ ns(year,current.knot), data = proportions_final_s, family=binomial(link = "logit"))
  spline_models[[i]] <-  model
}


rawaic<- as.data.frame(do.call(rbind,purrr::map(spline_models, "aic")))

nR <- dim(proportions_final_s)[1]
aic_output <- aictable(rawaic, nR)


## visually match the model estimates to raw data ##
library(ggeffects)

# according to AIC output 8 knots balances model fit best
synchpredict <- ggpredict(within_spline_8, terms = "year")

ggplot()+
  geom_line(synchpredict, aes(x, predicted))+
  geom_ribbon(aes(x, predicted, ymin=conf.low, ymax=conf.high), alpha = 0.1, fill = "red")

ggplot(synchpredict, aes(x=x, y=predicted))+
  geom_line(color = "red")+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.3, fill = "red")+
  geom_point(data = proportions_final_s, aes(x=year, y=synch), alpha = 0.5, shape = 1)+
  xlab("Year")+
  ylab("Synchronous events (annual proportion)")+
  theme_bw()

## ALL PLOTS - ALL BANDS ##




#### ACROSS PLOTS - WAVELET SPATIAL COHERENCE ####
avg_plot <- rwi_00s_filtered %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

avg_plot_w <- avg_plot %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")
  
# format matrix for analysis
avg_plot_w <- as.matrix(avg_plot_w)
colnames(avg_plot_w) <- NULL
avg_plot_w <- avg_plot_w[, c(2:120)] 

# convert character matrix to numeric
avg_plot_w = as.data.frame(avg_plot_w, stringsAsFactors = FALSE)
avg_plot_w = map_df(avg_plot_w, as.numeric)
avg_plot_w_mx <- as.matrix(avg_plot_w)

times <- 1900:2018
avg_plot_w_mx <- cleandat(avg_plot_w_mx, times, 1)$cdat
res<-wpmf(avg_plot_w_mx,times,sigmethod="quick")
plotmag(res)



#### PLOT VARIABILITY & SYNCHONRY ####
rwi_00s_filtered_long <- rwi_00s_filtered %>%
  pivot_longer(3:121, names_to = "year", values_to ="rwi")%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

raw_growth <- ggplot(rwi_00s_filtered_long, aes(x = year, y = avg_growth, group = plot, col=plot))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", angle = 90, hjust = .5, face = "plain"))

calcSE <- function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

plot_var <- rwi_00s_filtered %>%
  pivot_longer(3:121, names_to = "year", values_to ="rwi")%>%
  group_by(plot)%>% 
  summarize(avg_growth = mean(rwi), se_growth = calcSE(rwi), CI=1.96*calcSE(rwi))%>%
  mutate(plot_icov = (avg_growth/se_growth))


# calculate avg synchrony for each timescale interval for each plot
plots <- unique(rwi_00s_filtered$plot)
year <- 1900:2018

plot_current_results <- NULL

for(s in 1:length(plots)){
  current.plot<-plots[s]
  temp<-rwi_00s_filtered %>% 
    filter(plot %in% current.plot) %>% 
    select(-plot)
  rwi_00s_1 <- rwi_00s_filtered %>%
    filter(plot == current.plot)
  # format matrix for analysis
  rwi_00s_1 <- as.matrix(rwi_00s_1)
  colnames(rwi_00s_1) <- NULL
  rwi_00s_1 <- rwi_00s_1[, c(3:121)] 
  # convert character matrix to numeric
  rwi_00s_1 = as.data.frame(rwi_00s_1, stringsAsFactors = FALSE)
  rwi_00s_1 = map_df(rwi_00s_1, as.numeric)
  rwi_00s_1_mx <- as.matrix(rwi_00s_1)
  # calculate wavelet
  rwi_00s_1_mx <- cleandat(rwi_00s_1_mx, year, 1)
  res<-wpmf(rwi_00s_1_mx$cdat,year,sigmethod="quick")
  #extract raw values
  M1 <- as.data.frame(res$values)
  colnames(M1) <- res$timescales
  #fix the imaginary #s
  M1 <- abs(M1)
  M1$site <- current.plot

  plot_current_results <- rbind(plot_current_results, M1)
}


avg_sync_interval <- plot_current_results %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
  avg_sync_interval$ts <- as.numeric(avg_sync_interval$ts)

avg_sync_interval <- avg_sync_interval %>%
  mutate(interval = case_when(ts >= 2 & ts <= 4 ~ "short",
                              ts > 4 & ts <= 8 ~ "medium",
                              ts > 8 & ts <= 16 ~ "long",
                              ts > 16 & ts <= 32 ~ "xlong",
                              ts >= 32 ~ "ancient"))
avg_sync_interval <- na.omit(avg_sync_interval)

avg_sync_interval <- avg_sync_interval%>%
  group_by(site,interval)%>%
  summarise(avg_sync = mean(values))

plotVar_avgSync <- right_join(avg_sync_interval, plot_var, by = c("site" = "plot"))

pVAR<- ggplot(data = plotVar_avgSync, aes(x=plot_icov,y=avg_sync, group = interval, col=interval))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", formula = y~x, se=FALSE)+
  # scale_x_continuous(limits=c(0,.5))+
  # scale_y_continuous(limits=c(0,.5))+
  labs(x="Plot Level Variation (mean/se)", y="Average Degree of Synchrony")+
  theme_bw()+
  scale_color_brewer(palette = "Paired",
                     name = "Timescale Interval",
                     labels = c("Short", "Medium", "Long", "Xlong", "Ancient"))

m4 <- lm(avg_sync ~ plot_icov + interval, data = plotVar_avgSync)
m5 <- lm(avg_sync ~ plot_icov*interval, data = plotVar_avgSync)
summary(m4)
summary(m5)
anova(m4)
anova(m5)

interaction <- visreg::visreg(m5, xvar="plot_icov", by = "interval", overlay=T, scale="response")
no_interaction <- visreg::visreg(m4, xvar="plot_icov", by = "interval", overlay=T, scale="response")


#### Wavelet 1970 - 2018 ####

{sites <- unique(rwi_70s_filtered$plot)
times <- 1970:2018
pdf(file="./Figures/wpmf_wmf70.pdf",width=22,height=11,onefile=T)
for (s in 1:length(sites)){
  temp <- rwi_70s_filtered[which(rwi_70s_filtered$plot == sites[s]),]
  temp <- temp[,-c(1,2)]
  temp <- as.matrix(temp)
  names(temp) <- NULL
  
  temp_clean <- cleandat(temp, times, 1)
  temp_res <- wpmf(temp_clean$cdat,times,sigmethod="quick")
  temp_res2 <- wmf(temp_clean$cdat,times)
  
  par(mfcol=c(1,2))
  #png(filename=paste0("./Figures/wavelet_tree_",sites[s],".png"),width=2000,height=768)
  plotmag(temp_res)
  plotmag(temp_res2)
  #dev.off()
}
dev.off()
}

#### Wavelet 1800 - 2018 ####
rwi_1800s <- prismdat %>%
  filter(year >= 1800)%>%
  drop_na(value)

# find plots and years with at least 5 trees 
test <- rwi_1800s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)

# subsetting only plots with at least 5 trees
test2 <- left_join(test, rwi_1800s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, value)%>%
  group_by(plot, year)%>%
  pivot_wider(names_from = year, values_from = value) 

# removing any trees with missing data along the timeseries
rwi_1800s_filtered<- test2[rowSums(is.na(test2))==0,]



{sites <- unique(rwi_1800s_filtered$plot)
  times <- 1800:2018
  pdf(file="./Figures/wpmf_wmf1800.pdf",width=22,height=11,onefile=T)
  for (s in 1:length(sites)){
    temp <- rwi_1800s_filtered[which(rwi_1800s_filtered$plot == sites[s]),]
    temp <- temp[,-c(1,2)]
    temp <- as.matrix(temp)
    names(temp) <- NULL
    
    temp_clean <- cleandat(temp, times, 1)
    temp_res <- wpmf(temp_clean$cdat,times,sigmethod="quick")
    #temp_res2 <- wmf(temp_clean$cdat,times)
    
    #par(mfcol=c(1,2))
    #png(filename=paste0("./Figures/wavelet_tree_",sites[s],".png"),width=2000,height=768)
    wav<- Mod(get_values(temp_res))
    timescales <- get_timescales(temp_res)
    signif <- get_signif(temp_res)
    sigthresh <- c(0.025, 0.975)
    plotmag(temp_res, sigthresh = sigthresh)
    graphics::contour(x=times, y=log2(timescales), z=wav, levels=sigthresh[1], drawlabels=F,lty = 1, lwd=2,xaxs="i",xaxt="n",yaxt="n",xaxp=c(0,1,5),
                      las = 1,frame=F, add = TRUE)
    graphics::contour(x=times, y=log2(timescales), z=wav, levels=sigthresh[2], drawlabels=F,lty = 4, lwd=2,xaxs="i",xaxt="n",yaxt="n",xaxp=c(0,1,5),
                      las = 1,frame=F, add = TRUE)
    #plotmag(temp_res2)
    #dev.off()
  }
  dev.off()
}



#### PROPORTION SIGNIFICANT -- ACROSS PLOT ####

# Establish Thresholds
trees_per_plot <- rwi_00s_filtered %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")%>%
  group_by(plot)%>%
  summarize(numtree = (n()/119))
avg_trees_per_plot<-trees_per_plot%>%
  summarize(numtree = round(mean(numtree)))

n <- unique(avg_trees_per_plot$numtree)
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

# Across Plot Wavelet
avg_plot <- rwi_00s_filtered %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

avg_plot_w <- avg_plot %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")

# format matrix for analysis
avg_plot_w <- as.matrix(avg_plot_w)
colnames(avg_plot_w) <- NULL
avg_plot_w <- avg_plot_w[, c(2:120)] 

# convert character matrix to numeric
avg_plot_w = as.data.frame(avg_plot_w, stringsAsFactors = FALSE)
avg_plot_w = map_df(avg_plot_w, as.numeric)
avg_plot_w_mx <- as.matrix(avg_plot_w)

times <- 1900:2018
avg_plot_w_mx <- cleandat(avg_plot_w_mx, times, 1)$cdat
res<-wpmf(avg_plot_w_mx,times,sigmethod="quick")
plotmag(res)

# extract raw values
M1 <- as.data.frame(res$values)
colnames(M1) <- res$timescales
M1 <- abs(M1)
#define thresholds for plot 1
sync <- thresholds[1,1]
sync <- as.numeric(sync)
async <- thresholds[1,2]
async <- as.numeric(async)

#separate ts <=4
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
M2events <- M2events %>%
  mutate(interval = case_when(ts >= 2 & ts <= 5 ~ "short",
                              ts > 5 & ts <= 10 ~ "medium",
                              ts > 10 & ts <= 20 ~ "long",
                              ts > 20 & ts <= 30 ~ "xlong"))


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

## Visualize significance proportions ## 

prop_sync <- ggplot(prop_sync_final, aes(x=year, y=synch, color=interval))+
  #geom_smooth(method='GAM', span = 2.75,  se = TRUE)+
  #geom_point()+
  #geom_smooth(method='lm', se = TRUE)+
  #geom_smooth(method="gam")+
  geom_line()+
  theme_bw()+
  ggtitle("Across Plot")+
  xlab("Year")+
  ylab("Proportion Synchronous")+
  scale_fill_discrete(breaks=c("short","medium","long","xlong"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-5","medium = 5-10","long = 10-20","ancient = 20-30"))


prop_async <- ggplot(prop_sync_final, aes(x=year, y=asynch, color=interval))+
  geom_smooth(method='lm', se = TRUE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Asynchronous")+
  scale_fill_discrete(breaks=c("short","medium","long","xlong","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-4","medium = 4-8","long = 8-16","xlong = 16-32","ancient = 32+"))

prop_NS <- ggplot(prop_sync_final, aes(x=year, y=ns, color=interval))+
  geom_smooth(method='lm', se = TRUE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Not Significant")+
  scale_fill_discrete(breaks=c("short","medium","long","xlong","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-4","medium = 4-8","long = 8-16","xlong = 16-32","ancient = 32+"))


# run stats

prop_sync_final_long <- prop_sync_final %>%
  pivot_longer(synch:ns, names_to = "synchrony", values_to = "proportion")


prop_model <- glm(proportion ~ interval, family = binomial(link = "logit"), data = prop_sync_final_long)
summary(prop_model)
