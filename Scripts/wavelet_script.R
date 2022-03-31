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


# read in data

prismdat <- read.csv(here("data/rwi_prismdat.csv"))
anprismplot <- read.csv(here("data/whitebark_plot_prism_annual.csv"))
moprismplot <- read.csv(here("data/whitebark_plot_prism_monthly.csv"))

#### PREPARE DATA ####
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


#### WAVELET ANALYSIS FOR TREES PER PLOT ####

#subset plot 1
rwi_00s_1 <- rwi_00s_filtered %>%
  filter(plot == "site01")

# format matrix for analysis
rwi_00s_1 <- as.matrix(rwi_00s_1)
colnames(rwi_00s_1) <- NULL
rwi_00s_1 <- rwi_00s_1[, c(3:121)] 

# convert character matrix to numeric
rwi_00s_1 = as.data.frame(rwi_00s_1, stringsAsFactors = FALSE)
rwi_00s_1 = map_df(rwi_00s_1, as.numeric)
rwi_00s_1_mx <- as.matrix(rwi_00s_1)


# NOT SURE IF THIS IS NECESSARY #
# cmat<-cor(t(rwi_00s_1))
# diag(cmat)<-NA
# cmat<-as.vector(cmat)
# cmat<-cmat[!is.na(cmat)]
# hist(cmat,breaks = "Sturges",xlab="Pearson correlation",ylab="Count")


times <- 1900:2018
rwi_00s_1_mx <- cleandat(rwi_00s_1_mx, times, 1)
res<-wpmf(rwi_00s_1_mx$cdat,times,sigmethod="quick")
plotmag(res)


# ## loop through plot to produce WPMF's for each plot
# {sites <- unique(rwi_00s_filtered$plot)
# times <- 1900:2018
# pdf(file="./Figures/wpmf.pdf",width=11,height=11,onefile=T)
# for (s in 1:length(sites)){
#   temp <- rwi_00s_filtered[which(rwi_00s_filtered$plot == sites[s]),]
#   temp <- temp[,-c(1,2)]
#   temp <- as.matrix(temp)
#   names(temp) <- NULL
#   
#   temp_clean <- cleandat(temp, times, 1)
#   temp_res <- wpmf(temp_clean$cdat,times,sigmethod="quick")
#   
#   #png(filename=paste0("./wavelet_tree_",sites[s],".png"),width=1000,height=768)
#   plotmag(temp_res)
#   #dev.off()
# }
# dev.off()
# }

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


## calculate quantiles of phase synchrony expected by chance

#n = number of trees
#nreps = number of randomizations to generate distribution
#q = quantiles to return


tree_per_plot <- rwi_00s_filtered %>%
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
hist(rndphas.avg)


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
thresholds$upper <- as.numeric(thresholds$upper)
thresholds$lower <- as.numeric(thresholds$lower)

thresholds99 <- thresholds %>%
  summarise(meanuppper = mean(upper), meanlower=mean(lower))



#### PROPORTION SIGNIFICANT -- PLOT 1 PRACTICE ####

rwi_00s_1 <- rwi_00s_filtered %>%
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
  mutate(interval = case_when(ts >= 2 & ts <= 4 ~ "short",
                              ts > 4 & ts <= 8 ~ "medium",
                              ts > 8 & ts <= 16 ~ "long",
                              ts > 16 & ts <= 32 ~ "xlong",
                              ts >= 32 ~ "ancient"))
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

# filter for xlong term
M2events_xl <- M2events %>%
  filter(interval == "xlong")
#significantly synchronous in the short term
SSxl <- M2events_xl %>%
  filter(event == "synchronous")%>%
  group_by(year)%>%
  summarise(xl_sync = n())
#the number of observations per year to divide by to get the proportion
prop_den_xl <- M2events_xl %>%
  group_by(year)%>%
  summarise(obs = n())
# proportion synchronous
prop_calc_Sxl <- left_join(prop_den_xl, SSxl)
prop_sync_xl <- prop_calc_Sxl %>%
  group_by(year)%>%
  summarise(propxl = xl_sync/obs)
prop_sync_xl[is.na(prop_sync_xl)] = 0
# plot proportion med-term synchronous through time
xl_sync_plot1m <- ggplot(prop_sync_xl, aes(x=year, y=prop))+
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
xl <- prop_sync_xl$propxl
a <- prop_sync_a$propa

plot1_synchrony <- cbind(year, s, m, l, xl, a)
plot1_synchrony <- as.data.frame(plot1_synchrony)

plot1_synchrony <- plot1_synchrony %>%
  pivot_longer(cols = s:a, names_to = "timescale_interval", values_to = "prop")

plot1_prop_sync <- ggplot(plot1_synchrony, aes(x=year, y=prop, col=timescale_interval))+
  geom_smooth(method='loess', formula= y~x, se = FALSE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Synchronous")+
  scale_colour_discrete(breaks=c("s","m","l","xl","a"),
                        name="Timescale\nInterval",
                        labels=c("short = 2-4","medium = 4-8","long = 8-16","xlong = 16-32","ancient = 32+"))



#### LOOP TO CALCULATE SIGNIFICANT EVENTS ACROSS PLOTS FOR EACH TIMESCALE INTERVAL ####
# reload data
prismdat <- read.csv(here("data/rwi_prismdat.csv"))
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

## calculate quantiles of phase synchrony expected by chance
#n = number of trees
#nreps = number of randomizations to generate distribution
#q = quantiles to return

tree_per_plot <- rwi_00s_filtered %>%
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
  year <- 1900:2018
  rwi_00s_1_mx <- cleandat(rwi_00s_1_mx, year, 1)
  res<-wpmf(rwi_00s_1_mx$cdat,year,sigmethod="quick")
  
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
  M2events <- M2events %>%
    mutate(interval = case_when(ts >= 2 & ts <= 4 ~ "short",
                                ts > 4 & ts <= 8 ~ "medium",
                                ts > 8 & ts <= 16 ~ "long",
                                ts > 16 & ts <= 32 ~ "xlong",
                                ts >= 32 ~ "ancient"))
  
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

prop_sync <- ggplot(proportions_final, aes(x=year, y=synch, color=interval))+
  geom_smooth(method='lm', span = 2.75,  se = TRUE)+
  #geom_smooth(method='lm', se = TRUE)+
  #geom_smooth()+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Synchronous")+
  scale_fill_discrete(breaks=c("short","medium","long","xlong","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-4","medium = 4-8","long = 8-16","xlong = 16-32","ancient = 32+"))


prop_async <- ggplot(proportions_final, aes(x=year, y=asynch, color=interval))+
  geom_smooth(method='lm', se = TRUE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Asynchronous")+
  scale_fill_discrete(breaks=c("short","medium","long","xlong","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-4","medium = 4-8","long = 8-16","xlong = 16-32","ancient = 32+"))

prop_NS <- ggplot(proportions_final, aes(x=year, y=ns, color=interval))+
  geom_smooth(method='lm', se = TRUE)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion Not Significant")+
  scale_fill_discrete(breaks=c("short","medium","long","xlong","ancient"),
                      name="Timescale\nInterval",
                      labels=c("short = 2-4","medium = 4-8","long = 8-16","xlong = 16-32","ancient = 32+"))




# Create wavelet to calculate spatial coherence across plots

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


#### WAVELET COHERENCE WITH ENVIRONMENTAL VARIABLES ####

# create matrices for environmental variables
test2 <- left_join(test, rwi_00s, by = c("year", "plot"))

ppt <- test2 %>%
  select(year, plot, ppt)%>%
  group_by(plot,year)%>%
  summarize(mean_ppt = mean(ppt))%>%
  pivot_wider(names_from = year, values_from = mean_ppt, id_cols = plot)

tmax <- test2 %>%
  select(year, plot, tmax)%>%
  group_by(plot,year)%>%
  summarize(mean_tmax = mean(tmax))%>%
  pivot_wider(names_from = year, values_from = mean_tmax, id_cols = plot)

ppt <- na.omit(ppt)
tmax <- na.omit(tmax)

# format matrix for analysis
ppt <- as.matrix(ppt)
colnames(ppt) <- NULL
ppt <- ppt[, c(2:120)] 

tmax <- as.matrix(tmax)
colnames(tmax) <- NULL
tmax <- tmax[, c(2:120)] 

# convert character matrix to numeric
ppt = as.data.frame(ppt, stringsAsFactors = FALSE)
ppt = map_df(ppt, as.numeric)
ppt_mx <- as.matrix(ppt)

tmax = as.data.frame(tmax, stringsAsFactors = FALSE)
tmax = map_df(tmax, as.numeric)
tmax_mx <- as.matrix(tmax)

# prepare data for coherence
tmax_mx <- cleandat(tmax_mx, times,1)$cdat
ppt_mx <- cleandat(ppt_mx, times,1)$cdat
times <- 1:119

# run coherence separately for both variables
res_ppt <- coh(dat1 = ppt_mx, dat2=avg_plot_w_mx, times=times,norm="powall",
               sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)
plotmag(res_ppt)

res_tmax <- coh(dat1 = tmax_mx, dat2=avg_plot_w_mx, times=times,norm="powall",
               sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)
plotmag(res_tmax)

# from plot - investigate significance for timescales 2-5 yrs & 10-20 yrs
res_ppt<-bandtest(res_ppt,c(2,5))
res_ppt<-bandtest(res_ppt,c(10,20))
get_bandp(res_ppt)

res_tmax<-bandtest(res_tmax,c(2,5))
res_tmax<-bandtest(res_tmax,c(10,20))
get_bandp(res_tmax)

# WAVELET LINEAR MODEL
# prepare data
dat<-list(rwi=avg_plot_w_mx,ppt=ppt_mx,tmax=tmax_mx)

# create model
wlm_all<-wlm(dat,times,resp=1,pred=2:3,norm="powall",scale.max.input=28)
# run model for each environmental variable
wlm_all_drop1<-wlmtest(wlm_all,drop="ppt",sigmethod="fft",nrand=1000)
wlm_all_drop2<-wlmtest(wlm_all,drop="tmax",sigmethod="fft",nrand=1000)

#specify timescales to test significance
short <- c(2,5)
long <- c(10,20)

# significance of both variables at each timescale
wlm_all_drop1<-bandtest(wlm_all_drop1,short)
wlm_all_drop1<-bandtest(wlm_all_drop1,long)
wlm_all_drop2<-bandtest(wlm_all_drop2,short)
wlm_all_drop2<-bandtest(wlm_all_drop2,long)
get_bandp(wlm_all_drop1)
get_bandp(wlm_all_drop2)




