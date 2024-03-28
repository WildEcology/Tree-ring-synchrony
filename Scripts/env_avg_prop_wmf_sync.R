
# source the data from cleaning code
source(here::here("scripts/cleaning_code.R"))

# produce wmf's for ppt and tmin
tmin_wmf <- plotmag(res_tmin_wmf)
ppt_wmf <- plotmag(res_ppt_wmf)


#### Average Sync ####
# PPT 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_ppt <- as.data.frame(res_ppt_wmf$values)
colnames(M_ppt) <- res_ppt_wmf$timescales
#fix the imaginary #s
M_ppt <- abs(M_ppt)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_ppt <- M_ppt[,1:67]
year <- 1900:2018
M_ppt$year <- year
# classify sync, async and ns
M_ppt<- M_ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
M_ppt <- na.omit(M_ppt)
M_pptevents <- M_ppt %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents <- M_pptevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))


avg_sync_ppt <- M_pptevents %>%
  group_by(year, interval) %>%
  summarise(avg_sync = mean(values))
avg_sync_ppt$driver <- "ppt"

# TMIN
# avg sync per timescale band through time
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_tmin <- as.data.frame(res_tmin_wmf$values)
colnames(M_tmin) <- res_tmin_wmf$timescales
#fix the imaginary #s
M_tmin <- abs(M_tmin)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_tmin <- M_tmin[,1:67]
year <- 1900:2018
M_tmin$year <- year
# classify sync, async and ns
M_tmin<- M_tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
M_tmin <- na.omit(M_tmin)
M_tminevents <- M_tmin %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents <- M_tminevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))


avg_sync_tmin <- M_tminevents %>%
  group_by(year, interval) %>%
  summarise(avg_sync = mean(values))
avg_sync_tmin$driver <- "tmin"

# avg sync plot
# rbind data
avg_env_sync <- rbind(avg_sync_ppt, avg_sync_tmin)
avg_env_sync <- na.omit(avg_env_sync)

avg_env_sync$interval <- factor(avg_env_sync$interval, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

regional_avg_env <- ggplot(data = avg_env_sync, aes(x = year, y = avg_sync, group = interval, color = interval)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()+
  facet_wrap(~driver)+
  scale_color_brewer(palette="Spectral", type = "seq")+
  scale_x_continuous(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average synchrony")+
  xlab("Year")

#### plotting avg env sync vs avg tv coh ####
avg_env_sync <- avg_env_sync %>%
  rename("band" = interval, "times" = year)
avg_env_sync$times <- as.character(avg_env_sync$times)

#combine datasets
avg_sync_avg_coh_env <- right_join(avg.tv.coh, avg_env_sync)
avg_sync_avg_coh_env$times <- as.numeric(avg_sync_avg_coh_env$times)
avg_sync_avg_coh_env$driver <- factor(avg_sync_avg_coh_env$driver, levels=c('ppt', 'tmin'))
avg_sync_avg_coh_env$band<- factor(avg_sync_avg_coh_env$band, levels=c("biennial","multiannual","decadal", "multidecadal"))
avgsynccohenv<- ggplot() +
  geom_point(data = avg_sync_avg_coh_env, aes(x = avg_coh, y = avg_sync, group = driver, color = driver), size = 3) +
  facet_wrap(~ band)+
  theme_bw()+
  scale_color_manual(values = c("blue","red"))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Coherence")+
  xlab("Average Synchrony")


#### Proportion Sync ####
#PPT
# calculate signficance thresholds 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_ppt <- as.data.frame(res_ppt_wpmf$values)
colnames(M_ppt) <- res_ppt_wpmf$timescales
#fix the imaginary #s
M_ppt <- abs(M_ppt)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_ppt <- M_ppt[,1:66]
year <- 1900:2018
M_ppt$year <- year
# classify sync, async and ns
M_ppt<- M_ppt %>%
  pivot_longer(1:66, names_to = "ts", values_to = "values")
M_ppt <- na.omit(M_ppt)
M_pptevents <- M_ppt %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents <- M_pptevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))



prop_ppt_sync <- data.frame(year = NA, synch = NA, asynch = NA, ns = NA, interval = NA, obs = NA)

for (xx in 1:length(unique(M_pptevents$interval))) {
  
  current <- unique(M_pptevents$interval)[xx]
  M_pptevents_s <- M_pptevents %>%
    filter(interval == current)
  
  #significantly synchronous in the short term
  SSs <- M_pptevents_s %>%
    filter(event == "synchronous")%>%
    group_by(year)%>%
    summarise(short_sync = n())
  
  #significantly asynchronous in the short term
  SAs <- M_pptevents_s %>%
    filter(event == "asynchronous")%>%
    group_by(year)%>%
    summarise(short_async = n())
  
  #not significant in the short term
  NSs <- M_pptevents_s %>%
    filter(event == "NS")%>%
    group_by(year)%>%
    summarise(short_NS = n())
  
  #the number of observations per year to divide by to get the proportion
  prop_den_s <- M_pptevents_s %>%
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
    full_join(prop_ns, by="year") %>%
    full_join(prop_den_s, by="year")
  
  plot1_temp$interval <- current
  
  prop_ppt_sync <- rbind(prop_ppt_sync, plot1_temp)
}

prop_ppt_sync <- prop_ppt_sync[2:381,]

prop_ppt_sync[is.na(prop_ppt_sync)] <- 0


# TMIN
# calculate signficance thresholds 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_tmin <- as.data.frame(res_tmin_wpmf$values)
colnames(M_tmin) <- res_tmin_wpmf$timescales
#fix the imaginary #s
M_tmin <- abs(M_tmin)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_tmin <- M_tmin[,1:66]
year <- 1900:2018
M_tmin$year <- year
# classify sync, async and ns
M_tmin<- M_tmin %>%
  pivot_longer(1:66, names_to = "ts", values_to = "values")
M_tmin <- na.omit(M_tmin)
M_tminevents <- M_tmin %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents <- M_tminevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))



prop_tmin_sync <- data.frame(year = NA, synch = NA, asynch = NA, ns = NA, interval = NA, obs = NA)

for (xx in 1:length(unique(M_tminevents$interval))) {
  
  current <- unique(M_tminevents$interval)[xx]
  M_tminevents_s <- M_tminevents %>%
    filter(interval == current)
  
  #significantly synchronous in the short term
  SSs <- M_tminevents_s %>%
    filter(event == "synchronous")%>%
    group_by(year)%>%
    summarise(short_sync = n())
  
  #significantly asynchronous in the short term
  SAs <- M_tminevents_s %>%
    filter(event == "asynchronous")%>%
    group_by(year)%>%
    summarise(short_async = n())
  
  #not significant in the short term
  NSs <- M_tminevents_s %>%
    filter(event == "NS")%>%
    group_by(year)%>%
    summarise(short_NS = n())
  
  #the number of observations per year to divide by to get the proportion
  prop_den_s <- M_tminevents_s %>%
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
    full_join(prop_ns, by="year") %>%
    full_join(prop_den_s, by="year")
  
  plot1_temp$interval <- current
  
  prop_tmin_sync <- rbind(prop_tmin_sync, plot1_temp)
}

prop_tmin_sync <- prop_tmin_sync[2:381,]

prop_tmin_sync[is.na(prop_tmin_sync)] <- 0

# join datasets for plotting
prop_tmin_sync$driver <- "tmin"
prop_ppt_sync$driver <- "ppt"
prop_env_sync <- rbind(prop_ppt_sync, prop_tmin_sync)

prop_env_sync$interval <- factor(prop_env_sync$interval, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

regional_prop_env <- ggplot(data = prop_env_sync, aes(x = year, y = synch, group = interval, color = interval)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()+
  facet_wrap(~driver)+
  scale_x_continuous(breaks = seq(1900,2018,10))+
  scale_color_brewer(palette="Spectral", type = "seq", labels = c("biennial", "multiannual","decadal","multidecadal"))+
  scale_y_continuous(breaks = seq(0,2,0.5))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Significant Synchrony (Proportion)")+
  xlab("Year")

