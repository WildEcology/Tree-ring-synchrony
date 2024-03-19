source(here::here("updated_cleaning_code.R"))
source(here::here("Scripts/coh_tv.R"))

x = avg_plot_growth_mx
y1 = winter_ppt_mx
y2 = summer_tmin_mx
#y3 = avg_vpdmax_mx
times = 1900:2018

# calculate time varying coherence for each variable across whole time series
# leave f0 and scale_max_input blank
tv_timeseries_ppt <- coh_tv(dat1 = x, dat2 = y1, times = times, norm = "powall",
                            sigmethod = "fftsurrog1", nrand = 1000)
tv_timeseries_tmin <- coh_tv(dat1 = x, dat2 = y2, times = times, norm = "powall",
                             sigmethod = "fftsurrog1", nrand = 1000)
#tv_timeseries_vpdmax <- coh_tv(dat1 = x, dat2 = y3, times = times, norm = "powall",
                               #sigmethod = "fftsurrog1", nrand = 1000)
# plot phase relationships through time and across timescales
# PPT
timescales <- tv_timeseries_ppt$timescales
tvcoh_ppt_mx <- matrix(data=tv_timeseries_ppt$coher, length(times), length(timescales))
tts_ppt <- tts(times, timescales, tvcoh_ppt_mx)
plotphase(tts_ppt)

# extract the phases
tvcoh_ppt_phases <- Arg(tvcoh_ppt_mx)
tvcoh_ppt_phases <- as.data.frame(tvcoh_ppt_phases)
colnames(tvcoh_ppt_phases) <- timescales
tvcoh_ppt_phases$time <- times

tvcoh_ppt_phases <- tvcoh_ppt_phases %>%
  pivot_longer(1:67, names_to = "timescale", values_to = "phase")
tvcoh_ppt_phases$timescale <- as.numeric(tvcoh_ppt_phases$timescale)
tvcoh_ppt_phases <- tvcoh_ppt_phases %>%
  mutate(band = case_when(timescale >= 2 & timescale <= 3 ~ "biennial",
                          timescale > 3 & timescale <= 10 ~ "multiannual",
                          timescale > 10 & timescale <= 20 ~ "decadal",
                          timescale > 20 & timescale <= 30 ~ "multidecadal"))

tvcoh_ppt_phases <- na.omit(tvcoh_ppt_phases)
avg_phase_tvcoh_ppt <- tvcoh_ppt_phases %>%
  group_by(time, band) %>%
  summarise(avg_phase = mean(phase))

avg_phase_tvcoh_ppt$band <- factor(avg_phase_tvcoh_ppt$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

ggplot() +
  geom_line(data = avg_phase_tvcoh_ppt, aes(x = time, y = avg_phase)) +
  facet_grid(~ band)+
  theme_bw()+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Phase")+
  xlab("Year")



# TMIN
timescales <- tv_timeseries_tmin$timescales
tvcoh_tmin_mx <- matrix(data=tv_timeseries_tmin$coher, length(times), length(timescales))
tts_tmin <- tts(times, timescales, tvcoh_tmin_mx)
plotphase(tts_tmin)

tvcoh_tmin_phases <- Arg(tvcoh_tmin_mx)
tvcoh_tmin_phases <- as.data.frame(tvcoh_tmin_phases)
colnames(tvcoh_tmin_phases) <- timescales
tvcoh_tmin_phases$time <- times

tvcoh_tmin_phases <- tvcoh_tmin_phases %>%
  pivot_longer(1:67, names_to = "timescale", values_to = "phase")
tvcoh_tmin_phases$timescale <- as.numeric(tvcoh_tmin_phases$timescale)
tvcoh_tmin_phases <- tvcoh_tmin_phases %>%
  mutate(band = case_when(timescale >= 2 & timescale <= 3 ~ "biennial",
                          timescale > 3 & timescale <= 10 ~ "multiannual",
                          timescale > 10 & timescale <= 20 ~ "decadal",
                          timescale > 20 & timescale <= 30 ~ "multidecadal"))

tvcoh_tmin_phases <- na.omit(tvcoh_tmin_phases)
avg_phase_tvcoh_tmin <- tvcoh_tmin_phases %>%
  group_by(time, band) %>%
  summarise(avg_phase = mean(phase))

avg_phase_tvcoh_tmin$band <- factor(avg_phase_tvcoh_tmin$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

ggplot() +
  geom_line(data = avg_phase_tvcoh_tmin, aes(x = time, y = avg_phase)) +
  facet_grid(~ band)+
  theme_bw()+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Phase")+
  xlab("Year")



# combine both variables to compare phase relationships through time
avg_phase_tvcoh_ppt$driver <- "ppt"
avg_phase_tvcoh_tmin$driver <- "tmin"
avg_phase_coherence <- rbind(avg_phase_tvcoh_ppt, avg_phase_tvcoh_tmin)


ggplot() +
  geom_line(data = avg_phase_coherence, aes(x = time, y = avg_phase, group = driver, color = driver)) +
  facet_wrap(~ band)+
  theme_bw()+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Phase")+
  xlab("Year")


# when synchrony is significant
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M1 <- as.data.frame(res_growth_wpmf$values)
colnames(M1) <- res_growth_wpmf$timescales
#fix the imaginary #s
M1 <- abs(M1)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M2 <- M1[,1:66]
year <- 1900:2018
M2$year <- year
# classify sync, async and ns
M2<- M2 %>%
  pivot_longer(1:66, names_to = "ts", values_to = "values")
M2 <- na.omit(M2)
M2events <- M2 %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M2events$ts <- as.numeric(M2events$ts)
M2events$ts <- as.numeric(M2events$ts)
M2events <- M2events %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))

avg_sync<- M2events %>%
  filter(event == "synchronous") %>%
  group_by(year, interval) %>%
  summarise(avg_sync = mean(values))
  
avg_sync <- avg_sync %>%
  rename(time = year)%>%
  rename(band = interval)

avg_sync <- na.omit(avg_sync)

avg_sync_ppt_phase <- full_join(avg_phase_tvcoh_ppt, avg_sync)
avg_sync_ppt_phase <- na.omit(avg_sync_ppt_phase)

avg_sync_tmin_phase <- full_join(avg_phase_tvcoh_tmin, avg_sync)
avg_sync_tmin_phase <- na.omit(avg_sync_tmin_phase)

avg_sync_ppt_phase$driver <- "ppt"
avg_sync_tmin_phase$driver <- "tmin"
avg_sync_avg_phase_per_driver <- rbind(avg_sync_ppt_phase, avg_sync_tmin_phase)

# plot
ggplot() +
  geom_point(data = avg_sync_avg_phase_per_driver, aes(x = time, y = avg_phase, group = driver, color = driver)) +
  facet_wrap(~ band)+
  theme_bw()+
  ylim(-3.1,3.1)+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Phase")+
  xlab("Year")

# putting plots together
# plot
avg_phase_coherence$band <- factor(avg_phase_coherence$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
avg_sync_avg_phase_per_driver$band <- factor(avg_sync_avg_phase_per_driver$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
avg_phase_coherence$driver <- factor(avg_phase_coherence$driver, levels=c('Winter PRECIP', 'Summer TEMP'))

ggplot() +
  geom_line(data = avg_phase_coherence, aes(x = time, y = avg_phase, group = driver, color = driver))+
  geom_point(data = avg_sync_avg_phase_per_driver, aes(x = time, y = avg_phase, group = driver, color = driver), alpha = 0.6) +
  facet_wrap(~ band)+
  theme_bw()+
  scale_y_continuous(breaks = seq(-3.1, 3.1, 0.775))+
  scale_x_continuous(breaks = seq(1900,2018,10))+
  scale_color_manual(values = c("blue","red"))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 12,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Phase")+
  xlab("Year")
