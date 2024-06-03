# run updated_cleaning_code script to pull in cleaned data
source(here::here("updated_cleaning_code.R"))


#### REGIONAL ####
# calculate signficance thresholds 
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
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "annual",
                              ts > 3 & ts <= 10 ~ "interannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))


#### PROPORTIONS OF SIGNFICANT SYNCHRONY ####
prop_sync_final <- data.frame(year = NA, synch = NA, asynch = NA, ns = NA, interval = NA, obs = NA)

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
    full_join(prop_ns, by="year") %>%
    full_join(prop_den_s, by="year")
  
  plot1_temp$interval <- current
  
  prop_sync_final <- rbind(prop_sync_final, plot1_temp)
}

prop_sync_final <- prop_sync_final[2:381,]

prop_sync_final[is.na(prop_sync_final)] <- 0

#### PLOT WAVELET PHASOR MEAN FIELD ####
wav<-Mod(get_values(res_growth_wpmf))
times<-get_times(res_growth_wpmf)
timescales<-get_timescales(res_growth_wpmf)

plotmag(res_growth_wpmf)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=upper,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1, add=TRUE)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=lower,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1,col="white", add=TRUE)

timescales_b <- data.frame(unique(M2events$ts))

timescales_per_band <- timescales_b %>%
  mutate(band = case_when(unique.M2events.ts. >= 2 & unique.M2events.ts. <= 3 ~ "annual",
                          unique.M2events.ts. > 3 & unique.M2events.ts. <= 10 ~ "interannual",
                          unique.M2events.ts. > 10 & unique.M2events.ts. <= 20 ~ "decadal",
                          unique.M2events.ts. > 20 & unique.M2events.ts. <= 30 ~ "multidecadal"))%>%
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



prop_sync_final <- inner_join(prop_sync_final, final_timeseries)

prop_sync_final <- prop_sync_final %>%
  filter(year >= starting.year & year <= ending.year)%>%
  select(year, synch, obs, interval)

prop_sync_final <- na.omit(prop_sync_final)

#### MODEL FIT ####
library(glmmTMB)
library(ggeffects)


prop_sync_final$interval <- as.factor(prop_sync_final$interval)
prop_sync_final$scaled_year <- scale(prop_sync_final$year)
prop_sync_final$x <- as.numeric(round(prop_sync_final$scaled_year,2))

# all bands
null_model <- glmmTMB(synch~1, 
                       data = prop_sync_final,
                       family=binomial(),
                       weights = obs)

linear_model <-
  glmmTMB(synch ~ scaled_year* interval, 
          data= prop_sync_final,
          family = binomial(),
          weights = obs)

quad_model <-
  glmmTMB(synch ~ poly(scaled_year, 2, raw = TRUE) * interval, 
          data= prop_sync_final,
          family = binomial(),
          weights = obs)

AIC(null_model, linear_model, quad_model)

vis_prod <- ggemmeans(quad_model,
                      terms= c("scaled_year[all]", "interval"),
                      type="fe",
                      ci.lvl = 0.95)

# break up by band
# short term 
prop_sync_annual <- prop_sync_final %>%
  filter(interval == "annual")
anull_model <- glmmTMB(synch~1, 
                      data = prop_sync_annual,
                      family=binomial(),
                      weights = obs)

alinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_annual,
          family = binomial(),
          weights = obs)

aquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_annual,
          family = binomial(),
          weights = obs)

AIC(anull_model, alinear_model, aquad_model)
# linear fit best


prop_sync_inter <- prop_sync_final%>%
  filter(interval == "interannual")

inull_model <- glmmTMB(synch~1, 
                      data = prop_sync_inter,
                      family=binomial(),
                      weights = obs)

ilinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_inter,
          family = binomial(),
          weights = obs)

iquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_inter,
          family = binomial(),
          weights = obs)

AIC(inull_model, ilinear_model, iquad_model)
# quad fit best

prop_sync_decadal <- prop_sync_final%>%
  filter(interval == "decadal")

dnull_model <- glmmTMB(synch~1, 
                      data = prop_sync_decadal,
                      family=binomial(),
                      weights = obs)

dlinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_decadal,
          family = binomial(),
          weights = obs)

dquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_decadal,
          family = binomial(),
          weights = obs)

AIC(dnull_model, dlinear_model, dquad_model)
# quad fit best

prop_sync_multi <- prop_sync_final%>%
  filter(interval == "multidecadal")

mnull_model <- glmmTMB(synch~1, 
                      data = prop_sync_multi,
                      family=binomial(),
                      weights = obs)

mlinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_multi,
          family = binomial(),
          weights = obs)

mquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_multi,
          family = binomial(),
          weights = obs)

AIC(mnull_model, mlinear_model, mquad_model)
#linear fits best

# All together:
avis_prod <- ggemmeans(alinear_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
avis_prod$band <- "annual"
ivis_prod <- ggemmeans(iquad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
ivis_prod$band <- "interannual"
dvis_prod <- ggemmeans(dquad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
dvis_prod$band <- "decadal"
mvis_prod <- ggemmeans(mlinear_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
mvis_prod$band <- "multidecadal"




prop_sync_final_tojoin <- prop_sync_final %>% 
  select(year,x) %>%
  unique()

final_prop_data <- rbind(avis_prod, ivis_prod, dvis_prod, mvis_prod)


final_prop_data <- left_join(prop_sync_final_tojoin, final_prop_data, by="x")
final_prop_data$year <- as.character(final_prop_data$year)
prop_sync_final$year <- as.character(prop_sync_final$year)

prop_sync_final$interval <- factor(prop_sync_final$interval, levels=c('annual', 'interannual', 'decadal', 'multidecadal'))
final_prop_data$band <- factor(final_prop_data$band, levels=c('annual', 'interannual', 'decadal', 'multidecadal'))


mycols1 <- colors()[c(91, 128, 148, 99)]
mypal1 <- palette(mycols1)
names(mypal1) = c("annual", "interannual", "decadal", "multidecadal")
colScale1 <- scale_colour_manual(name = "interval", values = mypal1)
colScale2 <- scale_fill_manual(name = "interval", values = mypal1)
colScale2 <- scale_fill_manual(name = "band", values = mypal1)

regional_prop <- ggplot() +
  #geom_point(data = prop_sync_final, aes(x=year, y=synch, col= interval), alpha = 0.2) +
  geom_line(data = final_prop_data, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = final_prop_data, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
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
  ylab("Synchrony (proportion significant)")+
  xlab("year")+
  colScale1+
  colScale2


png("/Users/kaitlynmcknight/Documents/Teamtree_finalfigures/prop_sync.png", width = 5, height = 5, units = 'in', res = 600)
regional_prop
dev.off()

shortbands <- c("annual", "interannual")
short <- prop_sync_final %>%
  filter(interval %in% shortbands)
short_prop <- final_prop_data %>%
  filter(band %in% shortbands)
longbands <- c("decadal", "multidecadal")
long <- prop_sync_final %>%
  filter(interval %in% longbands)
long_prop <- final_prop_data %>%
  filter(band %in% longbands)

regional_prop_short <- ggplot() +
  geom_point(data = short, aes(x=year, y=synch, col= interval), alpha = 0.1) +
  geom_line(data = short_prop, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = short_prop, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  #scale_color_brewer(palette = "Dark2")+
  #scale_fill_brewer(palette = "Dark2")+
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
  ylab("Synchrony (proportion significant)")+
  ylim(0,1)+
  xlab("Year")+
  colScale1+
  colScale2
png("/Users/kaitlynmcknight/Documents/esapresgraphics/propshort.png", width = 5, height = 5, units = 'in', res = 600)
regional_prop_short
dev.off()

regional_prop_long <- ggplot() +
  geom_point(data = long, aes(x=year, y=synch, col= interval), alpha = 0.1) +
  geom_line(data = long_prop, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = long_prop, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
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
  ylab("Synchrony (proportion significant)")+
  ylim(0,1)+
  xlab("Year")+
  colScale1+
  colScale2

png("/Users/kaitlynmcknight/Documents/esapresgraphics/proplong.png", width = 5, height = 5, units = 'in', res = 600)
regional_prop_long
dev.off()





#### LOCAL #####

# determine the number of trees per plot to calculate significance thresholds
tree_per_plot <- rwi_00s_plot_filtered_wide %>%
  pivot_longer(3:121, names_to = "year")%>%
  group_by(plot, year)%>%
  summarize(numtree = n())

tree_per_plot <- tree_per_plot %>% distinct(plot,numtree)

# calculate significant thresholds for each plot
n <- tree_per_plot$numtree
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

  rwi_00s_plot <- rwi_00s_plot_filtered_wide %>%
    filter(plot == current.plot)
  
  # format matrix for analysis
  rwi_00s_plot <- as.matrix(rwi_00s_plot)
  colnames(rwi_00s_plot) <- NULL
  rwi_00s_plot <- rwi_00s_plot[, c(4:122)] 
  
  # convert character matrix to numeric
  rwi_00s_plot = as.data.frame(rwi_00s_plot, stringsAsFactors = FALSE)
  rwi_00s_plot = map_df(rwi_00s_plot, as.numeric)
  rwi_00s_plot_mx <- as.matrix(rwi_00s_plot)
  
  # calculate wavelet
  year <- 1900:2018
  rwi_00s_plot_mx <- cleandat(rwi_00s_plot_mx, year, clev = 5)$cdat
  res<-wpmf(rwi_00s_plot_mx,year,sigmethod="quick")
  
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
    mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "annual",
                                ts > 3 & ts <= 10 ~ "interannual",
                                ts > 10 & ts <= 20 ~ "decadal",
                                ts > 20 & ts <= 30 ~ "multidecadal"))
  
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
      full_join(prop_ns, by="year") %>%
      full_join(prop_den_s, by="year")
    
    plot1_temp$interval <- current
    plot1_temp$location <- current.plot
    
    ### RBIND FOR ROWS
    plot_current_results <- rbind(plot_current_results, plot1_temp)
    
  }
  
  proportions_final <- rbind(proportions_final, plot_current_results)
  
}

proportions_final[is.na(proportions_final)] <- 0

proportions_final <- inner_join(proportions_final, final_timeseries)

proportions_final <- proportions_final %>%
  filter(year >= starting.year & year <= ending.year)%>%
  select(location, year, synch, obs, interval)

proportions_final$interval <- as.factor(proportions_final$interval)
proportions_final$year <- as.numeric(proportions_final$year)
proportions_final$scaled_year <- scale(proportions_final$year)
proportions_final$x <- as.numeric(round(proportions_final$scaled_year,2))

# all bands
null_model <- glmmTMB(synch~1+(1|location), 
                      data = proportions_final,
                      family=binomial(),
                      weights = obs)

linear_model <-
  glmmTMB(synch ~ scaled_year* interval+(1|location), 
          data= proportions_final,
          family = binomial(),
          weights = obs)

quad_model <-
  glmmTMB(synch ~ poly(scaled_year, 2, raw = TRUE) * interval + (1|location), 
          data= proportions_final,
          family = binomial(),
          weights = obs)

AIC(null_model, linear_model, quad_model)

vis_prod <- ggemmeans(quad_model,
                      terms= c("scaled_year[all]", "interval"),
                      type="fe",
                      ci.lvl = 0.95)

# break up by band
# short term 
prop_sync_annual <- proportions_final %>%
  filter(interval == "annual")
anull_model <- glmmTMB(synch~1 + (1|location), 
                       data = prop_sync_annual,
                       family=binomial(),
                       weights = obs)

alinear_model <-
  glmmTMB(synch ~ scaled_year + (1|location), 
          data= prop_sync_annual,
          family = binomial(),
          weights = obs)

aquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= prop_sync_annual,
          family = binomial(),
          weights = obs)

AIC(anull_model, alinear_model, aquad_model)
# linear fit best


prop_sync_inter <- proportions_final%>%
  filter(interval == "interannual")

inull_model <- glmmTMB(synch~1 + (1|location), 
                       data = prop_sync_inter,
                       family=binomial(),
                       weights = obs)

ilinear_model <-
  glmmTMB(synch ~ scaled_year + (1|location), 
          data= prop_sync_inter,
          family = binomial(),
          weights = obs)

iquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= prop_sync_inter,
          family = binomial(),
          weights = obs)

AIC(inull_model, ilinear_model, iquad_model)
# quad fit best

prop_sync_decadal <- proportions_final%>%
  filter(interval == "decadal")

dnull_model <- glmmTMB(synch~1 + (1|location), 
                       data = prop_sync_decadal,
                       family=binomial(),
                       weights = obs)

dlinear_model <-
  glmmTMB(synch ~ scaled_year + (1|location), 
          data= prop_sync_decadal,
          family = binomial(),
          weights = obs)

dquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= prop_sync_decadal,
          family = binomial(),
          weights = obs)

AIC(dnull_model, dlinear_model, dquad_model)
# quad fit best

prop_sync_multi <- proportions_final%>%
  filter(interval == "multidecadal")

mnull_model <- glmmTMB(synch~1 + (1|location), 
                       data = prop_sync_multi,
                       family=binomial(),
                       weights = obs)

mlinear_model <-
  glmmTMB(synch ~ scaled_year + (1|location), 
          data= prop_sync_multi,
          family = binomial(),
          weights = obs)

mquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= prop_sync_multi,
          family = binomial(),
          weights = obs)

AIC(mnull_model, mlinear_model, mquad_model)
# quad fits best

# All together:
avis_prod <- ggpredict(alinear_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "re", 
                       ci.lvl = .95)
avis_prod$band <- "annual"
ivis_prod <- ggpredict(iquad_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "re", 
                       ci.lvl = .95)
ivis_prod$band <- "interannual"
dvis_prod <- ggpredict(dquad_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "re", 
                       ci.lvl = .95)
dvis_prod$band <- "decadal"
mvis_prod <- ggpredict(mquad_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "re", 
                       ci.lvl = .95)
mvis_prod$band <- "multidecadal"




proportions_final_tojoin <- proportions_final %>% 
  select(year,x) %>%
  unique()

final_prop_data <- rbind(avis_prod, ivis_prod, dvis_prod, mvis_prod)


final_prop_data <- left_join(proportions_final_tojoin, final_prop_data, by="x")
final_prop_data$year <- as.character(final_prop_data$year)
proportions_final$year <- as.character(proportions_final$year)
local_prop <- ggplot() +
  geom_point(data = proportions_final, aes(x=year, y=synch, col= interval), alpha = 0.1) +
  geom_line(data = final_prop_data, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = final_prop_data, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,5))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("synchrony (proportion significant)")+
  xlab("year")


