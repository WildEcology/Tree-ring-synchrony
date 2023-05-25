

# source the data
source(here::here("scripts/cleaning_code.R"))


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
year <- 1901:2018
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
  mutate(interval = case_when(ts >= 2 & ts <= 5 ~ "short",
                              ts > 5 & ts <= 10 ~ "medium",
                              ts > 10 & ts <= 20 ~ "long",
                              ts > 20 & ts <= 30 ~ "xlong"))


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
  mutate(band = case_when(unique.M2events.ts. >= 2 & unique.M2events.ts. <= 5 ~ "short",
                          unique.M2events.ts. > 5 & unique.M2events.ts. <= 10 ~ "medium",
                          unique.M2events.ts. > 10 & unique.M2events.ts. <= 20 ~ "long",
                          unique.M2events.ts. > 20 & unique.M2events.ts. <= 30 ~ "xlong"))%>%
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
prop_sync_short <- prop_sync_final %>%
  filter(interval == "short")
snull_model <- glmmTMB(synch~1, 
                      data = prop_sync_short,
                      family=binomial(),
                      weights = obs)

slinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_short,
          family = binomial(),
          weights = obs)

squad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_short,
          family = binomial(),
          weights = obs)

AIC(snull_model, slinear_model, squad_model)
# linear fit best


prop_sync_med <- prop_sync_final%>%
  filter(interval == "medium")

mnull_model <- glmmTMB(synch~1, 
                      data = prop_sync_med,
                      family=binomial(),
                      weights = obs)

mlinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_med,
          family = binomial(),
          weights = obs)

mquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_med,
          family = binomial(),
          weights = obs)

AIC(mnull_model, mlinear_model, mquad_model)
# quad fit best

prop_sync_long <- prop_sync_final%>%
  filter(interval == "long")

lnull_model <- glmmTMB(synch~1, 
                      data = prop_sync_long,
                      family=binomial(),
                      weights = obs)

llinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_long,
          family = binomial(),
          weights = obs)

lquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_long,
          family = binomial(),
          weights = obs)

AIC(lnull_model, llinear_model, lquad_model)
# quad fit best

prop_sync_xlong <- prop_sync_final%>%
  filter(interval == "xlong")

xnull_model <- glmmTMB(synch~1, 
                      data = prop_sync_xlong,
                      family=binomial(),
                      weights = obs)

xlinear_model <-
  glmmTMB(synch ~ scaled_year, 
          data= prop_sync_xlong,
          family = binomial(),
          weights = obs)

xquad_model <-
  glmmTMB(synch~ poly(scaled_year, 2, raw=TRUE), 
          data= prop_sync_xlong,
          family = binomial(),
          weights = obs)

AIC(xnull_model, xlinear_model, xquad_model)
#linear fits best

# All together:
svis_prod <- ggemmeans(slinear_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
svis_prod$band <- "short"
mvis_prod <- ggemmeans(mquad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
mvis_prod$band <- "medium"
lvis_prod <- ggemmeans(lquad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
lvis_prod$band <- "long"
xvis_prod <- ggemmeans(xlinear_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
xvis_prod$band <- "xlong"




prop_sync_final_tojoin <- prop_sync_final %>% 
  select(year,x) %>%
  unique()

final_prop_data <- rbind(svis_prod, mvis_prod, lvis_prod, xvis_prod)


final_prop_data <- left_join(prop_sync_final_tojoin, final_prop_data, by="x")

ggplot() +
  geom_point(data = prop_sync_final, aes(x=year, y=synch, col= interval), alpha = 0.2) +
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
  theme_classic() +
  ylab("synchrony (proportion significant)")+
  xlab("year")

