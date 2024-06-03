
# source the data from cleaning code
source(here::here("updated_cleaning_code.R"))

# perform a moving window to compute the average across a window of years to
# capture a timescale-specific measure of ppt and tmin

# ppt 3 years 
window_length <- 3
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  ppt_3 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  ppt_window_3 <- bind_rows(ppt_window_3, ppt_3)
}

# tmin 3 years
window_length <- 3
#How many years after the start year
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  

  tmin_3 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  tmin_window_3 <- bind_rows(tmin_window_3, tmin_3)
}

# rwi 3 years
window_length <- 3
#How many years after the start year
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  rwi_3 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  rwi_window_3 <- bind_rows(rwi_window_3, rwi_3)
}

# ppt 7 years
window_length <- 7
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_7 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  ppt_7 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  ppt_window_7 <- bind_rows(ppt_window_7, ppt_7)
}

# tmin 7 years
window_length <- 7
#How many years after the start year
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_7 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  tmin_7 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  tmin_window_7 <- bind_rows(tmin_window_7, tmin_7)
}

# rwi 7 years
window_length <- 7
#How many years after the start year
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_7 <- tibble()
for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  rwi_7 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  rwi_window_7 <- bind_rows(rwi_window_7, rwi_7)
}

# ppt 15 years 
window_length <- 15
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_15 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  ppt_15 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  ppt_window_15 <- bind_rows(ppt_window_15, ppt_15)
}

# tmin 15 years
window_length <- 15
#How many years after the start year
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_15 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  tmin_15 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  tmin_window_15 <- bind_rows(tmin_window_15, tmin_15)
}

# rwi 15 years
window_length <- 15
#How many years after the start year
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_15 <- tibble()
for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  rwi_15 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  rwi_window_15 <- bind_rows(rwi_window_15, rwi_15)
}

# ppt 25 years
window_length <- 25
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_25 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  ppt_25 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  ppt_window_25 <- bind_rows(ppt_window_25, ppt_25)
}

# tmin 25 years
window_length <- 25
#How many years after the start year
start_year <- min(summer_tmin$year) 
end_year <- max(summer_tmin$year)

tmin_window_25 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  tmin_25 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  tmin_window_25 <- bind_rows(tmin_window_25, tmin_25)
}

# rwi 25 years
window_length <- 25
#How many years after the start year
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_25 <- tibble()
for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  rwi_25 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  rwi_window_25 <- bind_rows(rwi_window_25, rwi_25)
}

timescale_specific_avg_ppt <- rbind(ppt_window_3, ppt_window_7, ppt_window_15, ppt_window_25) %>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_ppt = mean(window_ppt))

timescale_specific_avg_tmin <- rbind(tmin_window_3, tmin_window_7, tmin_window_15, tmin_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_tmin = mean(window_tmin))

timescale_specific_avg_rwi <- rbind(rwi_window_3, rwi_window_7, rwi_window_15, rwi_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_rwi = mean(window_rwi))

timescale_specific_avg_env <- inner_join(timescale_specific_avg_ppt, timescale_specific_avg_tmin, 
                                     by = c("window_year", "band", "w_start",
                                            "w_end", "window_lenth"))
timescale_spec_avg_dat <- inner_join(timescale_specific_avg_env, timescale_specific_avg_rwi, 
                                 by = c("window_year", "band", "w_start",
                                        "w_end", "window_lenth"))


# load in avg growth sync data
load("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_growth_sync.RData")

# clean up in order to join with the timescale specific environmental data
avg_sync_standardband <- avg_sync_standardband %>%
  select(1:3)
avg_sync_standardband$year <- as.numeric(avg_sync_standardband$year)
avg_growth_sync_timescale_env <- inner_join(timescale_spec_avg_dat, avg_sync_standardband, by=join_by(window_year == year, band == interval))

timescale_avg_rwi_avg_sync<- ggplot(data = avg_growth_sync_timescale_env, aes(x = window_avg_rwi, y = avg_sync, color = band)) +
  geom_point(data = avg_growth_sync_timescale_env, aes(x = window_avg_rwi, y = avg_sync, color = band), alpha = 0.5) +
  geom_smooth(method = "loess", se= FALSE)+
  theme_bw()+
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
  xlab("timescale specific rwi")+
  ylab("average growth synchrony")



# load in avg coherence data
avg_tv_coh <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_tv_coh.rds")

# clean up in order to join with the timescale specific environmental data
avg_tv_coh <- avg_tv_coh %>%
  pivot_wider(names_from = "driver", values_from = "avg_coh")%>%
  rename(avg_ppt_coh = "ppt")%>%
  rename(avg_tmin_coh = "tmin") %>%
  rename(year = "times")
avg_tv_coh$year <- as.numeric(avg_tv_coh$year)
timescale_spec_avg_dat$band <- factor(timescale_spec_avg_dat$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))
avg_coh_timescale_env <- inner_join(timescale_spec_avg_dat,avg_tv_coh, by=join_by(window_year == year, band == band))

timescale_avg_ppt_avg_coh<- ggplot(data = avg_coh_timescale_env, aes(x = window_avg_ppt, y = avg_ppt_coh, group = band, color = band)) +
  geom_point(data = avg_coh_timescale_env, aes(x = window_avg_ppt, y = avg_ppt_coh, group = band, color = band), alpha = 0.5) +
  geom_smooth(method = "loess", se= FALSE)+
  theme_bw()+
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
  xlab("timescale specific ppt")+
  ylab("average coherence")


#### Test sensitivity to drivers ####

ggplot()+
  geom_line(data = timescale_spec_avg_dat, aes(x=window_year, y=window_avg_ppt, color = band))+
  facet_wrap(~band) +
  theme_bw()

ggplot()+
  geom_line(data = timescale_spec_avg_dat, aes(x=window_year, y=window_avg_tmin, color = band))+
  facet_wrap(~band) +
  theme_bw()


ggplot()+
  geom_line(data = timescale_spec_avg_dat, aes(x=window_year, y=window_avg_rwi, color = band))+
  facet_wrap(~band) +
  theme_bw()

ggplot()+
  geom_point(data = timescale_spec_dat, aes(x=window_avg_ppt, y=window_avg_rwi, color = band), alpha = 0.4)+
  geom_smooth(data = timescale_spec_dat, aes(x=window_avg_ppt, y=window_avg_rwi, color = band),
              method = "lm", se = FALSE)+
  facet_wrap(~band) +
  theme_bw()

ggplot()+
  geom_point(data = timescale_spec_dat, aes(x=window_avg_tmin, y=window_avg_rwi, color = band), alpha = 0.4)+
  geom_smooth(data = timescale_spec_dat, aes(x=window_avg_tmin, y=window_avg_rwi, color = band),
              method = "loess", se = FALSE)+
  facet_wrap(~band) +
  theme_bw()

timescale_specific_ppt <- rbind(ppt_window_3, ppt_window_7, ppt_window_15, ppt_window_25)
timescale_specific_tmin <- rbind(tmin_window_3, tmin_window_7, tmin_window_15, tmin_window_25)
timescale_specific_rwi <- rbind(rwi_window_3, rwi_window_7, rwi_window_15, rwi_window_25)
timescale_specific_env <- inner_join(timescale_specific_ppt, timescale_specific_tmin, 
                                     by = c("plot","window_year", "band", "w_start",
                                            "w_end", "window_lenth"))
timescale_spec_dat <- inner_join(timescale_specific_env, timescale_specific_rwi, 
                                 by = c("plot", "window_year", "band", "w_start",
                                        "w_end", "window_lenth"))

# remove data points with window's that don't exist in the data
timescale_spec_dat <- timescale_spec_dat %>%
  filter(w_start >= 1900) %>%
  filter(w_end <= 2018)

ts_var_dat <- timescale_spec_dat %>%
  group_by(window_year, band)%>%
  mutate(var_ppt = var(window_ppt), var_tmin = var(window_tmin), var_rwi = var(window_rwi)) %>%
  distinct(window_year, var_ppt, var_tmin, var_rwi)

final_var_dat <- inner_join(ts_var_dat,avg_sync_standardband, by=join_by(window_year == year, band == interval))

ggplot()+
  geom_point(data = final_var_dat, aes(x=var_ppt, y=avg_sync, color = band), alpha = 0.4)+
  geom_smooth(data = final_var_dat, aes(x=var_ppt, y=avg_sync, color = band),
              method = "lm", se = FALSE)+
  facet_wrap(~band) +
  theme_bw()

ggplot()+
  geom_line(data = final_var_dat, aes(x=window_year, y=var_rwi, color = band))+
  facet_wrap(~band) +
  theme_bw()



#### ts env vs env sync ####

avg_env_sync_wide <- avg_env_sync %>%
  pivot_wider(names_from = "driver", values_from = "avg_sync")
avg_env_sync_wide <- avg_env_sync_wide %>%
  rename(avg_ppt_sync = "ppt", avg_tmin_sync = "tmin")
avg_env_sync_wide$times <- as.numeric(avg_env_sync_wide$times)

avg_env_sync_ts_env <- inner_join(timescale_spec_avg_dat,avg_env_sync_wide, by=join_by(window_year == times, band == band))

ggplot()+
  geom_point(data = avg_env_sync_ts_env, aes(x=window_avg_tmin, y=avg_ppt_sync, color = band), alpha = 0.4)+
  geom_smooth(data = avg_env_sync_ts_env, aes(x=window_avg_tmin, y=avg_ppt_sync, color = band),
              method = "loess", se = FALSE)+
  facet_wrap(~band) +
  theme_bw()

ggplot()+
  geom_point(data = avg_coh_timescale_env, aes(x=window_avg_tmin, y=avg_ppt_coh, color = band), alpha = 0.4)+
  geom_smooth(data = avg_coh_timescale_env, aes(x=window_avg_tmin, y=avg_ppt_coh, color = band),
              method = "loess", se = FALSE)+
  facet_wrap(~band) +
  theme_bw()

ggplot()+
  geom_point(data = avg_env_sync_ts_env, aes(x=window_avg_ppt, y=avg_ppt_sync, color = band), alpha = 0.4)+
  geom_smooth(data = avg_env_sync_ts_env, aes(x=window_avg_ppt, y=avg_ppt_sync, color = band),
              method = "loess", se = FALSE)+
  facet_wrap(~band) +
  theme_bw()

ggplot()+
  geom_point(data = avg_coh_timescale_env, aes(x=window_avg_ppt, y=avg_ppt_coh, color = band), alpha = 0.4)+
  geom_smooth(data = avg_coh_timescale_env, aes(x=window_avg_ppt, y=avg_ppt_coh, color = band),
              method = "loess", se = FALSE)+
  facet_wrap(~band) +
  theme_bw()
