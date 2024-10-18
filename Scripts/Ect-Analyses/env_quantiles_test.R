
source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))
# create timescale specific temperatures for each of the four timescale bands
names(summer_tmin)[3] <- "tmin"

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
    summarise(window_tmin = mean(tmin)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  tmin_window_3 <- bind_rows(tmin_window_3, tmin_3)
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
    summarise(window_tmin = mean(tmin)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  tmin_window_7 <- bind_rows(tmin_window_7, tmin_7)
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
    summarise(window_tmin = mean(tmin)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  tmin_window_15 <- bind_rows(tmin_window_15, tmin_15)
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
    summarise(window_tmin = mean(tmin)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  tmin_window_25 <- bind_rows(tmin_window_25, tmin_25)
}


timescale_specific_avg_tmin <- rbind(tmin_window_3, tmin_window_7, tmin_window_15, tmin_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_tmin = mean(window_tmin))

biennial_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "biennial")
quantile(biennial_tmin_quantiles$window_avg_tmin)

biennial_tmin_quantiles <-  biennial_tmin_quantiles %>%
  mutate(quantile = case_when(window_avg_tmin <= 3.079533  ~ 1,
                              window_avg_tmin >  3.079533 & window_avg_tmin <= 3.757356  ~ 2,
                              window_avg_tmin > 3.757356 & window_avg_tmin <= 4.116697~ 3,
                              window_avg_tmin > 4.116697  & window_avg_tmin <= 6.956917  ~ 4))
biennial_tmin_quantiles <- biennial_tmin_quantiles %>%
  mutate(ts_tmin = case_when(quantile == 1 ~ 3.079533,
                             quantile == 2 ~ 3.757356,
                             quantile == 3 ~ 4.116697,
                             quantile == 4 ~ 6.956917))

multiannual_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "multiannual")
quantile(multiannual_tmin_quantiles$window_avg_tmin)

multiannual_tmin_quantiles <-  multiannual_tmin_quantiles %>%
  mutate(quantile = case_when(window_avg_tmin <= 3.200671 ~ 1,
                              window_avg_tmin >  3.200671 & window_avg_tmin <= 3.612057  ~ 2,
                              window_avg_tmin > 3.612057 & window_avg_tmin <= 4.035188~ 3,
                              window_avg_tmin > 4.035188 & window_avg_tmin <= 6.801167  ~ 4))
multiannual_tmin_quantiles <- multiannual_tmin_quantiles %>%
  mutate(ts_tmin = case_when(quantile == 1 ~ 3.200671,
                             quantile == 2 ~ 3.612057,
                             quantile == 3 ~ 4.035188,
                             quantile == 4 ~ 6.801167))

decadal_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "decadal")
quantile(decadal_tmin_quantiles$window_avg_tmin)

decadal_tmin_quantiles <-  decadal_tmin_quantiles %>%
  mutate(quantile = case_when(window_avg_tmin <= 3.268946 ~ 1,
                              window_avg_tmin >  3.268946 & window_avg_tmin <= 3.541862   ~ 2,
                              window_avg_tmin > 3.541862  & window_avg_tmin <= 4.015177~ 3,
                              window_avg_tmin > 4.015177 & window_avg_tmin <= 6.300619  ~ 4))
decadal_tmin_quantiles <- decadal_tmin_quantiles %>%
  mutate(ts_tmin = case_when(quantile == 1 ~ 3.268946,
                             quantile == 2 ~ 3.541862,
                             quantile == 3 ~ 4.015177,
                             quantile == 4 ~ 6.300619))

multidecadal_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "multidecadal")
quantile(multidecadal_tmin_quantiles$window_avg_tmin)

multidecadal_tmin_quantiles <-  multidecadal_tmin_quantiles %>%
  mutate(quantile = case_when(window_avg_tmin <= 3.322079 ~ 1,
                              window_avg_tmin >  3.322079 & window_avg_tmin <= 3.503817  ~ 2,
                              window_avg_tmin > 3.503817  & window_avg_tmin <= 3.939669~ 3,
                              window_avg_tmin > 3.939669 & window_avg_tmin <= 6.300619  ~ 4))
multidecadal_tmin_quantiles <- multidecadal_tmin_quantiles %>%
  mutate(ts_tmin = case_when(quantile == 1 ~ 3.322079,
                             quantile == 2 ~ 3.503817,
                             quantile == 3 ~ 3.939669,
                             quantile == 4 ~ 6.300619))

# rbind all bands df back into one
timescale_specific_avg_tmin <- rbind(biennial_tmin_quantiles, multiannual_tmin_quantiles, decadal_tmin_quantiles, multidecadal_tmin_quantiles)

avg_env_sync <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_env_sync.RDS")
avg_env_sync_ppt <- avg_env_sync %>%
  filter(driver == "ppt")
avg_ppt_sync_timescale_tmin <- inner_join(timescale_specific_avg_tmin, avg_env_sync_ppt, by=join_by(window_year == year, band == interval))
avg_ppt_sync_timescale_tmin_CIs <- avg_ppt_sync_timescale_tmin %>%
  group_by(band, quantile, ts_tmin)%>%
  summarise(mean.ppt.sync = mean(avg_sync, na.rm = TRUE),
            sd.ppt.sync = sd(avg_sync, na.rm = TRUE),
            n.ppt.sync = n()) %>%
  mutate(se.ppt.sync = sd.ppt.sync / sqrt(n.ppt.sync),
         lower.ci.ppt.sync = mean.ppt.sync - qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync,
         upper.ci.ppt.sync = mean.ppt.sync + qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync)

avg_ppt_sync_timescale_tmin <- left_join(avg_ppt_sync_timescale_tmin, avg_ppt_sync_timescale_tmin_CIs)
avg_ppt_sync_timescale_tmin$band <- factor(avg_ppt_sync_timescale_tmin$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))


avg_ppt_sync_tmin_quant <- ggplot(data = avg_ppt_sync_timescale_tmin, aes(x = quantile, y = mean.ppt.sync, col = band)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.ppt.sync, ymax = upper.ci.ppt.sync, x = quantile, y=mean.ppt.sync, width = 0.2)) +
  theme_bw()+
  ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  ylab("Average Precipitation Synchrony")+
  xlab("Temperature Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 




# create timescale specific temperatures for each of the four timescale bands
names(winter_ppt)[3] <- "ppt"

window_length <- 3
#How many years after the start year
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  
  ppt_3 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  ppt_window_3 <- bind_rows(ppt_window_3, ppt_3)
}

# tmin 7 years
window_length <- 7
#How many years after the start year
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
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  ppt_window_7 <- bind_rows(ppt_window_7, ppt_7)
}

# tmin 15 years
window_length <- 15
#How many years after the start year
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
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  ppt_window_15 <- bind_rows(ppt_window_15, ppt_15)
}


# tmin 25 years
window_length <- 25
#How many years after the start year
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
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  ppt_window_25 <- bind_rows(ppt_window_25, ppt_25)
}


timescale_specific_avg_ppt <- rbind(ppt_window_3, ppt_window_7, ppt_window_15, ppt_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_ppt = mean(window_ppt))

biennial_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "biennial")
quantile(biennial_ppt_quantiles$window_avg_ppt)

biennial_ppt_quantiles <-  biennial_ppt_quantiles %>%
  mutate(quantile = case_when(window_avg_ppt <= 103.43028  ~ 1,
                              window_avg_ppt >  103.43028  & window_avg_ppt <= 120.18358  ~ 2,
                              window_avg_ppt > 120.18358 & window_avg_ppt <=141.83602~ 3,
                              window_avg_ppt > 141.83602  & window_avg_ppt <= 174.85795   ~ 4))
biennial_ppt_quantiles <- biennial_ppt_quantiles %>%
  mutate(ts_ppt = case_when(quantile == 1 ~ 103.43028,
                             quantile == 2 ~ 120.18358,
                             quantile == 3 ~ 141.83602,
                             quantile == 4 ~ 174.85795))

multiannual_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "multiannual")
quantile(multiannual_ppt_quantiles$window_avg_ppt)

multiannual_ppt_quantiles <-  multiannual_ppt_quantiles %>%
  mutate(quantile = case_when(window_avg_ppt <= 109.23701 ~ 1,
                              window_avg_ppt >  109.23701 & window_avg_ppt <= 118.74515  ~ 2,
                              window_avg_ppt > 118.74515 & window_avg_ppt <= 131.05482~ 3,
                              window_avg_ppt > 131.05482 & window_avg_ppt <=  162.07990  ~ 4))
multiannual_ppt_quantiles <- multiannual_ppt_quantiles %>%
  mutate(ts_ppt = case_when(quantile == 1 ~ 109.23701,
                             quantile == 2 ~ 118.74515,
                             quantile == 3 ~ 131.05482,
                             quantile == 4 ~ 162.07990))

decadal_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "decadal")
quantile(decadal_ppt_quantiles$window_avg_ppt)

decadal_ppt_quantiles <-  decadal_ppt_quantiles %>%
  mutate(quantile = case_when(window_avg_ppt <= 113.6924 ~ 1,
                              window_avg_ppt >  113.6924 & window_avg_ppt <= 119.7291   ~ 2,
                              window_avg_ppt > 119.7291  & window_avg_ppt <= 125.1360~ 3,
                              window_avg_ppt > 125.1360 & window_avg_ppt <= 158.6863  ~ 4))
decadal_ppt_quantiles <- decadal_ppt_quantiles %>%
  mutate(ts_ppt = case_when(quantile == 1 ~ 113.6924,
                             quantile == 2 ~ 119.7291,
                             quantile == 3 ~ 125.1360,
                             quantile == 4 ~ 158.6863))

multidecadal_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "multidecadal")
quantile(multidecadal_ppt_quantiles$window_avg_ppt)

multidecadal_ppt_quantiles <-  multidecadal_ppt_quantiles %>%
  mutate(quantile = case_when(window_avg_ppt <= 115.6615 ~ 1,
                              window_avg_ppt >  115.6615 & window_avg_ppt <= 119.3605  ~ 2,
                              window_avg_ppt > 119.3605  & window_avg_ppt <= 123.3394~ 3,
                              window_avg_ppt > 123.3394 & window_avg_ppt <= 146.3873  ~ 4))
multidecadal_ppt_quantiles <- multidecadal_ppt_quantiles %>%
  mutate(ts_ppt = case_when(quantile == 1 ~ 115.6615,
                             quantile == 2 ~ 119.3605,
                             quantile == 3 ~ 123.3394,
                             quantile == 4 ~ 146.3873))

# rbind all bands df back into one
timescale_specific_avg_ppt <- rbind(biennial_ppt_quantiles, multiannual_ppt_quantiles, decadal_ppt_quantiles, multidecadal_ppt_quantiles)

avg_env_sync <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_env_sync.RDS")
avg_env_sync_tmin <- avg_env_sync %>%
  filter(driver == "tmin")
avg_tmin_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_env_sync_tmin, by=join_by(window_year == year, band == interval))
avg_tmin_sync_timescale_ppt_CIs <- avg_tmin_sync_timescale_ppt %>%
  group_by(band, quantile, ts_ppt)%>%
  summarise(mean.tmin.sync = mean(avg_sync, na.rm = TRUE),
            sd.tmin.sync = sd(avg_sync, na.rm = TRUE),
            n.tmin.sync = n()) %>%
  mutate(se.tmin.sync = sd.tmin.sync / sqrt(n.tmin.sync),
         lower.ci.tmin.sync = mean.tmin.sync - qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync,
         upper.ci.tmin.sync = mean.tmin.sync + qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync)

avg_tmin_sync_timescale_ppt <- left_join(avg_tmin_sync_timescale_ppt, avg_tmin_sync_timescale_ppt_CIs)
avg_tmin_sync_timescale_ppt$band <- factor(avg_tmin_sync_timescale_ppt$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))
avg_tmin_sync_timescale_ppt <- na.omit(avg_tmin_sync_timescale_ppt)


avg_tmin_sync_ppt_quant <- ggplot(data = avg_tmin_sync_timescale_ppt, aes(x = quantile, y = mean.tmin.sync, col = band)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.tmin.sync, ymax = upper.ci.tmin.sync, x = quantile, y=mean.tmin.sync, width = 0.2)) +
  theme_bw()+
  ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  #facet_wrap(~band)+
  ylab("Average Temperature Synchrony")+
  xlab("Precipitation Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 






avg_rwi_sync <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_rwi_sync.RDS")
avg_rwi_sync <- avg_rwi_sync %>%
  select(1:3)
names(avg_rwi_sync)[2] <- "band"
avg_rwi_sync$year <- as.numeric(avg_rwi_sync$year)
avg_rwi_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_rwi_sync, by=join_by(window_year == year, band))
avg_rwi_sync_timescale_ppt_CIs <- avg_rwi_sync_timescale_ppt %>%
  group_by(band, quantile, ts_ppt)%>%
  summarise(mean.rwi.sync = mean(avg_sync, na.rm = TRUE),
            sd.rwi.sync = sd(avg_sync, na.rm = TRUE),
            n.rwi.sync = n()) %>%
  mutate(se.rwi.sync = sd.rwi.sync / sqrt(n.rwi.sync),
         lower.ci.rwi.sync = mean.rwi.sync - qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync,
         upper.ci.rwi.sync = mean.rwi.sync + qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync)

avg_rwi_sync_timescale_ppt <- left_join(avg_rwi_sync_timescale_ppt, avg_rwi_sync_timescale_ppt_CIs)
avg_rwi_sync_timescale_ppt$band <- factor(avg_rwi_sync_timescale_ppt$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))
avg_rwi_sync_timescale_ppt <- na.omit(avg_rwi_sync_timescale_ppt)

avg_rwi_sync_ppt_quant <- ggplot(data = avg_rwi_sync_timescale_ppt, aes(x = quantile, y = mean.rwi.sync, col = band)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.rwi.sync, ymax = upper.ci.rwi.sync, x = quantile, y=mean.rwi.sync, width = 0.2)) +
  theme_bw()+
  ylim(0.0, 1.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  #facet_wrap(~band)+
  ylab("Average Growth Synchrony")+
  xlab("Precipitation Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 





avg_rwi_sync_timescale_tmin <- inner_join(timescale_specific_avg_tmin, avg_rwi_sync, by=join_by(window_year == year, band))

avg_rwi_sync_timescale_tmin <- avg_rwi_sync_timescale_tmin %>%
  group_by(band, quantile)%>%
  summarise(mean.rwi.sync = mean(avg_sync, na.rm = TRUE),
            sd.rwi.sync = sd(avg_sync, na.rm = TRUE),
            n.rwi.sync = n()) %>%
  mutate(se.rwi.sync = sd.rwi.sync / sqrt(n.rwi.sync),
         lower.ci.rwi.sync = mean.rwi.sync - qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync,
         upper.ci.rwi.sync = mean.rwi.sync + qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync)
avg_rwi_sync_timescale_tmin <- na.omit(avg_rwi_sync_timescale_tmin)

avg_rwi_sync_timescale_tmin$band <- factor(avg_rwi_sync_timescale_tmin$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))

avg_rwi_sync_tmin_quant <- ggplot(data = avg_rwi_sync_timescale_tmin, aes(x = quantile, y = mean.rwi.sync, col = band)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.rwi.sync, ymax = upper.ci.rwi.sync, x = quantile, y=mean.rwi.sync, width = 0.2)) +
  theme_bw()+
  ylim(0.0, 1.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  #facet_wrap(~band)+
  ylab("Average Growth Synchrony")+
  xlab("Temperature Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())


#### Perform pairwise comparisons of multiple groups using t.tests with bonferroni p-value adjustment ####
# correction Factor applied for 6 tests per band with 4 quantiles in each band
corr_p_value <- (0.05/6)
## RWI Sync across TMIN quantiles ##

# expand data to include all data points
avg_rwi_sync_timescale_tmin <- inner_join(timescale_specific_avg_tmin, avg_rwi_sync, by=join_by(window_year == year, band))
# remove NAs
avg_rwi_sync_timescale_tmin <- na.omit(avg_rwi_sync_timescale_tmin)
# make sure quantile is a factor
avg_rwi_sync_timescale_tmin$quantile <- factor(avg_rwi_sync_timescale_tmin$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_rwi_sync_tmin_quantiles_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(avg_rwi_sync_timescale_tmin$band))) {
  
  current <- unique(avg_rwi_sync_timescale_tmin$band)[xx]
  band_data <- avg_rwi_sync_timescale_tmin %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_rwi_sync_tmin_quantiles_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_rwi_sync_tmin_quantiles_results  <- do.call(rbind, t.test_rwi_sync_tmin_quantiles_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_rwi_sync_tmin_quantiles_results$Mean_Difference <- t.test_rwi_sync_tmin_quantiles_results$Mean_Difference * -1




## RWI Sync across PPT quantiles ##

# expand data to include all data points
avg_rwi_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_rwi_sync, by=join_by(window_year == year, band))
# remove NAs
avg_rwi_sync_timescale_ppt <- na.omit(avg_rwi_sync_timescale_ppt)
# make sure quantile is a factor
avg_rwi_sync_timescale_ppt$quantile <- factor(avg_rwi_sync_timescale_ppt$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_rwi_sync_ppt_quantiles_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(avg_rwi_sync_timescale_ppt$band))) {
  
  current <- unique(avg_rwi_sync_timescale_ppt$band)[xx]
  band_data <- avg_rwi_sync_timescale_ppt %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_rwi_sync_ppt_quantiles_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_rwi_sync_ppt_quantiles_results  <- do.call(rbind, t.test_rwi_sync_ppt_quantiles_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_rwi_sync_ppt_quantiles_results$Mean_Difference <- t.test_rwi_sync_ppt_quantiles_results$Mean_Difference * -1




## PPT Sync across TMIN quantiles ##

# expand data to include all data points
avg_ppt_sync_timescale_tmin <- inner_join(timescale_specific_avg_tmin, avg_env_sync_ppt, by=join_by(window_year == year, band == interval))
# remove NAs
avg_ppt_sync_timescale_tmin <- na.omit(avg_ppt_sync_timescale_tmin)
# make sure quantile is a factor
avg_ppt_sync_timescale_tmin$quantile <- factor(avg_ppt_sync_timescale_tmin$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_ppt_sync_tmin_quantiles_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(avg_ppt_sync_timescale_tmin$band))) {
  
  current <- unique(avg_ppt_sync_timescale_tmin$band)[xx]
  band_data <- avg_ppt_sync_timescale_tmin %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_ppt_sync_tmin_quantiles_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_ppt_sync_tmin_quantiles_results  <- do.call(rbind, t.test_ppt_sync_tmin_quantiles_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_ppt_sync_tmin_quantiles_results$Mean_Difference <- t.test_ppt_sync_tmin_quantiles_results$Mean_Difference * -1



## TMIN Sync across PPT quantiles ##
# expand data to include all data points
avg_tmin_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_env_sync_tmin, by=join_by(window_year == year, band == interval))
# remove NAs
avg_tmin_sync_timescale_ppt <- na.omit(avg_tmin_sync_timescale_ppt)
# make sure quantile is a factor
avg_tmin_sync_timescale_ppt$quantile <- factor(avg_tmin_sync_timescale_ppt$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_tmin_sync_ppt_quantiles_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(avg_tmin_sync_timescale_ppt$band))) {
  
  current <- unique(avg_tmin_sync_timescale_ppt$band)[xx]
  band_data <- avg_tmin_sync_timescale_ppt %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_tmin_sync_ppt_quantiles_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_tmin_sync_ppt_quantiles_results  <- do.call(rbind, t.test_tmin_sync_ppt_quantiles_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_tmin_sync_ppt_quantiles_results$Mean_Difference <- t.test_tmin_sync_ppt_quantiles_results$Mean_Difference * -1



