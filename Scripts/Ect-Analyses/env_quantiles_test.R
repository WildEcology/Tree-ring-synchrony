
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

avg_ppt_sync_tmin_quant <- ggplot(data = avg_ppt_sync_timescale_tmin, aes(x = ts_tmin, y = mean.ppt.sync, col = band)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.ppt.sync, ymax = upper.ci.ppt.sync, x = ts_tmin, y=mean.ppt.sync, width = 0.2)) +
  theme_bw()+
  #facet_wrap(~band)+
  ylab("Avg PPT Sync")+
  xlab("TS TMIN")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = .5,
                                   face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = .5,
                                   face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"))



#### left off here ####



# create timescale specific temperatures for each of the four timescale bands
names(winter_ppt)[3] <- "ppt"
names(winter_ppt)[2] <- "year"
window_length <- 3
#How many years after the start year
start_year <- min(winter_ppt$year)
end_year <- max(winter_ppt$year)

ppt_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  
  ppt_3 <- winter_ppt %>% 
    filter(year %in% select_years) %>%
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  ppt_window_3 <- bind_rows(ppt_window_3, ppt_3)
}

# ppt 7 years
window_length <- 7
#How many years after the start year
start_year <- min(winter_ppt$year)
end_year <- max(winter_ppt$year)

ppt_window_7 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  ppt_7 <- winter_ppt %>% 
    filter(year %in% select_years) %>%
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  ppt_window_7 <- bind_rows(ppt_window_7, ppt_7)
}

# ppt 15 years
window_length <- 15
#How many years after the start year
start_year <- min(winter_ppt$year)
end_year <- max(winter_ppt$year)

ppt_window_15 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  ppt_15 <- winter_ppt %>% 
    filter(year %in% select_years) %>%
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  ppt_window_15 <- bind_rows(ppt_window_15, ppt_15)
}


# ppt 25 years
window_length <- 25
#How many years after the start year
start_year <- min(winter_ppt$year) 
end_year <- max(winter_ppt$year)

ppt_window_25 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  ppt_25 <- winter_ppt %>% 
    filter(year %in% select_years) %>%
    summarise(window_ppt = mean(ppt)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  ppt_window_25 <- bind_rows(ppt_window_25, ppt_25)
}

timescale_specific_avg_ppt <- rbind(ppt_window_3, ppt_window_7, ppt_window_15, ppt_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_ppt = mean(window_ppt))

# create quantiles for ppt
quantile(timescale_specific_avg_ppt$window_avg_ppt)

timescale_specific_avg_ppt <-  timescale_specific_avg_ppt %>%
  mutate(quantile = case_when(window_avg_ppt <  65.15394 ~ 0,
                              window_avg_ppt >  65.15394 & window_avg_ppt <=  112.06326 ~ 25,
                              window_avg_ppt >  112.06326 & window_avg_ppt <= 119.49155 ~ 50,
                              window_avg_ppt > 119.49155 & window_avg_ppt <= 128.02164~ 75,
                              window_avg_ppt > 128.02164  & window_avg_ppt <= 174.85795  ~ 100))


avg_env_sync_tmin <- avg_env_sync %>%
  filter(driver == "tmin")
avg_tmin_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_env_sync_tmin, by=join_by(window_year == year, band == interval))
avg_tmin_sync_timescale_ppt <- avg_tmin_sync_timescale_ppt %>%
  group_by(band, quantile)%>%
  summarise(mean.tmin.sync = mean(avg_sync, na.rm = TRUE),
            sd.tmin.sync = sd(avg_sync, na.rm = TRUE),
            n.tmin.sync = n()) %>%
  mutate(se.tmin.sync = sd.tmin.sync / sqrt(n.tmin.sync),
         lower.ci.tmin.sync = mean.tmin.sync - qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync,
         upper.ci.tmin.sync = mean.tmin.sync + qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync)
avg_tmin_sync_timescale_ppt <- na.omit(avg_tmin_sync_timescale_ppt)

avg_tmin_sync_timescale_ppt$band <- factor(avg_tmin_sync_timescale_ppt$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))

avg_tmin_sync_ppt_quant <- ggplot(data = avg_tmin_sync_timescale_ppt, aes(x = quantile, y = mean.tmin.sync, col = band)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.tmin.sync, ymax = upper.ci.tmin.sync, x = quantile, y=mean.tmin.sync, width = 0.2), position = position_dodge(width = 0.8)) +
  theme_bw()+
  #facet_wrap(~band)+
  ylab("Avg TMIN Sync")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = .5,
                                   face = "plain"),
        axis.title.x = element_blank(),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"))


avg_rwi_sync <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_rwi_sync.RDS")
avg_rwi_sync <- avg_rwi_sync %>%
  select(1:3)
names(avg_rwi_sync)[2] <- "band"
avg_rwi_sync$year <- as.numeric(avg_rwi_sync$year)
avg_rwi_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_rwi_sync, by=join_by(window_year == year))

avg_rwi_sync_timescale_ppt <- avg_rwi_sync_timescale_ppt %>%
  group_by(band.x, quantile)%>%
  summarise(mean.rwi.sync = mean(avg_sync, na.rm = TRUE),
            sd.rwi.sync = sd(avg_sync, na.rm = TRUE),
            n.rwi.sync = n()) %>%
  mutate(se.rwi.sync = sd.rwi.sync / sqrt(n.rwi.sync),
         lower.ci.rwi.sync = mean.rwi.sync - qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync,
         upper.ci.rwi.sync = mean.rwi.sync + qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync)
avg_rwi_sync_timescale_ppt <- na.omit(avg_rwi_sync_timescale_ppt)

avg_rwi_sync_timescale_ppt$band.x <- factor(avg_rwi_sync_timescale_ppt$band.x, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))

avg_rwi_sync_ppt_quant <- ggplot(data = avg_rwi_sync_timescale_ppt, aes(x = quantile, y = mean.rwi.sync, col = band.x)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.rwi.sync, ymax = upper.ci.rwi.sync, x = quantile, y=mean.rwi.sync, width = 0.2), position = position_dodge(width = 0.8)) +
  theme_bw()+
  #facet_wrap(~band)+
  ylab("Avg GROWTH Sync")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = .5,
                                   face = "plain"),
        axis.title.x = element_blank(),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"))




avg_rwi_sync_timescale_tmin <- inner_join(timescale_specific_avg_tmin, avg_rwi_sync, by=join_by(window_year == year))

avg_rwi_sync_timescale_tmin <- avg_rwi_sync_timescale_tmin %>%
  group_by(band.x, quantile)%>%
  summarise(mean.rwi.sync = mean(avg_sync, na.rm = TRUE),
            sd.rwi.sync = sd(avg_sync, na.rm = TRUE),
            n.rwi.sync = n()) %>%
  mutate(se.rwi.sync = sd.rwi.sync / sqrt(n.rwi.sync),
         lower.ci.rwi.sync = mean.rwi.sync - qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync,
         upper.ci.rwi.sync = mean.rwi.sync + qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync)
avg_rwi_sync_timescale_tmin <- na.omit(avg_rwi_sync_timescale_tmin)

avg_rwi_sync_timescale_tmin$band.x <- factor(avg_rwi_sync_timescale_tmin$band.x, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))

avg_rwi_sync_tmin_quant <- ggplot(data = avg_rwi_sync_timescale_tmin, aes(x = quantile, y = mean.rwi.sync, col = band.x)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.rwi.sync, ymax = upper.ci.rwi.sync, x = quantile, y=mean.rwi.sync, width = 0.2), position = position_dodge(width = 0.8)) +
  theme_bw()+
  #facet_wrap(~band)+
  ylab("Avg GROWTH Sync")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = .5,
                                   face = "plain"),
        axis.title.x = element_blank(),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"))
