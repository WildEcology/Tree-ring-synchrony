source(here::here("updated_cleaning_code.R"))


# produce water-year variable 
wateryear_tmin <- tmin_dat %>%
  mutate(wateryear = case_when(month == "10" ~ year+1,
                               month == "11" ~ year+1,
                               month == "12" ~ year+1,
                               TRUE ~ as.numeric(year)))

# filter for water-year months (Oct - May)
months <- c(1, 2, 3, 4, 5, 10, 11, 12)
wateryear_tmin <- wateryear_tmin %>%
  filter(month %in% months)

# calculate avg tmin per plot per water-year
wateryear_tmin <- wateryear_tmin %>%
  group_by(plot, wateryear)%>%
  summarise(tmin = mean(tmin))%>%
  filter(wateryear >= 1900)%>%
  filter(wateryear <= 2018)

# find quantiles of wateryear tmin
wateryear_tmin <- wateryear_tmin %>% mutate(quantile = ntile(tmin, 5)) %>%
  rename("year" = "wateryear")

# create timescale specific temperatures for each of the four timescale bands
window_length <- 3
#How many years after the start year
start_year <- min(wateryear_tmin$year)
end_year <- max(wateryear_tmin$year)

tmin_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  
  tmin_3 <- wateryear_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(tmin)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  tmin_window_3 <- bind_rows(tmin_window_3, tmin_3)
}

# tmin 7 years
window_length <- 7
#How many years after the start year
start_year <- min(wateryear_tmin$year)
end_year <- max(wateryear_tmin$year)

tmin_window_7 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  tmin_7 <- wateryear_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(tmin)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  tmin_window_7 <- bind_rows(tmin_window_7, tmin_7)
}

# tmin 15 years
window_length <- 15
#How many years after the start year
start_year <- min(wateryear_tmin$year)
end_year <- max(wateryear_tmin$year)

tmin_window_15 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  tmin_15 <- wateryear_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(tmin)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  tmin_window_15 <- bind_rows(tmin_window_15, tmin_15)
}


# tmin 25 years
window_length <- 25
#How many years after the start year
start_year <- min(wateryear_tmin$year) 
end_year <- max(wateryear_tmin$year)

tmin_window_25 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  #VR Calcs
  tmin_25 <- wateryear_tmin %>% 
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

# create quantiles for tmin
quantile(timescale_specific_avg_tmin$window_avg_tmin)

timescale_specific_avg_tmin <-  timescale_specific_avg_tmin %>%
  mutate(quantile = case_when(window_avg_tmin < -9.215773 ~ 0,
                                window_avg_tmin > -9.215773 & window_avg_tmin <=  -7.922993 ~ 25,
                                window_avg_tmin >  -7.922993 & window_avg_tmin <= -7.660335 ~ 50,
                                window_avg_tmin > -7.660335 & window_avg_tmin <= -7.019134 ~ 75,
                                window_avg_tmin > -7.019134 & window_avg_tmin <= -4.988054 ~ 100))


avg_env_sync <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_env_sync.RDS")

avg_env_sync_timescale_env <- inner_join(timescale_specific_avg_tmin, avg_env_sync, by=join_by(window_year == year, band == interval))
avg_env_sync_timescale_env_ppt <- avg_env_sync_timescale_env %>%
  filter(driver == "ppt") %>%
  group_by(band, quantile)%>%
  summarise(mean.ppt.sync = mean(avg_sync, na.rm = TRUE),
            sd.ppt.sync = sd(avg_sync, na.rm = TRUE),
            n.ppt.sync = n()) %>%
  mutate(se.ppt.sync = sd.ppt.sync / sqrt(n.ppt.sync),
         lower.ci.ppt.sync = mean.ppt.sync - qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync,
         upper.ci.ppt.sync = mean.ppt.sync + qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync)


avg_ppt_sync_tmin_quant <- ggplot(data = avg_env_sync_timescale_env_ppt, aes(x = quantile, y = mean.ppt.sync)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower.ci.ppt.sync, ymax = upper.ci.ppt.sync, x = quantile, y=mean.ppt.sync, width = 0.2), position = position_dodge(width = 0.8)) +
  theme_bw()+
  facet_wrap(~band)+
  ylab("Avg PPT Sync")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = .5,
                                   face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"))
