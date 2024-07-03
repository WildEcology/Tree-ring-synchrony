# source the data from cleaning code
source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))


#### Calculating timescale specific environmental data ####
# perform a moving window to compute the average across a window of years to
# capture a timescale-specific measure of ppt and tmin

## Precipitation ##
# data set needed = winter_ppt

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
  
  ppt_3 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  ppt_window_3 <- bind_rows(ppt_window_3, ppt_3)
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
  
  ppt_7 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  ppt_window_7 <- bind_rows(ppt_window_7, ppt_7)
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
  
  ppt_15 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  ppt_window_15 <- bind_rows(ppt_window_15, ppt_15)
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
  
  ppt_25 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  ppt_window_25 <- bind_rows(ppt_window_25, ppt_25)
}

# rbind all windows
timescale_specific_avg_ppt <- rbind(ppt_window_3, ppt_window_7, ppt_window_15, ppt_window_25) %>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_ppt = mean(window_ppt))

ts_ppt <- timescale_specific_avg_ppt %>%
  rename("year" = "window_year")
ts_ppt$driver <- "ppt"

# calculate quartiles for ppt
quantile(ts_ppt$window_avg_ppt)
ts_ppt <-  ts_ppt %>%
  mutate(quantile = case_when(window_avg_ppt <= 65.15394 ~ 0,
                              window_avg_ppt > 65.15394 & window_avg_ppt <=  112.06326 ~ 25,
                              window_avg_ppt >  112.06326 & window_avg_ppt <= 119.49155 ~ 50,
                              window_avg_ppt > 119.49155 & window_avg_ppt <= 128.02164 ~ 75,
                              window_avg_ppt > 128.02164 & window_avg_ppt <= 174.85795 ~ 100)) %>%
  rename("average" = window_avg_ppt)


## TEMPERATURE ##
# data set needed = summer_tmin

# tmin 3 years
window_length <- 3
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

# tmin 7 years
window_length <- 7
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_7 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  tmin_7 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  tmin_window_7 <- bind_rows(tmin_window_7, tmin_7)
}

# tmin 15 years
window_length <- 15
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_15 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  tmin_15 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  tmin_window_15 <- bind_rows(tmin_window_15, tmin_15)
}

# tmin 25 years
window_length <- 25
start_year <- min(summer_tmin$year) 
end_year <- max(summer_tmin$year)

tmin_window_25 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  tmin_25 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  tmin_window_25 <- bind_rows(tmin_window_25, tmin_25)
}

# rbind all the windows into one dataframe
timescale_specific_avg_tmin <- rbind(tmin_window_3, tmin_window_7, tmin_window_15, tmin_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_tmin = mean(window_tmin))

ts_tmin <- timescale_specific_avg_tmin %>%
  rename("year" = "window_year")
ts_tmin$driver <- "tmin"

# calculate quartiles for tmin
quantile(ts_tmin$window_avg_tmin)
ts_tmin <-  ts_tmin %>%
  mutate(quantile = case_when(window_avg_tmin <= 1.675428 ~ 0,
                              window_avg_tmin > 1.675428 & window_avg_tmin <=  3.216548 ~ 25,
                              window_avg_tmin >  3.216548 & window_avg_tmin <= 3.593133 ~ 50,
                              window_avg_tmin > 3.593133 & window_avg_tmin <= 4.072364 ~ 75,
                              window_avg_tmin > 4.072364 & window_avg_tmin <= 6.956917 ~ 100))%>%
  rename("average" = window_avg_tmin)

# join both datasets
ts_env_data <- full_join(ts_tmin, ts_ppt) %>%
  ungroup()%>%
  select(year,band,average,driver,quantile)

saveRDS(ts_env_data, "/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/ts_env_data.rds")
