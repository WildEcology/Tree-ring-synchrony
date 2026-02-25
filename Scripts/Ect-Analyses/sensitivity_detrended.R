#### Sensitivity analysis -- different detrending methods ####

# load necessary packages
library("tidyverse")
library("dplR")
library("wsyn")
library("here")
library("glmmTMB")
library("ggeffects")
library("purrr")
library("emmeans")
library("broom")

# load raw data & look up table
rwl_dat <- read.csv(here("Data/raw_tree_ring_data/wbp_raw_xdated_v4.csv"))  
lookup <- read.csv(here("Data/raw_tree_ring_data/wbp_tree_lookup.csv"))


# wrangle raw data column names to be able to match with lookup table plot names
rwl_dat_long <- rwl_dat %>%
  pivot_longer(2:799, names_to = "identifier", values_to = "rwl") %>%
  separate(identifier, c("plot", "tree_num"), sep = "_(?=[^_]+$)",
           remove = FALSE)

lookup <- lookup %>%
  select(plot_label_2, plot_id_needle) %>%
  distinct() %>%
  rename(plot = plot_label_2)

# join raw data with plot names from lookup table & remove NAs
rwl_dat_long <- left_join(rwl_dat_long, lookup) %>%
  na.omit()

# select only columns needed for analyses
rwl_dat_cleaned <- rwl_dat_long %>%
  select(Year, plot_id_needle, tree_num, rwl) %>%
  rename(plot = "plot_id_needle") 

# subset 1900-2018, each row represents an individual tree's data
rwl_00s <- rwl_dat_cleaned %>%
  filter(Year >= 1900)%>%
  pivot_wider(names_from = Year, values_from=rwl)

# remove any trees with missing data along the time series 
rwl_00s_tree_filtered<- rwl_00s[rowSums(is.na(rwl_00s))==0,]

# find plots with at least 5 trees with the whole time series
rwl_00s_plot_filtered <- rwl_00s_tree_filtered %>% 
  pivot_longer(3:121, names_to = "Year", values_to = "rwl")%>%
  group_by(plot) %>%
  mutate(num_trees = round(n()/118))%>%
  filter(num_trees >= 5)

# check number of trees per site to compare to original analyses
num_trees_per_plot <- rwl_00s_plot_filtered %>%
  group_by(plot) %>%
  distinct(num_trees)
mean(num_trees_per_plot$num_trees) # = 15.3 trees per site (original = 15.7 trees per site)

## detrend rwl with different approaches ##

# create tree_id variable by combining plot name with tree number and pivot wider so each tree is a column 
rwl_00s_detrending_df <- rwl_00s_plot_filtered %>%
  select(-num_trees) %>%
  unite(tree_id, 1:2, sep = "_", remove = TRUE) %>%
  pivot_wider(names_from = tree_id, values_from = rwl)

# create year as vector to add as row names to df
year <- rwl_00s_detrending_df$Year

# remove year from df
rwl_00s_detrending_df <- rwl_00s_detrending_df %>%
  select(-Year)

# make year row names to represent a timeseries for each tree
rownames(rwl_00s_detrending_df) <- year 

# single detrend with spline 50yr stiffness level
rwi_spline50 <- detrend(rwl_00s_detrending_df, method = "Spline", nyrs = 50) 

# single detrend with spline 90yr stiffness level
rwi_spline90 <- detrend(rwl_00s_detrending_df, method = "Spline", nyrs = 90) 

# double detrend with modnegexp and spline 50yr stiffness level
rwi_double50 <- detrend(detrend(rwl_00s_detrending_df, method = "ModNegExp", pos.slope = FALSE),
                       method = "Spline", nyrs = 50)

# double detrend with modnegexp and spline 90yr stiffness level
rwi_double90 <- detrend(detrend(rwl_00s_detrending_df, method = "ModNegExp", pos.slope = FALSE),
                        method = "Spline", nyrs = 90)

## average growth values across detrended values per site ##

# make rownames the year column, pivot each dataframe longer, and separate column names into plot and tree number

rwi_spline50_long <- rwi_spline50 %>%
  rownames_to_column("year") %>%
  pivot_longer(2:307, names_to = "identifier", values_to = "rwi") %>%
  separate(identifier, c("plot", "tree_num"), sep = "_(?=[^_]+$)",
           remove = FALSE)

rwi_spline90_long <- rwi_spline90 %>%
  rownames_to_column("year") %>%
  pivot_longer(2:307, names_to = "identifier", values_to = "rwi") %>%
  separate(identifier, c("plot", "tree_num"), sep = "_(?=[^_]+$)",
           remove = FALSE)

rwi_double50_long <- rwi_double50 %>%
  rownames_to_column("year") %>%
  pivot_longer(2:307, names_to = "identifier", values_to = "rwi") %>%
  separate(identifier, c("plot", "tree_num"), sep = "_(?=[^_]+$)",
           remove = FALSE)

rwi_double90_long <- rwi_double90 %>%
  rownames_to_column("year") %>%
  pivot_longer(2:307, names_to = "identifier", values_to = "rwi") %>%
  separate(identifier, c("plot", "tree_num"), sep = "_(?=[^_]+$)",
           remove = FALSE)

# group by plot and year, take average rwi, give detrended identifier for joining and plotting
# make trend dfs to plot alongside plot timeseries

avg_spline50_rwi <- rwi_spline50_long %>%
  group_by(plot, year) %>%
  summarise(avg_rwi = mean(rwi)) %>%
  mutate(detrend_method = "single_50yr")

trend_spline50_rwi <- avg_spline50_rwi %>%
  group_by(year)%>%
  summarize(trend = mean(avg_rwi)) %>%
  mutate(plot = "Trend") %>%
  mutate(detrend_method = "single_50yr")

avg_spline90_rwi <- rwi_spline90_long %>%
  group_by(plot, year) %>%
  summarise(avg_rwi = mean(rwi)) %>%
  mutate(detrend_method = "single_90yr")

trend_spline90_rwi <- avg_spline90_rwi %>%
  group_by(year)%>%
  summarize(trend = mean(avg_rwi)) %>%
  mutate(plot = "Trend") %>%
  mutate(detrend_method = "single_90yr")

avg_double50_rwi <- rwi_double50_long %>%
  group_by(plot, year) %>%
  summarise(avg_rwi = mean(rwi)) %>%
  mutate(detrend_method = "double_50yr")

trend_double50_rwi <- avg_double50_rwi %>%
  group_by(year)%>%
  summarize(trend = mean(avg_rwi)) %>%
  mutate(plot = "Trend") %>%
  mutate(detrend_method = "double_50yr")

avg_double90_rwi <- rwi_double90_long %>%
  group_by(plot, year) %>%
  summarise(avg_rwi = mean(rwi)) %>%
  mutate(detrend_method = "double_90yr")

trend_double90_rwi <- avg_double90_rwi %>%
  group_by(year)%>%
  summarize(trend = mean(avg_rwi)) %>%
  mutate(plot = "Trend") %>%
  mutate(detrend_method = "double_90yr")

# rbind datasets 
sensitivity_plot_level <- rbind(avg_spline50_rwi, avg_spline90_rwi, avg_double50_rwi, avg_double90_rwi)
sensitivity_trend <- rbind(trend_spline50_rwi, trend_spline90_rwi, trend_double50_rwi, trend_double90_rwi)

# plot all detrended timeseries, both plot level and overall trend
sensitivity_plot_level$year <- as.character(sensitivity_plot_level$year)
sensitivity_trend$year <- as.character(sensitivity_trend$year)
detrended_rwi <- ggplot()+
  geom_line(data = sensitivity_plot_level, aes(x=year, y=avg_rwi, group=plot, col=plot))+
  geom_line(data = sensitivity_trend, mapping = aes(x=year, y=trend, group = plot, col = plot), color = "black", linewidth = 1.25)+
  theme_bw()+
  scale_colour_grey(start = 1, end = 0.5)+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average Annual Growth \n(Ring Width Index)") +
  facet_wrap(~ detrend_method)

detrended_trend <- ggplot() +
  geom_line(data = sensitivity_trend, aes(x = year, y = trend, group = detrend_method, col = detrend_method)) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  xlab("Year")+
  ylab("Trend in Annual Growth \n(Average Ring Width Index)")
  
## wmfs for different detrending methods ##

# prep each detrended dataframe for wavelet analysis

# pivot averaged df wider
avg_spline50_wide <- avg_spline50_rwi %>%
  pivot_wider(names_from = "year", values_from = "avg_rwi") %>%
  select(-detrend_method)

avg_spline90_wide <- avg_spline90_rwi %>%
  pivot_wider(names_from = "year", values_from = "avg_rwi") %>%
  select(-detrend_method)

avg_double50_wide <- avg_double50_rwi %>%
  pivot_wider(names_from = "year", values_from = "avg_rwi") %>%
  select(-detrend_method)

avg_double90_wide <- avg_double90_rwi %>%
  pivot_wider(names_from = "year", values_from = "avg_rwi") %>%
  select(-detrend_method)

# format matrices for wavelet analysis
avg_spline50_wide <- as.matrix(avg_spline50_wide)
colnames(avg_spline50_wide) <- NULL # remove column names
avg_spline50_wide <- avg_spline50_wide[, c(2:120)] # time series 1900 -2018

avg_spline90_wide <- as.matrix(avg_spline90_wide)
colnames(avg_spline90_wide) <- NULL # remove column names
avg_spline90_wide <- avg_spline90_wide[, c(2:120)] # time series 1900 -2018

avg_double50_wide <- as.matrix(avg_double50_wide)
colnames(avg_double50_wide) <- NULL # remove column names
avg_double50_wide <- avg_double50_wide[, c(2:120)] # time series 1900 -2018

avg_double90_wide <- as.matrix(avg_double90_wide)
colnames(avg_double90_wide) <- NULL # remove column names
avg_double90_wide <- avg_double90_wide[, c(2:120)] # time series 1900 -2018

# convert character matrices to numeric
avg_spline50_wide = as.data.frame(avg_spline50_wide, stringsAsFactors = FALSE)
avg_spline50_wide = map_df(avg_spline50_wide, as.numeric)
avg_spline50_mx <- as.matrix(avg_spline50_wide)

avg_spline90_wide = as.data.frame(avg_spline90_wide, stringsAsFactors = FALSE)
avg_spline90_wide = map_df(avg_spline90_wide, as.numeric)
avg_spline90_mx <- as.matrix(avg_spline90_wide)

avg_double50_wide = as.data.frame(avg_double50_wide, stringsAsFactors = FALSE)
avg_double50_wide = map_df(avg_double50_wide, as.numeric)
avg_double50_mx <- as.matrix(avg_double50_wide)

avg_double90_wide = as.data.frame(avg_double90_wide, stringsAsFactors = FALSE)
avg_double90_wide = map_df(avg_double90_wide, as.numeric)
avg_double90_mx <- as.matrix(avg_double90_wide)

# clean timeseries data for wmfs
times <- 1900:2018

avg_spline50_mx <- cleandat(avg_spline50_mx, times, clev = 5)$cdat
avg_spline90_mx <- cleandat(avg_spline90_mx, times, clev = 5)$cdat
avg_double50_mx <- cleandat(avg_double50_mx, times, clev = 5)$cdat
avg_double90_mx <- cleandat(avg_double90_mx, times, clev = 5)$cdat

# calculate wmfs per detrended method

spline50_wmf<-wmf(avg_spline50_mx,times)
spline90_wmf<-wmf(avg_spline90_mx,times)
double50_wmf<-wmf(avg_double50_mx,times)
double90_wmf<-wmf(avg_double90_mx,times)

# plot wmfs
plotmag(spline50_wmf)
plotmag(spline90_wmf)
plotmag(double50_wmf)
plotmag(double90_wmf)

## average synchrony figures for different detrending methods ##


#extract raw values for each detrended wmf, add timescales as column names
spline50_wmf_values <- as.data.frame(spline50_wmf$values)
colnames(spline50_wmf_values) <- spline50_wmf$timescales

spline90_wmf_values <- as.data.frame(spline90_wmf$values)
colnames(spline90_wmf_values) <- spline90_wmf$timescales

double50_wmf_values <- as.data.frame(double50_wmf$values)
colnames(double50_wmf_values) <- double50_wmf$timescales

double90_wmf_values <- as.data.frame(double90_wmf$values)
colnames(double90_wmf_values) <- double90_wmf$timescales

# remove the imaginary #s
spline50_wmf_values <- abs(spline50_wmf_values)
spline90_wmf_values <- abs(spline90_wmf_values)
double50_wmf_values <- abs(double50_wmf_values)
double90_wmf_values <- abs(double90_wmf_values)

# define year column in each df
year <- 1900:2018

spline50_wmf_values$year <- year 
spline90_wmf_values$year <- year  
double50_wmf_values$year <- year  
double90_wmf_values$year <- year  

# pivot each df longer and remove nas
spline50_wmf_values<- spline50_wmf_values %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
spline50_wmf_values <- na.omit(spline50_wmf_values)

spline90_wmf_values<- spline90_wmf_values %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
spline90_wmf_values <- na.omit(spline90_wmf_values)

double50_wmf_values<- double50_wmf_values %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
double50_wmf_values <- na.omit(double50_wmf_values)

double90_wmf_values<- double90_wmf_values %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
double90_wmf_values <- na.omit(double90_wmf_values)

# make ts numeric and classify timescale band intervals
spline50_wmf_values$ts <- as.numeric(spline50_wmf_values$ts)
spline50_wmf_values <- spline50_wmf_values %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))

spline90_wmf_values$ts <- as.numeric(spline90_wmf_values$ts)
spline90_wmf_values <- spline90_wmf_values %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))

double50_wmf_values$ts <- as.numeric(double50_wmf_values$ts)
double50_wmf_values <- double50_wmf_values %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))

double90_wmf_values$ts <- as.numeric(double90_wmf_values$ts)
double90_wmf_values <- double90_wmf_values %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))

# find how many timescales are included in each band per year
spline50_wmf_ts_coverage <- spline50_wmf_values %>%
  group_by(year, band) %>%
  summarise(num_ts = n())

spline90_wmf_ts_coverage <- spline90_wmf_values %>%
  group_by(year, band) %>%
  summarise(num_ts = n())

double50_wmf_ts_coverage <- double50_wmf_values %>%
  group_by(year, band) %>%
  summarise(num_ts = n())

double90_wmf_ts_coverage <- double90_wmf_values %>%
  group_by(year, band) %>%
  summarise(num_ts = n())

# determine how many unique timescales are in each band
spline50_ts_counts <- spline50_wmf_values %>%
  group_by(band) %>%
  summarise(ts_num = n_distinct(ts))

spline90_ts_counts <- spline90_wmf_values %>%
  group_by(band) %>%
  summarise(ts_num = n_distinct(ts))

double50_ts_counts <- double50_wmf_values %>%
  group_by(band) %>%
  summarise(ts_num = n_distinct(ts))

double90_ts_counts <- double90_wmf_values %>%
  group_by(band) %>%
  summarise(ts_num = n_distinct(ts))

# filter for years with full timescale coverage for each band
spline50_band_coverage <- spline50_wmf_ts_coverage %>%
  left_join(spline50_ts_counts, by = "band") %>%
  filter(num_ts == ts_num) %>%
  unite("uID", year, band, remove = FALSE)

spline90_band_coverage <- spline90_wmf_ts_coverage %>%
  left_join(spline90_ts_counts, by = "band") %>%
  filter(num_ts == ts_num) %>%
  unite("uID", year, band, remove = FALSE)

double50_band_coverage <- double50_wmf_ts_coverage %>%
  left_join(double50_ts_counts, by = "band") %>%
  filter(num_ts == ts_num) %>%
  unite("uID", year, band, remove = FALSE)

double90_band_coverage <- double90_wmf_ts_coverage %>%
  left_join(double90_ts_counts, by = "band") %>%
  filter(num_ts == ts_num) %>%
  unite("uID", year, band, remove = FALSE)

# recombine and filter full dataset to only include those complete time-band combos
spline50_wmf_values <- spline50_wmf_values %>%
  dplyr::select(year, band, ts, values) %>%
  unite("uID", year, band, remove = FALSE)
spline50_filtered_timeseries <- spline50_wmf_values %>%
  filter(uID %in% spline50_band_coverage$uID)

spline90_wmf_values <- spline90_wmf_values %>%
  dplyr::select(year, band, ts, values) %>%
  unite("uID", year, band, remove = FALSE)
spline90_filtered_timeseries <- spline90_wmf_values %>%
  filter(uID %in% spline90_band_coverage$uID)

double50_wmf_values <- double50_wmf_values %>%
  dplyr::select(year, band, ts, values) %>%
  unite("uID", year, band, remove = FALSE)
double50_filtered_timeseries <- double50_wmf_values %>%
  filter(uID %in% double50_band_coverage$uID)

double90_wmf_values <- double90_wmf_values %>%
  dplyr::select(year, band, ts, values) %>%
  unite("uID", year, band, remove = FALSE)
double90_filtered_timeseries <- double90_wmf_values %>%
  filter(uID %in% double90_band_coverage$uID)

# calculate average synchrony per year and band
avg_spline50_sync <- spline50_filtered_timeseries %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))

avg_spline90_sync <- spline90_filtered_timeseries %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))

avg_double50_sync <- double50_filtered_timeseries %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))

avg_double90_sync <- double90_filtered_timeseries %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))

# scale year term for models
avg_spline50_sync$scaled_year <- scale(avg_spline50_sync$year)
avg_spline90_sync$scaled_year <- scale(avg_spline90_sync$year)
avg_double50_sync$scaled_year <- scale(avg_double50_sync$year)
avg_double90_sync$scaled_year <- scale(avg_double90_sync$year)

# round scaled year term to join original dataset with predicted dataset by x (aka. scaled_year or year)
avg_spline50_sync$x <- as.numeric(round(avg_spline50_sync$scaled_year,2))
avg_spline90_sync$x <- as.numeric(round(avg_spline90_sync$scaled_year,2))
avg_double50_sync$x <- as.numeric(round(avg_double50_sync$scaled_year,2))
avg_double90_sync$x <- as.numeric(round(avg_double90_sync$scaled_year,2))

# separate out the biennial band
avg_spline50_sync_biennial <- avg_spline50_sync %>%
  filter(band == "biennial")

avg_spline90_sync_biennial <- avg_spline90_sync %>%
  filter(band == "biennial")

avg_double50_sync_biennial <- avg_double50_sync %>%
  filter(band == "biennial")

avg_double90_sync_biennial <- avg_double90_sync %>%
  filter(band == "biennial")

# test null, linear, and quadratic models
spline50_b_null_model <- glmmTMB(avg_sync~1, data = avg_spline50_sync_biennial)
spline50_b_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline50_sync_biennial)
spline50_b_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline50_sync_biennial)

spline90_b_null_model <- glmmTMB(avg_sync~1, data = avg_spline90_sync_biennial)
spline90_b_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline90_sync_biennial)
spline90_b_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline90_sync_biennial)

double50_b_null_model <- glmmTMB(avg_sync~1, data = avg_double50_sync_biennial)
double50_b_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double50_sync_biennial)
double50_b_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double50_sync_biennial)

double90_b_null_model <- glmmTMB(avg_sync~1, data = avg_double90_sync_biennial)
double90_b_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double90_sync_biennial)
double90_b_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double90_sync_biennial)

# AIC comparison
spline50_b_aic <- AIC(spline50_b_null_model, spline50_b_linear_model, spline50_b_quad_model)
# spline50 = quadratic model fit best
spline90_b_aic <- AIC(spline90_b_null_model, spline90_b_linear_model, spline90_b_quad_model)
# spline90 = quadratic model fit best
double50_b_aic <- AIC(double50_b_null_model, double50_b_linear_model, double50_b_quad_model)
# double50 = quadratic model fit best
double90_b_aic <- AIC(double90_b_null_model, double90_b_linear_model, double90_b_quad_model)
# double90 = quadratic model fit best

# repeat for multiannual band
avg_spline50_sync_multiannual <- avg_spline50_sync %>%
  filter(band == "multiannual")

avg_spline90_sync_multiannual <- avg_spline90_sync %>%
  filter(band == "multiannual")

avg_double50_sync_multiannual <- avg_double50_sync %>%
  filter(band == "multiannual")

avg_double90_sync_multiannual <- avg_double90_sync %>%
  filter(band == "multiannual")

# test null, linear, and quadratic models
spline50_ma_null_model <- glmmTMB(avg_sync~1, data = avg_spline50_sync_multiannual)
spline50_ma_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline50_sync_multiannual)
spline50_ma_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline50_sync_multiannual)

spline90_ma_null_model <- glmmTMB(avg_sync~1, data = avg_spline90_sync_multiannual)
spline90_ma_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline90_sync_multiannual)
spline90_ma_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline90_sync_multiannual)

double50_ma_null_model <- glmmTMB(avg_sync~1, data = avg_double50_sync_multiannual)
double50_ma_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double50_sync_multiannual)
double50_ma_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double50_sync_multiannual)

double90_ma_null_model <- glmmTMB(avg_sync~1, data = avg_double90_sync_multiannual)
double90_ma_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double90_sync_multiannual)
double90_ma_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double90_sync_multiannual)

# AIC comparison
spline50_ma_aic <- AIC(spline50_ma_null_model, spline50_ma_linear_model, spline50_ma_quad_model)
# spline50 = quadratic fits best
spline90_ma_aic <- AIC(spline90_ma_null_model, spline90_ma_linear_model, spline90_ma_quad_model)
# spline90 = quadratic fits best
double50_ma_aic <- AIC(double50_ma_null_model, double50_ma_linear_model, double50_ma_quad_model)
# double50 = quadratic fits best
double90_ma_aic <- AIC(double90_ma_null_model, double90_ma_linear_model, double90_ma_quad_model)
# double90 = quadratic fits best

# repeat for decadal band
avg_spline50_sync_decadal <- avg_spline50_sync %>%
  filter(band == "decadal")

avg_spline90_sync_decadal <- avg_spline90_sync %>%
  filter(band == "decadal")

avg_double50_sync_decadal <- avg_double50_sync %>%
  filter(band == "decadal")

avg_double90_sync_decadal <- avg_double90_sync %>%
  filter(band == "decadal")

# test null, linear, and quadratic models
spline50_d_null_model <- glmmTMB(avg_sync~1, data = avg_spline50_sync_decadal)
spline50_d_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline50_sync_decadal)
spline50_d_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline50_sync_decadal)

spline90_d_null_model <- glmmTMB(avg_sync~1, data = avg_spline90_sync_decadal)
spline90_d_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline90_sync_decadal)
spline90_d_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline90_sync_decadal)

double50_d_null_model <- glmmTMB(avg_sync~1, data = avg_double50_sync_decadal)
double50_d_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double50_sync_decadal)
double50_d_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double50_sync_decadal)

double90_d_null_model <- glmmTMB(avg_sync~1, data = avg_double90_sync_decadal)
double90_d_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double90_sync_decadal)
double90_d_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double90_sync_decadal)

# AIC comparison
spline50_d_aic <- AIC(spline50_d_null_model, spline50_d_linear_model, spline50_d_quad_model)
# spline50 = quadratic fits best
spline90_d_aic <- AIC(spline90_d_null_model, spline90_d_linear_model, spline90_d_quad_model)
# spline90 = quadratic fits best
double50_d_aic <- AIC(double50_d_null_model, double50_d_linear_model, double50_d_quad_model)
# double50 = quadratic fits best
double90_d_aic <- AIC(double90_d_null_model, double90_d_linear_model, double90_d_quad_model)
# double90 = quadratic fits best


# repeat for decadal band
avg_spline50_sync_multidecadal <- avg_spline50_sync %>%
  filter(band == "multidecadal")

avg_spline90_sync_multidecadal <- avg_spline90_sync %>%
  filter(band == "multidecadal")

avg_double50_sync_multidecadal <- avg_double50_sync %>%
  filter(band == "multidecadal")

avg_double90_sync_multidecadal <- avg_double90_sync %>%
  filter(band == "multidecadal")

# test null, linear, and quadratic models
spline50_md_null_model <- glmmTMB(avg_sync~1, data = avg_spline50_sync_multidecadal)
spline50_md_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline50_sync_multidecadal)
spline50_md_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline50_sync_multidecadal)

spline90_md_null_model <- glmmTMB(avg_sync~1, data = avg_spline90_sync_multidecadal)
spline90_md_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_spline90_sync_multidecadal)
spline90_md_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_spline90_sync_multidecadal)

double50_md_null_model <- glmmTMB(avg_sync~1, data = avg_double50_sync_multidecadal)
double50_md_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double50_sync_multidecadal)
double50_md_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double50_sync_multidecadal)

double90_md_null_model <- glmmTMB(avg_sync~1, data = avg_double90_sync_multidecadal)
double90_md_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_double90_sync_multidecadal)
double90_md_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_double90_sync_multidecadal)

# AIC comparison
spline50_md_aic <- AIC(spline50_md_null_model, spline50_md_linear_model, spline50_md_quad_model)
# spline50 = quadratic fits best
spline90_md_aic <- AIC(spline90_md_null_model, spline90_md_linear_model, spline90_md_quad_model)
# spline90 = quadratic fits best
double50_md_aic <- AIC(double50_md_null_model, double50_md_linear_model, double50_md_quad_model)
# double50 = quadratic fits best
double90_md_aic <- AIC(double90_md_null_model, double90_md_linear_model, double90_md_quad_model)
# double90 = quadratic fits best

# predict precipitation synchrony using best fit models
spline50_b_vis_prod <- ggpredict(spline50_b_quad_model, terms = c("scaled_year[all]"), 
                            type = "fixed", ci_level = 0.95)
spline50_b_vis_prod$band <- "biennial"
spline50_ma_vis_prod <- ggpredict(spline50_ma_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
spline50_ma_vis_prod$band <- "multiannual"
spline50_d_vis_prod <- ggpredict(spline50_d_quad_model, terms = c("scaled_year[all]"), 
                                  type = "fixed", ci_level = 0.95)
spline50_d_vis_prod$band <- "decadal"
spline50_md_vis_prod <- ggpredict(spline50_md_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
spline50_md_vis_prod$band <- "multidecadal"


spline90_b_vis_prod <- ggpredict(spline90_b_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
spline90_b_vis_prod$band <- "biennial"
spline90_ma_vis_prod <- ggpredict(spline90_ma_quad_model, terms = c("scaled_year[all]"), 
                                  type = "fixed", ci_level = 0.95)
spline90_ma_vis_prod$band <- "multiannual"
spline90_d_vis_prod <- ggpredict(spline90_d_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
spline90_d_vis_prod$band <- "decadal"
spline90_md_vis_prod <- ggpredict(spline90_md_quad_model, terms = c("scaled_year[all]"), 
                                  type = "fixed", ci_level = 0.95)
spline90_md_vis_prod$band <- "multidecadal"


double50_b_vis_prod <- ggpredict(double50_b_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
double50_b_vis_prod$band <- "biennial"
double50_ma_vis_prod <- ggpredict(double50_ma_quad_model, terms = c("scaled_year[all]"), 
                                  type = "fixed", ci_level = 0.95)
double50_ma_vis_prod$band <- "multiannual"
double50_d_vis_prod <- ggpredict(double50_d_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
double50_d_vis_prod$band <- "decadal"
double50_md_vis_prod <- ggpredict(double50_md_quad_model, terms = c("scaled_year[all]"), 
                                  type = "fixed", ci_level = 0.95)
double50_md_vis_prod$band <- "multidecadal"


double90_b_vis_prod <- ggpredict(double90_b_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
double90_b_vis_prod$band <- "biennial"
double90_ma_vis_prod <- ggpredict(double90_ma_quad_model, terms = c("scaled_year[all]"), 
                                  type = "fixed", ci_level = 0.95)
double90_ma_vis_prod$band <- "multiannual"
double90_d_vis_prod <- ggpredict(double90_d_quad_model, terms = c("scaled_year[all]"), 
                                 type = "fixed", ci_level = 0.95)
double90_d_vis_prod$band <- "decadal"
double90_md_vis_prod <- ggpredict(double90_md_quad_model, terms = c("scaled_year[all]"), 
                                  type = "fixed", ci_level = 0.95)
double90_md_vis_prod$band <- "multidecadal"

# bind all predicted outputs
predicted_avg_spline50_sync <- rbind(spline50_b_vis_prod, spline50_ma_vis_prod, spline50_d_vis_prod, spline50_md_vis_prod)
predicted_avg_spline90_sync <- rbind(spline90_b_vis_prod, spline90_ma_vis_prod, spline90_d_vis_prod, spline90_md_vis_prod)
predicted_avg_double50_sync <- rbind(double50_b_vis_prod, double50_ma_vis_prod, double50_d_vis_prod, double50_md_vis_prod)
predicted_avg_double90_sync <- rbind(double90_b_vis_prod, double90_ma_vis_prod, double90_d_vis_prod, double90_md_vis_prod)

# join raw and predicted 
final_avg_spline50_sync <- inner_join(avg_spline50_sync, predicted_avg_spline50_sync, by=join_by(band, x))
final_avg_spline90_sync <- inner_join(avg_spline90_sync, predicted_avg_spline90_sync, by=join_by(band, x))
final_avg_double50_sync <- inner_join(avg_double50_sync, predicted_avg_double50_sync, by=join_by(band, x))
final_avg_double90_sync <- inner_join(avg_double90_sync, predicted_avg_double90_sync, by=join_by(band, x))

# make year a character for plotting
final_avg_spline50_sync$year <- as.character(final_avg_spline50_sync$year)
final_avg_spline90_sync$year <- as.character(final_avg_spline90_sync$year)
final_avg_double50_sync$year <- as.character(final_avg_double50_sync$year)
final_avg_double90_sync$year <- as.character(final_avg_double90_sync$year)

# factorize bands in timescale order: biennial, multiannual, decadal, multidecadal
final_avg_spline50_sync$band <- factor(final_avg_spline50_sync$band, 
                                     levels = c("biennial", "multiannual", "decadal", "multidecadal"))
final_avg_spline90_sync$band <- factor(final_avg_spline90_sync$band, 
                                       levels = c("biennial", "multiannual", "decadal", "multidecadal"))
final_avg_double50_sync$band <- factor(final_avg_double50_sync$band, 
                                       levels = c("biennial", "multiannual", "decadal", "multidecadal"))
final_avg_double90_sync$band <- factor(final_avg_double90_sync$band, 
                                       levels = c("biennial", "multiannual", "decadal", "multidecadal"))

# plot average growth synchrony, both raw and predicted values
avg_spline50_sync_plot <- ggplot() +
  geom_point(data = final_avg_spline50_sync, aes(x=year, y=avg_sync, col=band), alpha = 0.4) +
  geom_line(data = final_avg_spline50_sync, aes(x=year, y=predicted, group=band, col=band), linewidth = 1) +
  geom_ribbon(data = final_avg_spline50_sync, aes(
    x = year,
    y = predicted,
    alpha = 0.4,
    group = band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    show.legend = FALSE) +
  theme_bw() +
  scale_x_discrete(breaks = seq(1920, 2020, 20)) +
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    ),
    labels = c(
      "biennial" = "Biennial (2-3 yrs)",
      "multiannual" = "Multiannual (3-10 yrs)",
      "decadal" = "Decadal (10-20 yrs)",
      "multidecadal" = "Multidecadal (20-30 yrs)"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  theme(
    axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey20", size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Average Growth Synchrony") +
  xlab("Year") +
  ggtitle("Spline50")

avg_spline90_sync_plot <- ggplot() +
  geom_point(data = final_avg_spline90_sync, aes(x=year, y=avg_sync, col=band), alpha = 0.4) +
  geom_line(data = final_avg_spline90_sync, aes(x=year, y=predicted, group=band, col=band), linewidth = 1) +
  geom_ribbon(data = final_avg_spline90_sync, aes(
    x = year,
    y = predicted,
    alpha = 0.4,
    group = band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    show.legend = FALSE) +
  theme_bw() +
  scale_x_discrete(breaks = seq(1920, 2020, 20)) +
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    ),
    labels = c(
      "biennial" = "Biennial (2-3 yrs)",
      "multiannual" = "Multiannual (3-10 yrs)",
      "decadal" = "Decadal (10-20 yrs)",
      "multidecadal" = "Multidecadal (20-30 yrs)"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  theme(
    axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey20", size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Average Growth Synchrony") +
  xlab("Year") +
  ggtitle("Spline90")

avg_double50_sync_plot <- ggplot() +
  geom_point(data = final_avg_double50_sync, aes(x=year, y=avg_sync, col=band), alpha = 0.4) +
  geom_line(data = final_avg_double50_sync, aes(x=year, y=predicted, group=band, col=band), linewidth = 1) +
  geom_ribbon(data = final_avg_double50_sync, aes(
    x = year,
    y = predicted,
    alpha = 0.4,
    group = band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    show.legend = FALSE) +
  theme_bw() +
  scale_x_discrete(breaks = seq(1920, 2020, 20)) +
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    ),
    labels = c(
      "biennial" = "Biennial (2-3 yrs)",
      "multiannual" = "Multiannual (3-10 yrs)",
      "decadal" = "Decadal (10-20 yrs)",
      "multidecadal" = "Multidecadal (20-30 yrs)"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  theme(
    axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey20", size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Average Growth Synchrony") +
  xlab("Year") +
  ggtitle("Double50")


avg_double90_sync_plot <- ggplot() +
  geom_point(data = final_avg_double90_sync, aes(x=year, y=avg_sync, col=band), alpha = 0.4) +
  geom_line(data = final_avg_double90_sync, aes(x=year, y=predicted, group=band, col=band), linewidth = 1) +
  geom_ribbon(data = final_avg_double90_sync, aes(
    x = year,
    y = predicted,
    alpha = 0.4,
    group = band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    show.legend = FALSE) +
  theme_bw() +
  scale_x_discrete(breaks = seq(1920, 2020, 20)) +
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    ),
    labels = c(
      "biennial" = "Biennial (2-3 yrs)",
      "multiannual" = "Multiannual (3-10 yrs)",
      "decadal" = "Decadal (10-20 yrs)",
      "multidecadal" = "Multidecadal (20-30 yrs)"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  theme(
    axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey20", size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Average Growth Synchrony") +
  xlab("Year") +
  ggtitle("Double90")


## use emmeans to compare min and max years of different detrended synchrony data ##
# extract the min and max years and corresponding scaled_year values for each band
spline50_extremes <- final_avg_spline50_sync %>%
  group_by(band) %>%
  filter(year == max(year) | year == min(year)) %>%
  arrange(band, year) %>%
  dplyr::select(band, year, scaled_year)

spline90_extremes <- final_avg_spline90_sync %>%
  group_by(band) %>%
  filter(year == max(year) | year == min(year)) %>%
  arrange(band, year) %>%
  dplyr::select(band, year, scaled_year)

double50_extremes <- final_avg_double50_sync %>%
  group_by(band) %>%
  filter(year == max(year) | year == min(year)) %>%
  arrange(band, year) %>%
  dplyr::select(band, year, scaled_year)

double90_extremes <- final_avg_double90_sync %>%
  group_by(band) %>%
  filter(year == max(year) | year == min(year)) %>%
  arrange(band, year) %>%
  dplyr::select(band, year, scaled_year)

# create named list of best fit models per band (all quadratic)
spline50_band_models <- list(
  biennial = spline50_b_quad_model,
  multiannual = spline50_ma_quad_model,
  decadal = spline50_d_quad_model,
  multidecadal = spline50_md_quad_model
)

spline90_band_models <- list(
  biennial = spline90_b_quad_model,
  multiannual = spline90_ma_quad_model,
  decadal = spline90_d_quad_model,
  multidecadal = spline90_md_quad_model
)

double50_band_models <- list(
  biennial = double50_b_quad_model,
  multiannual = double50_ma_quad_model,
  decadal = double50_d_quad_model,
  multidecadal = double50_md_quad_model
)

double90_band_models <- list(
  biennial = double90_b_quad_model,
  multiannual = double90_ma_quad_model,
  decadal = double90_d_quad_model,
  multidecadal = double90_md_quad_model
)

# loop and return contrasts for each band
spline50_contrast_summary <- map_dfr(names(spline50_band_models), function(band_name) {
  
  # subset the extremes for each band and ensure chronological order
  spline50_band_extremes <- spline50_extremes %>%
    filter(band == band_name) %>%
    arrange(year)
  
  year_min <- spline50_band_extremes$year[1]
  year_max <- spline50_band_extremes$year[2]
  
  scaled_min <- spline50_band_extremes$scaled_year[1]
  scaled_max <- spline50_band_extremes$scaled_year[2]
  
  # get the appropriate model
  model <- spline50_band_models[[band_name]]
  
  # estimate contrast between both time points and return outputs as a dataframe
  em_res <- emmeans(model, ~ scaled_year, at = list(scaled_year = c(scaled_min, scaled_max)))
  
  spline50_contrast <- contrast(em_res, method = list("end - start" = c(-1, 1))) %>%
    tidy() %>%
    mutate(
      band = band_name,
      year_min = year_min,
      year_max = year_max,
      direction = case_when(
        estimate > 0 ~ "increasing",
        estimate < 0 ~ "decreasing",
        TRUE ~ "no change"
      )
    ) %>%
    dplyr::select(band, year_min, year_max, estimate, std.error, statistic, p.value, direction)
  
  return(spline50_contrast)
})

spline90_contrast_summary <- map_dfr(names(spline90_band_models), function(band_name) {
  
  # subset the extremes for each band and ensure chronological order
  spline90_band_extremes <- spline90_extremes %>%
    filter(band == band_name) %>%
    arrange(year)
  
  year_min <- spline90_band_extremes$year[1]
  year_max <- spline90_band_extremes$year[2]
  
  scaled_min <- spline90_band_extremes$scaled_year[1]
  scaled_max <- spline90_band_extremes$scaled_year[2]
  
  # get the appropriate model
  model <- spline90_band_models[[band_name]]
  
  # estimate contrast between both time points and return outputs as a dataframe
  em_res <- emmeans(model, ~ scaled_year, at = list(scaled_year = c(scaled_min, scaled_max)))
  
  spline90_contrast <- contrast(em_res, method = list("end - start" = c(-1, 1))) %>%
    tidy() %>%
    mutate(
      band = band_name,
      year_min = year_min,
      year_max = year_max,
      direction = case_when(
        estimate > 0 ~ "increasing",
        estimate < 0 ~ "decreasing",
        TRUE ~ "no change"
      )
    ) %>%
    dplyr::select(band, year_min, year_max, estimate, std.error, statistic, p.value, direction)
  
  return(spline90_contrast)
})

# loop and return contrasts for each band
double50_contrast_summary <- map_dfr(names(double50_band_models), function(band_name) {
  
  # subset the extremes for each band and ensure chronological order
  double50_band_extremes <- double50_extremes %>%
    filter(band == band_name) %>%
    arrange(year)
  
  year_min <- double50_band_extremes$year[1]
  year_max <- double50_band_extremes$year[2]
  
  scaled_min <- double50_band_extremes$scaled_year[1]
  scaled_max <- double50_band_extremes$scaled_year[2]
  
  # get the appropriate model
  model <- double50_band_models[[band_name]]
  
  # estimate contrast between both time points and return outputs as a dataframe
  em_res <- emmeans(model, ~ scaled_year, at = list(scaled_year = c(scaled_min, scaled_max)))
  
  double50_contrast <- contrast(em_res, method = list("end - start" = c(-1, 1))) %>%
    tidy() %>%
    mutate(
      band = band_name,
      year_min = year_min,
      year_max = year_max,
      direction = case_when(
        estimate > 0 ~ "increasing",
        estimate < 0 ~ "decreasing",
        TRUE ~ "no change"
      )
    ) %>%
    dplyr::select(band, year_min, year_max, estimate, std.error, statistic, p.value, direction)
  
  return(double50_contrast)
})

# loop and return contrasts for each band
double90_contrast_summary <- map_dfr(names(double90_band_models), function(band_name) {
  
  # subset the extremes for each band and ensure chronological order
  double90_band_extremes <- double90_extremes %>%
    filter(band == band_name) %>%
    arrange(year)
  
  year_min <- double90_band_extremes$year[1]
  year_max <- double90_band_extremes$year[2]
  
  scaled_min <- double90_band_extremes$scaled_year[1]
  scaled_max <- double90_band_extremes$scaled_year[2]
  
  # get the appropriate model
  model <- double90_band_models[[band_name]]
  
  # estimate contrast between both time points and return outputs as a dataframe
  em_res <- emmeans(model, ~ scaled_year, at = list(scaled_year = c(scaled_min, scaled_max)))
  
  double90_contrast <- contrast(em_res, method = list("end - start" = c(-1, 1))) %>%
    tidy() %>%
    mutate(
      band = band_name,
      year_min = year_min,
      year_max = year_max,
      direction = case_when(
        estimate > 0 ~ "increasing",
        estimate < 0 ~ "decreasing",
        TRUE ~ "no change"
      )
    ) %>%
    dplyr::select(band, year_min, year_max, estimate, std.error, statistic, p.value, direction)
  
  return(double90_contrast)
})
