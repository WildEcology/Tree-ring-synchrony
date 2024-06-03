source(here::here("updated_cleaning_code.R"))
source(here::here("Scripts/coh_tv.R"))

## Summer - tmin ##
# match up plots with rwi data
# remove annual measurements from dataset
tmin_dat <- na.omit(tmin_dat)
plots <- unique(rwi_00s_plot_filtered$plot)
tmin_dat <- tmin_dat %>%
  filter(plot %in% plots)

# produce water-year variable 
winter_tmin <- tmin_dat %>%
  mutate(wateryear = case_when(month == "10" ~ year+1,
                               month == "11" ~ year+1,
                               month == "12" ~ year+1,
                               TRUE ~ as.numeric(year)))

# filter for water-year months (Oct - May)
months <- c(1, 2, 3, 4, 5, 10, 11, 12)
winter_tmin <- winter_tmin %>%
  filter(month %in% months)

# calculate avg winter precip per plot per water-year
winter_tmin <- winter_tmin %>%
  group_by(plot, wateryear)%>%
  summarise(winter_tmin = sum(tmin))%>%
  filter(wateryear >= 1900)%>%
  filter(wateryear <= 2018)

winter_tmin_wide <- winter_tmin %>%
  pivot_wider(names_from = "wateryear", values_from = "winter_tmin")

# format matrix for wavelet analysis
winter_tmin_wide <- as.matrix(winter_tmin_wide)
colnames(winter_tmin_wide) <- NULL
winter_tmin_wide <- winter_tmin_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
winter_tmin_wide = as.data.frame(winter_tmin_wide, stringsAsFactors = FALSE)
winter_tmin_wide = map_df(winter_tmin_wide, as.numeric)
winter_tmin_mx <- as.matrix(winter_tmin_wide)

# clean data for wpmf
times <- 1900:2018
winter_tmin_mx <- cleandat(winter_tmin_mx, times, clev = 5)$cdat
winter_tmin_df <- as.data.frame(winter_tmin_mx)
colnames(winter_tmin_df) <- 1900:2018
winter_tmin_df <- winter_tmin_df %>%
  pivot_longer(1:119, names_to="year", values_to = "winter_tmin_cleaned")
winter_tmin_df$plot <- winter_tmin$plot

# produce wpmf for growth data
res_tmin_wpmf<-wpmf(winter_tmin_mx,times,sigmethod="none")
res_tmin_wmf<-wmf(winter_tmin_mx,times)

## Minimum summer temperatures ##
# remove annual measurements from the dataset
# match up plots with growth data
ppt_dat <- na.omit(ppt_dat)
ppt_dat <- ppt_dat %>%
  filter(plot %in% plots)

# filter for summer months (June - Aug)
months <- c(6, 7, 8)
summer_ppt <- ppt_dat %>%
  filter(month %in% months )

# calculate avg ppt in summer months
summer_ppt <- summer_ppt %>%
  group_by(plot, year)%>%
  summarise(summer_ppt = mean(ppt))%>%
  filter(year >= 1900)%>%
  filter(year <= 2018)

summer_ppt_wide <- summer_ppt %>%
  pivot_wider(names_from = "year", values_from = "summer_ppt")

# format matrix for wavelet analysis
summer_ppt_wide <- as.matrix(summer_ppt_wide)
colnames(summer_ppt_wide) <- NULL
summer_ppt_wide <- summer_ppt_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
summer_ppt_wide = as.data.frame(summer_ppt_wide, stringsAsFactors = FALSE)
summer_ppt_wide = map_df(summer_ppt_wide, as.numeric)
summer_ppt_mx <- as.matrix(summer_ppt_wide)

# clean data for wpmf
times <- 1900:2018
summer_ppt_mx <- cleandat(summer_ppt_mx, times, clev = 5)$cdat
summer_ppt_df <- as.data.frame(summer_ppt_mx)
colnames(summer_ppt_df) <- 1900:2018
summer_ppt_df <- summer_ppt_df %>%
  pivot_longer(1:119, names_to="year", values_to = "summer_ppt_cleaned")
summer_ppt_df$plot <- summer_ppt$plot

# produce wpmf for growth data
res_ppt_wpmf<-wpmf(summer_ppt_mx,times,sigmethod="none")
res_ppt_wmf<-wmf(summer_ppt_mx,times)

# test coherence

x = avg_plot_growth_mx
y1 = winter_tmin_mx
y2 = summer_ppt_mx
tv_timeseries_ppt <- coh_tv(dat1 = x, dat2 = y2, times = times, norm = "powall",
                            sigmethod = "fftsurrog1", nrand = 1000)
tv_timeseries_tmin <- coh_tv(dat1 = x, dat2 = y1, times = times, norm = "powall",
                             sigmethod = "fftsurrog1", nrand = 1000)


coh.ppt <- as.data.frame(tv_timeseries_ppt$signif$gt)
colnames(coh.ppt) <- tv_timeseries_ppt$timescales
coh.ppt$times <- tv_timeseries_ppt$times
coh.ppt <- coh.ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.ppt$ts <- as.numeric(coh.ppt$ts)
coh.ppt <- coh.ppt %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.ppt <- coh.ppt %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.ppt <- na.omit(avg.coh.ppt)  

# tmin
coh.tmin <- as.data.frame(tv_timeseries_tmin$signif$gt)
colnames(coh.tmin) <- tv_timeseries_tmin$timescales
coh.tmin$times <- tv_timeseries_tmin$times
coh.tmin <- coh.tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.tmin$ts <- as.numeric(coh.tmin$ts)
coh.tmin <- coh.tmin %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.tmin <- coh.tmin %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.tmin <- na.omit(avg.coh.tmin)  

# combine into one data frame for plotting purposes
avg.coh.ppt$driver <- "ppt"
avg.coh.tmin$driver <- "tmin"
avg.tv.coh <- rbind(avg.coh.ppt, avg.coh.tmin)
avg.tv.coh$times <- as.character(avg.tv.coh$times)
avg.tv.coh$band <- factor(avg.tv.coh$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

# plot avg coherence across time per band for each driver

labels <- c(biennial = "biennial", multiannual = "multiannual", decadal = "decadal", multidecadal = "multidecadal")
avg.tv.coh$driver <- factor(avg.tv.coh$driver, levels=c('ppt', 'tmin'))
ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
  facet_wrap(~ band, labeller=labeller(band = labels))+
  theme_bw()+
  scale_color_manual(values = c("blue","red"), labels = c("summer ppt", "winter tmin"))+
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
  ylab("Average Coherence")+
  xlab("Year")
