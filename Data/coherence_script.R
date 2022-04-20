##------------------------


# SCRIPT NAME: Coherence

# AUTHOR: Kaitlyn McKnight (University of Wyoming)

# DATE CREATED: 2022-04-14


##------------------------
# load data
prismdat <- read.csv(here("data/rwi_prismdat.csv"))

#### CLEAN DATA ####
##  RWI  ##
# subset 1900-2018
rwi_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)
# find plots and years with at least 5 trees 
rwi_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)
# subsetting only plots with at least 5 trees
rwi_00s_unfiltered_long <- left_join(rwi_5, rwi_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, value)
rwi_00s_unfiltered_wide <- rwi_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = value)
# removing any trees with missing data along the timeseries
rwi_00s_filtered_wide<- rwi_00s_unfiltered_wide[rowSums(is.na(rwi_00s_unfiltered_wide))==0,]
rwi_00s_filtered_long <- rwi_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")

##  PPT ##
# subset 1900-2018
ppt_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)
# find plots and years with at least 5 trees 
ppt_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)
# subsetting only plots with at least 5 trees
ppt_00s_unfiltered_long <- left_join(ppt_5, ppt_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, ppt)
ppt_00s_unfiltered_wide <- ppt_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = ppt)
# removing any trees with missing data along the timeseries
ppt_00s_filtered_wide<- ppt_00s_unfiltered_wide[rowSums(is.na(ppt_00s_unfiltered_wide))==0,]
ppt_00s_filtered_long <- ppt_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "ppt")

##  TMAX  ##
# subset 1900-2018
tmax_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)
# find plots and years with at least 5 trees 
tmax_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)
# subsetting only plots with at least 5 trees
tmax_00s_unfiltered_long <- left_join(tmax_5, tmax_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, tmax)
tmax_00s_unfiltered_wide <- tmax_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = tmax)
# removing any trees with missing data along the timeseries
tmax_00s_filtered_wide<- tmax_00s_unfiltered_wide[rowSums(is.na(tmax_00s_unfiltered_wide))==0,]
tmax_00s_filtered_long <- tmax_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "tmax")

#### ACROSS PLOT WAVELET ####
avg_plot_growth <- rwi_00s_filtered_long %>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

avg_plot_growth_wide <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")

# format matrix for analysis
avg_plot_growth_wide <- as.matrix(avg_plot_growth_wide)
colnames(avg_plot_growth_wide) <- NULL
avg_plot_growth_wide <- avg_plot_growth_wide[, c(2:120)] 

# convert character matrix to numeric
avg_plot_growth_wide = as.data.frame(avg_plot_growth_wide, stringsAsFactors = FALSE)
avg_plot_growth_wide = map_df(avg_plot_growth_wide, as.numeric)
avg_plot_growth_mx <- as.matrix(avg_plot_growth_wide)

times <- 1900:2018
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, 1)$cdat
res<-wpmf(avg_plot_growth_mx,times,sigmethod="quick")
plotmag(res)

#### PREPARE DATA FOR COHERENCE (PPT & TMAX) ####
# avg ppt per plot per year 
avg_plot_ppt <- ppt_00s_filtered_long %>%
  group_by(plot,year)%>%
  summarise(avg_ppt = mean(ppt))%>%
  pivot_wider(names_from = "year", values_from = "avg_ppt")

# avg tmax per plot per year
avg_plot_tmax <- tmax_00s_filtered_long %>%
  group_by(plot,year)%>%
  summarise(avg_tmax = mean(tmax))%>%
  pivot_wider(names_from = "year", values_from = "avg_tmax")

# format matrix for analysis
ppt <- as.matrix(avg_plot_ppt)
colnames(ppt) <- NULL
ppt <- ppt[, c(2:120)] 

tmax <- as.matrix(avg_plot_tmax)
colnames(tmax) <- NULL
tmax <- tmax[, c(2:120)] 

# convert character matrix to numeric
ppt = as.data.frame(ppt, stringsAsFactors = FALSE)
ppt = map_df(ppt, as.numeric)
ppt_mx <- as.matrix(ppt)

tmax = as.data.frame(tmax, stringsAsFactors = FALSE)
tmax = map_df(tmax, as.numeric)
tmax_mx <- as.matrix(tmax)

# cleaning data for coherence
tmax_mx <- cleandat(tmax_mx, times,1)$cdat
ppt_mx <- cleandat(ppt_mx, times,1)$cdat
times <- 1:119

#### COHERENCE TESTING (PPT & TMAX) ####
res_ppt <- coh(dat1 = ppt_mx, dat2=avg_plot_growth_mx, times=times,norm="powall",
               sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)
plotmag(res_ppt)

res_tmax <- coh(dat1 = tmax_mx, dat2=avg_plot_growth_mx, times=times,norm="powall",
                sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)
plotmag(res_tmax)

# from plot - investigate significance for timescales 2-5 yrs & 10-20 yrs
res_ppt<-bandtest(res_ppt,c(2,5))
res_ppt<-bandtest(res_ppt,c(10,20))
get_bandp(res_ppt)

res_tmax<-bandtest(res_tmax,c(2,5))
res_tmax<-bandtest(res_tmax,c(10,20))
get_bandp(res_tmax)

# investigate significance for timescale intervales
short <- c(2,4)
medium <- c(4,8)
long <- c(8,16)
xlong <- c(16,32)

res_ppt<-bandtest(res_ppt,short)
res_ppt<-bandtest(res_ppt,medium)
res_ppt<-bandtest(res_ppt,long)
res_ppt<-bandtest(res_ppt,xlong)
ppt_coherence <- get_bandp(res_ppt)

res_tmax<-bandtest(res_tmax,short)
res_tmax<-bandtest(res_tmax,medium)
res_tmax<-bandtest(res_tmax,long)
res_tmax<-bandtest(res_tmax,xlong)
tmax_coherence <- get_bandp(res_tmax)

ppt_coherence <- ppt_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

tmax_coherence <- tmax_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))


#### WATER YEAR COHERENCE ####

# load data
ppt_month <- read.csv(here("Data/prism_plots_1900.csv"))
ppt_month$plot <- as.character(ppt_month$plot)
ppt_month$type <- as.character(ppt_month$type)
ppt_month$year <- as.character(ppt_month$year)
ppt_month$month <- as.character(ppt_month$month)
ppt_month$value <- as.numeric(ppt_month$value)

# fix plot names in rwi data to match water year data
rwi_00s_filtered_long_update<- rwi_00s_filtered_long %>%
  mutate(plot2 = case_when(plot == "RC" ~ "RC_NA",
                           plot == "K" ~ "K_NA",
                           plot == "JM" ~ "JM_NA", 
                           TRUE ~ as.character(plot)))

# prepare water year data
match_plots_rwi <- unique(rwi_00s_filtered_long_update$plot2)

water_year_wide <- ppt_month %>%
  filter(month == c("1", "2", "3", "4", "5", "10", "11", "12"), plot %in% match_plots_rwi)%>%
  group_by(plot, year)%>%
  summarize(mean_ppt = mean(value))%>%
  pivot_wider(names_from = year, values_from = mean_ppt, id_cols = plot)

# format matrix for analysis
wateryear <- as.matrix(water_year_wide)
colnames(wateryear) <- NULL
wateryear <- wateryear[, c(2:120)] 

# convert character matrix to numeric
wateryear = as.data.frame(wateryear, stringsAsFactors = FALSE)
wateryear = map_df(wateryear, as.numeric)
wateryear_mx <- as.matrix(wateryear)

# cleaning data for coherence
times <- 1:119
wateryear_mx <- cleandat(wateryear_mx, times,1)$cdat


# filter avg plot growth data for only plots also in water year data
match_plots_wateryear <- unique(water_year_wide$plot)
avg_plot_growth_update <- rwi_00s_filtered_long_update %>%
  filter(plot2 %in% match_plots_wateryear)%>%
  group_by(plot2, year)%>%
  summarize(avg_growth = mean(rwi))
avg_plot_growth_wide_update <- avg_plot_growth_update %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")
# format matrix for analysis
avg_plot_growth_wide_update <- as.matrix(avg_plot_growth_wide_update)
colnames(avg_plot_growth_wide_update) <- NULL
avg_plot_growth_wide_update <- avg_plot_growth_wide_update[, c(2:120)] 
# convert character matrix to numeric
avg_plot_growth_wide_update = as.data.frame(avg_plot_growth_wide_update, stringsAsFactors = FALSE)
avg_plot_growth_wide_update = map_df(avg_plot_growth_wide_update, as.numeric)
avg_plot_growth_update_mx <- as.matrix(avg_plot_growth_wide_update)

avg_plot_growth_update_mx <- cleandat(avg_plot_growth_update_mx, times,1)$cdat
## Coherence test ##
res_wateryear <- coh(dat1 = wateryear_mx, dat2 = avg_plot_growth_update_mx, times=times, norm="powall",
               sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)

plotmag(res_wateryear)


# from plot - investigate significance for timescales 2-5 yrs & 10-20 yrs
res_wateryear<-bandtest(res_wateryear,c(2,5))
res_wateryear<-bandtest(res_wateryear,c(10,20))
get_bandp(res_wateryear)

# investigate significance for timescale intervales
short <- c(2,4)
medium <- c(4,8)
long <- c(8,16)
xlong <- c(16,32)

res_wateryear<-bandtest(res_wateryear,short)
res_wateryear<-bandtest(res_wateryear,medium)
res_wateryear<-bandtest(res_wateryear,long)
res_wateryear<-bandtest(res_wateryear,xlong)
wateryear_coherence <- get_bandp(res_wateryear)

wateryear_coherence <- wateryear_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))


#### PDSI COHERENCE ####
# read in data 
pdsi_data <- read.csv(here("Data/0405-pdsi-all-1-1900-2018.csv"))
pdsi_data <- pdsi_data[4:1420,]
colnames(pdsi_data) <- NULL
pdsi_columns <- c("date", "pdsi", "anomaly")
colnames(pdsi_data) <- pdsi_columns
pdsi_data$pdsi <- as.numeric(pdsi_data$pdsi)
# fix dates
pdsi_data <- pdsi_data %>%
  separate(date, into = c("year", "month"), sep = 4, remove=TRUE)

# find avg pdsi per year
avg_pdsi <- pdsi_data %>%
  group_by(year) %>%
  summarise(avg_pdsi = mean(pdsi))

# join with avg rwi dataset to create repeated matrix for each plot

repeated_pdsi <- full_join(avg_pdsi, avg_plot_growth, by = "year")
pdsi_wide <- repeated_pdsi %>%
  select(year, plot, avg_pdsi)%>%
  pivot_wider(names_from = "year", values_from = "avg_pdsi")

# format matrix for analysis
pdsi <- as.matrix(pdsi_wide)
colnames(pdsi) <- NULL
pdsi <- pdsi[, c(2:120)] 

# convert character matrix to numeric
pdsi = as.data.frame(pdsi, stringsAsFactors = FALSE)
pdsi = map_df(pdsi, as.numeric)
pdsi_mx <- as.matrix(pdsi)

# cleaning data for coherence
times <- 1:119
pdsi_mx <- cleandat(pdsi_mx, times,1)$cdat

## Coherence test ##
res_pdsi <- coh(dat1 = pdsi_mx, dat2 = avg_plot_growth_mx, times=times, norm="powall",
                     sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)

plotmag(res_pdsi)


# from plot - investigate significance for timescales 2-5 yrs & 10-20 yrs
res_pdsi<-bandtest(res_pdsi,c(2,5))
res_pdsi<-bandtest(res_pdsi,c(10,20))
get_bandp(res_pdsi)

# investigate significance for timescale intervales
short <- c(2,4)
medium <- c(4,8)
long <- c(8,16)
xlong <- c(16,32)

res_pdsi<-bandtest(res_pdsi,short)
res_pdsi<-bandtest(res_pdsi,medium)
res_pdsi<-bandtest(res_pdsi,long)
res_pdsi<-bandtest(res_pdsi,xlong)
pdsi_coherence <- get_bandp(res_pdsi)

pdsi_coherence <- pdsi_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

#### PDO COHERENCE ####
# read in data 
PDO_data <- read.table(here("Data/ersst.v5.pdo.dat.txt"), header=TRUE)

# pivot longer to summarise and find avg PDO per year
PDO_data <- PDO_data %>%
  pivot_longer(Jan:Dec, names_to = "month", values_to = "PDO")%>%
  filter(Year >= 1900)

# find avg PDO per year
avg_PDO <- PDO_data %>%
  group_by(Year) %>%
  summarise(avg_PDO = mean(PDO))

# join with avg rwi dataset to create repeated matrix for each plot
avg_PDO$Year <- as.character(avg_PDO$Year)
repeated_PDO <- full_join(avg_PDO, avg_plot_growth, by = c("Year" = "year"))
PDO_wide <- repeated_PDO %>%
  select(Year, plot, avg_PDO)%>%
  pivot_wider(names_from = "Year", values_from = "avg_PDO")

# format matrix for analysis
PDO <- as.matrix(PDO_wide)
colnames(PDO) <- NULL
PDO <- PDO[, c(2:120)] 

# convert character matrix to numeric
PDO = as.data.frame(PDO, stringsAsFactors = FALSE)
PDO = map_df(PDO, as.numeric)
PDO_mx <- as.matrix(PDO)

# cleaning data for coherence
times <- 1:119
PDO_mx <- cleandat(PDO_mx, times,1)$cdat

## Coherence test ##
res_PDO <- coh(dat1 = PDO_mx, dat2 = avg_plot_growth_mx, times=times, norm="powall",
                sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)

plotmag(res_PDO)


# from plot - investigate significance for timescales 2-5 yrs & 10-20 yrs
res_PDO<-bandtest(res_PDO,c(2,5))
res_PDO<-bandtest(res_PDO,c(10,20))
get_bandp(res_PDO)

# investigate significance for timescale intervales
short <- c(2,4)
medium <- c(4,8)
long <- c(8,16)
xlong <- c(16,32)

res_PDO<-bandtest(res_PDO,short)
res_PDO<-bandtest(res_PDO,medium)
res_PDO<-bandtest(res_PDO,long)
res_PDO<-bandtest(res_PDO,xlong)
PDO_coherence <- get_bandp(res_PDO)

PDO_coherence <- PDO_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

#### VPD COHERENCE ####
# subset 1900-2018
vpdmax_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)
# find plots and years with at least 5 trees 
vpdmax_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)
# subsetting only plots with at least 5 trees
vpdmax_00s_unfiltered_long <- left_join(vpdmax_5, vpdmax_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, vpdmax)
vpdmax_00s_unfiltered_wide <- vpdmax_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = vpdmax)
# removing any trees with missing data along the timeseries
vpdmax_00s_filtered_wide<- vpdmax_00s_unfiltered_wide[rowSums(is.na(vpdmax_00s_unfiltered_wide))==0,]
vpdmax_00s_filtered_long <- vpdmax_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "vpdmax")


avg_plot_vpdmax <- vpdmax_00s_filtered_long %>%
  group_by(plot,year)%>%
  summarise(avg_vpdmax = mean(vpdmax))%>%
  pivot_wider(names_from = "year", values_from = "avg_vpdmax")

# format matrix for analysis
vpdmax <- as.matrix(avg_plot_vpdmax)
colnames(vpdmax) <- NULL
vpdmax <- vpdmax[, c(2:120)] 

# convert character matrix to numeric
vpdmax = as.data.frame(vpdmax, stringsAsFactors = FALSE)
vpdmax = map_df(vpdmax, as.numeric)
vpdmax_mx <- as.matrix(vpdmax)

# cleaning data for coherence
vpdmax_mx <- cleandat(vpdmax_mx, times,1)$cdat
times <- 1:119

# coherence test
res_vpdmax <- coh(dat1 = vpdmax_mx, dat2=avg_plot_growth_mx, times=times,norm="powall",
               sigmethod = "fast", nrand=1000, f0=0.5, scale.max.input = 28)
plotmag(res_vpdmax)

# from plot - investigate significance for timescales 2-5 yrs & 10-20 yrs
res_vpdmax<-bandtest(res_vpdmax,c(2,5))
res_vpdmax<-bandtest(res_vpdmax,c(10,20))
get_bandp(res_vpdmax)

# investigate significance for timescale intervales
short <- c(2,4)
medium <- c(4,8)
long <- c(8,16)
xlong <- c(16,32)

res_vpdmax<-bandtest(res_vpdmax,short)
res_vpdmax<-bandtest(res_vpdmax,medium)
res_vpdmax<-bandtest(res_vpdmax,long)
res_vpdmax<-bandtest(res_vpdmax,xlong)
vpdmax_coherence <- get_bandp(res_vpdmax)


vpdmax_coherence <- vpdmax_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))


