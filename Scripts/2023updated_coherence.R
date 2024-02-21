# run updated_cleaning_code script to pull in cleaned data
source(here::here("updated_cleaning_code.R"))
source(here::here("Scripts/coh_tv.R"))

#### Full Time Series ##########################################################
# calculate coherence of each predictor across whole time series
x = avg_plot_growth_mx
y1 = winter_ppt_mx
y2 = summer_tmin_mx
y3 = avg_vpdmax_mx
times = 1900:2018

res_timeseries_ppt <- coh(dat1 = x, dat2 = y1, times = times, norm = "powall",
                          sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                          scale.max.input = 50)
res_timeseries_tmin <- coh(dat1 = x, dat2 = y2, times = times, norm = "powall",
                          sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                          scale.max.input = 50)
res_timeseries_vpdmax <- coh(dat1 = x, dat2 = y3, times = times, norm = "powall",
                          sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                          scale.max.input = 50)

# band test each variable for significant coherence at different timescale bands
annual <- c(2,3)
interannual <- c(3,10)
decadal <- c(8,12)
multidecadal <- c(20,30)

# PPT
res_bt_ppt_a<-bandtest(res_timeseries_ppt,annual)
res_bt_ppt_ia<-bandtest(res_timeseries_ppt,interannual)
res_bt_ppt_d<-bandtest(res_timeseries_ppt,decadal)
res_bt_ppt_md<-bandtest(res_timeseries_ppt,multidecadal)
ppt_coherence <- rbind(get_bandp(res_bt_ppt_a),get_bandp(res_bt_ppt_ia), 
                       get_bandp(res_bt_ppt_d),get_bandp(res_bt_ppt_md))

ppt_coherence <- ppt_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

# TMIN
res_bt_tmin_a<-bandtest(res_timeseries_tmin,annual)
res_bt_tmin_ia<-bandtest(res_timeseries_tmin,interannual)
res_bt_tmin_d<-bandtest(res_timeseries_tmin,decadal)
res_bt_tmin_md<-bandtest(res_timeseries_tmin,multidecadal)
tmin_coherence <- rbind(get_bandp(res_bt_tmin_a),get_bandp(res_bt_tmin_ia), 
                       get_bandp(res_bt_tmin_d),get_bandp(res_bt_tmin_md))

tmin_coherence <- tmin_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

# VPD
res_bt_vpdmax_a<-bandtest(res_timeseries_vpdmax,annual)
res_bt_vpdmax_ia<-bandtest(res_timeseries_vpdmax,interannual)
res_bt_vpdmax_d<-bandtest(res_timeseries_vpdmax,decadal)
res_bt_vpdmax_md<-bandtest(res_timeseries_vpdmax,multidecadal)
vpdmax_coherence <- rbind(get_bandp(res_bt_vpdmax_a),get_bandp(res_bt_vpdmax_ia), 
                       get_bandp(res_bt_vpdmax_d),get_bandp(res_bt_vpdmax_md))

vpdmax_coherence <- vpdmax_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))


# calculate synchrony explained by each variable across the entire time series for each band
# input all three predictors into a wavelet linear model
env_all<- list(avg_plot_growth_mx, winter_ppt_mx, summer_tmin_mx, avg_vpdmax_mx)
wlm_all<-wlm(dat = env_all,times = times, resp=1,pred=2:4,norm="powall",scale.max.input=50)
se_all <- syncexpl(wlm_all)

# calculate synchrony explained by ppt only across the entire time series for each band
# input ppt into a wavelet linear model
env_ppt <- list(avg_plot_growth_mx, winter_ppt_mx)
wlm_ppt<-wlm(dat = env_ppt,times = times, resp=1,pred=2,norm="powall",scale.max.input=50)
se_ppt <- syncexpl(wlm_ppt)

# ANNUAL 2-3 yr timescales
se_all_annual<-se_all[se_all$timescales>=annual[1] & se_all$timescales<=annual[2],] 
se_all_annual <- as.data.frame(round(100*colMeans(se_all_annual[,c(3:12)])/mean(se_all_annual$sync),4))
names(se_all_annual) <- NULL

se_ppt_annual<-se_ppt[se_ppt$timescales>=annual[1] & se_ppt$timescales<=annual[2],] 
se_ppt_annual <- as.data.frame(round(100*colMeans(se_ppt_annual[,c(3:6)])/mean(se_ppt_annual$sync),4))
names(se_ppt_annual) <- NULL

# INTERANNUAL 3-10 yr timescales
se_all_interannual<-se_all[se_all$timescales>=interannual[1] & se_all$timescales<=interannual[2],] 
se_all_interannual <- as.data.frame(round(100*colMeans(se_all_interannual[,c(3:12)])/mean(se_all_interannual$sync),4))
names(se_all_interannual) <- NULL

se_ppt_interannual<-se_ppt[se_ppt$timescales>=interannual[1] & se_ppt$timescales<=interannual[2],] 
se_ppt_interannual <- as.data.frame(round(100*colMeans(se_ppt_interannual[,c(3:6)])/mean(se_ppt_interannual$sync),4))
names(se_ppt_interannual) <- NULL

# DECADAL 10-20 yr timescales
se_all_decadal<-se_all[se_all$timescales>=decadal[1] & se_all$timescales<=decadal[2],] 
se_all_decadal <- as.data.frame(round(100*colMeans(se_all_decadal[,c(3:12)])/mean(se_all_decadal$sync),4))
names(se_all_decadal) <- NULL

se_ppt_decadal<-se_ppt[se_ppt$timescales>=decadal[1] & se_ppt$timescales<=decadal[2],] 
se_ppt_decadal <- as.data.frame(round(100*colMeans(se_ppt_decadal[,c(3:6)])/mean(se_ppt_decadal$sync),4))
names(se_ppt_decadal) <- NULL

# MULTIDECADAL 20-30 yr timecales
se_all_multidecadal<-se_all[se$timescales>=multidecadal[1] & se_all$timescales<=multidecadal[2],] 
se_all_multidecadal <- as.data.frame(round(100*colMeans(se_all_multidecadal[,c(3:12)])/mean(se_all_multidecadal$sync),4))
names(se_all_multidecadal) <- NULL

se_ppt_multidecadal<-se_ppt[se$timescales>=multidecadal[1] & se_ppt$timescales<=multidecadal[2],] 
se_ppt_multidecadal <- as.data.frame(round(100*colMeans(se_ppt_multidecadal[,c(3:6)])/mean(se_ppt_multidecadal$sync),4))
names(se_ppt_multidecadal) <- NULL

# create dataframe for full timeseries coh/sync explained results
timeseries_all_coh <- cbind(se_all_annual, se_all_interannual, se_all_decadal, se_all_multidecadal)
timeseries_ppt_coh <- cbind(se_ppt_annual, se_ppt_interannual, se_ppt_decadal, se_ppt_multidecadal)

#### Two Time Periods ################################################
# repeat coherence tests for the two time periods
x_e = early_growth_mx
y1_e = early_ppt_mx
y2_e = early_tmin_mx
y3_e = early_vpdmax_mx
times_e = as.numeric(1917:1967)

res_early_ppt <- coh(dat1 = x_e, dat2 = y1_e, times = times_e, norm = "powall",
                          sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                          scale.max.input = 25)
res_early_tmin <- coh(dat1 = x_e, dat2 = y2_e, times = times_e, norm = "powall",
                           sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                           scale.max.input = 25)
res_early_vpdmax <- coh(dat1 = x_e, dat2 = y3_e, times = times_e, norm = "powall",
                             sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                             scale.max.input = 25)

x_l = late_growth_mx
y1_l = late_ppt_mx
y2_l = late_tmin_mx
y3_l = late_vpdmax_mx
times_l = as.numeric(1968:2018)

res_late_ppt <- coh(dat1 = x_l, dat2 = y1_l, times = times_l, norm = "powall",
                     sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                     scale.max.input = 25)
res_late_tmin <- coh(dat1 = x_l, dat2 = y2_l, times = times_l, norm = "powall",
                      sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                      scale.max.input = 25)
res_late_vpdmax <- coh(dat1 = x_l, dat2 = y3_l, times = times_l, norm = "powall",
                        sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                        scale.max.input = 25)

# bandtest each variable for significant coherence at different timescale bands
annual <- c(2,3)
interannual <- c(3,10)
decadal <- c(10,20)
multidecadal <- c(20,30)

# PPT
res_early_ppt_a<-bandtest(res_early_ppt,annual)
res_early_ppt_ia<-bandtest(res_early_ppt,interannual)
res_early_ppt_d<-bandtest(res_early_ppt,decadal)
res_early_ppt_md<-bandtest(res_early_ppt,multidecadal)
early_ppt_coherence <- rbind(get_bandp(res_early_ppt_a),get_bandp(res_early_ppt_ia), 
                       get_bandp(res_early_ppt_d),get_bandp(res_early_ppt_md))

early_ppt_coherence <- early_ppt_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

res_late_ppt_a<-bandtest(res_late_ppt,annual)
res_late_ppt_ia<-bandtest(res_late_ppt,interannual)
res_late_ppt_d<-bandtest(res_late_ppt,decadal)
res_late_ppt_md<-bandtest(res_late_ppt,multidecadal)
late_ppt_coherence <- rbind(get_bandp(res_late_ppt_a),get_bandp(res_late_ppt_ia), 
                             get_bandp(res_late_ppt_d),get_bandp(res_late_ppt_md))

late_ppt_coherence <- late_ppt_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

# TMIN
res_early_tmin_a<-bandtest(res_early_tmin,annual)
res_early_tmin_ia<-bandtest(res_early_tmin,interannual)
res_early_tmin_d<-bandtest(res_early_tmin,decadal)
res_early_tmin_md<-bandtest(res_early_tmin,multidecadal)
early_tmin_coherence <- rbind(get_bandp(res_early_tmin_a),get_bandp(res_early_tmin_ia), 
                             get_bandp(res_early_tmin_d),get_bandp(res_early_tmin_md))

early_tmin_coherence <- early_tmin_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

res_late_tmin_a<-bandtest(res_late_tmin,annual)
res_late_tmin_ia<-bandtest(res_late_tmin,interannual)
res_late_tmin_d<-bandtest(res_late_tmin,decadal)
res_late_tmin_md<-bandtest(res_late_tmin,multidecadal)
late_tmin_coherence <- rbind(get_bandp(res_late_tmin_a),get_bandp(res_late_tmin_ia), 
                            get_bandp(res_late_tmin_d),get_bandp(res_late_tmin_md))

late_tmin_coherence <- late_tmin_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

# VPD
res_early_vpdmax_a<-bandtest(res_early_vpdmax,annual)
res_early_vpdmax_ia<-bandtest(res_early_vpdmax,interannual)
res_early_vpdmax_d<-bandtest(res_early_vpdmax,decadal)
res_early_vpdmax_md<-bandtest(res_early_vpdmax,multidecadal)
early_vpdmax_coherence <- rbind(get_bandp(res_early_vpdmax_a),get_bandp(res_early_vpdmax_ia), 
                              get_bandp(res_early_vpdmax_d),get_bandp(res_early_vpdmax_md))

early_vpdmax_coherence <- early_vpdmax_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))

res_late_vpdmax_a<-bandtest(res_late_vpdmax,annual)
res_late_vpdmax_ia<-bandtest(res_late_vpdmax,interannual)
res_late_vpdmax_d<-bandtest(res_late_vpdmax,decadal)
res_late_vpdmax_md<-bandtest(res_late_vpdmax,multidecadal)
late_vpdmax_coherence <- rbind(get_bandp(res_late_vpdmax_a),get_bandp(res_late_vpdmax_ia), 
                             get_bandp(res_late_vpdmax_d),get_bandp(res_late_vpdmax_md))

late_vpdmax_coherence <- late_vpdmax_coherence %>%
  mutate(phase_test = (mn_phs/pi))%>%
  mutate(phase = case_when(phase_test <= 0.25 ~ "in",
                           phase_test >= 0.25 & phase_test <= 0.75 ~ "lag",
                           phase_test >= 0.75 ~ "anti"))%>%
  mutate(sig = case_when(p_val >= 0.05 ~ "non",
                         p_val <= 0.05 ~ "sig"))


# calculate synchrony explained by each variable across the two timeperiods for each band
# input all three predictors into a wavelet linear model
early_env_all<- list(early_growth_mx, early_ppt_mx, early_tmin_mx, early_vpdmax_mx)
late_env_all<- list(late_growth_mx, late_ppt_mx, late_tmin_mx, late_vpdmax_mx)
early_wlm_all<-wlm(dat = early_env_all,times = times_e, resp=1,pred=2:4,norm="powall",scale.max.input=20)
late_wlm_all<-wlm(dat = late_env_all,times = times_l, resp=1,pred=2:4,norm="powall",scale.max.input=20)
early_se_all <- syncexpl(early_wlm_all)
late_se_all <- syncexpl(late_wlm_all)

# calculate synchrony explained for ppt only across the two timeperiods for each band
early_env_ppt <- list(early_growth_mx, early_ppt_mx)
late_env_ppt <- list(late_growth_mx, late_ppt_mx)
early_wlm_ppt<-wlm(dat = early_env_ppt,times = times_e, resp=1,pred=2,norm="powall",scale.max.input=20)
late_wlm_ppt<-wlm(dat = late_env_ppt,times = times_l, resp=1,pred=2,norm="powall",scale.max.input=20)
early_se_ppt <- syncexpl(early_wlm_ppt)
late_se_ppt <- syncexpl(late_wlm_ppt)

# ANNUAL 2-3 yr timescales
early_se_all_annual<-early_se_all[early_se_all$timescales>=annual[1] & early_se_all$timescales<=annual[2],] 
early_se_all_annual <- as.data.frame(round(100*colMeans(early_se_all_annual[,c(3:12)])/mean(early_se_all_annual$sync),4))
names(early_se_all_annual) <- NULL
late_se_all_annual<-late_se_all[late_se_all$timescales>=annual[1] & late_se_all$timescales<=annual[2],] 
late_se_all_annual <- as.data.frame(round(100*colMeans(late_se_all_annual[,c(3:12)])/mean(late_se_all_annual$sync),4))
names(late_se_all_annual) <- NULL

early_se_ppt_annual<-early_se_ppt[early_se_ppt$timescales>=annual[1] & early_se_ppt$timescales<=annual[2],] 
early_se_ppt_annual <- as.data.frame(round(100*colMeans(early_se_ppt_annual[,c(3:6)])/mean(early_se_ppt_annual$sync),4))
names(early_se_ppt_annual) <- NULL
late_se_ppt_annual<-late_se_ppt[late_se_ppt$timescales>=annual[1] & late_se_ppt$timescales<=annual[2],] 
late_se_ppt_annual <- as.data.frame(round(100*colMeans(late_se_ppt_annual[,c(3:6)])/mean(late_se_ppt_annual$sync),4))
names(late_se_ppt_annual) <- NULL

# INTERANNUAL 3-10 yr timescales
early_se_all_interannual<-early_se_all[early_se_all$timescales>=interannual[1] & early_se_all$timescales<=interannual[2],] 
early_se_all_interannual <- as.data.frame(round(100*colMeans(early_se_all_interannual[,c(3:12)])/mean(early_se_all_interannual$sync),4))
names(early_se_all_interannual) <- NULL
late_se_all_interannual<-late_se_all[late_se_all$timescales>=interannual[1] & late_se_all$timescales<=interannual[2],] 
late_se_all_interannual <- as.data.frame(round(100*colMeans(late_se_all_interannual[,c(3:12)])/mean(late_se_all_interannual$sync),4))
names(late_se_all_interannual) <- NULL

early_se_ppt_interannual<-early_se_ppt[early_se_ppt$timescales>=interannual[1] & early_se_ppt$timescales<=interannual[2],] 
early_se_ppt_interannual <- as.data.frame(round(100*colMeans(early_se_ppt_interannual[,c(3:6)])/mean(early_se_ppt_interannual$sync),4))
names(early_se_ppt_interannual) <- NULL
late_se_ppt_interannual<-late_se_ppt[late_se_ppt$timescales>=interannual[1] & late_se_ppt$timescales<=interannual[2],] 
late_se_ppt_interannual <- as.data.frame(round(100*colMeans(late_se_ppt_interannual[,c(3:6)])/mean(late_se_ppt_interannual$sync),4))
names(late_se_ppt_interannual) <- NULL

# DECADAL 10-20 yr timescales
early_se_all_decadal<-early_se_all[early_se_all$timescales>=decadal[1] & early_se_all$timescales<=decadal[2],] 
early_se_all_decadal <- as.data.frame(round(100*colMeans(early_se_all_decadal[,c(3:12)])/mean(early_se_all_decadal$sync),4))
names(early_se_all_decadal) <- NULL
late_se_all_decadal<-late_se_all[late_se_all$timescales>=decadal[1] & late_se_all$timescales<=decadal[2],] 
late_se_all_decadal <- as.data.frame(round(100*colMeans(late_se_all_decadal[,c(3:12)])/mean(late_se_all_decadal$sync),4))
names(late_se_all_decadal) <- NULL

early_se_ppt_decadal<-early_se_ppt[early_se_ppt$timescales>=decadal[1] & early_se_ppt$timescales<=decadal[2],] 
early_se_ppt_decadal <- as.data.frame(round(100*colMeans(early_se_ppt_decadal[,c(3:6)])/mean(early_se_ppt_decadal$sync),4))
names(early_se_ppt_decadal) <- NULL
late_se_ppt_decadal<-late_se_ppt[late_se_ppt$timescales>=decadal[1] & late_se_ppt$timescales<=decadal[2],] 
late_se_ppt_decadal <- as.data.frame(round(100*colMeans(late_se_ppt_decadal[,c(3:6)])/mean(late_se_ppt_decadal$sync),4))
names(late_se_ppt_decadal) <- NULL

# MULTIDECADAL 20-30 yr timecales
early_se_all_multidecadal<-early_se_all[early_se_all$timescales>=multidecadal[1] & early_se_all$timescales<=multidecadal[2],] 
early_se_all_multidecadal <- as.data.frame(round(100*colMeans(early_se_all_multidecadal[,c(3:12)])/mean(early_se_all_multidecadal$sync),4))
names(early_se_all_multidecadal) <- NULL
late_se_all_multidecadal<-late_se_all[late_se_all$timescales>=multidecadal[1] & late_se_all$timescales<=multidecadal[2],] 
late_se_all_multidecadal <- as.data.frame(round(100*colMeans(late_se_all_multidecadal[,c(3:12)])/mean(late_se_all_multidecadal$sync),4))
names(late_se_all_multidecadal) <- NULL

early_se_ppt_multidecadal<-early_se_ppt[early_se_ppt$timescales>=multidecadal[1] & early_se_ppt$timescales<=multidecadal[2],] 
early_se_ppt_multidecadal <- as.data.frame(round(100*colMeans(early_se_ppt_multidecadal[,c(3:6)])/mean(early_se_ppt_multidecadal$sync),4))
names(early_se_ppt_multidecadal) <- NULL
late_se_ppt_multidecadal<-late_se_ppt[late_se_ppt$timescales>=multidecadal[1] & late_se_ppt$timescales<=multidecadal[2],] 
late_se_ppt_multidecadal <- as.data.frame(round(100*colMeans(late_se_ppt_multidecadal[,c(3:6)])/mean(late_se_ppt_multidecadal$sync),4))
names(late_se_ppt_multidecadal) <- NULL

# create dataframe for full timeseries coh/sync explained results
early_all_coh <- cbind(early_se_all_annual, early_se_all_interannual, early_se_all_decadal, early_se_all_multidecadal)
early_ppt_coh <- cbind(early_se_ppt_annual, early_se_ppt_interannual, early_se_ppt_decadal, early_se_ppt_multidecadal)
late_all_coh <- cbind(late_se_all_annual, late_se_all_interannual, late_se_all_decadal, late_se_all_multidecadal)
late_ppt_coh <- cbind(late_se_ppt_annual, late_se_ppt_interannual, late_se_ppt_decadal, late_se_ppt_multidecadal)


#### Time Varying Coherence ####################################################
# data for each variable
x = avg_plot_growth_mx
y1 = winter_ppt_mx
y2 = summer_tmin_mx
y3 = avg_vpdmax_mx
times = 1900:2018

# calculate time varying coherence for each variable across whole time series
# leave f0 and scale_max_input blank
tv_timeseries_ppt <- coh_tv(dat1 = x, dat2 = y1, times = times, norm = "powall",
                          sigmethod = "fftsurrog1", nrand = 1000)
tv_timeseries_tmin <- coh_tv(dat1 = x, dat2 = y2, times = times, norm = "powall",
                           sigmethod = "fftsurrog1", nrand = 1000)
tv_timeseries_vpdmax <- coh_tv(dat1 = x, dat2 = y3, times = times, norm = "powall",
                             sigmethod = "fftsurrog1", nrand = 1000)


# average coherence across timescale bands for each timestep
# ppt
coh.ppt <- as.data.frame(tv_timeseries_ppt$signif$gt)
colnames(coh.ppt) <- tv_timeseries_ppt$timescales
coh.ppt$times <- tv_timeseries_ppt$times
coh.ppt <- coh.ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.ppt$ts <- as.numeric(coh.ppt$ts)
coh.ppt <- coh.ppt %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "annual",
                          ts > 3  & ts <= 10 ~ "interannual",
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
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "annual",
                          ts > 3  & ts <= 10 ~ "interannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.tmin <- coh.tmin %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.tmin <- na.omit(avg.coh.tmin)  

# vpdmax
coh.vpdmax <- as.data.frame(tv_timeseries_vpdmax$signif$gt)
colnames(coh.vpdmax) <- tv_timeseries_vpdmax$timescales
coh.vpdmax$times <- tv_timeseries_vpdmax$times
coh.vpdmax <- coh.vpdmax %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.vpdmax$ts <- as.numeric(coh.vpdmax$ts)
coh.vpdmax <- coh.vpdmax %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "annual",
                          ts > 3  & ts <= 10 ~ "interannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.vpdmax <- coh.vpdmax %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.vpdmax <- na.omit(avg.coh.vpdmax)  

# combine into one data frame for plotting purposes
avg.coh.ppt$driver <- "Winter PRECIP"
avg.coh.tmin$driver <- "Summer TEMP"
avg.coh.vpdmax$driver <- "Average VPD"
avg.tv.coh <- rbind(avg.coh.ppt, avg.coh.tmin, avg.coh.vpdmax)
avg.tv.coh$times <- as.character(avg.tv.coh$times)
avg.tv.coh$band <- factor(avg.tv.coh$band , levels=c('annual', 'interannual', 'decadal', 'multidecadal'))

# plot avg coherence across time per band for each driver



ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = band, color = band)) +
  facet_wrap(~ driver)+
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, face = "plain"),
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



avg.tv.coh$driver <- factor(avg.tv.coh$driver, levels=c('Winter PRECIP', 'Summer TEMP', 'Average VPD'))

mycols1 <- colors()[c(91, 128, 99)]
mypal1 <- palette(mycols1)
names(mypal1) = c("Summer TEMP", "Winter PRECIP", "Average VPD")
colScale1 <- scale_colour_manual(name = "driver", values = mypal1)


driver_band <- ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
  facet_wrap(~ band)+
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
  ylab("Average Coherence")+
  xlab("Year")+
  colScale1
png("/Users/kaitlynmcknight/Documents/Teamtree_finalfigures/env_coh.png", width = 5, height = 5, units = 'in', res = 600)
driver_band
dev.off()




  