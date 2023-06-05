# run updated_cleaning_code script to pull in cleaned data
source(here::here("updated_cleaning_code.R"))

# calculate coherence of each predictor across whole timeseries
x = avg_plot_growth_mx
y1 = winter_ppt_mx
y2 = summer_tmin_mx
y3 = avg_vpdmax_mx

res_timeseries_ppt <- coh(dat1 = x, dat2 = y1, times = times, norm = "powall",
                          sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                          scale.max.input = 50)
res_timeseries_tmin <- coh(dat1 = x, dat2 = y2, times = times, norm = "powall",
                          sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                          scale.max.input = 50)
res_timeseries_vpdmax <- coh(dat1 = x, dat2 = y3, times = times, norm = "powall",
                          sigmethod = "fast", nrand = 1000, f0 = 0.5, 
                          scale.max.input = 50)

# bandtest each variable for significant coherence at different timescale bands
annual <- c(2,3)
interannual <- c(3,10)
decadal <- c(10,20)
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


# calculate synchrony explained by each variable across the entire timeseries for each band
# input all three predictors into a wavelet linear model
data <- list(avg_plot_growth_mx, winter_ppt_mx, summer_tmin_mx, avg_vpdmax_mx)
wlm_all<-wlm(dat = data,times = times, resp=1,pred=2:4,norm="powall",scale.max.input=28)
se <- syncexpl(wlm_all)

# ANNUAL 2-3 yr timescales
se_annual<-se[se$timescales>=annual[1] & se$timescales<=annual[2],] 
se_annual <- as.data.frame(round(100*colMeans(se_annual[,c(3:12)])/mean(se_annual$sync),4))
names(se_annual) <- NULL

# INTERANNUAL 3-10 yr timescales
se_interannual<-se[se$timescales>=interannual[1] & se$timescales<=interannual[2],] 
se_interannual <- as.data.frame(round(100*colMeans(se_interannual[,c(3:12)])/mean(se_interannual$sync),4))
names(se_interannual) <- NULL

# DECADAL 10-20 yr timescales
se_decadal<-se[se$timescales>=decadal[1] & se$timescales<=decadal[2],] 
se_decadal <- as.data.frame(round(100*colMeans(se_decadal[,c(3:12)])/mean(se_decadal$sync),4))
names(se_decadal) <- NULL

# MULTIDECADAL 20-30 yr timecales
se_multidecadal<-se[se$timescales>=multidecadal[1] & se$timescales<=multidecadal[2],] 
se_multidecadal <- as.data.frame(round(100*colMeans(se_multidecadal[,c(3:12)])/mean(se_multidecadal$sync),4))
names(se_multidecadal) <- NULL

# create dataframe for full timeseries coh/sync explained results
timeseries_coh <- cbind(se_annual, se_interannual, se_decadal, se_multidecadal)


