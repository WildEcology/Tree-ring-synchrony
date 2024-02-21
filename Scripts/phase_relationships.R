source(here::here("updated_cleaning_code.R"))
source(here::here("Scripts/coh_tv.R"))

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
# plot phase relationships through time and across timescales
# PPT
timescales <- tv_timeseries_ppt$timescales
tvcoh_ppt_mx <- matrix(data=tv_timeseries_ppt$coher, length(times), length(timescales))
tts_ppt <- tts(times, timescales, tvcoh_ppt_mx)
plotphase(tts_ppt)


# TMIN
timescales <- tv_timeseries_tmin$timescales
tvcoh_tmin_mx <- matrix(data=tv_timeseries_tmin$coher, length(times), length(timescales))
tts_tmin <- tts(times, timescales, tvcoh_tmin_mx)
plotphase(tts_tmin)
