
# need to source dataframes from env_quantiles_test.R


# expand data to include all data points
avg_rwi_sync_timescale_tmin <- inner_join(timescale_specific_avg_tmin, avg_rwi_sync, by=join_by(window_year == year, band))
# make sure quantile is a factor
avg_rwi_sync_timescale_tmin$quantile <- ordered(avg_rwi_sync_timescale_tmin$quantile, levels = c("1", "2", "3", "4"))
# subset data for biennial timescale band
avg_rwi_sync_timescale_tmin_b <- avg_rwi_sync_timescale_tmin %>%
  filter(band == "biennial")
b.rwi.tmin.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_tmin_b)
summary(b.rwi.tmin.aov)
TukeyHSD(b.rwi.tmin.aov)
plot(b.rwi.tmin.aov, 1)
# multiannual
avg_rwi_sync_timescale_tmin_ma <- avg_rwi_sync_timescale_tmin %>%
  filter(band == "multiannual")
ma.rwi.tmin.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_tmin_ma)
summary(ma.rwi.tmin.aov)
TukeyHSD(ma.rwi.tmin.aov)
plot(ma.rwi.tmin.aov, 1)
# decadal
avg_rwi_sync_timescale_tmin_d <- avg_rwi_sync_timescale_tmin %>%
  filter(band == "decadal")
d.rwi.tmin.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_tmin_d)
summary(d.rwi.tmin.aov)
TukeyHSD(d.rwi.tmin.aov)
plot(d.rwi.tmin.aov, 1)
# multidecadal
avg_rwi_sync_timescale_tmin_md <- avg_rwi_sync_timescale_tmin %>%
  filter(band == "multidecadal")
md.rwi.tmin.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_tmin_md)
summary(md.rwi.tmin.aov)
TukeyHSD(md.rwi.tmin.aov)
plot(md.rwi.tmin.aov, 1)

# rwi sync - ppt quartiles
# expand data to include all data points
avg_rwi_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_rwi_sync, by=join_by(window_year == year, band))
# make sure quantile is a factor
avg_rwi_sync_timescale_ppt$quantile <- ordered(avg_rwi_sync_timescale_ppt$quantile, levels = c("1", "2", "3", "4"))
# subset data for biennial timescale band
avg_rwi_sync_timescale_ppt_b <- avg_rwi_sync_timescale_ppt %>%
  filter(band == "biennial")
b.rwi.ppt.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_ppt_b)
summary(b.rwi.ppt.aov)
TukeyHSD(b.rwi.ppt.aov)
plot(b.rwi.ppt.aov,1)
# multiannual
avg_rwi_sync_timescale_ppt_ma <- avg_rwi_sync_timescale_ppt %>%
  filter(band == "multiannual")
ma.rwi.ppt.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_ppt_ma)
summary(ma.rwi.ppt.aov)
TukeyHSD(ma.rwi.ppt.aov)
plot(ma.rwi.ppt.aov,1)
# decadal
avg_rwi_sync_timescale_ppt_d <- avg_rwi_sync_timescale_ppt %>%
  filter(band == "decadal")
d.rwi.ppt.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_ppt_d)
summary(d.rwi.ppt.aov)
TukeyHSD(d.rwi.ppt.aov)
plot(d.rwi.ppt.aov,1)
# multidecadal
avg_rwi_sync_timescale_ppt_md <- avg_rwi_sync_timescale_ppt %>%
  filter(band == "multidecadal")
md.rwi.ppt.aov <- aov(avg_sync~quantile, data = avg_rwi_sync_timescale_ppt_md)
summary(md.rwi.ppt.aov)
TukeyHSD(md.rwi.ppt.aov)
plot(md.rwi.ppt.aov,1)

# ppt sync - tmin quartiles
# expand data to include all data points
avg_ppt_sync_timescale_tmin <- inner_join(timescale_specific_avg_tmin, avg_env_sync_ppt, by=join_by(window_year == year, band == interval))
# make sure quantile is a factor
avg_ppt_sync_timescale_tmin$quantile <- ordered(avg_ppt_sync_timescale_tmin$quantile, levels = c("1", "2", "3", "4"))
# subset data for biennial timescale band
avg_ppt_sync_timescale_tmin_b <- avg_ppt_sync_timescale_tmin %>%
  filter(band == "biennial")
b.ppt.tmin.aov <- aov(avg_sync~quantile, data = avg_ppt_sync_timescale_tmin_b)
summary(b.ppt.tmin.aov)
TukeyHSD(b.ppt.tmin.aov)
plot(b.ppt.tmin.aov,1)
# multiannual
avg_ppt_sync_timescale_tmin_ma <- avg_ppt_sync_timescale_tmin %>%
  filter(band == "multiannual")
ma.ppt.tmin.aov <- aov(avg_sync~quantile, data = avg_ppt_sync_timescale_tmin_ma)
summary(ma.ppt.tmin.aov)
TukeyHSD(ma.ppt.tmin.aov)
plot(ma.ppt.tmin.aov,1)
# decadal
avg_ppt_sync_timescale_tmin_d <- avg_ppt_sync_timescale_tmin %>%
  filter(band == "decadal")
d.ppt.tmin.aov <- aov(avg_sync~quantile, data = avg_ppt_sync_timescale_tmin_d)
summary(d.ppt.tmin.aov)
TukeyHSD(d.ppt.tmin.aov)
plot(d.ppt.tmin.aov,1)
# multidecadal
avg_ppt_sync_timescale_tmin_md <- avg_ppt_sync_timescale_tmin %>%
  filter(band == "multidecadal")
md.ppt.tmin.aov <- aov(avg_sync~quantile, data = avg_ppt_sync_timescale_tmin_md)
summary(md.ppt.tmin.aov)
TukeyHSD(md.ppt.tmin.aov)
plot(md.ppt.tmin.aov,1)

# tmin sync - ppt quartiles
# expand data to include all data points
avg_tmin_sync_timescale_ppt <- inner_join(timescale_specific_avg_ppt, avg_env_sync_tmin, by=join_by(window_year == year, band == interval))
# make sure quantile is a factor
avg_tmin_sync_timescale_ppt$quantile <- ordered(avg_tmin_sync_timescale_ppt$quantile, levels = c("1", "2", "3", "4"))
# subset data for biennial timescale band
avg_tmin_sync_timescale_ppt_b <- avg_tmin_sync_timescale_ppt %>%
  filter(band == "biennial")
b.tmin.ppt.aov <- aov(avg_sync~quantile, data = avg_tmin_sync_timescale_ppt_b)
summary(b.tmin.ppt.aov)
TukeyHSD(b.tmin.ppt.aov)
plot(b.tmin.ppt.aov,1)
# multiannual
avg_tmin_sync_timescale_ppt_ma <- avg_tmin_sync_timescale_ppt %>%
  filter(band == "multiannual")
ma.tmin.ppt.aov <- aov(avg_sync~quantile, data = avg_tmin_sync_timescale_ppt_ma)
summary(ma.tmin.ppt.aov)
TukeyHSD(ma.tmin.ppt.aov)
plot(ma.tmin.ppt.aov,1)
# decadal
avg_tmin_sync_timescale_ppt_d <- avg_tmin_sync_timescale_ppt %>%
  filter(band == "decadal")
d.tmin.ppt.aov <- aov(avg_sync~quantile, data = avg_tmin_sync_timescale_ppt_d)
summary(d.tmin.ppt.aov)
TukeyHSD(d.tmin.ppt.aov)
plot(d.tmin.ppt.aov,1)
# multidecadal
avg_tmin_sync_timescale_ppt_md <- avg_tmin_sync_timescale_ppt %>%
  filter(band == "multidecadal")
md.tmin.ppt.aov <- aov(avg_sync~quantile, data = avg_tmin_sync_timescale_ppt_md)
summary(md.tmin.ppt.aov)
TukeyHSD(md.tmin.ppt.aov)
plot(md.tmin.ppt.aov,1)


