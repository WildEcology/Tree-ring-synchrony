# run updated_cleaning_code script to pull in cleaned data
source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))

#### Full Time Series ##########################################################
# plot raw timeseries data for each variable
## rwi ##
colnames(avg_plot_growth_wide) <- 1900:2018

avg_rwi <- avg_plot_growth_wide %>%
  pivot_longer(1:119, names_to = "year", values_to = "rwi")%>%
  group_by(year)%>%
  summarize(trend = mean(rwi))

avg_rwi$plot <- "Trend"

avg_rwi$year <- as.character(avg_rwi$year)
avg_plot_growth$year <- as.character(avg_plot_growth$year)
raw_rwi <- ggplot()+
  geom_line(data = avg_plot_growth, aes(x=year, y=avg_growth, group=plot, col=plot))+
  geom_line(data = avg_rwi, mapping = aes(x=year, y=trend, group = plot, col = plot), color = "black", linewidth = 1.25)+
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
  ylab("Average Growth \n(Ring Width Index)")

library(svglite)
ggsave(file="/Users/kaitlynmcknight/Documents/J/rawgrowth.svg", plot=raw_rwi, width=6, height=5)

png("/Users/kaitlynmcknight/Documents/esapresgraphics/newrawrwi.png", width = 5, height = 5, units = 'in', res = 600)
raw_rwi
dev.off()


hist(avg_plot_growth$avg_growth)

## ppt ##
colnames(winter_ppt_wide) <- 1900:2018

winter_ppt_trend <- winter_ppt_wide %>%
  pivot_longer(1:119, names_to = "wateryear", values_to = "ppt")%>%
  group_by(wateryear)%>%
  summarize(trend = mean(ppt))

winter_ppt_trend$plot <- "Trend"

winter_ppt$wateryear <- as.character(winter_ppt$wateryear)
winter_ppt_trend$wateryear <- as.character(winter_ppt_trend$wateryear)
raw_ppt <- ggplot()+
  geom_line(data = winter_ppt, mapping = aes(x=wateryear, y=winter_ppt, group=plot, col=plot))+
  geom_line(data = winter_ppt_trend, mapping = aes(x=wateryear, y=trend, group = plot, col = plot), color = "black", linewidth = 1.25)+
  theme_bw()+
  scale_colour_grey(start = 1, end = 0.5)+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Water-Year Precipitation \n(Oct - May, mm)")

png("/Users/kaitlynmcknight/Documents/Teamtree_finalfigures/newrawppt.png", width = 5, height = 5, units = 'in', res = 600)
raw_ppt
dev.off()
## tmin ##
colnames(summer_tmin_wide) <- 1900:2018
summer_tmin_trend <- summer_tmin_wide %>%
  pivot_longer(1:119, names_to = "year", values_to = "tmin")%>%
  group_by(year)%>%
  summarize(trend = mean(tmin))

summer_tmin_trend$plot <- "Trend"

summer_tmin$year <- as.character(summer_tmin$year)
summer_tmin_trend$year <- as.character(summer_tmin_trend$year)
summer_tmin$year <- as.character(summer_tmin$year)
raw_tmin <- ggplot()+
  geom_line(data = summer_tmin, aes(x=year, y=summer_tmin, group=plot, col=plot))+
  geom_line(data = summer_tmin_trend, mapping = aes(x=year, y=trend, group = plot, col = plot), color = "black", linewidth = 1.25)+
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
  ylab("Summer Minimum Temperatures \n(Jun - Aug, C)")
png("/Users/kaitlynmcknight/Documents/Teamtree_finalfigures/newrawtmin.png", width = 5, height = 5, units = 'in', res = 600)
raw_tmin
dev.off()

## vpdmax ##
avg_vpdmax$wateryear <- as.character(avg_vpdmax$wateryear)
raw_vpd <- ggplot()+
  geom_line(data = avg_vpdmax, aes(x=wateryear, y=avg_vpdmax, group=plot, col=plot))+
  theme_bw()+
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
  ylab("OCT-SEPT, hPa")

png("/Users/kaitlynmcknight/Documents/Teamtree_finalfigures/newrawvpd.png", width = 5, height = 5, units = 'in', res = 600)
raw_vpd
dev.off()
#### Two Time Periods ##########################################################
# plot raw timeseries data for each variable
## rwi ##

early_raw_rwi <- ggplot()+
  geom_line(data = early_growth_long, aes(x=year, y=avg_growth, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1917,1967,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average growth (annual rwi)")

late_raw_rwi <- ggplot()+
  geom_line(data = late_growth_long, aes(x=year, y=avg_growth, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1968,2018,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average growth (annual rwi)")

## ppt ##
early_raw_ppt <- ggplot()+
  geom_line(data = early_ppt_long, aes(x=year, y=winter_ppt, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1917,1967,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Winter Precipitation (October - May, mm)")

late_raw_ppt <- ggplot()+
  geom_line(data = late_ppt_long, aes(x=year, y=winter_ppt, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1968,2018,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Winter Precipitation (October - May, mm)")

## tmin ##
early_raw_tmin <- ggplot()+
  geom_line(data = early_tmin_long, aes(x=year, y=summer_tmin, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1917,1967,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average minimum summer temperature (June - August, C)")


late_raw_tmin <- ggplot()+
  geom_line(data = late_tmin_long, aes(x=year, y=summer_tmin, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1968,2018,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average minimum summer temperature (June - August, C)")

## vpdmax ##
early_raw_vpdmax <- ggplot()+
  geom_line(data = early_vpdmax_long, aes(x=year, y=avg_vpdmax, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1917,1967,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average maximum VPD (October-September, hPa)")


late_raw_vpdmax <- ggplot()+
  geom_line(data = late_vpdmax_long, aes(x=year, y=avg_vpdmax, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1968,2018,2))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average maximum VPD (October-September, hPa)")

