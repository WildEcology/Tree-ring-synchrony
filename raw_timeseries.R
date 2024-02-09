# run updated_cleaning_code script to pull in cleaned data
source(here::here("updated_cleaning_code.R"))

#### Full Time Series ##########################################################
# plot raw timeseries data for each variable
## rwi ##
raw_rwi <- ggplot()+
  geom_line(data = avg_plot_growth, aes(x=year, y=avg_growth, group=plot, col=plot))+
  theme_bw()+
  scale_x_discrete(breaks = seq(19,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average RWI")

library(svglite)
ggsave(file="/Users/kaitlynmcknight/Documents/J/rawgrowth.svg", plot=raw_rwi, width=6, height=5)

png("/Users/kaitlynmcknight/Documents/esapresgraphics/newrawrwi.png", width = 5, height = 5, units = 'in', res = 600)
raw_rwi
dev.off()

## ppt ##
winter_ppt$wateryear <- as.character(winter_ppt$wateryear)
raw_ppt <- ggplot()+
  geom_line(data = winter_ppt, aes(x=wateryear, y=winter_ppt, group=plot, col=plot))+
  theme_bw()+
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
  ylab("OCT - MAY, mm")

png("/Users/kaitlynmcknight/Documents/esapresgraphics/newrawppt.png", width = 5, height = 5, units = 'in', res = 600)
raw_ppt
dev.off()
## tmin ##
summer_tmin$year <- as.character(summer_tmin$year)
raw_tmin <- ggplot()+
  geom_line(data = summer_tmin, aes(x=year, y=summer_tmin, group=plot, col=plot))+
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
  ylab("JUN - AUG, C")
png("/Users/kaitlynmcknight/Documents/esapresgraphics/newrawtmin.png", width = 5, height = 5, units = 'in', res = 600)
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

png("/Users/kaitlynmcknight/Documents/esapresgraphics/newrawvpd.png", width = 5, height = 5, units = 'in', res = 600)
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

