load("~/Documents/GitHub/Tree-ring-synchrony/Data/avg_growth_sync.RData")
source(here::here("updated_cleaning_code.R"))

## plotting avg growth synchrony with raw ppt and tmin values
winter_ppt_reg <- winter_ppt %>%
  rename("ppt" = winter_ppt)%>%
  rename("year" = wateryear)

winter_ppt_reg$year <- as.character(winter_ppt_reg$year)
winter_ppt_reg<- winter_ppt_reg %>%
  group_by(year) %>%
  summarise(regional_avg_ppt = mean(ppt))
winter_ppt_avg_growth_sync <- left_join(avg_sync_standardband, winter_ppt_reg)


reg_avg_ppt_avg_sync<- ggplot(data = winter_ppt_avg_growth_sync, aes(x = regional_avg_ppt, y = avg_sync, color = interval)) +
  geom_point(data = winter_ppt_avg_coh, aes(x = regional_avg_ppt, y = avg_sync, color = interval), alpha = 0.5) +
  geom_smooth(method = "loess", se= FALSE, span = 5.0)+
  theme_bw()+
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
  ylab("Average Growth Sync")+
  xlab("Regional Avg PPT")

# repeat for tmin
summer_tmin <- summer_tmin %>%
  rename("tmin" = summer_tmin)

summer_tmin$year <- as.character(summer_tmin$year)
summer_tmin_reg<- summer_tmin %>%
  group_by(year) %>%
  summarise(regional_avg_tmin = mean(tmin))
summer_tmin_reg$year <- as.character(summer_tmin_reg$year)
summer_tmin_avg_growth_sync <- left_join(avg_sync_standardband, summer_tmin_reg)


reg_avg_tmin_growth_sync<- ggplot(data = summer_tmin_avg_growth_sync, aes(x = regional_avg_tmin, y = avg_sync, color = interval)) +
  geom_point(data = summer_tmin_avg_growth_sync, aes(x = regional_avg_tmin, y = avg_sync, color = interval), alpha = 0.5) +
  geom_smooth(method = "loess", se= FALSE, span = 2)+
  theme_bw()+
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
  ylab("Average Growth Synchrony")+
  xlab("Regional Avg TMIN")





