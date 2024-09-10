# run updated_cleaning_code script to pull in cleaned data
source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))

# calculate signficance thresholds 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M1 <- as.data.frame(res_growth_wmf$values)
colnames(M1) <- res_growth_wmf$timescales
#fix the imaginary #s
M1 <- abs(M1)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M2 <- M1[,1:67]
year <- 1900:2018
M2$year <- year
# classify sync, async and ns
M2<- M2 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
M2 <- na.omit(M2)
M2events <- M2 %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M2events$ts <- as.numeric(M2events$ts)
M2events$ts <- as.numeric(M2events$ts)
M2events <- M2events %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))


avg_sync <- M2events %>%
  group_by(year, interval) %>%
  summarise(avg_sync = mean(values))
  
avg_sync_standardband <- avg_sync %>%
  ungroup()%>%
  group_by(interval)%>%
  mutate(z_scores = (avg_sync - (mean(avg_sync))/sd(avg_sync)))

avg_sync_standardband <- na.omit(avg_sync_standardband)
avg_sync_standardband$year <- as.numeric(avg_sync_standardband$year)


avg_sync_standardband$interval <- as.factor(avg_sync_standardband$interval)
avg_sync_standardband$scaled_year <- scale(avg_sync_standardband$year)
avg_sync_standardband$x <- as.numeric(round(avg_sync_standardband$scaled_year,2))

library(glmmTMB)
library(ggeffects)
avg_sync_annual <- avg_sync_standardband %>%
  filter(interval == "biennial")
anull_model <- glmmTMB(avg_sync~1, 
                       data = avg_sync_annual)

alinear_model <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_sync_annual)

aquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_sync_annual)

AIC(anull_model, alinear_model, aquad_model)
# quad fit best


avg_sync_inter <- avg_sync_standardband%>%
  filter(interval == "multiannual")

inull_model <- glmmTMB(avg_sync~1, 
                       data = avg_sync_inter)

ilinear_model <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_sync_inter)

iquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_sync_inter)

AIC(inull_model, ilinear_model, iquad_model)
# quad fit best

avg_sync_decadal <- avg_sync_standardband%>%
  filter(interval == "decadal")

dnull_model <- glmmTMB(avg_sync~1, 
                       data = avg_sync_decadal)

dlinear_model <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_sync_decadal)

dquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_sync_decadal)

AIC(dnull_model, dlinear_model, dquad_model)
# linear fit best

avg_sync_multi <- avg_sync_standardband%>%
  filter(interval == "multidecadal")

mnull_model <- glmmTMB(avg_sync~1, 
                       data = avg_sync_multi)

mlinear_model <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_sync_multi)

mquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_sync_multi)

AIC(mnull_model, mlinear_model, mquad_model)
# quad fits best

# All together:
avis_prod <- ggpredict(aquad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
avis_prod$band <- "biennial"
ivis_prod <- ggpredict(iquad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
ivis_prod$band <- "multiannual"
dvis_prod <- ggpredict(dlinear_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
dvis_prod$band <- "decadal"
mvis_prod <- ggpredict(mquad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
mvis_prod$band <- "multidecadal"

avg_sync_standardband_tojoin <- avg_sync_standardband %>% 
  select(year,x) %>%
  unique()

final_avg_data <- rbind(avis_prod, ivis_prod, dvis_prod, mvis_prod)

final_avg_data <- full_join(avg_sync_standardband_tojoin, final_avg_data, by="x")
final_avg_data$year <- as.character(final_avg_data$year)
final_avg_data$band <- factor(final_avg_data$band, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
avg_sync_standardband$year <- as.character(avg_sync_standardband$year)
avg_sync_standardband$band <- factor(avg_sync_standardband$interval , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))


regional_avg <- ggplot() +
  geom_point(data = avg_sync_standardband, aes(x=year, y=avg_sync, col= band), alpha = 0.1) +
  geom_line(data = final_avg_data, aes(x = year, y = predicted, group = band, col = band),
            linewidth = 1) +
  geom_ribbon(data = final_avg_data, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  #facet_grid(~factor(band, levels = c("biennial", "multiannual", "decadal", "multidecadal")), labeller = labeller(band = c("Biennial" = "biennial", 
                                                 #"Multiannual" = "multiannual", 
                                                 #"Decadal" = "decadal", 
                                                 #"Multidecadal" = "multidecadal")))+
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  scale_color_brewer(palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  scale_fill_brewer(palette = "RdYlBu", direction = -1)+
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        #legend.text = element_blank(),
        #legend.position = "none",
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Growth Synchrony")+
  xlab("Year")


# plot z-scores
avg_sync_standardband <- na.omit(avg_sync_standardband)
avg_sync_standardband$year <- as.character(avg_sync_standardband$year)
avg_sync_standardband$interval <- factor(avg_sync_standardband$interval, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
final_avg_data$band <- factor(final_avg_data$band, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

ggplot() +
  geom_line(data = avg_sync_standardband, aes(x = year, y = z_scores, group = interval, color = interval)) +
  theme_bw()+
  scale_color_brewer(palette="Spectral", type = "seq")+
  scale_x_discrete(breaks = seq(1900,2018,10))+
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
  ylab("Average Synchrony")+
  xlab("Year")

shortbands <- c("annual", "interannual")
short <- avg_sync_standardband %>%
  filter(interval %in% shortbands)
short_av <- final_avg_data %>%
  filter(band %in% shortbands)
longbands <- c("decadal", "multidecadal")
long <- avg_sync_standardband %>%
  filter(interval %in% longbands)
long_av <- final_avg_data %>%
  filter(band %in% longbands)

final_avg_data$band <- factor(final_avg_data$band, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'),
                              labels = c("biennial (2-3 yrs)", "multiannual (3-10 yrs)", "decadal (10-20 yrs)", "multidecadal (20-30 yrs)"))

regional_avg <- ggplot() +
  #geom_point(data = avg_sync_standardband, aes(x=year, y=avg_sync, col= interval), alpha = 0.1) +
  geom_line(data = final_avg_data, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = final_avg_data, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  scale_x_discrete(breaks = seq(1900,2018,10))+
  scale_color_brewer(palette="Spectral", type = "seq")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 12,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average synchrony")+
  ylim(0,1)+
  xlab("Year")

png("/Users/kaitlynmcknight/Documents/Teamtree_finalfigures/avg_sync.png", width = 5, height = 5, units = 'in', res = 600)
regional_avg
dev.off()

regional_avg_short <- ggplot() +
  geom_point(data = short, aes(x=year, y=avg_sync, col= interval), alpha = 0.1) +
  geom_line(data = short_av, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = short_av, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  #scale_color_brewer(palette = "Dark2")+
  #scale_fill_brewer(palette = "Dark2")+
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
  ylab("Average synchrony")+
  ylim(0,1)+
  xlab("Year")+
  colScale1+
  colScale2
png("/Users/kaitlynmcknight/Documents/esapresgraphics/avgshort.png", width = 5, height = 5, units = 'in', res = 600)
regional_avg_short
dev.off()

regional_avg_long <- ggplot() +
  geom_point(data = long, aes(x=year, y=avg_sync, col= interval), alpha = 0.1) +
  geom_line(data = long_av, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = long_av, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
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
  ylab("Average synchrony")+
  ylim(0,1)+
  xlab("Year")+
  colScale1+
  colScale2

png("/Users/kaitlynmcknight/Documents/esapresgraphics/navglong.png", width = 5, height = 5, units = 'in', res = 600)
regional_avg_long
dev.off()


avg_sync_standardband <- na.omit(avg_sync_standardband)
avg_sync_standardband$year <- as.character(avg_sync_standardband$year)
shortbands <- c("annual", "interannual")
short <- avg_sync_standardband %>%
  filter(interval %in% shortbands)
longbands <- c("decadal", "multidecadal")
long <- avg_sync_standardband %>%
  filter(interval %in% longbands)
ggplot() +
  geom_line(data = avg_sync_standardband, aes(x = year, y = z_scores, group = interval, color = interval)) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
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
  ylab("Average Synchrony")+
  xlab("Year")


avg_sync_standard <- avg_sync %>%
  ungroup()%>%
  mutate(z_scores = (avg_sync - mean(avg_sync))/sd(avg_sync))

avg_sync_standard <- na.omit(avg_sync_standard)
avg_sync_standard$year <- as.character(avg_sync_standard$year)
ggplot() +
  geom_line(data = avg_sync_standard, aes(x = year, y = z_scores, group = interval, color = interval)) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
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
  ylab("Average Synchrony")+
  xlab("Year")

#### LOCAL ####
# determine the number of trees per plot to calculate significance thresholds
tree_per_plot <- rwi_00s_plot_filtered_wide %>%
  pivot_longer(3:121, names_to = "year")%>%
  group_by(plot, year)%>%
  summarize(numtree = n())

tree_per_plot <- tree_per_plot %>% distinct(plot,numtree)

# calculate significant thresholds for each plot
n <- tree_per_plot$numtree
plots <- unique(tree_per_plot$plot)
thresholds<- matrix(NA, ncol=3, nrow=length(plots))
colnames(thresholds)<- c("plot","lower","upper")
plot_current_results <- data.frame()

for(s in 1:length(plots)){
  current.plot<-plots[s]
  temp<-tree_per_plot %>% 
    filter(plot %in% current.plot) %>% 
    select(-plot)
  n<-temp$numtree[1]
  nreps <- 10000
  temp<-psync.by.chance(n=n, nreps=nreps, prob=c(0.005, 0.995))
  thresholds[s,1] <- current.plot
  thresholds[s,2:3] <- temp
  
  rwi_00s_plot <- rwi_00s_plot_filtered_wide %>%
    filter(plot == current.plot)
  
  # format matrix for analysis
  rwi_00s_plot <- as.matrix(rwi_00s_plot)
  colnames(rwi_00s_plot) <- NULL
  rwi_00s_plot <- rwi_00s_plot[, c(4:122)] 
  
  # convert character matrix to numeric
  rwi_00s_plot = as.data.frame(rwi_00s_plot, stringsAsFactors = FALSE)
  rwi_00s_plot = map_df(rwi_00s_plot, as.numeric)
  rwi_00s_plot_mx <- as.matrix(rwi_00s_plot)
  
  # calculate wavelet
  year <- 1900:2018
  rwi_00s_plot_mx <- cleandat(rwi_00s_plot_mx, year, clev = 5)$cdat
  res<-wpmf(rwi_00s_plot_mx,year,sigmethod="quick")
  
  #extract raw values
  M1 <- as.data.frame(res$values)
  colnames(M1) <- res$timescales
  #fix the imaginary #s
  M1 <- abs(M1)
  # define thresholds
  sync <- temp[2]
  sync <- as.numeric(sync)
  async <- temp[1]
  async <- as.numeric(async)
  
  M2 <- M1[,1:67]
  M2$year <- year
  # classify sync, async and ns
  M2<- M2 %>%
    pivot_longer(1:67, names_to = "ts", values_to = "values")
  M2 <- na.omit(M2)
  M2events <- M2 %>%
    mutate(event = case_when(values >= sync ~ "synchronous",
                             values <= async ~ "asynchronous",
                             TRUE ~ "NS"))
  
  # classify timescale intervals
  M2events$ts <- as.numeric(M2events$ts)
  M2events$ts <- as.numeric(M2events$ts)
  M2events <- M2events %>%
    mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "annual",
                                ts > 3 & ts <= 10 ~ "interannual",
                                ts > 10 & ts <= 20 ~ "decadal",
                                ts > 20 & ts <= 30 ~ "multidecadal"))
  M2events$location <- current.plot
  
  # make dataframe to save results
  plot_current_results <- rbind(plot_current_results, M2events)
}

avg_sync <-plot_current_results %>%
  group_by(year, interval,location) %>%
  summarise(avg_sync = mean(values))

avg_sync_standardband <- avg_sync %>%
  ungroup()%>%
  group_by(interval)%>%
  mutate(z_scores = (avg_sync - mean(avg_sync))/sd(avg_sync))

avg_sync_standardband <- na.omit(avg_sync_standardband)
avg_sync_standardband$year <- as.numeric(avg_sync_standardband$year)


avg_sync_standardband$interval <- as.factor(avg_sync_standardband$interval)
avg_sync_standardband$scaled_year <- scale(avg_sync_standardband$year)
avg_sync_standardband$x <- as.numeric(round(avg_sync_standardband$scaled_year,2))

library(glmmTMB)
library(ggeffects)
avg_sync_annual <- avg_sync_standardband %>%
  filter(interval == "annual")
anull_model <- glmmTMB(avg_sync~1 + (1|location), 
                       data = avg_sync_annual)

alinear_model <-
  glmmTMB(avg_sync ~ scaled_year + (1|location), 
          data= avg_sync_annual)

aquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= avg_sync_annual)

AIC(anull_model, alinear_model, aquad_model)
# quad fit best


avg_sync_inter <- avg_sync_standardband%>%
  filter(interval == "interannual")

inull_model <- glmmTMB(avg_sync~1 + (1|location), 
                       data = avg_sync_inter)

ilinear_model <-
  glmmTMB(avg_sync ~ scaled_year + (1|location), 
          data= avg_sync_inter)

iquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= avg_sync_inter)

AIC(inull_model, ilinear_model, iquad_model)
# quad fit best

avg_sync_decadal <- avg_sync_standardband%>%
  filter(interval == "decadal")

dnull_model <- glmmTMB(avg_sync~1 + (1|location), 
                       data = avg_sync_decadal)

dlinear_model <-
  glmmTMB(avg_sync ~ scaled_year + (1|location), 
          data= avg_sync_decadal)

dquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= avg_sync_decadal)

AIC(dnull_model, dlinear_model, dquad_model)
# linear fit best

avg_sync_multi <- avg_sync_standardband%>%
  filter(interval == "multidecadal")

mnull_model <- glmmTMB(avg_sync~1 + (1|location), 
                       data = avg_sync_multi)

mlinear_model <-
  glmmTMB(avg_sync ~ scaled_year + (1|location), 
          data= avg_sync_multi)

mquad_model <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE) + (1|location), 
          data= avg_sync_multi)

AIC(mnull_model, mlinear_model, mquad_model)
# linear fits best

# All together:
avis_prod <- ggpredict(aquad_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "random", 
                       ci.lvl = .95)
avis_prod$band <- "annual"
ivis_prod <- ggpredict(iquad_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "random", 
                       ci.lvl = .95)
ivis_prod$band <- "interannual"
dvis_prod <- ggpredict(dlinear_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "random", 
                       ci.lvl = .95)
dvis_prod$band <- "decadal"
mvis_prod <- ggpredict(mlinear_model, 
                       terms = c("scaled_year[all]", "location"), 
                       type = "random", 
                       ci.lvl = .95)
mvis_prod$band <- "multidecadal"




avg_sync_standardband_tojoin <- avg_sync_standardband %>% 
  select(year,x) %>%
  unique()

final_avg_data <- rbind(avis_prod, ivis_prod, dvis_prod, mvis_prod)


final_avg_data <- right_join(avg_sync_standardband_tojoin, final_avg_data, by="x")
final_avg_data$year <- as.character(final_avg_data$year)
avg_sync_standardband$year <- as.character(avg_sync_standardband$year)
local_avg <- ggplot() +
  geom_point(data = avg_sync_standardband, aes(x=year, y=avg_sync, col= interval), alpha = 0.1) +
  geom_line(data = final_avg_data, aes(x = year, y = predicted, group = band, color = band),
            # color = colortreatpred,
            linewidth = 1) +
  geom_ribbon(data = final_avg_data, aes(
    x = year,
    y = predicted,
    group=band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,5))+
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
  ylab("average synchrony")+
  xlab("year")

ggplot() +
  geom_line(data = avg_sync_standardband, aes(x = year, y = z_scores, group = interval, color = interval)) +
  theme_bw()+
  scale_x_discrete(breaks = seq(1900,2018,10))+
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
  ylab("Average Synchrony")+
  xlab("Year")
