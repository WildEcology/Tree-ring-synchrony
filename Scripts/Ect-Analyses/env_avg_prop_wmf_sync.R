
# source the data from cleaning code
source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))

# produce wmf's for ppt and tmin
# Assuming you have two 'tts' objects: object1 and object2

# Get the wavelet fields
wav1 <- Mod(get_values(res_ppt_wmf))
wav2 <- Mod(get_values(res_tmin_wmf))

# Find the global range across both datasets
global_range <- range(c(wav1, wav2), na.rm = TRUE)




tmin_wmf <- plotmag(res_tmin_wmf, zlims = global_range) 
ppt_wmf <- plotmag(res_ppt_wmf, zlims = global_range)


#### Average Sync ####
# PPT 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_ppt <- as.data.frame(res_ppt_wmf$values)
colnames(M_ppt) <- res_ppt_wmf$timescales
#fix the imaginary #s
M_ppt <- abs(M_ppt)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_ppt <- M_ppt[,1:67]
year <- 1900:2018
M_ppt$year <- year
# classify sync, async and ns
M_ppt<- M_ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
M_ppt <- na.omit(M_ppt)
M_pptevents <- M_ppt %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents <- M_pptevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))
# find how many timescales are included in each band per year
M_pptevents_filtered <- M_pptevents %>%
  group_by(year, interval) %>%
  summarise(num_ts = n())

M_pptevents_filtered <- na.omit(M_pptevents_filtered)

# filter for years where all timescales in a band are included
num_ts_per_band <- M_pptevents %>%
  group_by(interval) %>%
  summarise(ts_num = n_distinct(ts))

M_pptevents_filtered <- left_join(M_pptevents_filtered, num_ts_per_band)

M_pptevents_filtered <- M_pptevents_filtered %>%
  mutate(keep = case_when(num_ts == ts_num ~ "yes",
                          num_ts != ts_num ~ "no"))

M_pptevents_filtered <- M_pptevents_filtered %>%
  filter(keep == "yes") %>%
  unite("uID", 1:2, sep = "_", remove = FALSE)

M_pptevents_filtered <- M_pptevents_filtered$uID

M_pptevents <- M_pptevents %>%
  select(year, interval, ts, values) %>%
  unite("uID", 1:2, sep = "_", remove = FALSE)

M_pptevents_condensed_timeseries <- M_pptevents %>%
  filter(uID %in% M_pptevents_filtered)

avg_sync_ppt <- M_pptevents_condensed_timeseries %>%
  group_by(year, interval) %>%
  summarise(avg_sync = mean(values))
avg_sync_ppt$driver <- "ppt"

# TMIN
# avg sync per timescale band through time
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_tmin <- as.data.frame(res_tmin_wmf$values)
colnames(M_tmin) <- res_tmin_wmf$timescales
#fix the imaginary #s
M_tmin <- abs(M_tmin)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_tmin <- M_tmin[,1:67]
year <- 1900:2018
M_tmin$year <- year
# classify sync, async and ns
M_tmin<- M_tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "values")
M_tmin <- na.omit(M_tmin)
M_tminevents <- M_tmin %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents <- M_tminevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))

# find how many timescales are included in each band per year
M_tminevents_filtered <- M_tminevents %>%
  group_by(year, interval) %>%
  summarise(num_ts = n())

M_tminevents_filtered <- na.omit(M_tminevents_filtered)

# filter for years where all timescales in a band are included
num_ts_per_band <- M_tminevents %>%
  group_by(interval) %>%
  summarise(ts_num = n_distinct(ts))

M_tminevents_filtered <- left_join(M_tminevents_filtered, num_ts_per_band)

M_tminevents_filtered <- M_tminevents_filtered %>%
  mutate(keep = case_when(num_ts == ts_num ~ "yes",
                          num_ts != ts_num ~ "no"))

M_tminevents_filtered <- M_tminevents_filtered %>%
  filter(keep == "yes") %>%
  unite("uID", 1:2, sep = "_", remove = FALSE)

M_tminevents_filtered <- M_tminevents_filtered$uID

M_tminevents <- M_tminevents %>%
  select(year, interval, ts, values) %>%
  unite("uID", 1:2, sep = "_", remove = FALSE)

M_tminevents_condensed_timeseries <- M_tminevents %>%
  filter(uID %in% M_tminevents_filtered)

avg_sync_tmin <- M_tminevents_condensed_timeseries %>%
  group_by(year, interval) %>%
  summarise(avg_sync = mean(values))
avg_sync_tmin$driver <- "tmin"

# avg sync plot
# rbind data
avg_env_sync <- rbind(avg_sync_ppt, avg_sync_tmin)
avg_env_sync <- na.omit(avg_env_sync)

avg_env_sync$interval <- factor(avg_env_sync$interval, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

# testing model fit for both variables
library(glmmTMB)
library(ggeffects)
avg_env_sync$year <- as.numeric(avg_env_sync$year)
avg_env_sync$interval <- as.factor(avg_env_sync$interval)
avg_env_sync$scaled_year <- scale(avg_env_sync$year)
avg_env_sync$x <- as.numeric(round(avg_env_sync$scaled_year,2))

# ppt
avg_env_sync_ppt <- avg_env_sync %>%
  filter(driver == "ppt")

# biennial 
avg_env_sync_ppt_B <- avg_env_sync_ppt%>%
  filter(interval == "biennial")
Bnull_model_ppt <- glmmTMB(avg_sync~1, 
                       data = avg_env_sync_ppt_B)

Blinear_model_ppt <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_ppt_B)

Bquad_model_ppt <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_ppt_B)

b_ppt_aic <- AIC(Bnull_model_ppt, Blinear_model_ppt, Bquad_model_ppt)
# linear fit best

# multiannual 
avg_env_sync_ppt_MA <- avg_env_sync_ppt%>%
  filter(interval == "multiannual")
MAnull_model_ppt <- glmmTMB(avg_sync~1, 
                           data = avg_env_sync_ppt_MA)

MAlinear_model_ppt <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_ppt_MA)

MAquad_model_ppt <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_ppt_MA)

ma_ppt_aic<- AIC(MAnull_model_ppt, MAlinear_model_ppt, MAquad_model_ppt)
# quad fit best

# decadal
avg_env_sync_ppt_D <- avg_env_sync_ppt%>%
  filter(interval == "decadal")
Dnull_model_ppt <- glmmTMB(avg_sync~1, 
                           data = avg_env_sync_ppt_D)

Dlinear_model_ppt <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_ppt_D)

Dquad_model_ppt <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_ppt_D)

d_ppt_aic<- AIC(Dnull_model_ppt, Dlinear_model_ppt, Dquad_model_ppt)
# quad fit best

# multidecadal
avg_env_sync_ppt_MD <- avg_env_sync_ppt%>%
  filter(interval == "multidecadal")
MDnull_model_ppt <- glmmTMB(avg_sync~1, 
                           data = avg_env_sync_ppt_MD)

MDlinear_model_ppt <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_ppt_MD)

MDquad_model_ppt <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_ppt_MD)

md_ppt_aic<- AIC(MDnull_model_ppt, MDlinear_model_ppt, MDquad_model_ppt)
# quad fit best


# All together:
Bvis_prod_ppt <- ggpredict(Blinear_model_ppt, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
Bvis_prod_ppt$interval <- "biennial"

intercept <- fixef(MAnull_model_ppt)$cond["(Intercept)"]
vcov_matrix <- vcov(MAnull_model_ppt)$cond
se_intercept <- sqrt(vcov_matrix["(Intercept)", "(Intercept)"])
ci_low <- intercept - 1.96 * se_intercept
ci_high <- intercept + 1.96 * se_intercept

# Create a data frame for the null model predictions
MAvis_prod_ppt <- data.frame(
  x = avg_env_sync_ppt_MA$x, # mimic "scaled_year[all]"
  predicted = intercept,
  std.error = se_intercept,
  conf.low = ci_low,
  conf.high = ci_high,
  group = "1",
  interval = "multiannual"# Ensure it matches the intervals in other models
)



Dvis_prod_ppt <- ggpredict(Dquad_model_ppt, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
Dvis_prod_ppt$interval <- "decadal"
MDvis_prod_ppt <- ggpredict(MDquad_model_ppt, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci.lvl = .95)
MDvis_prod_ppt$interval <- "multidecadal"

final_avg_ppt_data <- rbind(Bvis_prod_ppt, MAvis_prod_ppt, Dvis_prod_ppt, MDvis_prod_ppt)


# tmin
avg_env_sync_tmin <- avg_env_sync %>%
  filter(driver == "tmin")

# biennial 
avg_env_sync_tmin_B <- avg_env_sync_tmin%>%
  filter(interval == "biennial")
Bnull_model_tmin <- glmmTMB(avg_sync~1, 
                           data = avg_env_sync_tmin_B)

Blinear_model_tmin <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_tmin_B)

Bquad_model_tmin <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_tmin_B)

b_tmin_aic<- AIC(Bnull_model_tmin, Blinear_model_tmin, Bquad_model_tmin)
# quad fit best

# multiannual 
avg_env_sync_tmin_MA <- avg_env_sync_tmin%>%
  filter(interval == "multiannual")
MAnull_model_tmin <- glmmTMB(avg_sync~1, 
                            data = avg_env_sync_tmin_MA)

MAlinear_model_tmin <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_tmin_MA)

MAquad_model_tmin <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_tmin_MA)

ma_tmin_aic<- AIC(MAnull_model_tmin, MAlinear_model_tmin, MAquad_model_tmin)
# linear fit best

# decadal 
avg_env_sync_tmin_D <- avg_env_sync_tmin%>%
  filter(interval == "decadal")
Dnull_model_tmin <- glmmTMB(avg_sync~1, 
                           data = avg_env_sync_tmin_D)

Dlinear_model_tmin <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_tmin_D)

Dquad_model_tmin <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_tmin_D)

d_tmin_aic<- AIC(Dnull_model_tmin, Dlinear_model_tmin, Dquad_model_tmin)
# quad fit best

# multidecadal
avg_env_sync_tmin_MD <- avg_env_sync_tmin%>%
  filter(interval == "multidecadal")
MDnull_model_tmin <- glmmTMB(avg_sync~1, 
                            data = avg_env_sync_tmin_MD)

MDlinear_model_tmin <-
  glmmTMB(avg_sync ~ scaled_year, 
          data= avg_env_sync_tmin_MD)

MDquad_model_tmin <-
  glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), 
          data= avg_env_sync_tmin_MD)

md_tmin_aic<- AIC(MDnull_model_tmin, MDlinear_model_tmin, MDquad_model_tmin)
# quad fit best

# All together:
intercept <- fixef(Bnull_model_tmin)$cond["(Intercept)"]
vcov_matrix <- vcov(Bnull_model_tmin)$cond
se_intercept <- sqrt(vcov_matrix["(Intercept)", "(Intercept)"])
ci_low <- intercept - 1.96 * se_intercept
ci_high <- intercept + 1.96 * se_intercept

# Create a data frame for the null model predictions
Bvis_prod_tmin <- data.frame(
  x = avg_env_sync_tmin_B$x, # mimic "scaled_year[all]"
  predicted = intercept,
  std.error = se_intercept,
  conf.low = ci_low,
  conf.high = ci_high,
  group = "1",
  interval = "biennial"# Ensure it matches the intervals in other models
)

MAvis_prod_tmin <- ggpredict(MAlinear_model_tmin, 
                            terms = c("scaled_year[all]"), 
                            type = "fe", 
                            ci.lvl = .95)
MAvis_prod_tmin$interval <- "multiannual"
Dvis_prod_tmin <- ggpredict(Dquad_model_tmin, 
                           terms = c("scaled_year[all]"), 
                           type = "fe", 
                           ci.lvl = .95)
Dvis_prod_tmin$interval <- "decadal"
MDvis_prod_tmin <- ggpredict(MDquad_model_tmin, 
                            terms = c("scaled_year[all]"), 
                            type = "fe", 
                            ci.lvl = .95)
MDvis_prod_tmin$interval <- "multidecadal"

final_avg_tmin_data <- rbind(Bvis_prod_tmin, MAvis_prod_tmin, Dvis_prod_tmin, MDvis_prod_tmin)

# join datasets together for plotting
final_avg_ppt_data$driver = "ppt"
final_avg_tmin_data$driver = "tmin"

final_avg_env_data <- rbind(final_avg_ppt_data, final_avg_tmin_data)
final_avg_env_data <- left_join(final_avg_env_data, avg_env_sync)

avg_env_sync$year <- as.character(avg_env_sync$year)
avg_env_sync$interval <- factor(avg_env_sync$interval , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
final_avg_env_data$year <- as.character(final_avg_env_data$year)
final_avg_env_data$interval <- factor(final_avg_env_data$interval , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
final_avg_env_data$driver <- factor(final_avg_env_data$driver , levels=c('ppt', 'tmin'),
                                      labels = c("Precipitation", "Temperature"))
avg_env_sync$driver <- factor(avg_env_sync$driver , levels=c('ppt', 'tmin'),
                                    labels = c("Precipitation", "Temperature"))


final_avg_ppt_sync<- final_avg_env_data %>%
  filter(driver == "Precipitation")

final_avg_tmin_sync <- final_avg_env_data %>%
  filter(driver == "Temperature")

regional_avg_env <- ggplot() +
  geom_point(data = final_avg_ppt_sync, aes(x=year, y=avg_sync, group = interval, color= interval), alpha = 0.1) +
  geom_line(data = final_avg_ppt_sync, aes(x = year, y = predicted, group = interval, color = interval),
            linewidth = 1) +
  geom_ribbon(data = final_avg_ppt_sync, aes(
    x = year,
    y = predicted,
    group=interval,
    fill = interval,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.2,
    show.legend = F) +
  theme_bw()+
  scale_y_continuous(limits = c(0.5, 2.0), breaks = seq(0.5, 2.0, 0.5))+
  scale_x_discrete(breaks = seq(1920,2020,20))+
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "#CFA4CC"
    ),
    labels = c(
      "biennial" = "Biennial (2-3 yrs)",
      "multiannual" = "Multiannual (3-10 yrs)",
      "decadal" = "Decadal (10-20 yrs)",
      "multidecadal" = "Multidecadal (20-30 yrs)"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "#CFA4CC"
    )
  )+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Precipitation Synchrony")+
  xlab("Year")

ggsave("/Users/kaitlynmcknight/Desktop/TeamTreeMS1Figs/canva/canva4/avg_tmin_sync.eps", width = 8, height = 6, units = "in")



#### plotting avg env sync vs avg tv coh ####
avg_env_sync <- avg_env_sync %>%
  rename("band" = interval, "times" = year)
avg_env_sync$times <- as.character(avg_env_sync$times)

#combine datasets
avg.tv.coh <- readRDS("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/avg_tv_coh.rds")
avg_sync_avg_coh_env <- right_join(avg.tv.coh, avg_env_sync)
avg_sync_avg_coh_env$times <- as.numeric(avg_sync_avg_coh_env$times)
avg_sync_avg_coh_env$driver <- factor(avg_sync_avg_coh_env$driver, levels=c('ppt', 'tmin'))
avg_sync_avg_coh_env$band<- factor(avg_sync_avg_coh_env$band, levels=c("biennial","multiannual","decadal", "multidecadal"))
avgsynccohenv<- ggplot() +
  geom_point(data = avg_sync_avg_coh_env, aes(x = avg_sync, y = avg_coh, group = driver, color = driver), size = 3) +
  facet_wrap(~ band)+
  theme_bw()+
  scale_color_manual(values = c("blue","red"))+
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
  xlab("Average Synchrony")


## plotting avg coherence with raw ppt and tmin values
winter_ppt_reg <- winter_ppt %>%
  rename("ppt" = winter_ppt)
winter_ppt_reg <- winter_ppt %>%
  rename("times" = wateryear)

winter_ppt_reg$times <- as.character(winter_ppt_reg$times)
winter_ppt_reg<- winter_ppt_reg %>%
  group_by(times) %>%
  summarise(regional_avg_ppt = mean(ppt))
winter_ppt_avg_coh <- left_join(avg.tv.coh, winter_ppt_reg) %>%
  filter(driver == "ppt") 


reg_avg_ppt_coherence<- ggplot(data = winter_ppt_avg_coh, aes(x = regional_avg_ppt, y = avg_coh, color = band)) +
  geom_point(data = winter_ppt_avg_coh, aes(x = regional_avg_ppt, y = avg_coh, color = band), alpha = 0.5) +
  geom_smooth(method = "lm", se= FALSE)+
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
  ylab("Average Coherence")+
  xlab("Regional avg ppt")

# lets try variability in ppt

winter_ppt_var_space <- winter_ppt %>% # in a given year how variable is ppt
  group_by(wateryear)%>%
  summarize(var_ppt = var(winter_ppt))

winter_ppt_var_space <- winter_ppt_var_space%>%
  rename("ppt" = var_ppt)
winter_ppt_var_space <- winter_ppt_var_space %>%
  rename("times" = wateryear)

winter_ppt_var_space$times <- as.character(winter_ppt_var_space$times)

winter_ppt_var_coh <- left_join(avg.tv.coh, winter_ppt_var_space) %>%
  filter(driver == "ppt") 

reg_var_ppt_coh<- ggplot(data = winter_ppt_var_coh, aes(x = ppt, y = avg_coh, color = band)) +
  geom_point(data = winter_ppt_var_coh, aes(x = ppt, y = avg_coh, color = band), alpha = 0.5) +
  geom_smooth(method = "lm", se= FALSE)+
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
  ylab("Average Coherence")+
  xlab("Regional variability in ppt")

# repeat for tmin
summer_tmin <- summer_tmin %>%
  rename("tmin" = summer_tmin)
summer_tmin <- summer_tmin %>%
  rename("times" = year)

summer_tmin$times <- as.character(summer_tmin$times)
summer_tmin_reg<- summer_tmin %>%
  group_by(times) %>%
  summarise(regional_avg_tmin = mean(tmin))
summer_tmin_avg_coh <- left_join(avg.tv.coh, summer_tmin_reg) %>%
  filter(driver == "tmin")


reg_avg_ppt_coherence<- ggplot(data = summer_tmin_avg_coh, aes(x = regional_avg_tmin, y = avg_coh, color = band)) +
  geom_point(data = summer_tmin_avg_coh, aes(x = regional_avg_tmin, y = avg_coh, color = band), alpha = 0.5) +
  geom_smooth(method = "lm", se= FALSE)+
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
  ylab("Average Coherence")+
  xlab("Regional avg tmin")

summer_tmin_var_space <- summer_tmin %>% # in a given year how variable is ppt
  group_by(year)%>%
  summarize(var_tmin = var(summer_tmin))

summer_tmin_var_space <- summer_tmin_var_space%>%
  rename("tmin" = var_tmin)
summer_tmin_var_space <- summer_tmin_var_space %>%
  rename("times" = year)

summer_tmin_var_space$times <- as.character(summer_tmin_var_space$times)

summer_tmin_var_coh <- left_join(avg.tv.coh, summer_tmin_var_space) %>%
  filter(driver == "tmin") 

reg_var_tmin_coh<- ggplot(data = summer_tmin_var_coh, aes(x = tmin, y = avg_coh, color = band)) +
  geom_point(data = summer_tmin_var_coh, aes(x = tmin, y = avg_coh, color = band), alpha = 0.5) +
  geom_smooth(method = "lm", se= FALSE)+
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
  ylab("Average Coherence")+
  xlab("Regional variability in tmin")


#### Proportion Sync ####
#PPT
# calculate signficance thresholds 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_ppt <- as.data.frame(res_ppt_wpmf$values)
colnames(M_ppt) <- res_ppt_wpmf$timescales
#fix the imaginary #s
M_ppt <- abs(M_ppt)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_ppt <- M_ppt[,1:66]
year <- 1900:2018
M_ppt$year <- year
# classify sync, async and ns
M_ppt<- M_ppt %>%
  pivot_longer(1:66, names_to = "ts", values_to = "values")
M_ppt <- na.omit(M_ppt)
M_pptevents <- M_ppt %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents$ts <- as.numeric(M_pptevents$ts)
M_pptevents <- M_pptevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))



prop_ppt_sync <- data.frame(year = NA, synch = NA, asynch = NA, ns = NA, interval = NA, obs = NA)

for (xx in 1:length(unique(M_pptevents$interval))) {
  
  current <- unique(M_pptevents$interval)[xx]
  M_pptevents_s <- M_pptevents %>%
    filter(interval == current)
  
  #significantly synchronous in the short term
  SSs <- M_pptevents_s %>%
    filter(event == "synchronous")%>%
    group_by(year)%>%
    summarise(short_sync = n())
  
  #significantly asynchronous in the short term
  SAs <- M_pptevents_s %>%
    filter(event == "asynchronous")%>%
    group_by(year)%>%
    summarise(short_async = n())
  
  #not significant in the short term
  NSs <- M_pptevents_s %>%
    filter(event == "NS")%>%
    group_by(year)%>%
    summarise(short_NS = n())
  
  #the number of observations per year to divide by to get the proportion
  prop_den_s <- M_pptevents_s %>%
    group_by(year)%>%
    summarise(obs = n())
  
  # proportion synchronous
  prop_calc_Ss <- left_join(prop_den_s, SSs)
  prop_sync <- prop_calc_Ss %>%
    group_by(year)%>%
    summarise(synch = short_sync/obs)
  
  # proportion asynchronous
  prop_calc_As <- left_join(prop_den_s, SAs)
  prop_async <- prop_calc_As %>%
    group_by(year)%>%
    summarise(asynch = short_async/obs) 
  
  # proportion not significant
  prop_calc_Ns <- left_join(prop_den_s, NSs)
  prop_ns <- prop_calc_Ns %>%
    group_by(year)%>%
    summarise(ns = short_NS/obs)
  
  plot1_temp <- prop_sync %>%
    full_join(prop_async, by="year") %>%
    full_join(prop_ns, by="year") %>%
    full_join(prop_den_s, by="year")
  
  plot1_temp$interval <- current
  
  prop_ppt_sync <- rbind(prop_ppt_sync, plot1_temp)
}

prop_ppt_sync <- prop_ppt_sync[2:381,]

prop_ppt_sync[is.na(prop_ppt_sync)] <- 0


# TMIN
# calculate signficance thresholds 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M_tmin <- as.data.frame(res_tmin_wpmf$values)
colnames(M_tmin) <- res_tmin_wpmf$timescales
#fix the imaginary #s
M_tmin <- abs(M_tmin)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M_tmin <- M_tmin[,1:66]
year <- 1900:2018
M_tmin$year <- year
# classify sync, async and ns
M_tmin<- M_tmin %>%
  pivot_longer(1:66, names_to = "ts", values_to = "values")
M_tmin <- na.omit(M_tmin)
M_tminevents <- M_tmin %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents$ts <- as.numeric(M_tminevents$ts)
M_tminevents <- M_tminevents %>%
  mutate(interval = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))



prop_tmin_sync <- data.frame(year = NA, synch = NA, asynch = NA, ns = NA, interval = NA, obs = NA)

for (xx in 1:length(unique(M_tminevents$interval))) {
  
  current <- unique(M_tminevents$interval)[xx]
  M_tminevents_s <- M_tminevents %>%
    filter(interval == current)
  
  #significantly synchronous in the short term
  SSs <- M_tminevents_s %>%
    filter(event == "synchronous")%>%
    group_by(year)%>%
    summarise(short_sync = n())
  
  #significantly asynchronous in the short term
  SAs <- M_tminevents_s %>%
    filter(event == "asynchronous")%>%
    group_by(year)%>%
    summarise(short_async = n())
  
  #not significant in the short term
  NSs <- M_tminevents_s %>%
    filter(event == "NS")%>%
    group_by(year)%>%
    summarise(short_NS = n())
  
  #the number of observations per year to divide by to get the proportion
  prop_den_s <- M_tminevents_s %>%
    group_by(year)%>%
    summarise(obs = n())
  
  # proportion synchronous
  prop_calc_Ss <- left_join(prop_den_s, SSs)
  prop_sync <- prop_calc_Ss %>%
    group_by(year)%>%
    summarise(synch = short_sync/obs)
  
  # proportion asynchronous
  prop_calc_As <- left_join(prop_den_s, SAs)
  prop_async <- prop_calc_As %>%
    group_by(year)%>%
    summarise(asynch = short_async/obs) 
  
  # proportion not significant
  prop_calc_Ns <- left_join(prop_den_s, NSs)
  prop_ns <- prop_calc_Ns %>%
    group_by(year)%>%
    summarise(ns = short_NS/obs)
  
  plot1_temp <- prop_sync %>%
    full_join(prop_async, by="year") %>%
    full_join(prop_ns, by="year") %>%
    full_join(prop_den_s, by="year")
  
  plot1_temp$interval <- current
  
  prop_tmin_sync <- rbind(prop_tmin_sync, plot1_temp)
}

prop_tmin_sync <- prop_tmin_sync[2:381,]

prop_tmin_sync[is.na(prop_tmin_sync)] <- 0

# join datasets for plotting
prop_tmin_sync$driver <- "tmin"
prop_ppt_sync$driver <- "ppt"
prop_env_sync <- rbind(prop_ppt_sync, prop_tmin_sync)

prop_env_sync$interval <- factor(prop_env_sync$interval, levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

regional_prop_env <- ggplot(data = prop_env_sync, aes(x = year, y = synch, group = interval, color = interval)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()+
  facet_wrap(~driver)+
  scale_x_continuous(breaks = seq(1900,2018,10))+
  scale_color_brewer(palette="Spectral", type = "seq", labels = c("biennial", "multiannual","decadal","multidecadal"))+
  scale_y_continuous(breaks = seq(0,2,0.5))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Significant Synchrony (Proportion)")+
  xlab("Year")



# find min and max of avg sync per driver for quad models
ppt_quad_mods <- final_avg_ppt_sync %>%
  group_by(interval) %>%
  summarise(min = min(predicted), max = max(predicted))

tmin_quad_mods <- final_avg_tmin_sync %>%
  filter(year > 1966) %>%
  group_by(interval) %>%
  summarise(min = min(predicted), max = max(predicted))


extremes_ppt <- final_avg_ppt_sync %>%
  group_by(interval) %>%
  filter(predicted == min(predicted) | predicted == max(predicted)) %>%
  arrange(interval, predicted)

extremes_temp <- extremes_temp %>%
  select(interval, year, scaled_year, predicted)

extremes_temp <- final_avg_tmin_sync %>%
  group_by(interval) %>%
  filter(predicted == min(predicted) | predicted == max(predicted)) %>%
  arrange(interval, predicted)

