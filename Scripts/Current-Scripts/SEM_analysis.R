#### SEMs ####
library(lavaan)
library(psych)
library(semPlot)
library(piecewiseSEM)
library(dplyr)
library(tidyverse)

# load and prep necessary data
avg_rwi_sync <- readRDS("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/avg_rwi_sync.rds") %>%
  select(year, interval, avg_sync) %>%
  mutate(driver = "rwi")
avg_env_sync <- readRDS("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/avg_env_sync.RDS")
ts_env <- readRDS("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/ts_env_data.rds")

# separate environmental variables into dfs-wide format, give quantile values 1-4
ts_tmin <- ts_env %>%
  filter(driver == "tmin") %>%
  pivot_wider(names_from = "driver", values_from = "average") %>%
  rename(ts_tmin = tmin) %>%
  mutate(tmin_quartile = case_when(quantile == 100 ~ 4, 
                                   quantile == 75 ~ 3,
                                   quantile == 50 ~ 2,
                                   quantile == 25 ~ 1)) %>%
  select(-quantile)

ts_ppt <- ts_env %>%
  filter(driver == "ppt") %>%
  pivot_wider(names_from = "driver", values_from = "average") %>%
  rename(ts_ppt = ppt) %>%
  mutate(ppt_quartile = case_when(quantile == 100 ~ 4, 
                                  quantile == 75 ~ 3,
                                  quantile == 50 ~ 2,
                                  quantile == 25 ~ 1)) %>%
  select(-quantile)

# join env wide data
ts_env_wide <- inner_join(ts_tmin, ts_ppt)

# join tree and env sync data
avg_rwi_sync$year <- as.numeric(avg_rwi_sync$year)
avg_sync <- rbind(avg_rwi_sync, avg_env_sync) %>%
  pivot_wider(values_from = "avg_sync", names_from = "driver") %>%
  rename(rwi_sync = "rwi") %>%
  rename(ppt_sync = "ppt") %>%
  rename(tmin_sync = "tmin") %>%
  rename(band = "interval")

# combine all data into one df:  year, band, synchrony, env quartile data
model_df <- right_join(avg_sync, ts_env_wide)
model_df <- model_df %>% 
  mutate(group = case_when(tmin_quartile == 1 ~ "cool",
                           tmin_quartile == 2 ~ "cool", 
                           tmin_quartile == 3 ~ "warm",
                           tmin_quartile == 4 ~ "warm")) %>%
  filter(!is.na(group))

# make group a factor with levels = cool, warm 
model_df$group <- factor(model_df$group, levels = c("cool","warm"))

## SATURATED model (without covariance between endogenous variables) ##
SEM_sat <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync  ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

fit_sat <- sem(
  SEM_sat,
  data = model_df,
  group = "group",
  estimator = "MLR"
)

summary(fit_sat, standardized = TRUE, fit.measures = TRUE)

# trim model to include only significant pathways for each group
SEM_trim <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~ c(0,NA)*ts_ppt + c(0,NA)*ts_tmin
  tmin_sync ~ c(0,0)*ts_tmin + c(NA,0)*ts_ppt
  rwi_sync ~ ppt_sync +
             c(0,0)*tmin_sync +
             c(0,0)*ts_ppt +
             c(NA,0)*ts_tmin
'

fit_trim <- sem(
  SEM_trim,
  data = model_df,
  group = "group",
  estimator = "MLR"
)

summary(fit_trim, standardized=TRUE, fit.measures=TRUE)

# trim model to remove only non-sig pathways across both groups
SEM_trim2 <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_ppt
  rwi_sync  ~ ppt_sync + ts_tmin
'

fit_trim2 <- sem(
  SEM_trim2,
  data = model_df,
  group = "group",
  estimator = "MLR"
)

summary(fit_trim2, standardized=TRUE, fit.measures=TRUE)

# saturated model fit best, compare to equal (all pathways)
fit_equal_sat <- sem(
  SEM_sat,
  data = model_df,
  group = "group",
  estimator = "MLR",
  group.equal = "regressions"
)

summary(fit_equal_sat, standardized = TRUE, fit.measures = TRUE)

lavTestLRT(fit_sat,fit_equal_sat)
# at least one pathway significantly differs across temp conditions
# test all pathways for significant differences

SEM_diffs <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ c(a1_cl, a1_wr)*ts_ppt +
              c(a2_cl, a2_wr)*ts_tmin
  tmin_sync ~ c(a3_cl, a3_wr)*ts_tmin +
              c(a4_cl, a4_wr)*ts_ppt
  rwi_sync  ~ c(b1_cl, b1_wr)*ppt_sync +
              c(b2_cl, b2_wr)*tmin_sync +
              c(b3_cl, b3_wr)*ts_ppt +
              c(b4_cl, b4_wr)*ts_tmin
              
  # calculate differences            
  d_a1 := a1_wr - a1_cl
  d_a2 := a2_wr - a2_cl
  d_a3 := a3_wr - a3_cl
  d_a4 := a4_wr - a4_cl

  d_b1 := b1_wr - b1_cl
  d_b2 := b2_wr - b2_cl
  d_b3 := b3_wr - b3_cl
  d_b4 := b4_wr - b4_cl
'

fit_diffs <- sem(
  SEM_diffs,
  data = model_df,
  group = "group",
  estimator = "MLR"
)

parameterEstimates(fit_diffs, standardized=TRUE) %>%
  filter(op == ":=") %>%
  select(lhs, est, se, z, pvalue, std.all)

