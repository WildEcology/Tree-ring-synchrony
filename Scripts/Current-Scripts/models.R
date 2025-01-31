# load necessary packages
library(dplyr)
library(tidyverse)
library(lavaan)
require(psych)
require(semPlot)
library(piecewiseSEM)


# load and prep necessary data
avg_rwi_sync <- readRDS("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/avg_rwi_sync.rds") %>%
  select(year, interval, avg_sync) %>%
  mutate(driver = "rwi")

avg_env_sync <- readRDS("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/avg_env_sync.RDS")

ts_env <- readRDS("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/ts_env.rds") %>%
  select(window_year, band, w_start, w_end, window_avg_ppt, window_avg_tmin) %>%
  rename(ts_ppt = "window_avg_ppt") %>%
  rename(ts_tmin = "window_avg_tmin")


avg_rwi_sync$year <- as.numeric(avg_rwi_sync$year)
avg_sync <- rbind(avg_rwi_sync, avg_env_sync) %>%
  pivot_wider(values_from = "avg_sync", names_from = "driver") %>%
  rename(rwi_sync = "rwi") %>%
  rename(ppt_sync = "ppt") %>%
  rename(tmin_sync = "tmin")

model_df <- inner_join(avg_sync, ts_env, 
                       by = join_by(year == window_year, interval == band)) %>%
  select(-w_start, -w_end)

# subset each band and test effects of ts_tmin and ppt_sync on growth synchrony
# biennial
biennial_df <- model_df %>%
  filter(interval == "biennial")

test.biennial <- lm(rwi_sync ~ ts_tmin * ppt_sync, data = biennial_df)
summary(test.biennial)

# multiannual
multiannual_df <- model_df %>%
  filter(interval == "multiannual")

test.multiannual <- lm(rwi_sync ~ ts_tmin * ppt_sync, data = multiannual_df)
summary(test.multiannual)

# decadal
decadal_df <- model_df %>%
  filter(interval == "decadal")

test.decadal <- lm(rwi_sync ~ ts_tmin * ppt_sync, data = decadal_df)
summary(test.decadal)

# multidecdal
multidecadal_df <- model_df %>%
  filter(interval == "multidecadal")

test.multidecadal <- lm(rwi_sync ~ ts_tmin * ppt_sync, data = multidecadal_df)
summary(test.multidecadal)


# test SEM for each band and all pathways
# biennial
SEM.B  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt
        '
SEM.B.fit <- sem(SEM.B, data=biennial_df)
summary(SEM.B.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.B.fit, standardized = TRUE)
semPaths(SEM.B.fit, what="std", whatLabels="std", residuals=FALSE)

#multiannual
SEM.MA  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt'
SEM.MA.fit <- sem(SEM.MA, data=multiannual_df)
summary(SEM.MA.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MA.fit, standardized = TRUE)
semPaths(SEM.MA.fit, what="std", whatLabels="std", residuals=FALSE)

# decadal
SEM.D  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt'
SEM.D.fit <- sem(SEM.D, data=decadal_df)
summary(SEM.D.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.D.fit, standardized = TRUE)
semPaths(SEM.D.fit, what="std", whatLabels="std", residuals=FALSE)

# multidecadal
SEM.MD  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt'
SEM.MD.fit <- sem(SEM.MD, data=multidecadal_df)
summary(SEM.MD.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MD.fit, standardized = TRUE)
semPaths(SEM.MD.fit, what="std", whatLabels="std", residuals=FALSE)
