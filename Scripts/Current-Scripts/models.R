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


# subset data based on timescale band and temp quartile
b_tmin_1 <- model_df %>%
  filter(band == "biennial",
         tmin_quartile == 1) %>%
  drop_na()

ma_tmin_1 <- model_df %>%
  filter(band == "multiannual",
         tmin_quartile == 1) %>%
  drop_na()

d_tmin_1 <- model_df %>%
  filter(band == "decadal",
         tmin_quartile == 1) %>%
  drop_na()

md_tmin_1 <- model_df %>%
  filter(band == "multidecadal",
         tmin_quartile == 1) %>%
  drop_na()

b_tmin_2 <- model_df %>%
  filter(band == "biennial",
         tmin_quartile == 2) %>%
  drop_na()

ma_tmin_2 <- model_df %>%
  filter(band == "multiannual",
         tmin_quartile == 2) %>%
  drop_na()

d_tmin_2 <- model_df %>%
  filter(band == "decadal",
         tmin_quartile == 2) %>%
  drop_na()

md_tmin_2 <- model_df %>%
  filter(band == "multidecadal",
         tmin_quartile == 2) %>%
  drop_na()


b_tmin_3 <- model_df %>%
  filter(band == "biennial",
         tmin_quartile == 3) %>%
  drop_na()

ma_tmin_3 <- model_df %>%
  filter(band == "multiannual",
         tmin_quartile == 3) %>%
  drop_na()

d_tmin_3 <- model_df %>%
  filter(band == "decadal",
         tmin_quartile == 3) %>%
  drop_na()

md_tmin_3 <- model_df %>%
  filter(band == "multidecadal",
         tmin_quartile == 3) %>%
  drop_na()


b_tmin_4 <- model_df %>%
  filter(band == "biennial",
         tmin_quartile == 4) %>%
  drop_na()

ma_tmin_4 <- model_df %>%
  filter(band == "multiannual",
         tmin_quartile == 4) %>%
  drop_na()

d_tmin_4 <- model_df %>%
  filter(band == "decadal",
         tmin_quartile == 4) %>%
  drop_na()

md_tmin_4 <- model_df %>%
  filter(band == "multidecadal",
         tmin_quartile == 4 ) %>%
  drop_na()

# test SEM for each band/quartile combo and all sync pathways
SEM.B.1  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.B.1.fit <- sem(SEM.B.1, data=b_tmin_1)
summary(SEM.B.1.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.B.1.fit, standardized = TRUE)
semPaths(SEM.B.1.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.B.2 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.B.2.fit <- sem(SEM.B.2, data=b_tmin_2)
summary(SEM.B.2.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.B.2.fit, standardized = TRUE)
semPaths(SEM.B.2.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.B.3 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.B.3.fit <- sem(SEM.B.3, data=b_tmin_3)
summary(SEM.B.3.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.B.3.fit, standardized = TRUE)
semPaths(SEM.B.3.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.B.4 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.B.4.fit <- sem(SEM.B.4, data=b_tmin_4)
summary(SEM.B.4.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.B.4.fit, standardized = TRUE)
semPaths(SEM.B.4.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MA.1  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MA.1.fit <- sem(SEM.MA.1, data=ma_tmin_1)
summary(SEM.MA.1.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MA.1.fit, standardized = TRUE)
semPaths(SEM.MA.1.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MA.2 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MA.2.fit <- sem(SEM.MA.2, data=ma_tmin_2)
summary(SEM.MA.2.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MA.2.fit, standardized = TRUE)
semPaths(SEM.MA.2.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MA.3 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MA.3.fit <- sem(SEM.MA.3, data=ma_tmin_3)
summary(SEM.MA.3.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MA.3.fit, standardized = TRUE)
semPaths(SEM.MA.3.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MA.4 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MA.4.fit <- sem(SEM.MA.4, data=ma_tmin_4)
summary(SEM.MA.4.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MA.4.fit, standardized = TRUE)
semPaths(SEM.MA.4.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.D.1  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.D.1.fit <- sem(SEM.D.1, data=d_tmin_1)
summary(SEM.D.1.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.D.1.fit, standardized = TRUE)
semPaths(SEM.D.1.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.D.2 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.D.2.fit <- sem(SEM.D.2, data=d_tmin_2)
summary(SEM.D.2.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.D.2.fit, standardized = TRUE)
semPaths(SEM.D.2.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.D.3 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.D.3.fit <- sem(SEM.D.3, data=d_tmin_3)
summary(SEM.D.3.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.D.3.fit, standardized = TRUE)
semPaths(SEM.D.3.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.D.4 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.D.4.fit <- sem(SEM.D.4, data=d_tmin_4)
summary(SEM.D.4.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.D.4.fit, standardized = TRUE)
semPaths(SEM.D.4.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MD.1  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MD.1.fit <- sem(SEM.MD.1, data=md_tmin_1)
summary(SEM.MD.1.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MD.1.fit, standardized = TRUE)
semPaths(SEM.MD.1.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MD.2 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MD.2.fit <- sem(SEM.MD.2, data=md_tmin_2)
summary(SEM.MD.2.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MD.2.fit, standardized = TRUE)
semPaths(SEM.MD.2.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MD.3 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MD.3.fit <- sem(SEM.MD.3, data=md_tmin_3)
summary(SEM.MD.3.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MD.3.fit, standardized = TRUE)
semPaths(SEM.MD.3.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.MD.4 <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        '
SEM.MD.4.fit <- sem(SEM.MD.4, data=md_tmin_4)
summary(SEM.MD.4.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MD.4.fit, standardized = TRUE)
semPaths(SEM.MD.4.fit, what="std", whatLabels="std", residuals=FALSE)




# separate data into just temp quartiles
tmin_quart_1 <- model_df %>%
  filter(tmin_quartile == 1)
tmin_quart_2 <- model_df %>%
  filter(tmin_quartile == 2)
tmin_quart_3 <- model_df %>%
  filter(tmin_quartile == 3)
tmin_quart_4 <- model_df %>%
  filter(tmin_quartile == 4)

# run full sem across bands for each temp quartile
SEM.1  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt'
SEM.1.fit <- sem(SEM.1, data=tmin_quart_1)
summary(SEM.1.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.1.fit, standardized = TRUE)
semPaths(SEM.1.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.2  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt'
SEM.2.fit <- sem(SEM.2, data=tmin_quart_2)
summary(SEM.2.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.2.fit, standardized = TRUE)
semPaths(SEM.2.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.3  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt'
SEM.3.fit <- sem(SEM.3, data=tmin_quart_3)
summary(SEM.3.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.3.fit, standardized = TRUE)
semPaths(SEM.3.fit, what="std", whatLabels="std", residuals=FALSE)

SEM.4  <- 'rwi_sync ~ ppt_sync
        rwi_sync ~ tmin_sync
        rwi_sync ~ ts_ppt
        rwi_sync ~ ts_tmin
        tmin_sync ~~ ts_tmin
        tmin_sync ~~ ts_ppt
        ppt_sync ~~ ts_tmin
        ppt_sync ~~ ts_ppt'
SEM.4.fit <- sem(SEM.4, data=tmin_quart_4)
summary(SEM.4.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.4.fit, standardized = TRUE)
semPaths(SEM.4.fit, what="std", whatLabels="std", residuals=FALSE)


