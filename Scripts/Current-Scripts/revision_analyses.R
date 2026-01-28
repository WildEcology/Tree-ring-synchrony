source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))

#### full timeseries wlm ####
# define variables into a list
# variables include growth, precip, and temp in their cleaned matrix format (aka. cleandat has already been applied)
# avg_plot_growth_mx, winter_ppt_mx, summer_tmin_mx
dat <- list(rwi=avg_plot_growth_mx, ppt=winter_ppt_mx, tmin=summer_tmin_mx)
times <- 1900:2018
wlm_full<-wlm(dat,times,resp=1,pred=2:3,norm="powall")

# plot the predicted synchrony based on wlm with two predictors
prediction <- predsync(wlm_full)
plotmag(prediction)

# determine whether or not we can drop a variable out of the model
wlm_all_drop1<-wlmtest(wlm_full,drop="ppt",sigmethod="fft",nrand=1000)
wlm_all_drop2<-wlmtest(wlm_full,drop="tmin",sigmethod="fft",nrand=1000)

# specify timescale bands of interest
biennial <- c(2,3)
multiannual <- c(3,10)
decadal <- c(10, 20)
multidecadal <- c(20,30)

# band test dropping each variable
wlm_all_drop1<-bandtest(wlm_all_drop1,biennial)
wlm_all_drop1<-bandtest(wlm_all_drop1,multiannual)
wlm_all_drop1<-bandtest(wlm_all_drop1,decadal)
wlm_all_drop1<-bandtest(wlm_all_drop1,multidecadal)


wlm_all_drop2<-bandtest(wlm_all_drop2,biennial)
wlm_all_drop2<-bandtest(wlm_all_drop2,multiannual)
wlm_all_drop2<-bandtest(wlm_all_drop2,decadal)
wlm_all_drop2<-bandtest(wlm_all_drop2,multidecadal)

# results of dropping each variable across bands, results will layer for each band
get_bandp(wlm_all_drop1)
get_bandp(wlm_all_drop2)

plotmag(wlm_all_drop1)
plotrank(wlm_all_drop1)

plotmag(wlm_all_drop2)
plotrank(wlm_all_drop2)


se <- syncexpl(wlm_full)
se_biennial <- se[se$timescales>=biennial[1] & se$timescales<=biennial[2],]
se_biennial_output <- round(100*colMeans(se_biennial[,c(3:9)])/mean(se_biennial$sync),2)

se_multiannual <- se[se$timescales>=multiannual[1] & se$timescales<=multiannual[2],]
se_multiannual_output <- round(100*colMeans(se_multiannual[,c(3:9)])/mean(se_multiannual$sync),2)

se_decadal <- se[se$timescales>=decadal[1] & se$timescales<=decadal[2],]
se_decadal_output <- round(100*colMeans(se_decadal[,c(3:9)])/mean(se_decadal$sync),2)

se_multidecadal <- se[se$timescales>=multidecadal[1] & se$timescales<=multidecadal[2],]
se_multidecadal_output <- round(100*colMeans(se_multidecadal[,c(3:9)])/mean(se_multidecadal$sync),2)


#### before temp breakpoint wlm ####
# filter variables before breakpoint (1992), 2018-1992 = 26, 119-26=93
#rwi
avg_plot_growth_wide_before_bp <- as.matrix(avg_plot_growth_wide)
avg_plot_growth_wide_before_bp <- avg_plot_growth_wide[, c(1:93)] # time series 1900-1992

# convert character matrix to numeric
avg_plot_growth_wide_before_bp = as.data.frame(avg_plot_growth_wide_before_bp, stringsAsFactors = FALSE)
avg_plot_growth_wide_before_bp = map_df(avg_plot_growth_wide_before_bp, as.numeric)
avg_plot_growth_mx_before_bp <- as.matrix(avg_plot_growth_wide_before_bp)

# clean data for wmf
times <- 1900:1992
avg_plot_growth_mx_before_bp <- cleandat(avg_plot_growth_mx_before_bp, times, clev = 5)$cdat

#ppt
winter_ppt_wide_before_bp <- as.matrix(winter_ppt_wide)
winter_ppt_wide_before_bp <- winter_ppt_wide[, c(1:93)] # time series 1900 -1992

# convert character matrix to numeric
winter_ppt_wide_before_bp = as.data.frame(winter_ppt_wide_before_bp, stringsAsFactors = FALSE)
winter_ppt_wide_before_bp = map_df(winter_ppt_wide_before_bp, as.numeric)
winter_ppt_mx_before_bp <- as.matrix(winter_ppt_wide_before_bp)

# clean data for wmf
times <- 1900:1992
winter_ppt_mx_before_bp <- cleandat(winter_ppt_mx_before_bp, times, clev = 5)$cdat

#tmin
summer_tmin_wide_before_bp <- as.matrix(summer_tmin_wide)
summer_tmin_wide_before_bp <- summer_tmin_wide[, c(1:93)] # time series 1900 -1992

# convert character matrix to numeric
summer_tmin_wide_before_bp = as.data.frame(summer_tmin_wide_before_bp, stringsAsFactors = FALSE)
summer_tmin_wide_before_bp = map_df(summer_tmin_wide_before_bp, as.numeric)
summer_tmin_mx_before_bp <- as.matrix(summer_tmin_wide_before_bp)

# clean data for wmf
times <- 1900:1992
summer_tmin_mx_before_bp <- cleandat(summer_tmin_mx_before_bp, times, clev = 5)$cdat

# list variables 
dat <- list(rwi=avg_plot_growth_mx_before_bp, ppt=winter_ppt_mx_before_bp, tmin=summer_tmin_mx_before_bp)
times <- 1900:1992
wlm_before_bp<-wlm(dat,times,resp=1,pred=2:3,norm="powall",scale.max.input=28)

# determine whether or not we can drop a variable out of the model
wlm_before_bp_drop1<-wlmtest(wlm_before_bp,drop="ppt",sigmethod="fft",nrand=1000)
wlm_before_bp_drop2<-wlmtest(wlm_before_bp,drop="tmin",sigmethod="fft",nrand=1000)

# specify timescale bands of interest
biennial <- c(2,3)
multiannual <- c(3,10)
decadal <- c(10, 20)
multidecadal <- c(20,30)

# band test dropping each variable
wlm_before_bp_drop1<-bandtest(wlm_before_bp_drop1,biennial)
wlm_before_bp_drop1<-bandtest(wlm_before_bp_drop1,multiannual)
wlm_before_bp_drop1<-bandtest(wlm_before_bp_drop1,decadal)
wlm_before_bp_drop1<-bandtest(wlm_before_bp_drop1,multidecadal)

wlm_before_bp_drop2<-bandtest(wlm_before_bp_drop2,biennial)
wlm_before_bp_drop2<-bandtest(wlm_before_bp_drop2,multiannual)
wlm_before_bp_drop2<-bandtest(wlm_before_bp_drop2,decadal)
wlm_before_bp_drop2<-bandtest(wlm_before_bp_drop2,multidecadal)

# results of dropping each variable across bands, results will layer for each band
get_bandp(wlm_before_bp_drop1)
get_bandp(wlm_before_bp_drop2)

plotmag(wlm_before_bp_drop1)
plotrank(wlm_before_bp_drop1)

plotmag(wlm_before_bp_drop2)
plotrank(wlm_before_bp_drop2)


se_before_bp <- syncexpl(wlm_before_bp)
se_biennial_before_bp <- se_before_bp[se_before_bp$timescales>=biennial[1] & se_before_bp$timescales<=biennial[2],]
se_biennial_before_bp_output <- round(100*colMeans(se_biennial_before_bp[,c(3:9)])/mean(se_biennial_before_bp$sync),2)

se_multiannual_before_bp <- se_before_bp[se_before_bp$timescales>=multiannual[1] & se_before_bp$timescales<=multiannual[2],]
se_multiannual_before_bp_output <- round(100*colMeans(se_multiannual_before_bp[,c(3:9)])/mean(se_multiannual_before_bp$sync),2)

se_decadal_before_bp <- se_before_bp[se_before_bp$timescales>=decadal[1] & se_before_bp$timescales<=decadal[2],]
se_decadal_before_bp_output <- round(100*colMeans(se_decadal_before_bp[,c(3:9)])/mean(se_decadal_before_bp$sync),2)

se_multidecadal_before_bp <- se_before_bp[se_before_bp$timescales>=multidecadal[1] & se_before_bp$timescales<=multidecadal[2],]
se_multidecadal_before_bp_output <- round(100*colMeans(se_multidecadal_before_bp[,c(3:9)])/mean(se_multidecadal_before_bp$sync),2)


#### after temp breakpoint wlm ####
# filter variables after breakpoint (1992)
#rwi
avg_plot_growth_wide_after_bp <- as.matrix(avg_plot_growth_wide)
avg_plot_growth_wide_after_bp <- avg_plot_growth_wide[, c(94:119)] # time series 1993-2018

# convert character matrix to numeric
avg_plot_growth_wide_after_bp = as.data.frame(avg_plot_growth_wide_after_bp, stringsAsFactors = FALSE)
avg_plot_growth_wide_after_bp = map_df(avg_plot_growth_wide_after_bp, as.numeric)
avg_plot_growth_mx_after_bp <- as.matrix(avg_plot_growth_wide_after_bp)

# clean data for wmf
times <- 1993:2018
avg_plot_growth_mx_after_bp <- cleandat(avg_plot_growth_mx_after_bp, times, clev = 5)$cdat

#ppt
winter_ppt_wide_after_bp <- as.matrix(winter_ppt_wide)
winter_ppt_wide_after_bp <- winter_ppt_wide[, c(94:119)] # time series 1993-2018

# convert character matrix to numeric
winter_ppt_wide_after_bp = as.data.frame(winter_ppt_wide_after_bp, stringsAsFactors = FALSE)
winter_ppt_wide_after_bp = map_df(winter_ppt_wide_after_bp, as.numeric)
winter_ppt_mx_after_bp <- as.matrix(winter_ppt_wide_after_bp)

# clean data for wmf
times <- 1993:2018
winter_ppt_mx_after_bp <- cleandat(winter_ppt_mx_after_bp, times, clev = 5)$cdat

#tmin
summer_tmin_wide_after_bp <- as.matrix(summer_tmin_wide)
summer_tmin_wide_after_bp <- summer_tmin_wide[, c(94:119)] # time series 1993-2018

# convert character matrix to numeric
summer_tmin_wide_after_bp = as.data.frame(summer_tmin_wide_after_bp, stringsAsFactors = FALSE)
summer_tmin_wide_after_bp = map_df(summer_tmin_wide_after_bp, as.numeric)
summer_tmin_mx_after_bp <- as.matrix(summer_tmin_wide_after_bp)

# clean data for wmf
times <- 1993:2018
summer_tmin_mx_after_bp <- cleandat(summer_tmin_mx_after_bp, times, clev = 5)$cdat

# list variables 
dat <- list(rwi=avg_plot_growth_mx_after_bp, ppt=winter_ppt_mx_after_bp, tmin=summer_tmin_mx_after_bp)
times <- 1993:2018
wlm_after_bp<-wlm(dat,times,resp=1,pred=2:3,norm="powall") #removed scale.max.input for the model to select a new default based on number of years

# determine whether or not we can drop a variable out of the model
wlm_after_bp_drop1<-wlmtest(wlm_after_bp,drop="ppt",sigmethod="fft",nrand=1000)
wlm_after_bp_drop2<-wlmtest(wlm_after_bp,drop="tmin",sigmethod="fft",nrand=1000)

# specify timescale bands of interest
biennial <- c(2,3)
multiannual <- c(3,10)
decadal <- c(10, 20)
multidecadal <- c(20,30)

# band test dropping each variable
wlm_after_bp_drop1<-bandtest(wlm_after_bp_drop1,biennial)
wlm_after_bp_drop1<-bandtest(wlm_after_bp_drop1,multiannual)
wlm_after_bp_drop1<-bandtest(wlm_after_bp_drop1,decadal)
wlm_after_bp_drop1<-bandtest(wlm_after_bp_drop1,multidecadal)

wlm_after_bp_drop2<-bandtest(wlm_after_bp_drop2,biennial)
wlm_after_bp_drop2<-bandtest(wlm_after_bp_drop2,multiannual)
wlm_after_bp_drop2<-bandtest(wlm_after_bp_drop2,decadal)
wlm_after_bp_drop2<-bandtest(wlm_after_bp_drop2,multidecadal)

# results of dropping each variable across bands, results will layer for each band
get_bandp(wlm_after_bp_drop1)
get_bandp(wlm_after_bp_drop2)

plotmag(wlm_after_bp_drop1)
plotrank(wlm_after_bp_drop1)

plotmag(wlm_after_bp_drop2)
plotrank(wlm_after_bp_drop2)


se_after_bp <- syncexpl(wlm_after_bp)
se_biennial_after_bp <- se_after_bp[se_after_bp$timescales>=biennial[1] & se_after_bp$timescales<=biennial[2],]
se_biennial_after_bp_output <- round(100*colMeans(se_biennial_after_bp[,c(3:9)])/mean(se_biennial_after_bp$sync),2)

se_multiannual_after_bp <- se_after_bp[se_after_bp$timescales>=multiannual[1] & se_after_bp$timescales<=multiannual[2],]
se_multiannual_after_bp_output <- round(100*colMeans(se_multiannual_after_bp[,c(3:9)])/mean(se_multiannual_after_bp$sync),2)

se_decadal_after_bp <- se_after_bp[se_after_bp$timescales>=decadal[1] & se_after_bp$timescales<=decadal[2],]
se_decadal_after_bp_output <- round(100*colMeans(se_decadal_after_bp[,c(3:9)])/mean(se_decadal_after_bp$sync),2)

# not enough years to calculate at these timescales
#se_multidecadal_after_bp <- se_after_bp[se_after_bp$timescales>=multidecadal[1] & se_after_bp$timescales<=multidecadal[2],]
#se_multidecadal_after_bp_output <- round(100*colMeans(se_multidecadal_after_bp[,c(3:9)])/mean(se_multidecadal_after_bp$sync),2)



#### first half wlm ####
# filter variables for first half of timeseries, 2018-1900 = 118, 118/2=59
#rwi
avg_plot_growth_wide_first_half <- as.matrix(avg_plot_growth_wide)
avg_plot_growth_wide_first_half <- avg_plot_growth_wide[, c(1:60)] # time series 1900-1959

# convert character matrix to numeric
avg_plot_growth_wide_first_half = as.data.frame(avg_plot_growth_wide_first_half, stringsAsFactors = FALSE)
avg_plot_growth_wide_first_half = map_df(avg_plot_growth_wide_first_half, as.numeric)
avg_plot_growth_mx_first_half <- as.matrix(avg_plot_growth_wide_first_half)

# clean data for wmf
times <- 1900:1959
avg_plot_growth_mx_first_half <- cleandat(avg_plot_growth_mx_first_half, times, clev = 5)$cdat

#ppt
winter_ppt_wide_first_half <- as.matrix(winter_ppt_wide)
winter_ppt_wide_first_half <- winter_ppt_wide[, c(1:60)] # time series 1900 -1959

# convert character matrix to numeric
winter_ppt_wide_first_half = as.data.frame(winter_ppt_wide_first_half, stringsAsFactors = FALSE)
winter_ppt_wide_first_half = map_df(winter_ppt_wide_first_half, as.numeric)
winter_ppt_mx_first_half <- as.matrix(winter_ppt_wide_first_half)

# clean data for wmf
times <- 1900:1959
winter_ppt_mx_first_half <- cleandat(winter_ppt_mx_first_half, times, clev = 5)$cdat

#tmin
summer_tmin_wide_first_half <- as.matrix(summer_tmin_wide)
summer_tmin_wide_first_half <- summer_tmin_wide[, c(1:60)] # time series 1900 -1959

# convert character matrix to numeric
summer_tmin_wide_first_half = as.data.frame(summer_tmin_wide_first_half, stringsAsFactors = FALSE)
summer_tmin_wide_first_half = map_df(summer_tmin_wide_first_half, as.numeric)
summer_tmin_mx_first_half <- as.matrix(summer_tmin_wide_first_half)

# clean data for wmf
times <- 1900:1959
summer_tmin_mx_first_half <- cleandat(summer_tmin_mx_first_half, times, clev = 5)$cdat

# list variables 
dat <- list(rwi=avg_plot_growth_mx_first_half, ppt=winter_ppt_mx_first_half, tmin=summer_tmin_mx_first_half)
times <- 1900:1959
wlm_first_half<-wlm(dat,times,resp=1,pred=2:3,norm="powall")  #removed scale.max.input for the model to select a new default based on number of years

# determine whether or not we can drop a variable out of the model
wlm_first_half_drop1<-wlmtest(wlm_first_half,drop="ppt",sigmethod="fft",nrand=1000)
wlm_first_half_drop2<-wlmtest(wlm_first_half,drop="tmin",sigmethod="fft",nrand=1000)

# specify timescale bands of interest
biennial <- c(2,3)
multiannual <- c(3,10)
decadal <- c(10, 20)
multidecadal <- c(20,30)

# band test dropping each variable
wlm_first_half_drop1<-bandtest(wlm_first_half_drop1,biennial)
wlm_first_half_drop1<-bandtest(wlm_first_half_drop1,multiannual)
wlm_first_half_drop1<-bandtest(wlm_first_half_drop1,decadal)
wlm_first_half_drop1<-bandtest(wlm_first_half_drop1,multidecadal)

wlm_first_half_drop2<-bandtest(wlm_first_half_drop2,biennial)
wlm_first_half_drop2<-bandtest(wlm_first_half_drop2,multiannual)
wlm_first_half_drop2<-bandtest(wlm_first_half_drop2,decadal)
wlm_first_half_drop2<-bandtest(wlm_first_half_drop2,multidecadal)

# results of dropping each variable across bands, results will layer for each band
get_bandp(wlm_first_half_drop1)
get_bandp(wlm_first_half_drop2)

plotmag(wlm_first_half_drop1)
plotrank(wlm_first_half_drop1)

plotmag(wlm_first_half_drop2)
plotrank(wlm_first_half_drop2)


se_first_half <- syncexpl(wlm_first_half)
se_biennial_first_half <- se_first_half[se_first_half$timescales>=biennial[1] & se_first_half$timescales<=biennial[2],]
se_biennial_first_half_output <- round(100*colMeans(se_biennial_first_half[,c(3:9)])/mean(se_biennial_first_half$sync),2)

se_multiannual_first_half <- se_first_half[se_first_half$timescales>=multiannual[1] & se_first_half$timescales<=multiannual[2],]
se_multiannual_first_half_output <- round(100*colMeans(se_multiannual_first_half[,c(3:9)])/mean(se_multiannual_first_half$sync),2)

se_decadal_first_half <- se_first_half[se_first_half$timescales>=decadal[1] & se_first_half$timescales<=decadal[2],]
se_decadal_first_half_output <- round(100*colMeans(se_decadal_first_half[,c(3:9)])/mean(se_decadal_first_half$sync),2)

se_multidecadal_first_half <- se_first_half[se_first_half$timescales>=multidecadal[1] & se_first_half$timescales<=multidecadal[2],]
se_multidecadal_first_half_output <- round(100*colMeans(se_multidecadal_first_half[,c(3:9)])/mean(se_multidecadal_first_half$sync),2)


#### last half wlm ####
# filter variables for last half of timeseries, 1960-2018
#rwi
avg_plot_growth_wide_last_half <- as.matrix(avg_plot_growth_wide)
avg_plot_growth_wide_last_half <- avg_plot_growth_wide[, c(61:119)] # time series 1993-2018

# convert character matrix to numeric
avg_plot_growth_wide_last_half = as.data.frame(avg_plot_growth_wide_last_half, stringsAsFactors = FALSE)
avg_plot_growth_wide_last_half = map_df(avg_plot_growth_wide_last_half, as.numeric)
avg_plot_growth_mx_last_half <- as.matrix(avg_plot_growth_wide_last_half)

# clean data for wmf
times <- 1960:2018
avg_plot_growth_mx_last_half <- cleandat(avg_plot_growth_mx_last_half, times, clev = 5)$cdat

#ppt
winter_ppt_wide_last_half <- as.matrix(winter_ppt_wide)
winter_ppt_wide_last_half <- winter_ppt_wide[, c(61:119)] # time series 1993-2018

# convert character matrix to numeric
winter_ppt_wide_last_half = as.data.frame(winter_ppt_wide_last_half, stringsAsFactors = FALSE)
winter_ppt_wide_last_half = map_df(winter_ppt_wide_last_half, as.numeric)
winter_ppt_mx_last_half <- as.matrix(winter_ppt_wide_last_half)

# clean data for wmf
times <- 1960:2018
winter_ppt_mx_last_half <- cleandat(winter_ppt_mx_last_half, times, clev = 5)$cdat

#tmin
summer_tmin_wide_last_half <- as.matrix(summer_tmin_wide)
summer_tmin_wide_last_half <- summer_tmin_wide[, c(61:119)] # time series 1960-2018

# convert character matrix to numeric
summer_tmin_wide_last_half = as.data.frame(summer_tmin_wide_last_half, stringsAsFactors = FALSE)
summer_tmin_wide_last_half = map_df(summer_tmin_wide_last_half, as.numeric)
summer_tmin_mx_last_half <- as.matrix(summer_tmin_wide_last_half)

# clean data for wmf
times <- 1960:2018
summer_tmin_mx_last_half <- cleandat(summer_tmin_mx_last_half, times, clev = 5)$cdat

# list variables 
dat <- list(rwi=avg_plot_growth_mx_last_half, ppt=winter_ppt_mx_last_half, tmin=summer_tmin_mx_last_half)
times <- 1960:2018
wlm_last_half<-wlm(dat,times,resp=1,pred=2:3,norm="powall") #removed scale.max.input for the model to select a new default based on number of years

# determine whether or not we can drop a variable out of the model
wlm_last_half_drop1<-wlmtest(wlm_last_half,drop="ppt",sigmethod="fft",nrand=1000)
wlm_last_half_drop2<-wlmtest(wlm_last_half,drop="tmin",sigmethod="fft",nrand=1000)

# specify timescale bands of interest
biennial <- c(2,3)
multiannual <- c(3,10)
decadal <- c(10, 20)
multidecadal <- c(20,30)

# band test dropping each variable
wlm_last_half_drop1<-bandtest(wlm_last_half_drop1,biennial)
wlm_last_half_drop1<-bandtest(wlm_last_half_drop1,multiannual)
wlm_last_half_drop1<-bandtest(wlm_last_half_drop1,decadal)
wlm_last_half_drop1<-bandtest(wlm_last_half_drop1,multidecadal)

wlm_last_half_drop2<-bandtest(wlm_last_half_drop2,biennial)
wlm_last_half_drop2<-bandtest(wlm_last_half_drop2,multiannual)
wlm_last_half_drop2<-bandtest(wlm_last_half_drop2,decadal)
wlm_last_half_drop2<-bandtest(wlm_last_half_drop2,multidecadal)

# results of dropping each variable across bands, results will layer for each band
get_bandp(wlm_last_half_drop1)
get_bandp(wlm_last_half_drop2)

plotmag(wlm_last_half_drop1)
plotrank(wlm_last_half_drop1)

plotmag(wlm_last_half_drop2)
plotrank(wlm_last_half_drop2)


se_last_half <- syncexpl(wlm_last_half)
se_biennial_last_half <- se_last_half[se_last_half$timescales>=biennial[1] & se_last_half$timescales<=biennial[2],]
se_biennial_last_half_output <- round(100*colMeans(se_biennial_last_half[,c(3:9)])/mean(se_biennial_last_half$sync),2)

se_multiannual_last_half <- se_last_half[se_last_half$timescales>=multiannual[1] & se_last_half$timescales<=multiannual[2],]
se_multiannual_last_half_output <- round(100*colMeans(se_multiannual_last_half[,c(3:9)])/mean(se_multiannual_last_half$sync),2)

se_decadal_last_half <- se_last_half[se_last_half$timescales>=decadal[1] & se_last_half$timescales<=decadal[2],]
se_decadal_last_half_output <- round(100*colMeans(se_decadal_last_half[,c(3:9)])/mean(se_decadal_last_half$sync),2)


se_multidecadal_last_half <- se_last_half[se_last_half$timescales>=multidecadal[1] & se_last_half$timescales<=multidecadal[2],]
se_multidecadal_last_half_output <- round(100*colMeans(se_multidecadal_last_half[,c(3:9)])/mean(se_multidecadal_last_half$sync),2)



#### SEMs ####
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

# split model_df into temp quartile 1-2 and 3-4

model_df_12 <- model_df %>%
  filter(tmin_quartile < 3) %>%
  na.omit()

model_df_34 <- model_df %>%
  filter(tmin_quartile >= 3) %>%
  na.omit()

# split quartile grouped df into timescale bands
model_df_12_b <- model_df_12 %>%
  filter(band == "biennial")

model_df_12_ma <- model_df_12 %>%
  filter(band == "multiannual")

model_df_12_d <- model_df_12 %>%
  filter(band == "decadal")

model_df_12_md <- model_df_12 %>%
  filter(band == "multidecadal")

model_df_34_b <- model_df_34 %>%
  filter(band == "biennial")

model_df_34_ma <- model_df_34 %>%
  filter(band == "multiannual")

model_df_34_d <- model_df_34 %>%
  filter(band == "decadal")

model_df_34_md <- model_df_34 %>%
  filter(band == "multidecadal")

# sem biennial band quartiles 1-2
SEM.B.12 <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.B.12.fit <- sem(SEM.B.12, data=model_df_12_b)
summary(SEM.B.12.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.B.12.fit, standardized = TRUE)
semPaths(SEM.B.12.fit, what="std", whatLabels="std", residuals=FALSE)

# sem multiannual band quartiles 1-2
SEM.MA.12  <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.MA.12.fit <- sem(SEM.MA.12, data=model_df_12_ma)
summary(SEM.MA.12.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MA.12.fit, standardized = TRUE)
semPaths(SEM.MA.12.fit, what="std", whatLabels="std", residuals=FALSE)

# sem decadal band quartiles 1-2
SEM.D.12  <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.D.12.fit <- sem(SEM.D.12, data=model_df_12_d)
summary(SEM.D.12.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.D.12.fit, standardized = TRUE)
semPaths(SEM.D.12.fit, what="std", whatLabels="std", residuals=FALSE)

# sem multidecadal band quartiles 1-2
SEM.MD.12  <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.MD.12.fit <- sem(SEM.MD.12, data=model_df_12_md)
summary(SEM.MD.12.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MD.12.fit, standardized = TRUE)
semPaths(SEM.MD.12.fit, what="std", whatLabels="std", residuals=FALSE)





# sem biennial band quartiles 3-4
SEM.B.34 <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.B.34.fit <- sem(SEM.B.34, data=model_df_34_b)
summary(SEM.B.34.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.B.34.fit, standardized = TRUE)
semPaths(SEM.B.34.fit, what="std", whatLabels="std", residuals=FALSE)

# sem multiannual band quartiles 3-4
SEM.MA.34  <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.MA.34.fit <- sem(SEM.MA.34, data=model_df_34_ma)
summary(SEM.MA.34.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MA.34.fit, standardized = TRUE)
semPaths(SEM.MA.34.fit, what="std", whatLabels="std", residuals=FALSE)

# sem decadal band quartiles 3-4
SEM.D.34  <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.D.34.fit <- sem(SEM.D.34, data=model_df_34_d)
summary(SEM.D.34.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.D.34.fit, standardized = TRUE)
semPaths(SEM.D.34.fit, what="std", whatLabels="std", residuals=FALSE)

# sem multidecadal band quartiles 3-4
SEM.MD.34  <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.MD.34.fit <- sem(SEM.MD.34, data=model_df_34_md)
summary(SEM.MD.34.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.MD.34.fit, standardized = TRUE)
semPaths(SEM.MD.34.fit, what="std", whatLabels="std", residuals=FALSE)


# SEM 1-2 across all timescales
SEM.12 <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.12.fit <- sem(SEM.12, data=model_df_12)
summary(SEM.12.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.12.fit, standardized = TRUE)
semPaths(SEM.12.fit, what="std", whatLabels="std", residuals=FALSE)


# SEM 3-4 across all timescales
SEM.34  <- '
  ts_ppt ~~ ts_tmin
  ppt_sync ~~ tmin_sync
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

SEM.34.fit <- sem(SEM.34, data=model_df_34)
summary(SEM.34.fit, stand=TRUE, rsq=TRUE)

parameterEstimates(SEM.34.fit, standardized = TRUE)
semPaths(SEM.34.fit, what="std", whatLabels="std", residuals=FALSE)




#### Coherence Magnitudes ####
source(here::here("Scripts/Current-Scripts/coh_tv.R"))
# data for each variable
x = avg_plot_growth_mx
y1 = winter_ppt_mx
y2 = summer_tmin_mx
times = 1900:2018

# calculate time varying coherence for each variable across whole time series
# leave f0 and scale_max_input blank
tv_timeseries_ppt <- coh_tv(dat1 = x, dat2 = y1, times = times, norm = "powall",
                            sigmethod = "fftsurrog1", nrand = 1000)
tv_timeseries_tmin <- coh_tv(dat1 = x, dat2 = y2, times = times, norm = "powall",
                             sigmethod = "fftsurrog1", nrand = 1000)



# average coherence across timescale bands for each timestep
# ppt
coh.ppt <- as.data.frame(tv_timeseries_ppt$coher) # grab magnitudes of coherence
coh.ppt<- abs(coh.ppt)
colnames(coh.ppt) <- tv_timeseries_ppt$timescales
coh.ppt$times <- tv_timeseries_ppt$times
coh.ppt <- coh.ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.ppt$ts <- as.numeric(coh.ppt$ts)
coh.ppt <- coh.ppt %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.ppt <- coh.ppt %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.ppt <- na.omit(avg.coh.ppt)  

# tmin
coh.tmin <- as.data.frame(tv_timeseries_tmin$coher)
coh.tmin <- abs(coh.tmin)
colnames(coh.tmin) <- tv_timeseries_tmin$timescales
coh.tmin$times <- tv_timeseries_tmin$times
coh.tmin <- coh.tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.tmin$ts <- as.numeric(coh.tmin$ts)
coh.tmin <- coh.tmin %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.tmin <- coh.tmin %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.tmin <- na.omit(avg.coh.tmin)  

# combine into one data frame for plotting purposes
avg.coh.ppt$driver <- "ppt"
avg.coh.tmin$driver <- "tmin"
avg.tv.coh <- rbind(avg.coh.ppt, avg.coh.tmin)
avg.tv.coh$times <- as.character(avg.tv.coh$times)
avg.tv.coh$band <- factor(avg.tv.coh$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

# plot avg coherence across time per band for each driver

labels <- c(annual = "biennial", interannual = "multiannual", decadal = "decadal", multidecadal = "multidecadal")
avg.tv.coh$driver <- factor(avg.tv.coh$driver, levels=c('ppt', 'tmin'))
ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
  facet_grid(rows = "band", labeller=labeller(band = c("biennial" = "Biennial", "multiannual" = "Multiannual", "decadal" = "Decadal", "multidecadal"= "Multidecadal")))+
  theme_bw()+
  scale_color_manual(values = c("#377EB8", "#E41A1C"), labels = c("Winter Precipitation", "Summer Temperatures"))+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
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
  xlab("Year")


#### Coherence Sig ####
# average significant coherence across timescale bands for each timestep
# ppt
coh.ppt <- as.data.frame(tv_timeseries_ppt$signif$gt) # grab values greater than surrogate values
colnames(coh.ppt) <- tv_timeseries_ppt$timescales
coh.ppt$times <- tv_timeseries_ppt$times
coh.ppt <- coh.ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.ppt$ts <- as.numeric(coh.ppt$ts)
coh.ppt <- coh.ppt %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.ppt <- coh.ppt %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.ppt <- na.omit(avg.coh.ppt)  

# tmin
coh.tmin <- as.data.frame(tv_timeseries_tmin$signif$gt)
colnames(coh.tmin) <- tv_timeseries_tmin$timescales
coh.tmin$times <- tv_timeseries_tmin$times
coh.tmin <- coh.tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.tmin$ts <- as.numeric(coh.tmin$ts)
coh.tmin <- coh.tmin %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.tmin <- coh.tmin %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.tmin <- na.omit(avg.coh.tmin)  

# combine into one data frame for plotting purposes
avg.coh.ppt$driver <- "ppt"
avg.coh.tmin$driver <- "tmin"
avg.tv.coh <- rbind(avg.coh.ppt, avg.coh.tmin)
avg.tv.coh$times <- as.character(avg.tv.coh$times)
avg.tv.coh$band <- factor(avg.tv.coh$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))

# plot avg coherence across time per band for each driver
labels <- c(annual = "biennial", interannual = "multiannual", decadal = "decadal", multidecadal = "multidecadal")
avg.tv.coh$driver <- factor(avg.tv.coh$driver, levels=c('ppt', 'tmin'))
ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
  facet_grid(rows = "band", labeller=labeller(band = c("biennial" = "Biennial", "multiannual" = "Multiannual", "decadal" = "Decadal", "multidecadal"= "Multidecadal")))+
  theme_bw()+
  scale_color_manual(values = c("#377EB8", "#E41A1C"), labels = c("Winter Precipitation", "Summer Temperatures"))+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Significant Coherence")+
  xlab("Year")



