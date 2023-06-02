

# source the data
source(here::here("scripts/cleaning_code.R"))




{sites <- unique(rwi_00s_filtered_wide$plot)
  times <- 1900:2018
  pdf(file="./Figures/wpmf.pdf",width=11,height=11,onefile=T)
  for (s in 1:length(sites)){
    temp <- rwi_00s_filtered_wide[which(rwi_00s_filtered_wide$plot == sites[s]),]
    temp <- temp[,-c(1,2)]
    temp <- as.matrix(temp)
    names(temp) <- NULL
    
    temp_clean <- cleandat(temp, times, 1)
    temp_res <- wpmf(temp_clean$cdat,times,sigmethod="quick")
    
    #png(filename=paste0("./wavelet_tree_",sites[s],".png"),width=1000,height=768)
    plotmag(temp_res)
    #dev.off()
  }
  dev.off()
}


# calculate signficance thresholds 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
thresholds <- cbind(upper,lower)
rownames(thresholds) <- NULL

#extract raw values
M1 <- as.data.frame(res_growth_wpmf$values)
colnames(M1) <- res_growth_wpmf$timescales
#fix the imaginary #s
M1 <- abs(M1)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)

M2 <- M1[,1:66]
year <- 1901:2018
M2$year <- year
# classify sync, async and ns
M2<- M2 %>%
  pivot_longer(1:66, names_to = "ts", values_to = "values")
M2 <- na.omit(M2)
M2events <- M2 %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
M2events$ts <- as.numeric(M2events$ts)
M2events$ts <- as.numeric(M2events$ts)
M2events <- M2events %>%
  mutate(interval = case_when(ts >= 2 & ts <= 5 ~ "short",
                              ts > 5 & ts <= 10 ~ "medium",
                              ts > 10 & ts <= 20 ~ "long",
                              ts > 20 & ts <= 30 ~ "xlong"))


#### PROPORTIONS OF SIGNFICANT SYNCHRONY ####
prop_sync_final <- data.frame(year = NA, synch = NA, asynch = NA, ns = NA, interval = NA, obs = NA)

for (xx in 1:length(unique(M2events$interval))) {
  
  current <- unique(M2events$interval)[xx]
  M2events_s <- M2events %>%
    filter(interval == current)
  
  #significantly synchronous in the short term
  SSs <- M2events_s %>%
    filter(event == "synchronous")%>%
    group_by(year)%>%
    summarise(short_sync = n())
  
  #significantly asynchronous in the short term
  SAs <- M2events_s %>%
    filter(event == "asynchronous")%>%
    group_by(year)%>%
    summarise(short_async = n())
  
  #not significant in the short term
  NSs <- M2events_s %>%
    filter(event == "NS")%>%
    group_by(year)%>%
    summarise(short_NS = n())
  
  #the number of observations per year to divide by to get the proportion
  prop_den_s <- M2events_s %>%
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
  
  prop_sync_final <- rbind(prop_sync_final, plot1_temp)
}

prop_sync_final <- prop_sync_final[2:381,]

prop_sync_final[is.na(prop_sync_final)] <- 0

#### PLOT WAVELET PHASOR MEAN FIELD ####
wav<-Mod(get_values(res_growth_wpmf))
times<-get_times(res_growth_wpmf)
timescales<-get_timescales(res_growth_wpmf)

plotmag(res_growth_wpmf)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=upper,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1, add=TRUE)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=lower,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1,col="white", add=TRUE)



