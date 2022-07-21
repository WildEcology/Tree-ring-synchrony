

prismdat <- read.csv(here("Data/rwi_prismdat.csv"))

# subset 1900-2018
rwi_00s <- prismdat %>%
  filter(year >= 1900)%>%
  drop_na(value)%>%
  rename(site = plot)%>%
  rename(plot = plot_id_needle)
# find plots and years with at least 5 trees 
rwi_5 <- rwi_00s%>%
  group_by(year, plot)%>%
  summarise(number = n())%>%
  filter(number >= 5)
# subsetting only plots with at least 5 trees
rwi_00s_unfiltered_long <- left_join(rwi_5, rwi_00s, by = c("year", "plot"))%>%
  select(year, plot, tree_num, value)
rwi_00s_unfiltered_wide <- rwi_00s_unfiltered_long %>%
  pivot_wider(names_from = year, values_from = value)
# removing any trees with missing data along the timeseries
rwi_00s_filtered_wide<- rwi_00s_unfiltered_wide[rowSums(is.na(rwi_00s_unfiltered_wide))==0,]
rwi_00s_filtered_long <- rwi_00s_filtered_wide %>%
  pivot_longer(3:121, names_to = "year", values_to = "rwi")

# number of trees per plot
tree_per_plot <- rwi_00s_filtered_long %>%
  group_by(plot, year)%>%
  summarize(numtree = n())%>%
  distinct(plot, numtree)

#synchrony by chance function
psync.by.chance <- function(n, nreps=10000, prob=c(0.025, 0.975)){
  
  #generates sets of random phasors
  rndphas <- matrix(complex(modulus=1, argument=2*pi*runif(n*nreps)), n, nreps)
  #takes the average--this is analogous to the wavelet phasor mean field
  rndphas.avg <- Mod(apply(rndphas, FUN=mean, MARGIN=2))
  #spit out quantiles corresponding to prob
  return(quantile(rndphas.avg, prob))
  
}


#### WITHIN-PLOT WAVELET ANALYSIS - ALL PLOTS ####
{plots <- unique(tree_per_plot$plot)
pdf(file="./Figures/wpmf.pdf",width=11,height=11,onefile=T)
for(s in 1:length(plots)){
  current.plot<-plots[s]
  temp<-tree_per_plot %>% 
    filter(plot %in% current.plot) %>% 
    select(-plot)
  n<-temp$numtree[1]
  nreps <- 1000
  plot_psbc <- psync.by.chance(n=n, nreps=nreps, prob=c(0.025, 0.975))
  upper <- plot_psbc[2]
  lower <- plot_psbc[1]
  
  rwi_00s_plot <- rwi_00s_filtered_wide %>%
    filter(plot == current.plot)
  
  # format matrix for analysis
  rwi_00s_plot <- as.matrix(rwi_00s_plot)
  colnames(rwi_00s_plot) <- NULL
  rwi_00s_plot <- rwi_00s_plot[, c(3:121)] 
  
  # convert character matrix to numeric
  rwi_00s_plot = as.data.frame(rwi_00s_plot, stringsAsFactors = FALSE)
  rwi_00s_plot = map_df(rwi_00s_plot, as.numeric)
  rwi_00s_plot_mx <- as.matrix(rwi_00s_plot)
  
  # calculate wavelet
  times <- 1900:2018
  rwi_00s_plot_mx <- cleandat(rwi_00s_plot_mx, times, 1)
  res<-wpmf(rwi_00s_plot_mx$cdat,times,sigmethod="none")
  
  wav<-Mod(get_values(res))
  times<-get_times(res)
  timescales<-get_timescales(res)
  signif<-get_signif(res)
  
plotmag(res, title = current.plot)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=upper,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1, add=TRUE)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=lower,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1,col="white", add=TRUE)
}
dev.off()
}

