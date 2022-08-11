library(wsyn)
library(dbplyr)
library(ggplot2)
library(tidyverse)
# simulate data for example timeseries and wavelets


times1 <- 0:56
times2 <- 57:119
times <- c(times1, times2)
ts1 <- c(sin(2*pi*times1/10), sin(2*pi*times2/2))+1.1

dat <- matrix(NA, 20, length(times))
for(counter in 1:dim(dat)[1])
{
  ts2 <-  5*sin(5*pi*times/5+2*pi*runif(1))+5.1
  ts3 <- rnorm(length(times), 0, 1)
  dat[counter,] <- ts1+ts2+ts3
}
dat <- cleandat(dat,times,1)$cdat

plot(times,dat[1,]/10+1,type='l',xlab="Time",ylab="Time series index",ylim=c(0,12))
for (counter in 2:dim(dat)[1])
{
  lines(times,dat[counter,]/10+counter)
}


dat_df <- as.data.frame(dat)
colnames(dat_df) <- 1900:2019
dat_df$plot <- c(1:20)

dat_df <- dat_df %>%
  pivot_longer(1:119, names_to = "year", values_to = "growth")%>%
  mutate(rwi = growth+10)

dat_var <- dat_df %>%
  group_by(year)%>%
  mutate(cv = sd(rwi)/mean(rwi))
  




dat_var <- sapply(dat_df, function(x) sd(x)/mean(x) * 100)


dat_df$year <- as.numeric(dat_df$year)
dat_df$plot <- as.character(dat_df$plot)

plot810 <- dat_df %>%
  filter(plot == 8 | plot == 10)

fanmrfox <- c("#46ACC8", "#E58601", "#B40F20")

ggplot(plot810, aes(x=year, y=rwi, group = plot, col = plot))+
  geom_line(size = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(color = "grey20", size = 28, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.title.x = element_text(color = "grey20", size = 32, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 32, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text(color = "grey20", size = 28),
        legend.title = element_text(color = "grey20", size = 32))+
  #facet_grid(plot~.)
  scale_color_manual(values = fanmrfox,
                     name = "Population",
                     labels = c("North", "South"))+
  scale_x_continuous(limits = c(1965,1990))+
  xlab("Year")+
  ylab("Annual Growth")



plot8 <- dat_df %>%
  filter(plot == 8)



ggplot(plot8, aes(x=year, y=rwi))+
  geom_line(size = 1, col = "#46ACC8")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "grey20", size = 28, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.title.x = element_text(color = "grey20", size = 32, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 32, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  scale_x_continuous(limits = c(1965,1990))+
  xlab("Year")+
  ylab("Annual Growth")



plot10 <- dat_df %>%
  filter(plot == 10)


ggplot(plot10, aes(x=year, y=rwi))+
  geom_line(size = 1, col = "#E58601")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "grey20", size = 28, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.title.x = element_text(color = "grey20", size = 32, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 32, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  scale_x_continuous(limits = c(1965,1990))+
  xlab("Year")+
  ylab("Annual Growth")

n <- length(unique(dat_df$plot))
psync.by.chance <- function(n, nreps=10000, prob=c(0.025, 0.975)){
  
  #generates sets of random phasors
  rndphas <- matrix(complex(modulus=1, argument=2*pi*runif(n*nreps)), n, nreps)
  #takes the average--this is analogous to the wavelet phasor mean field
  rndphas.avg <- Mod(apply(rndphas, FUN=mean, MARGIN=2))
  #spit out quantiles corresponding to prob
  return(quantile(rndphas.avg, prob))
  
}

temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]
  
res <- wpmf(dat, times=1900:2019, sigmethod = "none")

wav<-Mod(get_values(res))
times<-get_times(res)
timescales<-get_timescales(res)
signif<-get_signif(res)
plotmag(res, cex.lab = 2)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=upper,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1, add=TRUE)
graphics::contour(x=times,y=log2(timescales),z=wav,levels=lower,drawlabels=F,lwd=2,
                  xaxs="i",xaxt="n",xaxp=c(0,1,5),las=1,frame=F,lty=1,col="white", add=TRUE)




