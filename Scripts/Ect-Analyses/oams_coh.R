source(here::here("updated_cleaning_code.R"))


## import OAM datasets
QBOrecon <- read.table(here("Data/climate patterns/QBOreconstruction.txt")) #1908-2015 
QBOrecord <- read.table(here("Data/climate patterns/newqborecord.txt")) #1979-2022
ENSOrecon <- read.table(here("Data/climate patterns/ENSOindex.txt")) #1301-2005
ENSOrecord <- read.table(here("Data/climate patterns/ENSOrecord.txt")) #1979-2022
NPGOrecon <- read.csv(here("Data/climate patterns/NPGOreconstruction.csv")) #1596-1990
NPGOrecord <- read.csv(here("Data/climate patterns/NPGOrecord.csv")) #1950-2023
PDO <- read.table(here("Data/climate patterns/PDOindex.txt")) #1854-2023


## clean up data sets to combine and make full time series
# QBO
QBOrecon <- janitor::row_to_names(dat = QBOrecon, row_number=1)
QBOrecon <- QBOrecon %>%
  dplyr::rename(month = MONTH)%>%
  dplyr::filter(YEAR < 1979) %>%
  dplyr::mutate(MONTH = case_when(month == 1 ~ "JAN",
                           month == 2 ~ "FEB",
                           month == 3 ~ "MAR",
                           month == 4 ~ "APR",
                           month == 5 ~ "MAY",
                           month == 6 ~ "JUN",
                           month == 7 ~ "JUL",
                           month == 8 ~ "AUG",
                           month == 9 ~ "SEP",
                           month == 10 ~ "OCT",
                           month == 11 ~ "NOV",
                           month == 12 ~ "DEC"))%>%
  dplyr::select(YEAR, MONTH, "30")
QBOrecord <- janitor::row_to_names(dat = QBOrecord, row_number=1)
QBOrecord <- QBOrecord %>%
  tidyr::pivot_longer(2:13, names_to = "MONTH", values_to = "30")
QBO <-rbind(QBOrecon, QBOrecord) #1908-2022
QBO <- QBO %>%
  dplyr::rename(qbo.index = "30")

#ENSO
ENSOrecord <- ENSOrecord %>%
  tidyr::pivot_longer(2:13, names_to = "month", values_to="sst") %>%
  dplyr::mutate(MONTH = case_when(month == "V2" ~ "JAN",
                                  month == "V3" ~ "FEB",
                                  month == "V4" ~ "MAR",
                                  month == "V5" ~ "APR",
                                  month == "V6" ~ "MAY",
                                  month == "V7" ~ "JUN",
                                  month == "V8" ~ "JUL",
                                  month == "V9" ~ "AUG",
                                  month == "V10" ~ "SEP",
                                  month == "V11" ~ "OCT",
                                  month == "V12" ~ "NOV",
                                  month == "V13" ~ "DEC")) %>%
  dplyr::rename(age_AD = "V1") %>%
  dplyr::select(age_AD, sst, MONTH) %>%
  dplyr::group_by(age_AD) %>%
  dplyr::summarise(sst.anom = mean(sst))
ENSOrecon <- janitor::row_to_names(dat = ENSOrecon, row_number=1) %>%
  dplyr::filter(age_AD < 1979)
ENSO <- rbind(ENSOrecon, ENSOrecord) #1301-2022

#NPGO
NPGOrecord <- NPGOrecord %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(npgo.index = mean(GO.index))
colnames(NPGOrecon) <- c("YEAR", "npgo.index")
NPGOrecon <- NPGOrecon %>%
  dplyr::filter(YEAR < 1950)
NPGO <- rbind(NPGOrecon, NPGOrecord) #1597-2023


## prepare data sets for coherence analyses
QBO$qbo.index <- as.numeric(QBO$qbo.index)
QBO <- QBO %>% 
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(qbo = mean(qbo.index))%>%
  dplyr::filter(YEAR < 2019) %>%
  tidyr::pivot_wider(names_from = "YEAR", values_from = qbo)
QBO <- QBO[rep(1:nrow(QBO),20),]  
colnames(QBO) <- NULL
QBO.mx <- as.matrix(QBO)
times <- 1908:2018
QBO.mx <- cleandat(QBO.mx, times, clev = 5)$cdat

avg_plot_growth_qbo <- avg_plot_growth_wide[, c(9:119)] #1908-2018
avg_plot_growth_qbo = as.data.frame(avg_plot_growth_qbo, stringsAsFactors = FALSE)
avg_plot_growth_qbo = map_df(avg_plot_growth_qbo, as.numeric)
avg_plot_growth_qbo_mx <- as.matrix(avg_plot_growth_qbo)
times.qbo = 1908:2018
avg_plot_growth_qbo_mx <- cleandat(avg_plot_growth_qbo_mx, times.qbo, clev = 5)$cdat

ENSO$sst.anom <- as.numeric(ENSO$sst.anom)
ENSO <- ENSO %>% 
  dplyr::filter(age_AD < 2019) %>%
  dplyr::filter(age_AD > 1899) %>%
  tidyr::pivot_wider(names_from = "age_AD", values_from = sst.anom)
ENSO <- ENSO[rep(1:nrow(ENSO),20),]  
colnames(ENSO) <- NULL
ENSO.mx <- as.matrix(ENSO)
times <- 1900:2018
ENSO.mx <- cleandat(ENSO.mx, times, clev = 5)$cdat

NPGO <- NPGO %>% 
  dplyr::filter(YEAR < 2019) %>%
  dplyr::filter(YEAR > 1899) %>%
  tidyr::pivot_wider(names_from = "YEAR", values_from = npgo.index)
NPGO <- NPGO[rep(1:nrow(NPGO),20),]  
colnames(NPGO) <- NULL
NPGO.mx <- as.matrix(NPGO)
times <- 1900:2018
NPGO.mx <- cleandat(NPGO.mx, times, clev = 5)$cdat

PDO <- janitor::row_to_names(dat = PDO, row_number=1)
PDO <- PDO %>%
  tidyr::pivot_longer(2:13, names_to = "MONTH", values_to ="PDO")%>%
  dplyr::group_by(Year) %>%
  summarise(pdo.index = mean(as.numeric(PDO)))
PDO <- PDO %>% 
  dplyr::filter(Year < 2019) %>%
  dplyr::filter(Year > 1899) %>%
  tidyr::pivot_wider(names_from = "Year", values_from = pdo.index)
PDO <- PDO[rep(1:nrow(PDO),20),]  
colnames(PDO) <- NULL
PDO.mx <- as.matrix(PDO)
times <- 1900:2018
PDO.mx <- cleandat(PDO.mx, times, clev = 5)$cdat

## run coherence tests 
x = avg_plot_growth_mx
x.qbo = avg_plot_growth_qbo_mx
y1 = QBO.mx
y2 = ENSO.mx
y3 = NPGO.mx
y4 = PDO.mx

times.qbo = 1908:2018
tv_qbo <- coh_tv(dat1 = x.qbo, dat2 = y1, times = times.qbo, norm = "powall",
                            sigmethod = "fftsurrog1", nrand = 1000)
times = 1900:2018
tv_enso <- coh_tv(dat1 = x, dat2 = y2, times = times, norm = "powall",
                 sigmethod = "fftsurrog1", nrand = 1000)
tv_npgo <- coh_tv(dat1 = x, dat2 = y3, times = times, norm = "powall",
                  sigmethod = "fftsurrog1", nrand = 1000)
tv_pdo <- coh_tv(dat1 = x, dat2 = y4, times = times, norm = "powall",
                  sigmethod = "fftsurrog1", nrand = 1000)

# average coherence across timescale bands for each timestep
# qbo
coh.qbo <- as.data.frame(tv_qbo$signif$gt)
colnames(coh.qbo) <- tv_qbo$timescales
coh.qbo$times <- tv_qbo$times
coh.qbo <- coh.qbo %>%
  tidyr::pivot_longer(1:65, names_to = "ts", values_to = "coh")
coh.qbo$ts <- as.numeric(coh.qbo$ts)
coh.qbo <- coh.qbo %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "annual",
                          ts > 3  & ts <= 10 ~ "interannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.qbo <- coh.qbo %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.qbo <- na.omit(avg.coh.qbo)  

# enso
coh.enso <- as.data.frame(tv_enso$signif$gt)
colnames(coh.enso) <- tv_enso$timescales
coh.enso$times <- tv_enso$times
coh.enso <- coh.enso %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.enso$ts <- as.numeric(coh.enso$ts)
coh.enso <- coh.enso %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "annual",
                          ts > 3  & ts <= 10 ~ "interannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.enso <- coh.enso %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.enso <- na.omit(avg.coh.enso)  

# npgo
coh.npgo <- as.data.frame(tv_npgo$signif$gt)
colnames(coh.npgo) <- tv_npgo$timescales
coh.npgo$times <- tv_npgo$times
coh.npgo <- coh.npgo %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.npgo$ts <- as.numeric(coh.npgo$ts)
coh.npgo <- coh.npgo %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "annual",
                          ts > 3  & ts <= 10 ~ "interannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.npgo <- coh.npgo %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.npgo <- na.omit(avg.coh.npgo)  

#pdo
coh.pdo <- as.data.frame(tv_pdo$signif$gt)
colnames(coh.pdo) <- tv_pdo$timescales
coh.pdo$times <- tv_pdo$times
coh.pdo <- coh.pdo %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")
coh.pdo$ts <- as.numeric(coh.pdo$ts)
coh.pdo <- coh.pdo %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "annual",
                          ts > 3  & ts <= 10 ~ "interannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

avg.coh.pdo <- coh.pdo %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.pdo <- na.omit(avg.coh.pdo) 

# combine into one data frame for plotting purposes
avg.coh.qbo$driver <- "qbo"
avg.coh.enso$driver <- "enso"
avg.coh.npgo$driver <- "npgo"
avg.coh.pdo$driver <- "pdo"
avg.tv.coh <- rbind(avg.coh.qbo, avg.coh.enso, avg.coh.npgo, avg.coh.pdo)
avg.tv.coh$times <- as.character(avg.tv.coh$times)
avg.tv.coh$band <- factor(avg.tv.coh$band , levels=c('annual', 'interannual', 'decadal', 'multidecadal'))

# plot avg coherence across time per band for each driver
oams_coh <- ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = band, color = band)) +
  facet_wrap(~ driver)+
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
  ylab("Average Coherence")+
  xlab("Year")

png("/Users/kaitlynmcknight/Documents/Teamtree_finalfigures/oams_coh.png", width = 5, height = 5, units = 'in', res = 600)
oams_coh
dev.off()

ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
  facet_wrap(~ band)+
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
  ylab("Average Coherence")+
  xlab("Year")




