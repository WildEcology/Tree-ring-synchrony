# multiannual
# load appropriate data
b_clust_1_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/b_clust_1_wmf.rds")
b_clust_2_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/b_clust_2_wmf.rds")

#extract raw values
clust1 <- as.data.frame(b_clust_1_wmf$values)
colnames(clust1) <- b_clust_1_wmf$timescales
#fix the imaginary #s
clust1 <- abs(clust1)
clust1$year <- 1900:2018
#extract raw values
clust2 <- as.data.frame(b_clust_2_wmf$values)
colnames(clust2) <- b_clust_2_wmf$timescales
#fix the imaginary #s
clust2 <- abs(clust2)
clust2$year <- 1900:2018
#pivot longer
clust1 <- clust1 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")
clust2 <- clust2 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")

# remove NAs
clust1 <- na.omit(clust1)
clust2 <- na.omit(clust2)

# specify timescale bands
clust1$ts <- as.numeric(clust1$ts)
clust1 <- clust1 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3 & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))


clust2$ts <- as.numeric(clust2$ts)
clust2 <- clust2 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3 & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average synchrony per year, band and cluster
clust_1_avg_sync <- clust1 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

clust_2_avg_sync <- clust2 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

b_clust_1_avg_sync <- clust_1_avg_sync %>%
  filter(band == "biennial") 
b_clust_1_avg_sync$cluster <- 1
b_clust_2_avg_sync <- clust_2_avg_sync %>%
  filter(band == "biennial")
b_clust_2_avg_sync$cluster <- 2
b_clust_avg_sync <- rbind(b_clust_1_avg_sync, b_clust_2_avg_sync)
b_clust_avg_sync$cluster <- as.factor(b_clust_avg_sync$cluster)
ggplot(data = b_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #geom_point(data = d_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #facet_wrap(~ band, labeller=labeller(band = c("biennial" = "Biennial", "multiannual" = "Multiannual", "decadal" = "Decadal", "multidecadal"= "Multidecadal")))+
  geom_smooth(method = "lm", se= FALSE)+
  theme_bw()+
  scale_color_manual(values = c("#377EB8", "#E41A1C"), labels = c("1", "2"))+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 14,angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Synchrony")+
  xlab("Year")+
  ggtitle("biennial")


# multiannual
# load appropriate data
ma_clust_1_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/ma_clust_1_wmf.rds")
ma_clust_2_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/ma_clust_2_wmf.rds")

#extract raw values
clust1 <- as.data.frame(ma_clust_1_wmf$values)
colnames(clust1) <- ma_clust_1_wmf$timescales
#fix the imaginary #s
clust1 <- abs(clust1)
clust1$year <- 1900:2018
#extract raw values
clust2 <- as.data.frame(ma_clust_2_wmf$values)
colnames(clust2) <- ma_clust_2_wmf$timescales
#fix the imaginary #s
clust2 <- abs(clust2)
clust2$year <- 1900:2018
#pivot longer
clust1 <- clust1 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")
clust2 <- clust2 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")

# remove NAs
clust1 <- na.omit(clust1)
clust2 <- na.omit(clust2)

# specify timescale bands
clust1$ts <- as.numeric(clust1$ts)
clust1 <- clust1 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3 & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))


clust2$ts <- as.numeric(clust2$ts)
clust2 <- clust2 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3 & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average synchrony per year, band and cluster
clust_1_avg_sync <- clust1 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

clust_2_avg_sync <- clust2 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

ma_clust_1_avg_sync <- clust_1_avg_sync %>%
  filter(band == "multiannual") 
ma_clust_1_avg_sync$cluster <- 1
ma_clust_2_avg_sync <- clust_2_avg_sync %>%
  filter(band == "multiannual")
ma_clust_2_avg_sync$cluster <- 2
ma_clust_avg_sync <- rbind(ma_clust_1_avg_sync, ma_clust_2_avg_sync)
ma_clust_avg_sync$cluster <- as.factor(ma_clust_avg_sync$cluster)
ggplot(data = ma_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #geom_point(data = d_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #facet_wrap(~ band, labeller=labeller(band = c("biennial" = "Biennial", "multiannual" = "Multiannual", "decadal" = "Decadal", "multidecadal"= "Multidecadal")))+
  geom_smooth(method = "lm", se= FALSE)+
  theme_bw()+
  scale_color_manual(values = c("#377EB8", "#E41A1C"), labels = c("1", "2"))+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 14,angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Synchrony")+
  xlab("Year")+
  ggtitle("multiannual")



# decadal
# load appropriate data
d_clust_1_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/d_clust_1_wmf.rds")
d_clust_2_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/Data/d_clust_2_wmf.rds")

#extract raw values
clust1 <- as.data.frame(d_clust_1_wmf$values)
colnames(clust1) <- d_clust_1_wmf$timescales
#fix the imaginary #s
clust1 <- abs(clust1)
clust1$year <- 1900:2018
#extract raw values
clust2 <- as.data.frame(d_clust_2_wmf$values)
colnames(clust2) <- d_clust_2_wmf$timescales
#fix the imaginary #s
clust2 <- abs(clust2)
clust2$year <- 1900:2018
#pivot longer
clust1 <- clust1 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")
clust2 <- clust2 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")

# remove NAs
clust1 <- na.omit(clust1)
clust2 <- na.omit(clust2)

# specify timescale bands
clust1$ts <- as.numeric(clust1$ts)
clust1 <- clust1 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                              ts > 3 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))


clust2$ts <- as.numeric(clust2$ts)
clust2 <- clust2 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3 & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average synchrony per year, band and cluster
clust_1_avg_sync <- clust1 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

clust_2_avg_sync <- clust2 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

d_clust_1_avg_sync <- clust_1_avg_sync %>%
  filter(band == "decadal") 
d_clust_1_avg_sync$cluster <- 1
d_clust_2_avg_sync <- clust_2_avg_sync %>%
  filter(band == "decadal")
d_clust_2_avg_sync$cluster <- 2
d_clust_avg_sync <- rbind(d_clust_1_avg_sync, d_clust_2_avg_sync)
d_clust_avg_sync$cluster <- as.factor(d_clust_avg_sync$cluster)
ggplot(data = d_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #geom_point(data = d_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #facet_wrap(~ band, labeller=labeller(band = c("biennial" = "Biennial", "multiannual" = "Multiannual", "decadal" = "Decadal", "multidecadal"= "Multidecadal")))+
  geom_smooth(method = "lm", se= FALSE)+
  theme_bw()+
  scale_color_manual(values = c("#377EB8", "#E41A1C"), labels = c("1", "2"))+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 14,angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Synchrony")+
  xlab("Year")+
  ggtitle("decadal")

# multidecadal
# load appropriate data
md_clust_1_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/md_clust_1_wmf.rds")
md_clust_2_wmf <- readRDS("~/Documents/GitHub/Tree-ring-synchrony/md_clust_2_wmf.rds")

#extract raw values
clust1 <- as.data.frame(md_clust_1_wmf$values)
colnames(clust1) <- md_clust_1_wmf$timescales
#fix the imaginary #s
clust1 <- abs(clust1)
clust1$year <- 1900:2018
#extract raw values
clust2 <- as.data.frame(md_clust_2_wmf$values)
colnames(clust2) <- md_clust_2_wmf$timescales
#fix the imaginary #s
clust2 <- abs(clust2)
clust2$year <- 1900:2018
#pivot longer
clust1 <- clust1 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")
clust2 <- clust2 %>%
  pivot_longer(1:67, names_to = "ts", values_to = "value")

# remove NAs
clust1 <- na.omit(clust1)
clust2 <- na.omit(clust2)

# specify timescale bands
clust1$ts <- as.numeric(clust1$ts)
clust1 <- clust1 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3 & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))


clust2$ts <- as.numeric(clust2$ts)
clust2 <- clust2 %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3 & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average synchrony per year, band and cluster
clust_1_avg_sync <- clust1 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

clust_2_avg_sync <- clust2 %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(value))

md_clust_1_avg_sync <- clust_1_avg_sync %>%
  filter(band == "multidecadal") 
md_clust_1_avg_sync$cluster <- 1
md_clust_2_avg_sync <- clust_2_avg_sync %>%
  filter(band == "multidecadal")
md_clust_2_avg_sync$cluster <- 2
md_clust_avg_sync <- rbind(md_clust_1_avg_sync, md_clust_2_avg_sync)
md_clust_avg_sync$cluster <- as.factor(md_clust_avg_sync$cluster)
ggplot(data = md_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #geom_point(data = md_clust_avg_sync, aes(x = year, y = avg_sync, group = cluster, color = cluster)) +
  #facet_wrap(~ band, labeller=labeller(band = c("biennial" = "Biennial", "multiannual" = "Multiannual", "decadal" = "Decadal", "multidecadal"= "Multidecadal")))+
  geom_smooth(method = "lm", se= FALSE)+
  theme_bw()+
  scale_color_manual(values = c("#377EB8", "#E41A1C"), labels = c("1", "2"))+
  #scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 14,angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Synchrony")+
  xlab("Year")+
  ggtitle("multidecadal")
