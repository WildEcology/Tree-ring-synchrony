source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))
plot_names_normalized <- read_csv("Data/plot_names_normalized.csv")

sm <- synmat(avg_plot_growth_mx, times, method = "pearson")
fields::image.plot(1:20,1:20,sm,col=heat.colors(20))


plot_info <- plot_names_normalized %>%
  rename("plot" = "plot_id_needle")
avg_plot_growth_wide_with_plot <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from= "avg_growth")
matching_plots <- left_join(avg_plot_growth_wide_with_plot, plot_info) %>%
  select(-plot_label, -plot_id_climate, -plot_id_genetics, -plot_id_tree.ring, -plot_label_2, -plot_number)
coords<-data.frame(X=matching_plots$lat, Y=matching_plots$long)

times <- 
clusters <- clust(dat=avg_plot_growth_mx, times, coords=coords, method="pearson")
get_clusters(clusters)
plotmap(clusters)
plot(get_times(clusters)[1:118], get_mns(clusters)[[2]][1,1:118], type = "l", col="red", xlab="timestep", ylab="mean growth")
lines(get_times(clusters)[1:118], get_mns(clusters)[[2]][2,1:118], type = "l", col="blue")
legend(x=1970, y =-0.65, legend=c("cluster 1", "cluster 2"), lty=c(1,1), col=c("red", "blue"))


cl12 <- addwmfs(clusters)
plotmag(get_wmfs(cl12)[[2]][[1]])
plotmag(get_wmfs(cl12)[[2]][[2]])

summary(clusters)

library("sf")
sf_df <- as.data.frame(cbind(clusters[["clusters"]][[2]], clusters[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `clusters[["clusters"]][[2]]`) %>%
  rename("weight" = `clusters[["modres"]][[2]][["nodeQ"]]`)

geog <- st_as_sf(sf_df, coords = c("Y","X"))

geog$cluster <- as.factor(geog$cluster)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("#FFCC33", "#003366"))



boundary <- st_read("/Users/kaitlynmcknight/Downloads/Sierra_Nevada_Conservancy_Boundary/Sierra_Nevada_Conservancy_Boundary.shp")
st_crs(boundary)
pj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
st_crs(geog) <- pj
crs <- st_crs(boundary)
geog <- st_transform(geog, crs)




# avg sync per cluster
cl12 <- addwmfs(clusters)
cluster1_wmf_values<- cl12[["wmfs"]][[2]][[1]][["values"]]
cluster2_wmf_values<- cl12[["wmfs"]][[2]][[2]][["values"]]
cl12 <- addwpmfs(clusters)
cluster1_wpmf_values<- cl12[["wpmfs"]][[2]][[1]][["values"]]
cluster2_wpmf_values<- cl12[["wpmfs"]][[2]][[2]][["values"]]


C1 <- as.data.frame(cluster1_wmf_values)
colnames(C1) <- cl12[["wmfs"]][[2]][[1]][["timescales"]]
#fix the imaginary #s
C1 <- abs(C1)
# define thresholds
sync <- thresholds[1]
sync <- as.numeric(sync)
async <- thresholds[2]
async <- as.numeric(async)


year <- 1901:2018
C1$year <- year
# classify sync, async and ns
C1<- C1 %>%
  pivot_longer(1:66, names_to = "ts", values_to = "values")
C1 <- na.omit(C1)
M2events <- M2 %>%
  mutate(event = case_when(values >= sync ~ "synchronous",
                           values <= async ~ "asynchronous",
                           TRUE ~ "NS"))

# classify timescale intervals
C1$ts <- as.numeric(C1$ts)
C1 <- C1 %>%
  mutate(interval = case_when(ts >= 2 & ts <= 5 ~ "biennial",
                              ts > 5 & ts <= 10 ~ "multiannual",
                              ts > 10 & ts <= 20 ~ "decadal",
                              ts > 20 & ts <= 30 ~ "multidecadal"))

C1 <- na.omit(C1)
avg_sync_C1 <- C1 %>%
  group_by(year, interval) %>%
  summarise(avg_sync = mean(values))

ggplot(data = avg_sync_C1, aes(x = year, y = avg_sync, group = interval, color = interval)) +
  #geom_point(data = avg_sync_C1, aes(x = year, y = avg_sync, group = interval, color = interval)) +
  geom_smooth(method=loess,   # Add linear regression lines
             se=FALSE,    # Don't add shaded confidence region
             fullrange=TRUE)+ # Extend regression line
  theme_bw()+
  scale_x_discrete(breaks = seq(1901,2018,10))+
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
  ylab("Average Synchrony")+
  xlab("Year")

C2 <- as.data.frame(cluster2_wmf_values) 
colnames(C2) <- cl12[["wmfs"]][[2]][[1]][["timescales"]]
C2 <- abs(C2)

  
year <- 1901:2018
C2$year <- year# classify sync, async and ns
C2<- C2 %>%
    pivot_longer(1:66, names_to = "ts", values_to = "values")
C2 <- na.omit(C2)
M2events <- M2 %>%
    mutate(event = case_when(values >= sync ~ "synchronous",
                             values <= async ~ "asynchronous",
                             TRUE ~ "NS"))
  
# classify timescale intervals
C2$ts <- as.numeric(C2$ts)
C2 <- C2 %>%
    mutate(interval = case_when(ts >= 2 & ts <= 5 ~ "annual",
                                ts > 5 & ts <= 10 ~ "interannual",
                                ts > 10 & ts <= 20 ~ "decadal",
                                ts > 20 & ts <= 30 ~ "multidecadal"))
C2 <- na.omit(C2)  
  
avg_sync_C2 <- C2 %>%
    group_by(year, interval) %>%
    summarise(avg_sync = mean(values))
  
ggplot(data = avg_sync_C2, aes(x = year, y = avg_sync, group = interval, color = interval)) +
    #geom_point(data = avg_sync_C2, aes(x = year, y = avg_sync, group = interval, color = interval)) +
    geom_smooth(method=loess,   # Add linear regression lines
                se=FALSE,    # Don't add shaded confidence region
                fullrange=TRUE)+ # Extend regression line
  theme_bw()+
    scale_x_discrete(breaks = seq(1901,2018,10))+
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
    ylab("Average Synchrony")+
    xlab("Year")
  
  