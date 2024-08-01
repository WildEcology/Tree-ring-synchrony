# load necessary data
source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))
plot_names_normalized <- read_csv("Data/plot_names_normalized.csv")

# extract plot coordinates
plot_info <- plot_names_normalized %>%
  rename("plot" = "plot_id_needle")
avg_plot_growth_wide_with_plot <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from= "avg_growth")
matching_plots <- left_join(avg_plot_growth_wide_with_plot, plot_info) %>%
  select(-plot_label, -plot_id_climate, -plot_id_genetics, -plot_id_tree.ring, -plot_label_2, -plot_number)
coords<-data.frame(X=matching_plots$lat, Y=matching_plots$long)

# cluster analysis for each timescale band
times <- 1900:2018
b_clust <- clust(dat=avg_plot_growth_mx, times, coords=coords, method="ReXWT", tsrange = c(2,3))
ma_clust <- clust(dat=avg_plot_growth_mx, times, coords=coords, method="ReXWT", tsrange = c(3,10))
d_clust <- clust(dat=avg_plot_growth_mx, times, coords=coords, method="ReXWT", tsrange = c(10,20))
md_clust <- clust(dat=avg_plot_growth_mx, times, coords=coords, method="ReXWT", tsrange = c(20,30))

# how many clusters per band
get_clusters(b_clust) # 3 clusters
get_clusters(ma_clust) # 4 clusters
get_clusters(d_clust) # 2 clusters
get_clusters(md_clust) # 2 clusters

# coordinate map with cluster membership
plotmap(b_clust)
plotmap(ma_clust)
plotmap(d_clust)
plotmap(md_clust)

# plot each timeseries based on cluster membership per timescale band
# biennial = 3 clusters
plot(get_times(b_clust)[1:118], get_mns(b_clust)[[3]][1,1:118], type = "l", col="red", 
     ylim=range(get_mns(b_clust)), xlab="timestep", ylab="mean growth")
lines(get_times(b_clust)[1:118], get_mns(b_clust)[[3]][2,1:118], type = "l", col="blue")
lines(get_times(b_clust)[1:118], get_mns(b_clust)[[3]][3,1:118], type = "l", col="green")
# multiannual = 4 clusters
plot(get_times(ma_clust)[1:118], get_mns(ma_clust)[[4]][1,1:118], type = "l", col="red", 
     ylim=range(get_mns(ma_clust)), xlab="timestep", ylab="mean growth")
lines(get_times(ma_clust)[1:118], get_mns(ma_clust)[[4]][2,1:118], type = "l", col="blue")
lines(get_times(ma_clust)[1:118], get_mns(ma_clust)[[4]][3,1:118], type = "l", col="green")
lines(get_times(ma_clust)[1:118], get_mns(ma_clust)[[4]][4,1:118], type = "l", col="purple")
# decadal = 2 clusters
plot(get_times(d_clust)[1:118], get_mns(d_clust)[[2]][1,1:118], type = "l", col="red", 
     ylim=range(get_mns(d_clust)), xlab="timestep", ylab="mean growth")
lines(get_times(d_clust)[1:118], get_mns(d_clust)[[2]][2,1:118], type = "l", col="blue")
# multidecadal = 2 clusters
plot(get_times(md_clust)[1:118], get_mns(md_clust)[[2]][1,1:118], type = "l", col="red", 
     ylim=range(get_mns(md_clust)), xlab="timestep", ylab="mean growth")
lines(get_times(md_clust)[1:118], get_mns(md_clust)[[2]][2,1:118], type = "l", col="blue")

# get wmf per cluster per band
# biennial
b_wmf <- addwmfs(b_clust)
plotmag(get_wmfs(b_wmf)[[3]][[1]])
plotmag(get_wmfs(b_wmf)[[3]][[2]])
plotmag(get_wmfs(b_wmf)[[3]][[3]])
# multiannual
ma_wmf <- addwmfs(ma_clust)
plotmag(get_wmfs(ma_wmf)[[4]][[1]])
plotmag(get_wmfs(ma_wmf)[[4]][[2]])
plotmag(get_wmfs(ma_wmf)[[4]][[3]]) # wmf does not exist for this cluster, only 1 plot?
plotmag(get_wmfs(ma_wmf)[[4]][[4]])
# decadal
d_wmf <- addwmfs(d_clust)
plotmag(get_wmfs(d_wmf)[[2]][[1]])
plotmag(get_wmfs(d_wmf)[[2]][[2]])
# multdecadal
md_wmf <- addwmfs(md_clust)
plotmag(get_wmfs(md_wmf)[[2]][[1]])
plotmag(get_wmfs(md_wmf)[[2]][[2]])

# plot clusters on the site map per band
library("sf")
# biennial
sf_df_b <- as.data.frame(cbind(b_clust[["clusters"]][[3]], b_clust[["modres"]][[3]][["nodeQ"]], coords)) %>%
  rename("cluster" = `b_clust[["clusters"]][[3]]`) %>%
  rename("weight" = `b_clust[["modres"]][[3]][["nodeQ"]]`)

geog_b <- st_as_sf(sf_df_b, coords = c("Y","X"))

geog_b$cluster <- as.factor(geog_b$cluster)

boundary <- st_read("/Users/kaitlynmcknight/Downloads/Sierra_Nevada_Conservancy_Boundary/Sierra_Nevada_Conservancy_Boundary.shp")
st_crs(boundary)
pj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
st_crs(geog_b) <- pj
crs <- st_crs(boundary)
geog_b <- st_transform(geog_b, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_b, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue", "green"))

# multiannual
sf_df_ma <- as.data.frame(cbind(ma_clust[["clusters"]][[4]], ma_clust[["modres"]][[4]][["nodeQ"]], coords)) %>%
  rename("cluster" = `ma_clust[["clusters"]][[4]]`) %>%
  rename("weight" = `ma_clust[["modres"]][[4]][["nodeQ"]]`)

geog_ma <- st_as_sf(sf_df_ma, coords = c("Y","X"))

geog_ma$cluster <- as.factor(geog_ma$cluster)

st_crs(geog_ma) <- pj
geog_ma <- st_transform(geog_ma, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_ma, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue", "green", "purple"))

# decadal
sf_df_d <- as.data.frame(cbind(d_clust[["clusters"]][[2]], d_clust[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `d_clust[["clusters"]][[2]]`) %>%
  rename("weight" = `d_clust[["modres"]][[2]][["nodeQ"]]`)

geog_d <- st_as_sf(sf_df_d, coords = c("Y","X"))

geog_d$cluster <- as.factor(geog_d$cluster)

st_crs(geog_d) <- pj
geog_d <- st_transform(geog_d, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_d, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

# multidecdal
sf_df_md <- as.data.frame(cbind(md_clust[["clusters"]][[2]], md_clust[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `md_clust[["clusters"]][[2]]`) %>%
  rename("weight" = `md_clust[["modres"]][[2]][["nodeQ"]]`)

geog_md <- st_as_sf(sf_df_md, coords = c("Y","X"))

geog_md$cluster <- as.factor(geog_md$cluster)

st_crs(geog_md) <- pj
geog_md <- st_transform(geog_md, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_md, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

# create cluster dataframes for each timescale band with plot classifications
plot_dem <- read.csv("Data/plot_demdat.csv")
plot_dem <- plot_dem %>% rename(plot = "plot_id_needle") %>% rename(X = "lat") %>% rename(Y = "long") %>% select(-plot_number)
plot_coords <- plot_info %>% select(plot, lat, long)
plot_coords <- plot_coords %>% rename(X = "lat") %>% rename(Y = "long")
plot_classification <- read_csv("Data/plot_classification.csv")
plot_classification <- plot_classification %>% mutate(plot2 = case_when(plot == "RC_NA" ~ "RC", plot == "K_NA" ~ "K", TRUE ~ as.character(plot)))
plot_classification <- plot_classification %>% select(pck, tmn, plot_type, plot2) %>% rename(plot = "plot2")

# biennial
b_clust_coords <- as.data.frame(b_clust[["coords"]])
b_clust_coords$cluster <- b_clust[["clusters"]][[2]]
b_clust_coords <- left_join(b_clust_coords, plot_coords)%>%
  left_join(plot_classification) %>%
  left_join(plot_dem)


# multiannual
ma_clust_coords <- as.data.frame(ma_clust[["coords"]])
ma_clust_coords$cluster <- ma_clust[["clusters"]][[2]]
ma_clust_coords <- left_join(ma_clust_coords, plot_coords)%>%
  left_join(plot_classification)%>%
  left_join(plot_dem)

# decadal
d_clust_coords <- as.data.frame(d_clust[["coords"]])
d_clust_coords$cluster <- d_clust[["clusters"]][[2]]
d_clust_coords <- left_join(d_clust_coords, plot_coords)%>%
  left_join(plot_classification)%>%
  left_join(plot_dem)


# multidecadal
md_clust_coords <- as.data.frame(md_clust[["coords"]])
md_clust_coords$cluster <- md_clust[["clusters"]][[2]]
md_clust_coords <- left_join(md_clust_coords, plot_coords)%>%
  left_join(plot_classification)%>%
  left_join(plot_dem)


ggplot(data = md_clust_coords, aes(x=tmn, y = cluster, col=elevation_m))+
  geom_point()

# plot clusters on the map using just two (minimum number of clusters produced across bands)
# biennial
sf_df_b_2 <- as.data.frame(cbind(b_clust[["clusters"]][[2]], b_clust[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `b_clust[["clusters"]][[2]]`) %>%
  rename("weight" = `b_clust[["modres"]][[2]][["nodeQ"]]`)

geog_b_2 <- st_as_sf(sf_df_b_2, coords = c("Y","X"))

geog_b_2$cluster <- as.factor(geog_b_2$cluster)
st_crs(geog_b_2) <- pj
geog_b_2 <- st_transform(geog_b_2, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_b_2, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

# multiannual
sf_df_ma_2 <- as.data.frame(cbind(ma_clust[["clusters"]][[2]], ma_clust[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `ma_clust[["clusters"]][[2]]`) %>%
  rename("weight" = `ma_clust[["modres"]][[2]][["nodeQ"]]`)

geog_ma_2 <- st_as_sf(sf_df_ma_2, coords = c("Y","X"))

geog_ma_2$cluster <- as.factor(geog_ma_2$cluster)

st_crs(geog_ma_2) <- pj
geog_ma_2 <- st_transform(geog_ma_2, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_ma_2, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

# decadal and multidecadal already only have two clusters 



#### REPEAT WITH ENV DATA ####
# PPT
times <- 1900:2018
b_clust_ppt <- clust(dat=winter_ppt_mx, times, coords=coords, method="ReXWT", tsrange = c(2,3))
ma_clust_ppt <- clust(dat=winter_ppt_mx, times, coords=coords, method="ReXWT", tsrange = c(3,10))
d_clust_ppt <- clust(dat=winter_ppt_mx, times, coords=coords, method="ReXWT", tsrange = c(10,20))
md_clust_ppt <- clust(dat=winter_ppt_mx, times, coords=coords, method="ReXWT", tsrange = c(20,30))

# how many clusters per band
get_clusters(b_clust_ppt) # 1 cluster
get_clusters(ma_clust_ppt) # 1 cluster
get_clusters(d_clust_ppt) # 1 cluster
get_clusters(md_clust_ppt) # 2 clusters

# only multidecadal has two clusters
sf_df_md_ppt <- as.data.frame(cbind(md_clust_ppt[["clusters"]][[2]], md_clust_ppt[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `md_clust_ppt[["clusters"]][[2]]`) %>%
  rename("weight" = `md_clust_ppt[["modres"]][[2]][["nodeQ"]]`)

geog_md_ppt <- st_as_sf(sf_df_md_ppt, coords = c("Y","X"))

geog_md_ppt$cluster <- as.factor(geog_md_ppt$cluster)

st_crs(geog_md_ppt) <- pj
geog_md_ppt <- st_transform(geog_md_ppt, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_md_ppt, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

# TMIN
times <- 1900:2018
b_clust_tmin <- clust(dat=summer_tmin_mx, times, coords=coords, method="ReXWT", tsrange = c(2,3))
ma_clust_tmin <- clust(dat=summer_tmin_mx, times, coords=coords, method="ReXWT", tsrange = c(3,10))
d_clust_tmin <- clust(dat=summer_tmin_mx, times, coords=coords, method="ReXWT", tsrange = c(10,20))
md_clust_tmin <- clust(dat=summer_tmin_mx, times, coords=coords, method="ReXWT", tsrange = c(20,30))

# how many clusters per band
get_clusters(b_clust_tmin) # 2 cluster
get_clusters(ma_clust_tmin) # 2 cluster
get_clusters(d_clust_tmin) # 2 cluster
get_clusters(md_clust_tmin) # 2 clusters


# biennial
sf_df_b_tmin <- as.data.frame(cbind(b_clust_tmin[["clusters"]][[2]], b_clust_tmin[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `b_clust_tmin[["clusters"]][[2]]`) %>%
  rename("weight" = `b_clust_tmin[["modres"]][[2]][["nodeQ"]]`)

geog_b_tmin <- st_as_sf(sf_df_b_tmin, coords = c("Y","X"))

geog_b_tmin$cluster <- as.factor(geog_b_tmin$cluster)

st_crs(geog_b_tmin) <- pj
geog_b_tmin <- st_transform(geog_b_tmin, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_b_tmin, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

# multiannual
sf_df_ma_tmin <- as.data.frame(cbind(ma_clust_tmin[["clusters"]][[2]], ma_clust_tmin[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `ma_clust_tmin[["clusters"]][[2]]`) %>%
  rename("weight" = `ma_clust_tmin[["modres"]][[2]][["nodeQ"]]`)

geog_ma_tmin <- st_as_sf(sf_df_ma_tmin, coords = c("Y","X"))

geog_ma_tmin$cluster <- as.factor(geog_ma_tmin$cluster)

st_crs(geog_ma_tmin) <- pj
geog_ma_tmin <- st_transform(geog_ma_tmin, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_ma_tmin, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

# decadal
sf_df_d_tmin <- as.data.frame(cbind(d_clust_tmin[["clusters"]][[2]], d_clust_tmin[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `d_clust_tmin[["clusters"]][[2]]`) %>%
  rename("weight" = `d_clust_tmin[["modres"]][[2]][["nodeQ"]]`)

geog_d_tmin <- st_as_sf(sf_df_d_tmin, coords = c("Y","X"))

geog_d_tmin$cluster <- as.factor(geog_d_tmin$cluster)

st_crs(geog_d_tmin) <- pj
geog_d_tmin <- st_transform(geog_d_tmin, crs)

ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_d_tmin, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))


# multidecadal
sf_df_md_tmin <- as.data.frame(cbind(md_clust_tmin[["clusters"]][[2]], md_clust_tmin[["modres"]][[2]][["nodeQ"]], coords)) %>%
  rename("cluster" = `md_clust_tmin[["clusters"]][[2]]`) %>%
  rename("weight" = `md_clust_tmin[["modres"]][[2]][["nodeQ"]]`)

geog_md_tmin <- st_as_sf(sf_df_md_tmin, coords = c("Y","X"))

geog_md_tmin$cluster <- as.factor(geog_md_tmin$cluster)

st_crs(geog_md_tmin) <- pj
geog_md_tmin <- st_transform(geog_md_tmin, crs)


ggplot()+
  geom_sf(data = boundary, fill=NA)+
  geom_sf(data = geog_md_tmin, aes(col=cluster, size = weight))+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))

#### further investigation of environmental clustering ####

ggplot(data = md_clust_coords, aes(x=tmn, y = cluster, col=elevation_m))+geom_point()

avg_tmin <- summer_tmin %>% group_by(plot) %>% summarise(avg_tmin_per_plot = mean(summer_tmin))
md_clust_coords <- left_join(md_clust_coords, avg_tmin)
md_clust_coords$quantiles <- as.factor(md_clust_coords$quantiles)
md_clust_coords$cluster <- as.factor(md_clust_coords$cluster)
ggplot(data = md_clust_coords, aes(x=cluster, y = avg_tmin_per_plot, col=elevation_m))+
  geom_point()

quantile(avg_tmin$avg_tmin_per_plot, probs = c(0.33, 0.66, 1.0))
avg_tmin <- avg_tmin %>% mutate(quantiles = case_when(avg_tmin_per_plot <= 3.201447 ~ 1,
                                                      avg_tmin_per_plot >= 3.201447 & avg_tmin_per_plot < 4.299613 ~ 2,
                                                      avg_tmin_per_plot >= 4.299613 & avg_tmin_per_plot < 7.559042 ~ 3,
                                                      plot == "RC" ~ 3))






ma_clust_coords <- ma_clust_coords %>%
  mutate(region = case_when(X > 38.5 ~ "N",
                            X < 38.5 & X > 37.5 ~ "M",
                            X < 37.5 ~ "S"))
ma_clust_coords <- left_join(ma_clust_coords, avg_tmin)
ma_clust_coords$cluster <- factor(ma_clust_coords$cluster)
ma_clust_coords$quantiles <- factor(ma_clust_coords$quantiles)
ma_clust_coords$region <- factor(ma_clust_coords$region, levels = c("N", "M", "S"))

ggplot(data = ma_clust_coords, aes(x=elevation_m, y = cluster))+
  geom_point()+
  facet_wrap(~region)+
  ggtitle("multiannual")

ggplot(data = ma_clust_coords, aes(x=avg_tmin_per_plot, y = cluster))+
  geom_point()+
  facet_wrap(~region)+
  ggtitle("multiannual")


d_clust_coords <- d_clust_coords %>%
  mutate(region = case_when(X > 38.5 ~ "N",
                            X < 38.5 & X > 37.5 ~ "M",
                            X < 37.5 ~ "S"))
d_clust_coords <- left_join(d_clust_coords, avg_tmin)

d_clust_coords$cluster <- factor(d_clust_coords$cluster)
d_clust_coords$region <- factor(d_clust_coords$region, levels = c("N", "M", "S"))

ggplot(data = d_clust_coords, aes(x=avg_tmin_per_plot, y = cluster))+
  geom_point()+
  facet_wrap(~region)+
  ggtitle("decadal")

ggplot(data = d_clust_coords, aes(x=avg_tmin_per_plot, y = cluster))+
  geom_point()+
  facet_wrap(~region)+
  ggtitle("decadal")


b_clust_coords <- b_clust_coords %>%
  mutate(region = case_when(X > 38.5 ~ "N",
                            X < 38.5 & X > 37.5 ~ "M",
                            X < 37.5 ~ "S"))
b_clust_coords <- left_join(b_clust_coords, avg_tmin)
b_clust_coords$cluster <- factor(b_clust_coords$cluster)
b_clust_coords$region <- factor(b_clust_coords$region, levels = c("N", "M", "S"))
ggplot(data = b_clust_coords, aes(x=avg_tmin_per_plot, y = cluster))+
  geom_point()+
  facet_wrap(~region)+
  ggtitle("biennial")

md_clust_coords <- md_clust_coords %>%
  mutate(region = case_when(X > 38.5 ~ "N",
                            X < 38.5 & X > 37.5 ~ "M",
                            X < 37.5 ~ "S"))
md_clust_coords <- left_join(md_clust_coords, avg_tmin)
md_clust_coords$cluster <- factor(md_clust_coords$cluster)
md_clust_coords$region <- factor(md_clust_coords$region, levels = c("N", "M", "S"))
ggplot(data = md_clust_coords, aes(x=avg_tmin_per_plot, y = cluster))+
  geom_point()+
  facet_wrap(~region)+
  ggtitle("multidecadal")


ma_cluster_1_N <- ma_clust_coords %>%
  filter(cluster == 1, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 2 plots
ma_cluster_1_M <- ma_clust_coords %>%
  filter(cluster == 1, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 3 plots
ma_cluster_1_S <- ma_clust_coords %>%
  filter(cluster == 1, region == "S") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m)# 7 plots

ma_cluster_2_N <- ma_clust_coords %>%
  filter(cluster == 2, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 2 plots
ma_cluster_2_M <- ma_clust_coords %>%
  filter(cluster == 2, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 4 plots
ma_cluster_2_S <- ma_clust_coords %>%
  filter(cluster == 2, region == "S")%>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 2 plots

t.test(ma_cluster_1_N, ma_cluster_2_N, var.equal = TRUE) # sig, p-value = 0.00639
t.test(ma_cluster_1_M, ma_cluster_2_M, var.equal = TRUE) # non-sig, p-value = 0.05707
t.test(ma_cluster_1_S, ma_cluster_2_S, var.equal = TRUE) # non-sig, p-value = 0.6438


d_cluster_1_N <- d_clust_coords %>%
  filter(cluster == 1, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 2 plots
d_cluster_1_M <- d_clust_coords %>%
  filter(cluster == 1, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 3 plots
d_cluster_1_S <- d_clust_coords %>%
  filter(cluster == 1, region == "S") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m)# 7 plots

d_cluster_2_N <- d_clust_coords %>%
  filter(cluster == 2, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 2 plots
d_cluster_2_M <- d_clust_coords %>%
  filter(cluster == 2, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 4 plots
d_cluster_2_S <- d_clust_coords %>%
  filter(cluster == 2, region == "S")%>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 2 plots

t.test(d_cluster_1_N, d_cluster_2_N, var.equal = TRUE) # sig, p-value = 0.005718
t.test(d_cluster_1_M, d_cluster_2_M, var.equal = TRUE) # non-sig, p-value = 0.3188
t.test(d_cluster_1_S, d_cluster_2_S, var.equal = TRUE) # non-sig, p-value = 0.1919

b_cluster_1_N <- b_clust_coords %>%
  filter(cluster == 1, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 3 plots
b_cluster_1_M <- b_clust_coords %>%
  filter(cluster == 1, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 6 plots
b_cluster_1_S <- b_clust_coords %>%
  filter(cluster == 1, region == "S") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m)# 9 plots

b_cluster_2_N <- b_clust_coords %>%
  filter(cluster == 2, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 1 plot
b_cluster_2_M <- b_clust_coords %>%
  filter(cluster == 2, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 1 plot
b_cluster_2_S <- b_clust_coords %>%
  filter(cluster == 2, region == "S")%>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 0 plots

t.test(b_cluster_1_N, b_cluster_2_N, var.equal = TRUE) # non-sig, p-value = 0.3698
t.test(b_cluster_1_M, b_cluster_2_M, var.equal = TRUE) # non-sig, p-value = 0.747
t.test(b_cluster_1_S, b_cluster_2_S, var.equal = TRUE) # no cluster 2 plots in S region for biennial band

md_cluster_1_N <- md_clust_coords %>%
  filter(cluster == 1, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 1 plot
md_cluster_1_M <- md_clust_coords %>%
  filter(cluster == 1, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 5 plots
md_cluster_1_S <- md_clust_coords %>%
  filter(cluster == 1, region == "S") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m)# 4 plots

md_cluster_2_N <- md_clust_coords %>%
  filter(cluster == 2, region == "N") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 3 plot
md_cluster_2_M <- md_clust_coords %>%
  filter(cluster == 2, region == "M") %>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 2 plot
md_cluster_2_S <- md_clust_coords %>%
  filter(cluster == 2, region == "S")%>% 
  select(plot, elevation_m) %>%
  pivot_wider(names_from = plot, values_from = elevation_m) # 5 plots

t.test(md_cluster_1_N, md_cluster_2_N, var.equal = TRUE) # non-sig, 0.3698
t.test(md_cluster_1_M, md_cluster_2_M, var.equal = TRUE) # non-sig, p-value = 0.9358
t.test(md_cluster_1_S, md_cluster_2_S, var.equal = TRUE) # non-sig, p-value = 0.117
