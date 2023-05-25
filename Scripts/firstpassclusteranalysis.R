source(here::here("scripts/cleaning_code.R"))

sm <- synmat(avg_plot_growth_mx, times, method = "pearson")
fields::image.plot(1:20,1:20,sm,col=heat.colors(20))


plot_info <- plot_names_normalized_v2_copy %>%
  rename("plot" = "plot_id_needle")
matching_plots <- left_join(avg_plot_growth_wide, plot_info) %>%
  select(-plot_label, -plot_id_climate, -plot_id_genetics, -plot_id_tree.ring, -plot_label_2, -plot_number)
coords<-data.frame(X=matching_plots$lat, Y=matching_plots$long)

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


