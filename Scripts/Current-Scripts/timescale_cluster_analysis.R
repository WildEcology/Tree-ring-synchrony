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
get_clusters(b_clust)
get_clusters(ma_clust)
get_clusters(d_clust)
get_clusters(md_clust)

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



