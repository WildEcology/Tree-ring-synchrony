# load necessary packages
library("prism")
library("sp")
library("dplyr")
library("tidyverse")
library("raster")

# set file path
prism_set_dl_dir(path = "/Users/kaitlynmcknight/Documents/PRISM", create = TRUE)

#### PPT ####
# 1981 - 2019 ppt prism pull
get_prism_monthlys(type = "vpdmax",
                   years = 1895:2019,
                   mon = 1:12,
                   keepZip = FALSE)
# list all downloaded prism data in the local archive
prism_get_dl_dir()
prism_archive_ls()
# store the data in a rasterstack 
RS <- pd_stack(prism_archive_ls())

# import site specific data to filter prism data for specific coordinates
sites <- read.csv("/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/plot_names_normalized_v2 copy.csv")
# filter dataset for plot name, lat and long
sites <- sites %>%
  dplyr::select(plot_id_needle, lat, long)
# remove NAs
sites <- na.omit(sites)

# make plot data spatially explicit
sites.spdf <-  SpatialPointsDataFrame(coords = sites[,c("long", "lat")],
                                      data = sites, proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))


# extract prism data for sites
.rs.unloadPackage("tidyr")
site.clim <- extract(RS, sites.spdf, na.rm = FALSE, sp = TRUE)

# transform spatialpointsdataframe object into simple dataframe to analyze
site_ppt <- as.data.frame(site.clim)
# wrangle the new dataframe to contain date, lat, long, site, and ppt
site_ppt <- site_ppt %>%
  tidyr::pivot_longer(PRISM_vpdmax_stable_4kmM3_1895_bil:PRISM_vpdmax_stable_4kmM3_201912_bil,
               names_to = "date",
               values_to = "vpdmax")
site_ppt <- site_ppt %>%
  tidyr::separate(date, into = c("owner", "clim.var", "data.type", "resolution", "date", "bil"), sep = "_", remove = TRUE)
site_ppt <- site_ppt %>%
  dplyr::select(plot_id_needle, lat, long, date, vpdmax)

site_ppt <- site_ppt %>%
  tidyr::separate(date, into = c("year","month"), sep = 4)

utils::write.csv(site_ppt, "vpdmax_prism_filtered.csv", row.names=FALSE, quote=FALSE) 
