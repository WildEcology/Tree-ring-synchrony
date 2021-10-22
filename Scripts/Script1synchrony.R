## ---------------------------
##
## Script name: 
##
## Author: 
##
## Date Created: 2021-10-21
##
## 
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(patchwork)
library(tidyverse)


theme_set(
  theme_bw(base_size = 12)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

summarize=dplyr::summarize
group_by=dplyr::group_by
select=dplyr::select


rwi_dat=read.csv("data/rwi_prismdat.csv")
head(rwi_dat)
