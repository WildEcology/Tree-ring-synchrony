##------------------------


# SCRIPT NAME: Wavelet

# AUTHOR: Kaitlyn McKnight (University of Wyoming)

# DATE CREATED: 2021-10-22


##------------------------

# load packages

library("tidyverse")
library("ggplot2")
library("tsvr")
library("codyn")
library("wsyn")
library("lubridate")
library("here")


# read in data

prismdat <- read.csv(here("data/rwi_prismdat.csv"))
anprismplot <- read.csv(here("data/whitebark_plot_prism_annual.csv"))
moprismplot <- read.csv(here("data/whitebark_plot_prism_monthly.csv"))


