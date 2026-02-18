##########################################
## whitebark pine study, Sierra Nevada, CA
## 800 cores collected across 27 plots 
## 774 unique series after removing duplicates and low
## series detrending and chronology developement
##set basepath
basepath<-"D:/Projects/WhitebarkPine/Analysis"
setwd(basepath)

## load needed packages
## instead of manually loading packages code to search for existing install and auto load missing packages
list.of.packages <- c("tidyverse","dplR","vegan","gridExtra","Hmisc","treeclim")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## load packages
library(tidyverse)
library(dplR)
library(vegan)
library(gridExtra) 
library(Hmisc)
library(treeclim)

###########################################
## load whitebark pine raw crosdated ring width series data as csv
## load whitebark pine tree lookup table as csv
wbp <- read.csv("D:/Projects/WhitebarkPine/Data/Dendro/RAW/wbp_raw_xdated_v4.csv")
wbp.tree.lookup <- read.csv("D:/Projects/WhitebarkPine/Data/wbp_tree_lookup.csv")


###############################
## for dup cores keep series with highest previously determined plot-level interseries correlation
## was based on old COREID_ALT so match to new COREID_2
dup.cores <-c("FS12120A","FS1510A","FS1530A","FSD2B04B","FSD2B16B","FSD4B07B",
              "FSD4B16A","FSD5C18A","SE04B12B","SE04B28A","SE18A21A","SE24A11A",
              "SE24A27A","SE4309A","SE4316A","SE4319B","SE4329A","SE67A28B","SE67C12B",
              "YO03A06B","YO03A11B","YO03A13A","YO03A16A","YO03A24B","YO03A30B","YO25A29B")
mm <- match(dup.cores, wbp.tree.lookup$COREID_ALT)
dup.cores[!is.na(mm)]<- as.character(wbp.tree.lookup$COREID_2[na.omit(mm)])

wbp <- wbp[,!names(wbp) %in% dup.cores]
wbp <- wbp[,2:774]
str(wbp)

## function to convert column named YEAR to the rownames and delete column named YEAR
YearRowName <- function(indataframe){
  rownames(indataframe) <- indataframe[,"Year"]
  indataframe <- indataframe[,2:ncol(indataframe)]
  return(indataframe)
}
wbp <- YearRowName(wbp)

##convert to rwl class
wbp <- as.rwl(wbp)

###############################
## detrended series
## double detrending
## first detrending neg expo or linear without positive slopes
## second detrending CSS where frequency response 0.50 at wavelength of 0.67 series length
wbp.rwi <- detrend(detrend(wbp, method = c("ModNegExp"), pos.slope = FALSE), method = "Spline")

##write.csv(wbp.rwi,"D:/Projects/WhitebarkPine/Data/Dendro/DetrendedChrons/wbp_rwi_v4.csv")

## subset detrended series by site
## subset by vector of sorted site_id and grep partial matching
## results in list of rwi by site
site.id <- sort(unique(substr(names(wbp.rwi), start = 1, stop = 7)), decreasing = FALSE)

wbp.rwi.lst <- list()
for (i in 1:length(site.id)){
  wbp.rwi.lst[[i]] <- wbp.rwi[,grep(paste("^",site.id[i], sep=""),names(wbp.rwi))]
}
names(wbp.rwi.lst) <- site.id

########################
## standard and residual chronology development 
## biwieght robust mean
## autoregressive modeling prewhitening applied

wbp.crn.lst <- list()
for (i in 1:length(wbp.rwi.lst)){
  wbp.crn.lst[[i]] <- chron(wbp.rwi.lst[[i]],
                            prefix = paste("S",formatC(i, width = 2, format = "d", flag = "0"), sep = ""),
                            biweight = TRUE, prewhiten = TRUE)
}
names(wbp.crn.lst) <- site.id

##unlist and correctly name chronologies
wbp.crn.sites <- do.call("cbind", wbp.crn.lst)
crn.sites <- substr(names(wbp.crn.sites),1,7)
crn.vars <- rep(c("std","res","depth"), 27)
names(wbp.crn.sites) <- paste(crn.sites, crn.vars, sep = "_") 
str(wbp.crn.sites)

##write as csv
write.csv(wbp.crn.sites, "D:/Projects/WhitebarkPine/Data/Dendro/DetrendedChrons/wbp_crns_v4.csv")
