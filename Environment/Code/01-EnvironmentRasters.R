# Libraries
library(sf)
library(sp)
library(tidyverse)
library(magrittr)
library(raster)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. LOAD ENVIRONMENTAL DATA & STUDY GRID
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./Environment/Environment.RData')
load('./eDriversGrids/Data/HexaGrid-1000m2.RData')
env <- dplyr::left_join(egslGrid, env, by = 'ID')

# ----------
# Bathymetry
# ----------
env$Bathy_Mean <- round(abs(env$Bathy_Mean))

# ------------------------
# Sea surface temperatures
# ------------------------
uid <- paste(paste('sst', c('2013','2014','2015','2016','2017'), sep = '-'), c(rep('08',5), rep('09',5)), sep = '-')
# uid <- stringr::str_detect(colnames(env), 'sst')
env$sst <- rowMeans(env[, uid, drop = T], na.rm = T)

# -----------------------
# Sea bottom temperatures
# -----------------------
# This layer is annual, so I only need the mean value
uid <- paste('sbt', c('2013','2014','2015'), sep = '-')
env$sbt <- rowMeans(env[, uid, drop = T], na.rm = T)

# Environmental variables
envCov <- c('Bathy_Mean',
            'SSAL_MEAN','SalMoyMoy',
            'sst','sbt',
            'ARAG','Present.Surface.pH.tif',
            'sat','Present.Surface.Dissolved.oxygen.Mean',
            'Present.Benthic.Mean.Depth.Primary.productivity.Mean','Present.Surface.Primary.productivity.Mean',
            'Megahabita','HAB_C_E',
            'Y','X')

# Subset environmental variables
env <- env[, envCov]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. TRANSFORM TO RASTER STACK
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Study area extent
# WARNING: Order is not the same for raster vs sf extent functions
ext <- st_bbox(env) %>%
       as.numeric() %>%
       .[c(1,3,2,4)] %>%
       extent()

# Empty raster
r <- raster(crs = st_crs(env)$proj4string, resolution = 1000, ext = ext)

# Transform as sp object
env <- as(env, "Spatial")

# Rasterize
envR <- vector('list', length(envCov))
for(i in 1:length(envR)) {
  cat(i, ' of ', length(envR), '\r')
  envR[[i]] <- rasterize(env, r, fun = 'mean', field = envCov[i])
}

env <- stack(envR)
names(env) <- envCov


save(env, file = './Environment/Data/EnvironmentRasters.RData')
