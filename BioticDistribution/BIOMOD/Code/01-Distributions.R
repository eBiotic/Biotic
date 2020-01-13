# Libraries
library(sf)
library(sp)
library(tidyverse)
library(magrittr)
library(raster)
library(biomod2)

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
uid <- stringr::str_detect(colnames(env), 'sst')
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
# for(i in 1:length(envR)) {
for(i in 1:11) {
  envR[[i]] <- rasterize(env, r, fun = 'mean', field = envCov[i])
}

r <- stack(envR)
names(r) <- envCov


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. LOAD BIOTIC DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluri.RData')
load('BioticData/ObsMer_1999-2015_MPO/Data/Biotic/SeaObserver.RData')
load('BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleFixed.RData')
load('BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleMobile.RData')
load('BioticData/ZIF-Fisheries-2010-2015/Data/Biotic/zif.RData')

# Select single column
pluri <- pluri[,'species']
obs <- obs[,'species']
sentFix <- sentFix[,'species']
sentMob <- sentMob[,'species']
zif <- zif[,'species']

# Single object
biotic <- rbind(pluri, obs, sentFix, sentMob, zif)

# Transform as sp object
biotic <- as(biotic, 'Spatial')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. LOAD SPECIES LIST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('BioticData/SpeciesList/Data/SpeciesList.RData')

# Minimum record number accepted (subjective)
minRec <- 50

# Filter
sp <- sp %>%
      filter(Count >= minRec)

# Biotic subset
uid <- biotic$species %in% sp$species
biotic <- biotic[uid, ]

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 3. INTERSECT BIOTIC W/ ENVIRONMENTAL DATA
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Polygon values to points
# biotic <- st_join(biotic, env, join = st_intersects)
#
# # Remove NAs
# uid <- apply(biotic, 1, function(x) any(is.na(x)))
# biotic <- biotic[!uid, ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. TRANSFORM TO SP OBJECT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# env <- as(env, 'Spatial')
# biotic <- as(biotic, 'Spatial')

# Model for all species
SDMdata <- SDMmodel <- SDMproj <- vector('list', nrow(sp))

for(i in 1:nrow(sp)) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 Data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset data
uid <- biotic$species == sp$species[i]
dat <- biotic[uid, 'species']

# Change data
colnames(dat@data) <- sp$species[i]
dat@data[,1] <- 1

# Biomod data
SDMdata[[i]] <- BIOMOD_FormatingData(resp.var = dat,
                                     expl.var = r,
                                     resp.name = sp$species[i],
                                     PA.nb.rep = 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 Model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic options for modelling
SDMoption <- biomod2::BIOMOD_ModelingOptions()

# SDM model
SDMmodel[[i]] <- biomod2::BIOMOD_Modeling(data = SDMdata[[i]],
                                          models = "GLM",
                                          model.options = SDMoption)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 diagnostic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# biomod2::get_evaluations(SDMmodel[[i]])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 projections
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SDMproj[[i]] <- biomod2::BIOMOD_Projection(modeling.output = SDMmodel[[i]],
                                          new.env = r,
                                          proj.name = '-',
                                          selected.models = 'all',
                                          binary.meth = 'TSS',
                                          compress = 'xz',
                                          clamping.mask = F,
                                          output.format = '.grd')

SDM <- list(SDMdata, SDMmodel, SDMproj)
save(SDM, file = './BioticDistribution/BIOMOD/Data/SDM.RData')
}
