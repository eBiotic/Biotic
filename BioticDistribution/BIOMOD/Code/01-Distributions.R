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


save(r, file = './Environment/EnvironmentRaster.RData')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. LOAD BIOTIC DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-= #
#   Plurispecific
# =-=-=-=-=-=-=-=-=-= #
# Plurispecific survey: taking the absences in this dataset as true absences
load('BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluri.RData')

# Remove duplicates, if any
uid <- duplicated(pluri)
pluri <- pluri[!uid, ]

# Get absences
pluri <- pluri %>%
         tidyr::spread(species, presence, fill = 0) %>%
         gather("species", "presence", -year, -month, -day, -latitude_st, -latitude_end, -longitude_st, -longitude_end, -surveyID, -releve, -geometry)

# =-=-=-=-=-=-=-=-=-= #
#   Other datasets
# =-=-=-=-=-=-=-=-=-= #
load('BioticData/ObsMer_1999-2015_MPO/Data/Biotic/SeaObserver.RData')
obs <- filter(obs, year > 2009)
load('BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleFixed.RData')
load('BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleMobile.RData')
load('BioticData/ZIF-Fisheries-2010-2015/Data/Biotic/zif.RData')

# Select single column
pluri <- pluri[,c('species', 'presence')]
obs <- obs[,c('species', 'presence')]
sentFix <- sentFix[,c('species', 'presence')]
sentMob <- sentMob[,c('species', 'presence')]
zif <- zif[,c('species', 'presence')]

# Single object
biotic <- rbind(pluri, obs, sentFix, sentMob, zif)

# Transform as sp object
biotic <- as(biotic, 'Spatial')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. LOAD SPECIES LIST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('BioticData/SpeciesList/Data/SpeciesList.RData')

# Minimum record number accepted (subjective)
minRec <- 30

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
  cat(i, ' of ', nrow(sp), '\r')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 Data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset data
uid <- biotic$species == sp$species[i]
dat <- biotic[uid, 'presence']

# # Change data
# colnames(dat@data) <- sp$species[i]
# dat@data[,1] <- 1

# Biomod data
SDMdata[[i]] <- BIOMOD_FormatingData(resp.var = dat,
                                     expl.var = env[[envCov]],
                                     resp.name = sp$species[i],
                                     PA.nb.rep = 0)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 Model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic options for modelling
SDMoption <- biomod2::BIOMOD_ModelingOptions()

# SDM model
SDMmodel[[i]] <- biomod2::BIOMOD_Modeling(data = SDMdata[[i]],
                                          models = "RF",
                                          model.options = SDMoption)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 diagnostic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# biomod2::get_evaluations(SDMmodel[[i]])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. BIOMOD2 projections
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SDMproj[[i]] <- biomod2::BIOMOD_Projection(modeling.output = SDMmodel[[i]],
                                          new.env = env[[envCov]],
                                          proj.name = '-',
                                          selected.models = 'all',
                                          binary.meth = 'TSS',
                                          compress = 'xz',
                                          clamping.mask = F,
                                          output.format = '.grd')
plot(SDMproj[[i]])
SDM <- list(SDMdata, SDMmodel, SDMproj)
save(SDM, file = './BioticDistribution/BIOMOD/Data/SDM.RData')
}


for(i in 1:nrow(sp)) {
uid <- biotic$species == sp$species[i]
dat <- biotic[uid, 'presence']
cat(sp$species[i], table(dat$presence), '\n')
}
