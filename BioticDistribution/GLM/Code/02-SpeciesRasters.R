# Libraries
library(sf)
library(sp)
library(tidyverse)
library(magrittr)
library(raster)

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. PSEUDO ABSENCES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate pseudo absences for each species for SDM analyses

# Study area polygon for pseudo absences
load('eDriversGrids/Data/egslSimple.RData')

# Load environmental raster to get extent of random sampling
load('./BioticDistribution/GLM/Data/envRasters.RData')

# Study area extent
ext <- extent(env)

# Empty raster
r <- raster(crs = projection(env), resolution = 1000, ext = ext)

# Raster for each species
species <- vector('list', nrow(sp))
names(species) <- sp$species

for(i in 1:nrow(sp)) {
  cat(i, ' of ', nrow(sp), '\r')
  # Subset data
  uid <- biotic$species == sp$species[i]
  dat <- biotic[uid, 'species']

  # Change data
  colnames(dat)[1] <- 'sp'
  dat[,1] <- 1

  # Pseudo absences
  # Distance to keep out of sampling
  keep <- 5000 # 2km buffer
  pseudo <- st_buffer(dat, keep) %>%
            st_union() %>%
            st_difference(egslSimple, .) %>%
            st_sample(size = nrow(dat)*2) %>%
            st_sf() %>%
            mutate(sp = 0)

  # Bind objects
  dat <- rbind(dat, pseudo)
  # plot(dat, cex = .2)

  # Change name
  colnames(dat)[1] <- sp$species[i]

  # Transform as sp object
  dat <- as(dat, 'Spatial')
  nm <- names(dat)

  # Rasterize
  dat <- rasterize(dat, r, field = names(dat))
  names(dat) <- nm

  # Add to list
  species[[i]] <- dat
}

# Raster stack
species <- stack(species)

# Export
save(species, file = './BioticDistribution/GLM/Data/speciesRasters.RData')
