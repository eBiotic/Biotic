# Libraries
library(sf)
library(tidyverse)
library(magrittr)
library(raster)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          Environment
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
load('./Environment/Data/EnvironmentRasters.RData')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          Occurrences
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
load('./BioticData/SpeciesOccurrence/Data/SpeciesOccurrences.RData')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#         Species names
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
sp <- sort(unique(biotic$species))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rasters
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Study area extent
ext <- extent(env)

# Empty raster with same structure as rasterized environmental data
r <- raster(crs = projection(env), resolution = 1000, ext = ext)

# Raster for each species
species <- vector('list', length(sp))
names(species) <- sp

for(i in 1:length(sp)) {
  cat(i, ' of ', length(sp), '\r')

  # Subset data
  uid <- biotic$species == sp[i]
  dat <- biotic[uid, 'presence']

  # Transform as sp object
  dat <- as(dat, 'Spatial')

  # Rasterize
  dat <- rasterize(dat, r, field = names(dat))
  names(dat) <- sp[i]

  # Add to list
  species[[i]] <- dat
}

# Raster stack
species <- stack(species)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save(species, file = './BioticData/SpeciesOccurrence/Data/SpeciesRasters.RData')
