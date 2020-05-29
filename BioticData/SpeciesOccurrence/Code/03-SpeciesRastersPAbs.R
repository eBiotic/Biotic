# source('./BioticData/SpeciesOccurrence/Code/03-SpeciesRastersPAbs.R')
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
#           Study area
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
load('./eDriversGrids/Data/egslSimple.RData')

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

# Buffer around obserations to block random sampling
# Distance to keep out of sampling
keep <- 5000 # 5km buffer

# Raster for each species
species <- vector('list', length(sp))
names(species) <- sp

for(i in 1:length(sp)) {
  cat(i, ' of ', length(sp), '\r')

  # Subset data
  uid <- biotic$species == sp[i]
  dat <- biotic[uid, 'presence']

  # Absences and presences
  # Pseudo absences if number of observed absences is lower
  # than number of observations
  obs <- data.frame(presence = sum(dat$presence == 1),
                    absence = sum(dat$presence == 0))

  if (obs$absence < obs$presence & obs$absence < 1000) {
    # Sample size
    samp <- ifelse(obs$presence < 1000,
                   obs$presence - obs$absence,
                   1000 - obs$absence)

    # Pseudo-absences
    pseudo <- dat %>%
              filter(presence == 1) %>%
              st_buffer(keep) %>%
              st_union() %>%
              st_difference(egslSimple, .) %>%
              st_sample(size = samp) %>%
              st_sf() %>%
              mutate(presence = 0)
    # Bind objects
    dat <- rbind(dat, pseudo)
  }

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
save(species, file = './BioticData/SpeciesOccurrence/Data/SpeciesRastersPAbs.RData')
