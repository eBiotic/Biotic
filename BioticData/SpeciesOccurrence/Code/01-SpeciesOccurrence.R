# source('./BioticData/SpeciesOccurrence/Code/01-SpeciesOccurrence.R')
# Libraries
library(sf)
library(tidyverse)
library(magrittr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Biotic data
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species list
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save(biotic, file = './BioticData/SpeciesOccurrence/Data/SpeciesOccurrences.RData')
