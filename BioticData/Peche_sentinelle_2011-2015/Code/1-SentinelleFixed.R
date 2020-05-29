# source('./BioticData/Peche_sentinelle_2011-2015/Code/1-SentinelleFixed.R')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize comes from DFO and cannot be shared
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './BioticData/Peche_sentinelle_2011-2015/Data/RawData'

# Data will need to be archived to Zenodo with restricted access and downloaded
# using an access token.
# Eventually it would ideally be part of the SLGO web portal

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# File name
fileName <- dir(output, pattern = '.zip')

# Unzip kmz file
unzip(zipfile = paste0(output, '/', fileName),
      exdir = output)


# Import data
sentFix <- read.table(file = paste0(output, '/PSF_QC-TN_Rel18-22.txt'),
                      header = TRUE,
                      sep = ";",
                      dec = '.',
                      stringsAsFactors = F)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               DATA INFORMATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -----------------------------------------------------------------------------
# DATA: Sentinel fisheries for fixed and mobile gear 2011-2015
#
# INFORMATION:
#
# /*****  Structure du fichier de données pour les pêches sentinelles avec engins fixes *****/
#
#     Source ";"     /* Source de données, Pêche sentinelle Qc=1, Pêche sentinelle TN=2*/
#     No_Rel ";"     /* Numéro du relevé */
#     NoStn ";"      /* Numéro de la station */
#     EngGen ";"     /* code de l'engin de pêche utilisé */
#     DatLevFx ";"   /* Date de l'activité de pêche */
#     EspGen ";"     /* Code d'espèce */
#     N_EspSci ";"   /* Nom scientifique de l'espèce*/
#     N_EspF ";"     /* Nom français de l'espèce */
#     LaLevFx ";"    /* Positionnement de l'engin de pêche, Latitude unité=ddmm.%% */
#     LoLevFX ";"    /* Positionnement de l'engin de pêche, Longitude unité=ddmm.%%  */
#     DpLevFx ";"    /* Profondeur, unité=m */
#     WCapOri ";"    /* Poids de la capture, unité=kg */
#     SNb_Capt;      /* Nombre individus capturés */
# # -----------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform data columns in correct type
sentFix[, 13] <- as.numeric(paste(sentFix[,13]))
sentFix$DatLevFx <- as.Date(sentFix$DatLevFx)

# Trim spaces on species names
sentFix$N_EspSci <- stringr::str_trim(sentFix$N_EspSci, 'both')

# Remove empty species
sentFix <- sentFix[!sentFix$N_EspSci == '', ]

# Transforming ddmm.%% to degree decimals
# See dmsTOdd() for more information
source('./BioticData/Peche_sentinelle_2011-2015/Code/dmsTOdd.R')
sentFix[, "LaLevFx"] <- unlist(lapply(X = sentFix[, "LaLevFx"], FUN = dmsTOdd))
sentFix[, "LoLevFX"] <- unlist(lapply(X = sentFix[, "LoLevFX"], FUN = dmsTOdd, type = 'long'))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  REMOVE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All information in './SpeciesFormatting/Code/removeTaxa.R'
# source('./SpeciesFormatting/Code/removeTaxa.R')
load('./BioticData/SpeciesFormatting/Data/removeTaxa.RData')
# zifSp <- zifSp[!zifSp$species %in% removeTaxa, ] # Remove from species list
sentFix <- sentFix[!sentFix$N_EspSci %in% removeTaxa, ] # Remove from dataset


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 COMBINE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Which taxa should be combined?
# All information in './SpeciesFormatting/Code/combineTaxa.R'
load('./BioticData/SpeciesFormatting/Data/combineTaxa.RData')

# Transform as list
combineTaxa <- str_split(combineTaxa, ' \\| ')
# for(i in 1:length(combineTaxa)) print(c(paste(combineTaxa[[i]][1]), paste(any(combineTaxa[[i]] %in% obsSp$species))))

# Duplicate taxa names in northPluri to match taxon name chosen for aggregation
for(i in 1:length(combineTaxa)) {
  for(j in 2:length(combineTaxa[[i]])) {
    id0 <- sentFix$N_EspSci == combineTaxa[[i]][j]
    sentFix$N_EspSci[id0] <- combineTaxa[[i]][1]
  }
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  SPECIES LIST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species list
sentSp <- table(sentFix$N_EspSci) %>%
          as.data.frame(stringsAsFactors = F) %>%
          rename(species = Var1,
                 CountRec = Freq)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SPATIAL OBJECTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Point shapefile
sentFix <- st_as_sf(sentFix, coords = c('LoLevFX','LaLevFx'), crs = 4326, remove = F) %>%
           st_transform(crs = 32198)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sentFix <- sentFix %>%
           mutate(year = format(DatLevFx,"%Y"),
                  month = format(DatLevFx,"%m"),
                  day = format(DatLevFx,"%d"),
                  surveyID = paste(No_Rel, No_Stn, sep = '-'),
                  presence = 1) %>%
           rename(latitude = LaLevFx,
                  longitude = LoLevFX,
                  species = N_EspSci,
                  gear = EngGen,
                  biomass = WCapOri) %>%
           select(-Source, -No_Rel, -DatLevFx, -EspGen, -N_EspF, -DpLevFx,
                  -SNb_Capt) %>%
           filter(presence > 0)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               SELECT OBSERVATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Choose points located in the St. Lawrence only
# Some are obvious mistakes (inland fishing events)
# Load EGSL outline
load('./eDriversGrids/Data/egslSimple.RData')

# Intersect fisheries events with EGSL
sentFix <- sentFix %>%
           st_intersects(egslSimple, .) %>%
           unlist() %>%
           sentFix[., ]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(sentFix, file = './BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleFixed.RData')
save(sentSp, file = './BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleFixedSP.RData')
